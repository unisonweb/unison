{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Compiled programs: the format of a .uc file, and loading one to run.
-- Both ucm (run.compiled) and unisonrt run them through here.
module Unison.Runtime.Standalone
  ( StoredCache (SCache),
    Compiled (..),
    ConstructorNames,
    putCompiled,
    decodeStandalone,
    putStoredCache,
    getStoredCache,
    putTextBig,
    getTextBig,
    standalone,
    loadStandalone,
    runMainComb,
  )
where

import Control.Concurrent.STM as STM
import Control.Monad.State.Strict
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (absurd)
import Foreign.Ptr (nullPtr)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as RF
import Unison.Runtime.ANF as ANF
import Unison.Runtime.ANF.Optimize (OptInfos)
import Unison.Runtime.ANF.Serialize as ANF (getGroupCurrent, getOptInfos, putGroup, putOptInfos)
import Unison.Runtime.Builtin
import Unison.Runtime.Exception (RuntimeExn, die)
import Unison.Runtime.JIT (JIT, initialNativeCount, jitCompileGroup, jitConfig, startJIT)
import Unison.Runtime.JIT.Config qualified as JIT
import Unison.Runtime.MCode
  ( Args (..),
    CombIx (..),
    FieldRef (..),
    GCombs,
    GInstr (..),
    GSection (..),
    RCombs,
    RecordFieldMappings (..),
    RefNums (..),
    absurdCombs,
    attachNativeCells,
    combTypes,
    emitComb,
    emptyRCtx,
    emptyRNs,
    newNativeCellPool,
    prettyComb,
    resolveCombs,
    sanitizeCombsOfForeignFuncs,
    takeNativeCells,
  )
import Unison.Runtime.MCode.Serialize
import Unison.Runtime.Machine
  ( CCache (..),
    Combs,
    Tracer (..),
    eval0,
    initialNativeCells,
    preEvalTopLevelConstants,
    refLookup,
    resolveSection,
  )
import Unison.Runtime.Serialize as SER
import Unison.Runtime.Serialize.Get
import Unison.Runtime.Stack
import Unison.Runtime.TypeNumbers qualified as TN
import Unison.Runtime.TypeTags qualified as TT
import Unison.Runtime.Wide (setWide, wideOn)
import Unison.Symbol (Symbol)
import Unison.Util.BiMap qualified as BM
import Unison.Util.BiMap qualified as BiMap
import Unison.Util.EnumContainers as EC
import UnliftIO qualified

-- Slightly inefficient method of encoding text. Matches the old way of
-- encoding e.g. the compiled version below. Compiled code is not
-- cross compatible with other versions, but keeping this format
-- allows older versions to fail more gracefully, rather than
-- encountering serialization errors.
putTextBig :: Text -> Builder
putTextBig text =
  BU.word32BE (fromIntegral $ B.length bs) <> BU.byteString bs
  where
    bs = encodeUtf8 text

getTextBig :: (PrimBase m) => Get m Text
getTextBig = do
  len <- getWord32be
  bs <- B.copy <$> getByteString (fromIntegral len)
  pure $ decodeUtf8 bs

-- | A compiled program, as a .uc file holds it.
data Compiled = Compiled
  { -- | the version of Unison that compiled it, which only the same
    -- version can run
    compiledVersion :: Text,
    -- | the hash of its main term, abbreviated
    compiledMain :: Text,
    compiledEntry :: CombIx,
    compiledCode :: StoredCache,
    -- | the codebase's names for the constructors of the types its code
    -- mentions, for printing its values without the codebase
    compiledConstructorNames :: ConstructorNames
  }

-- | For each type, the names of its constructors, by constructor number.
type ConstructorNames = Map Reference [Text]

putCompiled :: Compiled -> Builder
putCompiled (Compiled v rf ix sto names) =
  putTextBig v
    <> putTextBig rf
    <> putCombIx ix
    <> putStoredCache sto
    <> putMap putReference (putFoldable putText) names

-- | A .uc file's program. A file without constructor names (one written
-- before they were added) reads with none.
decodeStandalone :: B.ByteString -> IO (Either String Compiled)
decodeStandalone b = runGetCatchIO g b
  where
    g = do
      v <- getTextBig
      rf <- getTextBig
      ix <- getCombIx
      sto <- getStoredCache
      left <- remaining
      names <-
        if left == 0
          then pure mempty
          else getMap getReference (getList getText)
      pure (Compiled v rf ix sto names)

-- | Takes on what a compiled program was compiled with (its replacement
-- mode, calling convention and type numbering), restores its code, and
-- hands it to the JIT: everything before running its main.
loadStandalone :: (Bool -> Val -> Tracer) -> JIT -> Bool -> StoredCache -> IO (CCache ())
loadStandalone debugText jit sandboxed sc@(SCache _ _ _ _ _ trs _ _ _ _ _ _ _ repl wide) = do
  -- The file's replacement mode and calling convention, before anything
  -- decodes a value, consults the foreign function table or lays out a frame
  JIT.setReplacements repl
  setWide wide
  -- The file's type numbering, which its MCode has baked into its packed
  -- tags, before any code is restored (Unison.Runtime.TypeNumbers)
  TN.seedTypeNumbers trs
  -- What startRuntime and the loader (cacheAdd) do for the JIT on the
  -- codebase path, for a compiled program
  startJIT jit
  cc <- restoreCache debugText jit sandboxed sc
  combRefs' <- readTVarIO (combRefs cc)
  combs' <- readTVarIO (combs cc)
  when (JIT.dumpMCode (jitConfig jit)) $
    for_ (EC.mapToList combs') \(w, cmbs) ->
      for_ (EC.mapToList cmbs) \(i, c) -> JIT.jitDump (prettyComb w i c "")
  for_ (EC.mapToList combs') \(w, cmbs) ->
    for_ (EC.lookup w combRefs') \r -> jitCompileGroup jit sandboxed r w cmbs
  pure cc

-- | Runs a program's main, which takes unit.
runMainComb :: CombIx -> CCache () -> IO (Either RuntimeExn ())
runMainComb init cc = do
  rSection <- resolveSection cc $ Ins (Pack TT.unitTag ZArgs) $ Call True init init (VArg1 0)
  UnliftIO.try $ eval0 cc Nothing rSection

-- | A version of the Code Cache designed to be serialized to disk as
-- standalone bytecode.
data StoredCache
  = SCache
      (EnumMap Word64 Combs)
      (EnumMap Word64 Reference)
      -- the unoptimized copies of the rewritten groups, by the group's
      -- number (CCache.baseNums): a program's compiled code carries its
      -- MCode, so it carries these, which a continuation it reads resumes
      (EnumMap Word64 Word64)
      (EnumSet Word64)
      (OptInfos Reference Symbol)
      -- the numbering of the types the code mentions, which its packed
      -- tags carry: the process that runs it takes it on before loading
      -- anything (Unison.Runtime.TypeNumbers)
      (EnumMap Word64 Reference)
      Word64
      Word64
      (Map Reference (SuperGroup Reference Symbol))
      (Map Reference Word64)
      (BM.BiMap ANF.RecordSchema ANF.RecordRef)
      (Map Reference (Set Reference))
      RecordFieldMappings
      -- which of base's Haskell replacements the code was compiled with
      -- (JIT.Config): its MCode has them baked in, so the process that runs
      -- it has to decode values the same way, whatever its own environment
      -- says
      JIT.Replacements
      -- whether the code was compiled under the wide calling convention
      -- (Unison.Runtime.Wide), for the same reason
      Bool
  deriving (Show, Eq)

putStoredCache :: StoredCache -> Builder
putStoredCache (SCache cs crs bns cacheableCombs oinfo trs ftm frs int rtm rsLookup sbs rfm repl wide) =
  putEnumMap putNat (putEnumMap putNat (putComb absurd)) cs
    <> putEnumMap putNat putReference crs
    <> putEnumMap putNat putNat bns
    <> putEnumSet putNat cacheableCombs
    <> putOptInfos oinfo
    <> putEnumMap putNat putReference trs
    <> putNat ftm
    <> putNat frs
    <> putMap putReference (putGroup mempty False) int
    <> putMap putReference putNat rtm
    <> putMap putRecordSchema putRecordRef (BM.forward rsLookup)
    <> putMap putReference (putFoldable putReference) sbs
    <> putRecordFieldMappings rfm
    <> BU.word8 (case repl of JIT.AllReplacements -> 0; JIT.TaggedReplacements -> 1; JIT.NoReplacements -> 2)
    <> BU.word8 (if wide then 1 else 0)

putRecordFieldMappings :: RecordFieldMappings -> Builder
putRecordFieldMappings (RecordFieldMappings next rfm) =
  putFieldRef next <> putMap putText putFieldRef (BiMap.toMapL rfm)

getRecordFieldMappings :: (PrimBase m) => Get m RecordFieldMappings
getRecordFieldMappings = RecordFieldMappings <$> getFieldRef <*> (BiMap.fromMap <$> getMap getText getFieldRef)

putFieldRef :: FieldRef -> Builder
putFieldRef (FieldRef w) = putVarInt w

getFieldRef :: (PrimBase m) => Get m FieldRef
getFieldRef = FieldRef <$> getVarInt

getStoredCache :: (PrimBase m) => Get m StoredCache
getStoredCache =
  SCache
    <$> getEnumMap getNat (getEnumMap getNat getComb)
    <*> getEnumMap getNat getReference
    <*> getEnumMap getNat getNat
    <*> getEnumSet getNat
    <*> getOptInfos
    <*> getEnumMap getNat getReference
    <*> getNat
    <*> getNat
    <*> getMap getReference getGroupCurrent
    <*> getMap getReference getNat
    <*> (BM.fromMap <$> getMap getRecordSchema getRecordRef)
    <*> getMap getReference (Set.fromList <$> getList getReference)
    <*> getRecordFieldMappings
    <*> getReplacements
    <*> getWide
  where
    getWide =
      getWord8 >>= \case
        0 -> pure False
        1 -> pure True
        n -> fail ("getStoredCache: unknown calling convention " ++ show n)
    getReplacements =
      getWord8 >>= \case
        0 -> pure JIT.AllReplacements
        1 -> pure JIT.TaggedReplacements
        2 -> pure JIT.NoReplacements
        n -> fail ("getStoredCache: unknown replacement mode " ++ show n)

-- | The code cache of a compiled program, every combinator with a native
-- code cell, the program's top-level constants evaluated. The tracer
-- renders the values Debug.trace is given.
restoreCache :: (Bool -> Val -> Tracer) -> JIT -> Bool -> StoredCache -> IO (CCache ())
restoreCache debugText jit sandboxed (SCache cs crs bns cacheableCombs opt _ ftm frs int rtm recSchemas sbs rfm _ _) = do
  -- Every combinator gets a native code cell, as it would loading from a
  -- codebase (see JIT/design.md, "Where cells live"); without one the JIT
  -- never compiles it. The cells come from the cache's pool, which keeps
  -- enough spare for code the program loads later (Code.load).
  let uncelled =
        srcCombs
          & sanitizeCombsOfForeignFuncs sandboxed sandboxedForeignFuncs
      (cellCount, _) = attachNativeCells nullPtr uncelled
  pool0 <- newNativeCellPool (initialNativeCount jit) (cellCount + initialNativeCells)
  (cells, pool) <- case takeNativeCells cellCount pool0 of
    Just taken -> pure taken
    Nothing -> die [] "restoreCache: the native cell pool is too small"
  let unresolvedCombs :: EnumMap Word64 (GCombs Val CombIx)
      unresolvedCombs = absurdCombs (snd (attachNativeCells cells uncelled))
      combs :: EnumMap Word64 (RCombs Val)
      combs = resolveCombs Nothing unresolvedCombs
  cc <-
    CCache sandboxed debugText ()
      <$> newTVarIO srcCombs
      <*> newTVarIO combs
      <*> newTVarIO (crs <> builtinTermBackref)
      <*> newTVarIO bns
      <*> newTVarIO cacheableCombs
      <*> newTVarIO (opt <> builtinOptInfo)
      <*> newTVarIO ftm
      <*> newTVarIO frs
      <*> newTVarIO int
      <*> newTVarIO (rtm <> builtinTermNumbering)
      <*> newTVarIO recSchemas
      <*> newTVarIO (sbs <> baseSandboxInfo)
      <*> newTVarIO newRFM
      <*> newTVarIO pool
      <*> pure jit
  let (unresolvedCacheableCombs, unresolvedNonCacheableCombs) =
        unresolvedCombs
          & EC.mapToList
          & foldMap
            ( \(k, v) ->
                if k `member` cacheableCombs
                  then (EC.mapSingleton k v, mempty)
                  else (mempty, EC.mapSingleton k v)
            )
  preEvalTopLevelConstants unresolvedCacheableCombs unresolvedNonCacheableCombs cc
  pure cc
  where
    rns = emptyRNs {dnum = refLookup "ty" builtinTypeNumbering}
    rf k = builtinTermBackref ! k
    builtinCombs :: EnumMap Word64 Combs
    newRFM :: RecordFieldMappings
    (builtinCombs, newRFM) =
      flip
        runState
        rfm
        ( numberedTermLookup
            & traverseWithKey
              ( \k v -> do
                  fst <$> emitComb @Symbol rns (rf k) k emptyRCtx (0, v)
              )
        )
    srcCombs :: EnumMap Word64 Combs
    srcCombs = builtinCombs <> cs

traceNeeded ::
  Reference ->
  Map Reference (SuperGroup Reference Symbol) ->
  IO (Map Reference (SuperGroup Reference Symbol))
traceNeeded init src = go mempty init
  where
    go acc nx
      | RF.isBuiltin nx = pure acc
      | Map.member nx acc = pure acc
      | Just co <- Map.lookup nx src =
          foldlM go (Map.insert nx co acc) (groupTermLinks co)
      | otherwise =
          die [] $ "traceNeeded: unknown combinator: " ++ show nx

buildSCache ::
  EnumMap Word64 Reference ->
  EnumMap Word64 Combs ->
  EnumMap Word64 Word64 ->
  EnumSet Word64 ->
  OptInfos Reference Symbol ->
  EnumMap Word64 Reference ->
  Word64 ->
  Word64 ->
  Map Reference (SuperGroup Reference Symbol) ->
  Map Reference Word64 ->
  BM.BiMap ANF.RecordSchema ANF.RecordRef ->
  Map Reference (Set Reference) ->
  RecordFieldMappings ->
  JIT.Replacements ->
  Bool ->
  StoredCache
buildSCache crsrc cssrc bnsrc cacheableCombs optsrc trsrc ftm frs int rtmsrc rsLookup sndbx rfm repl wide =
  SCache
    cs
    crs
    bns
    cacheableCombs
    opt
    trs
    ftm
    frs
    int
    rtm
    rsLookup
    (restrictTmR sndbx)
    rfm
    repl
    wide
  where
    termRefs = Map.keysSet int

    -- Retain just the Reference->Word mappings for needed code
    rtm :: Map Reference Word64
    rtm = restrictTmR rtmsrc

    -- Retain numbers that correspond to the above termRefs
    combKeys :: EnumSet Word64
    combKeys = foldMap setSingleton rtm

    crs = restrictTmW crsrc

    -- the unoptimized copies of the needed groups come along, under their
    -- own numbers
    bns = restrictKeys bnsrc combKeys

    cs :: EnumMap Word64 Combs
    cs = restrictKeys cssrc (combKeys <> foldMap setSingleton bns)

    opt = bimap restrictTmR restrictTmR optsrc

    typeKeys = setFromList $ (foldMap . foldMap) combTypes cs
    trs = restrictTyW trsrc

    restrictTmW :: EnumMap Word64 a -> EnumMap Word64 a
    restrictTmW m = restrictKeys m combKeys
    restrictTmR :: Map Reference a -> Map Reference a
    restrictTmR m = Map.restrictKeys m termRefs

    restrictTyW m = restrictKeys m typeKeys

standalone :: CCache () -> Word64 -> IO StoredCache
standalone cc init =
  readTVarIO (combRefs cc) >>= \crs ->
    case EC.lookup init crs of
      Just rinit ->
        buildSCache crs
          <$> readTVarIO (srcCombs cc)
          <*> readTVarIO (baseNums cc)
          <*> readTVarIO (cacheableCombs cc)
          <*> readTVarIO (optInfos cc)
          <*> TN.typeReferences
          <*> readTVarIO (freshTm cc)
          <*> readTVarIO (freshRecSchema cc)
          <*> (readTVarIO (intermed cc) >>= traceNeeded rinit)
          <*> readTVarIO (refTm cc)
          <*> readTVarIO (recordRefs cc)
          <*> readTVarIO (sandbox cc)
          <*> readTVarIO (recordFieldMappings cc)
          <*> pure JIT.replacements
          <*> pure wideOn
      Nothing ->
        die [] $ "standalone: unknown combinator: " ++ show init

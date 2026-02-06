{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.ANF.Serialize where

import Control.Monad (replicateM)
import Control.Monad.ST (ST)
import Control.Monad.State.Strict (StateT (..))
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.ByteString.Lazy qualified as L
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Map as Map (Map, fromDistinctAscList, fromList, lookup)
-- machinery for special casing maps
import Data.Map.Strict.Internal (Map (..))
import Data.Maybe (mapMaybe)
import Data.Word (Word32, Word64)
import GHC.Stack
import Numeric.Natural (Natural)
import Unison.ABT.Normalized (Term (..))
import Unison.Builtin.Decls (mapBin, mapRef, mapTip)
import Unison.Reference (Reference, Reference' (Builtin), pattern Derived)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.ANF.Optimize as ANF
import Unison.Runtime.ANF.POp as ANF
import Unison.Runtime.ANF.Serialize.CodeV4 qualified as CodeV4
import Unison.Runtime.ANF.Serialize.Tags
import Unison.Runtime.ANF.Serialize.ValueV5 qualified as ValueV5
import Unison.Runtime.Exception (die, exn)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.Referenced
import Unison.Runtime.Serialize
import Unison.Runtime.Serialize.Get
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Type (ANFBlank), Var (..))
import Prelude hiding (getChar, putChar)

-- Version information is threaded through to allow handling
-- different formats. Transfer means that it is for saving
-- code/values to be restored later. Hash means we're just getting
-- bytes for hashing, so we don't need perfect information.
data Version = Transfer Word32 | Hash Word32
  deriving (Show)

index :: (Eq v) => [v] -> v -> Maybe Word64
index ctx u = go 0 ctx
  where
    go !_ [] = Nothing
    go n (v : vs)
      | v == u = Just n
      | otherwise = go (n + 1) vs

deindex :: (HasCallStack) => [v] -> Word64 -> v
deindex [] _ = exn [] "deindex: bad index"
deindex (v : vs) n
  | n == 0 = v
  | otherwise = deindex vs (n - 1)

pushCtx :: [v] -> [v] -> [v]
pushCtx us vs = reverse us ++ vs

putIndex :: Word64 -> Builder
putIndex = putVarInt
{-# INLINE putIndex #-}

getIndex :: (PrimBase m) => Get m Word64
getIndex = getVarInt

putVar :: (Eq v) => [v] -> v -> Builder
putVar ctx v
  | Just i <- index ctx v = putIndex i
  | otherwise = exn [] "putVar: variable not in context"

getVar :: (PrimBase m) => [v] -> Get m v
getVar ctx = deindex ctx <$> getIndex

putArgs :: (Eq v) => [v] -> [v] -> Builder
putArgs ctx is = putFoldable (putVar ctx) is

getArgs :: (PrimBase m) => [v] -> Get m [v]
getArgs ctx = getList (getVar ctx)

putCCs :: [Mem] -> Builder
putCCs ccs = putLength n <> foldMap putCC ccs
  where
    n = length ccs
    putCC UN = BU.word8 0
    putCC BX = BU.word8 1

getCCs :: (PrimBase m) => Get m [Mem]
getCCs =
  getList $
    getWord8 <&> \case
      0 -> UN
      1 -> BX
      _ -> exn [] "getCCs: bad calling convention"

-- Serializes a `SuperGroup`.
--
-- The Reference map allows certain term references to be switched out
-- for a given 64 bit word. This is used when re-hashing intermediate
-- code. For actual serialization, the empty map should be used, so
-- that the process is reversible. The purpose of this is merely to
-- strip out (mutual/)self-references when producing a byte sequence
-- to recompute a hash of a connected component of intermediate
-- definitons, since it is infeasible to
--
-- The EnumMap associates 'foreign' operations with a textual name
-- that is used as the serialized representation. Since they are
-- generated somewhat dynamically, it is not easy to associate them
-- with a fixed numbering like we can with POps.
putGroup ::
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  SuperGroup Reference v ->
  Builder
putGroup refrep fops (Rec bs e) =
  putLength n
    <> foldMap (putComb refrep fops ctx) cs
    <> putComb refrep fops ctx e
  where
    n = length us
    (us, cs) = unzip bs
    ctx = pushCtx us []

getGroup ::
  (PrimBase m) =>
  (Var v) =>
  GDeserial m (SuperGroup Reference v)
getGroup s = do
  l <- getLength
  let n = fromIntegral l
      vs = getFresh <$> take l [0 ..]
      ctx = pushCtx vs []
  cs <- replicateM l (getComb ctx n s)
  Rec (zip vs cs) <$> getComb ctx n s

putCode :: Bool -> Code Reference -> Builder
putCode fops (CodeRep g c) = putGroup mempty fops g <> putCacheability c

getCode :: (PrimBase m) => GDeserial m (Code Reference)
getCode s = CodeRep <$> getGroup s <*> getCacheability s

putInlineInfo ::
  (Var v) =>
  [v] ->
  InlineInfo Reference v ->
  Builder
putInlineInfo ctx (InlInfo clazz expr) =
  putInlineClass clazz <> putInlineExpr ctx expr

getInlineInfo ::
  (PrimBase m, Var v) =>
  [v] ->
  Word64 ->
  GDeserial m (InlineInfo Reference v)
getInlineInfo ctx frsh s =
  InlInfo <$> getInlineClass <*> getInlineExpr ctx frsh s

putInlineExpr ::
  (Var v) =>
  [v] ->
  ANormal Reference v ->
  Builder
putInlineExpr ctx (TAbss vs body) =
  putLength (length vs)
    <> putNormal mempty True (pushCtx vs ctx) body

getInlineExpr ::
  (PrimBase m, Var v) =>
  [v] ->
  Word64 ->
  GDeserial m (ANormal Reference v)
getInlineExpr ctx frsh0 s = do
  n <- getLength
  let frsh = frsh0 + fromIntegral n
      vs = getFresh <$> take n [frsh0 ..]
  TAbss vs <$> getNormal (pushCtx vs ctx) frsh s

putOptInfos :: (Var v) => OptInfos Reference v -> Builder
putOptInfos (arities, inls) =
  putMap putReference putVarInt arities
    <> putMap putReference (putInlineInfo []) inls

-- Note: current version
getOptInfos :: (PrimBase m, Var v) => Get m (OptInfos Reference v)
getOptInfos =
  (,)
    <$> getMap getReference gInt
    <*> getMap
      getReference
      (getInlineInfo [] 0 (Transfer codeVersion, True))
  where
    gInt = getVarInt

putInlineClass :: InlineClass -> Builder
putInlineClass = \case
  AnywhereInl -> BU.word8 0
  TailInl -> BU.word8 1
  Don'tInl -> BU.word8 2

getInlineClass :: (PrimBase m) => Get m InlineClass
getInlineClass =
  getWord8 >>= \case
    0 -> pure AnywhereInl
    1 -> pure TailInl
    2 -> pure Don'tInl
    n -> unknownTag "InlineClass" n

putCacheability :: Cacheability -> Builder
putCacheability Uncacheable = BU.word8 0
putCacheability Cacheable = BU.word8 1

getCacheability :: (PrimBase m) => GDeserial m Cacheability
getCacheability (ver, _) =
  case ver of
    Transfer v
      | v >= 3 ->
          getWord8 >>= \case
            0 -> pure Uncacheable
            1 -> pure Cacheable
            n -> exn [] $ "getBLit: unrecognized cacheability byte: " ++ show n
    _ -> pure Uncacheable

putComb ::
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  SuperNormal Reference v ->
  Builder
putComb refrep fops ctx (Lambda ccs (TAbss us e)) =
  putCCs ccs <> putNormal refrep fops (pushCtx us ctx) e

getFresh :: (Var v) => Word64 -> v
getFresh n = freshenId n $ typed ANFBlank

getComb ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  GDeserial m (SuperNormal Reference v)
getComb ctx frsh0 s = do
  ccs <- getCCs
  let us = zipWith (\_ -> getFresh) ccs [frsh0 ..]
      frsh = frsh0 + fromIntegral (length ccs)
  Lambda ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh s

putNormal ::
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  ANormal Reference v ->
  Builder
putNormal refrep fops ctx tm = case tm of
  TVar v -> putTag VarT <> putVar ctx v
  TFrc v -> putTag ForceT <> putVar ctx v
  TApp f as -> putTag AppT <> putFunc refrep fops ctx f <> putArgs ctx as
  THnd rs nh _ah e ->
    putTag HandleT
      <> putRefs rs
      <> putVar ctx nh
      <> putNormal refrep fops ctx e
  TShift r v e ->
    putTag ShiftT <> putReference r <> putNormal refrep fops (v : ctx) e
  TMatch v bs ->
    putTag MatchT
      <> putVar ctx v
      <> putBranches refrep fops ctx bs
  TLit l -> putTag LitT <> putLit l
  TBLit l -> putTag BxLitT <> putLit l
  TName v (Left r) as e ->
    putTag NameRefT
      <> pr
      <> putArgs ctx as
      <> putNormal refrep fops (v : ctx) e
    where
      pr
        | Just w <- Map.lookup r refrep = BU.word64BE w
        | otherwise = putReference r
  TName v (Right u) as e ->
    putTag NameVarT
      <> putVar ctx u
      <> putArgs ctx as
      <> putNormal refrep fops (v : ctx) e
  TLets Direct us ccs l e ->
    putTag LetDirT
      <> putCCs ccs
      <> putNormal refrep fops ctx l
      <> putNormal refrep fops (pushCtx us ctx) e
  TLets (Indirect w) us ccs l e ->
    putTag LetIndT
      <> BU.word16BE w
      <> putCCs ccs
      <> putNormal refrep fops ctx l
      <> putNormal refrep fops (pushCtx us ctx) e
  v -> exn [] $ "putNormal: malformed term\n" ++ show v

getNormal ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  GDeserial m (ANormal Reference v)
getNormal ctx frsh0 s =
  getTag >>= \case
    VarT -> TVar <$> getVar ctx
    ForceT -> TFrc <$> getVar ctx
    AppT -> TApp <$> getFunc ctx s <*> getArgs ctx
    HandleT ->
      THnd
        <$> getRefs
        <*> getVar ctx
        <*> pure Nothing
        <*> getNormal ctx frsh0 s
    ShiftT ->
      flip TShift v <$> getReference <*> getNormal (v : ctx) (frsh0 + 1) s
      where
        v = getFresh frsh0
    MatchT -> TMatch <$> getVar ctx <*> getBranches ctx frsh0 s
    LitT -> TLit <$> getLit
    BxLitT -> TBLit <$> getLit
    NameRefT ->
      TName v . Left
        <$> getReference
        <*> getArgs ctx
        <*> getNormal (v : ctx) (frsh0 + 1) s
      where
        v = getFresh frsh0
    NameVarT ->
      TName v . Right
        <$> getVar ctx
        <*> getArgs ctx
        <*> getNormal (v : ctx) (frsh0 + 1) s
      where
        v = getFresh frsh0
    LetDirT -> do
      ccs <- getCCs
      let l = length ccs
          frsh = frsh0 + fromIntegral l
          us = getFresh <$> take l [frsh0 ..]
      TLets Direct us ccs
        <$> getNormal ctx frsh0 s
        <*> getNormal (pushCtx us ctx) frsh s
    LetIndT -> do
      w <- getWord16be
      ccs <- getCCs
      let l = length ccs
          frsh = frsh0 + fromIntegral l
          us = getFresh <$> take l [frsh0 ..]
      TLets (Indirect w) us ccs
        <$> getNormal ctx frsh0 s
        <*> getNormal (pushCtx us ctx) frsh s

putFunc ::
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  Func Reference v ->
  Builder
putFunc refrep allowFop ctx f = case f of
  FVar v -> putTag FVarT <> putVar ctx v
  FComb r
    | Just w <- Map.lookup r refrep -> putTag FCombT <> BU.word64BE w
    | otherwise -> putTag FCombT <> putReference r
  FCont v -> putTag FContT <> putVar ctx v
  FCon r c -> putTag FConT <> putReference r <> putCTag c
  FReq r c -> putTag FReqT <> putReference r <> putCTag c
  FPrim (Left p) -> putTag FPrimT <> putPOp p
  FPrim (Right f)
    | allowFop -> putTag FForeignT <> putFOp f
    | otherwise ->
        exn [] $ "putFunc: could not serialize foreign operation: " ++ show f

getFunc ::
  (PrimBase m, Var v) => [v] -> GDeserial m (Func Reference v)
getFunc ctx (_, allowFOp) =
  getTag >>= \case
    FVarT -> FVar <$> getVar ctx
    FCombT -> FComb <$> getReference
    FContT -> FCont <$> getVar ctx
    FConT -> FCon <$> getReference <*> getCTag
    FReqT -> FReq <$> getReference <*> getCTag
    FPrimT -> FPrim . Left <$> getPOp
    FForeignT
      | allowFOp -> FPrim . Right <$> getFOp
      | otherwise -> exn [] "getFunc: can't deserialize a foreign func"

-- Note: this numbering is derived, and so not particularly stable.
-- However, foreign functions are not serialized for interchange. This
-- is for serializing optimization information for standalaone
-- programs.
putFOp :: ForeignFunc -> Builder
putFOp = putVarInt . fromEnum

getFOp :: (PrimBase m) => Get m ForeignFunc
getFOp = toEnum <$> getVarInt

putPOp :: POp -> Builder
putPOp op
  | Just w <- Map.lookup op pop2word = BU.word16BE w
  | otherwise = exn [] $ "putPOp: unknown POp: " ++ show op

getPOp :: (PrimBase m) => Get m POp
getPOp =
  getWord16be >>= \w -> case Map.lookup w word2pop of
    Just op -> pure op
    Nothing -> exn [] "getPOp: unknown enum code"

putLit :: Lit Reference -> Builder
putLit (I i) = putTag IT <> putInt i
putLit (N n) = putTag NT <> putNat n
putLit (F f) = putTag FT <> putFloat f
putLit (T t) = putTag TT <> putText (Util.Text.toText t)
putLit (C c) = putTag CT <> putChar c
putLit (LM r) = putTag LMT <> putReferent r
putLit (LY r) = putTag LYT <> putReference r

getLit :: (PrimBase m) => Get m (Lit Reference)
getLit =
  getTag >>= \case
    IT -> I <$> getInt
    NT -> N <$> getNat
    FT -> F <$> getFloat
    TT -> T . Util.Text.fromText <$> getText
    CT -> C <$> getChar
    LMT -> LM <$> getReferent
    LYT -> LY <$> getReference

putBLit :: Version -> BLit Reference -> Builder
putBLit _ (Text t) = putTag TextT <> putText (Util.Text.toText t)
putBLit v (List s) = putTag ListT <> putFoldable (putValue v) s
putBLit _ (TmLink r) = putTag TmLinkT <> putReferent r
putBLit _ (TyLink r) = putTag TyLinkT <> putReference r
putBLit _ (Bytes b) = putTag BytesT <> putBytes b
putBLit v (Quote vl) = putTag QuoteT <> putValue v vl
putBLit v (Code (CodeRep sg ch)) =
  putTag tag <> putGroup mempty False sg
  where
    -- Hashing treats everything as uncacheable for consistent
    -- results.
    tag
      | Cacheable <- ch,
        Transfer _ <- v =
          CachedCodeT
      | otherwise = CodeT
putBLit _ (BArr a) = putTag BArrT <> putByteArray a
putBLit _ (Pos n) = putTag PosT <> putPositive n
putBLit _ (Neg n) = putTag NegT <> putPositive n
putBLit _ (Char c) = putTag CharT <> putChar c
putBLit _ (Float d) = putTag FloatT <> putFloat d
putBLit v (Arr a) = putTag ArrT <> putFoldable (putValue v) a
putBLit _ (Map _) = exn [] "putBLit: impossible Map"
putBLit _ (BigInt i) = putTag BigIntT <> putInteger i
putBLit _ (BigNat n) = putTag BigNatT <> putNatural n

-- Serialize a Natural as a length-prefixed list of Word64 chunks (little-endian)
putNatural :: Natural -> Builder
putNatural n = putLength (length chunks) <> foldMap BU.word64BE chunks
  where
    chunks = naturalToWord64s n

-- Convert a Natural to a list of Word64 chunks (least significant first)
-- Uses accumulator for tail recursion
naturalToWord64s :: Natural -> [Word64]
naturalToWord64s = go []
  where
    go !acc 0 = acc
    go !acc n = go (fromIntegral (n `mod` (2 ^ (64 :: Int))) : acc) (n `shiftR` 64)

-- Deserialize a Natural from a list of Word64 chunks
getNatural :: (PrimBase m) => Get m Natural
getNatural = do
  len <- getLength
  chunks <- replicateM len getWord64be
  pure $ word64sToNatural chunks

-- Convert a list of Word64 chunks (most significant first after reversal) back to Natural
word64sToNatural :: [Word64] -> Natural
word64sToNatural = foldl' (\acc w -> acc `shiftL` 64 .|. fromIntegral w) 0

-- Serialize an Integer as a sign byte followed by the Natural magnitude
putInteger :: Integer -> Builder
putInteger n
  | n >= 0 = BU.word8 0 <> putNatural (fromInteger n)
  | otherwise = BU.word8 1 <> putNatural (fromInteger (abs n))

-- Deserialize an Integer
getInteger :: (PrimBase m) => Get m Integer
getInteger = do
  sign <- getWord8
  mag <- getNatural
  pure $ if sign == 0 then toInteger mag else negate (toInteger mag)

-- special function for serializing a list of pairs as a Unison map.
-- This allows us to avoid inflating the map to a unison value during
-- the interpreter->interchange step, which is expensive.
--
-- It is assumed that the list is in ascending order. We always
-- produce an ascending map during reflection, but if you deserialize
-- a non-ascending list and re-serialize using an old version, you
-- will get an invalid map. However, you might also just receive an
-- invalid serialized map.
putAsMap ::
  Version -> [(Value Reference, Value Reference)] -> Builder
putAsMap v = putter . fromDistinctAscList
  where
    putter Tip =
      putTag DataT
        <> putReference mapRef
        <> BU.word64BE mapTip
        <> putLength (0 :: Int) -- subfields
    putter (Bin sz k e l r) =
      putTag DataT
        <> putReference mapRef
        <> BU.word64BE mapBin
        <> putLength (5 :: Int)
        <> putValue v (BLit . Pos $ fromIntegral sz)
        <> putValue v k
        <> putValue v e
        <> putter l
        <> putter r

getBLit :: (PrimBase m) => GDeserial m (BLit Reference)
getBLit s@(v, fo) =
  getTag >>= \case
    TextT -> Text . Util.Text.fromText <$> getText
    ListT -> List <$> getSeq (getValue s)
    TmLinkT -> TmLink <$> getReferent
    TyLinkT -> TyLink <$> getReference
    BytesT -> Bytes <$> getBytes
    QuoteT -> Quote <$> (getValue s)
    CodeT ->
      Code . flip CodeRep Uncacheable <$> getGroup (valueToCode v, fo)
    BArrT -> BArr <$> getByteArray
    PosT -> Pos <$> getPositive
    NegT -> Neg <$> getPositive
    CharT -> Char <$> getChar
    FloatT -> Float <$> getFloat
    ArrT -> Arr <$> getArray (getValue s)
    CachedCodeT ->
      Code . flip CodeRep Cacheable <$> getGroup (valueToCode v, fo)
    MapT -> exn [] "getBLit: unsupported literal map"
    BigIntT -> BigInt <$> getInteger
    BigNatT -> BigNat <$> getNatural
{-# SPECIALIZE getBLit :: DeserialIO (BLit Reference) #-}
{-# SPECIALIZE getBLit :: DeserialST s (BLit Reference) #-}

putRefs :: [Reference] -> Builder
putRefs rs = putFoldable putReference rs

getRefs :: (PrimBase m) => Get m [Reference]
getRefs = getList getReference

putBranches ::
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  Branched Reference (ANormal Reference v) ->
  Builder
putBranches refrep fops ctx bs = case bs of
  MatchEmpty -> putTag MEmptyT
  MatchIntegral m df ->
    putTag MIntT
      <> putEnumMap BU.word64BE (putNormal refrep fops ctx) m
      <> putMaybe df (putNormal refrep fops ctx)
  MatchText m df ->
    putTag MTextT
      <> putMap (putText . Util.Text.toText) (putNormal refrep fops ctx) m
      <> putMaybe df (putNormal refrep fops ctx)
  MatchRequest m (TAbs v df) ->
    putTag MReqT
      <> putMapping putReference (putEnumMap putCTag (putCase refrep fops ctx)) m
      <> putNormal refrep fops (v : ctx) df
  MatchData r m df ->
    putTag MDataT
      <> putReference r
      <> putEnumMap putCTag (putCase refrep fops ctx) m
      <> putMaybe df (putNormal refrep fops ctx)
  MatchSum m ->
    putTag MSumT
      <> putEnumMap BU.word64BE (putCase refrep fops ctx) m
  MatchNumeric r m df ->
    putTag MNumT
      <> putReference r
      <> putEnumMap BU.word64BE (putNormal refrep fops ctx) m
      <> putMaybe df (putNormal refrep fops ctx)
  _ -> exn [] "putBranches: malformed intermediate term"

getBranches ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  GDeserial m (Branched Reference (ANormal Reference v))
getBranches ctx frsh0 s =
  getTag >>= \case
    MEmptyT -> pure MatchEmpty
    MIntT ->
      MatchIntegral
        <$> getEnumMap getWord64be (getNormal ctx frsh0 s)
        <*> getMaybe (getNormal ctx frsh0 s)
    MTextT ->
      MatchText
        <$> getMap (Util.Text.fromText <$> getText) (getNormal ctx frsh0 s)
        <*> getMaybe (getNormal ctx frsh0 s)
    MReqT ->
      MatchRequest
        <$> getMapping getReference (getEnumMap getCTag (getCase ctx frsh0 s))
        <*> (TAbs v <$> getNormal (v : ctx) (frsh0 + 1) s)
      where
        v = getFresh frsh0
    MDataT ->
      MatchData
        <$> getReference
        <*> getEnumMap getCTag (getCase ctx frsh0 s)
        <*> getMaybe (getNormal ctx frsh0 s)
    MSumT -> MatchSum <$> getEnumMap getWord64be (getCase ctx frsh0 s)
    MNumT ->
      MatchNumeric
        <$> getReference
        <*> getEnumMap getWord64be (getNormal ctx frsh0 s)
        <*> getMaybe (getNormal ctx frsh0 s)

putCase ::
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  ([Mem], ANormal Reference v) ->
  Builder
putCase refrep fops ctx (ccs, (TAbss us e)) =
  putCCs ccs <> putNormal refrep fops (pushCtx us ctx) e

getCase ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  GDeserial m ([Mem], ANormal Reference v)
getCase ctx frsh0 s = do
  ccs <- getCCs
  let l = length ccs
      frsh = frsh0 + fromIntegral l
      us = getFresh <$> take l [frsh0 ..]
  (,) ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh s

putCTag :: CTag -> Builder
putCTag c = putVarInt $ fromEnum c

getCTag :: (PrimBase m) => Get m CTag
getCTag = toEnum <$> getVarInt

putGroupRef :: GroupRef Reference -> Builder
putGroupRef (GR r i) =
  putReference r <> BU.word64BE i

getGroupRef :: (PrimBase m) => Get m (GroupRef Reference)
getGroupRef = GR <$> getReference <*> getWord64be

-- Notes
--
-- Starting with version 4 of the value format, it is expected that
-- unboxed data does not actually occur in the values being sent. For
-- most values this was not a problem:
--
--   - Partial applications had no way of directly including unboxed
--     values, because they all result from surface level unison
--     applications
--   - Unboxed values in Data only occurred to represent certain
--     builtin types. Those have been replaced by BLits.
--
-- However, some work was required to make sure no unboxed data ended
-- up in Cont. The runtime has been modified to avoid using the
-- unboxed stack in generated code, so now only builtins use it,
-- effectively. Since continuations are never captured inside builtins
-- (and even if we wanted to do that, we could arrange for a clean
-- unboxed stack), this is no longer a problem, either.
--
-- So, unboxed data is completely absent from the format. We are now
-- exchanging unison surface values, effectively.
putValue :: Version -> Value Reference -> Builder
putValue v (Partial gr vs) =
  putTag PartialT
    <> putGroupRef gr
    <> putFoldable (putValue v) vs
putValue v (Data r t vs) =
  putTag DataT
    <> putReference r
    <> BU.word64BE t
    <> putFoldable (putValue v) vs
putValue v (Cont bs k) =
  putTag ContT
    <> putFoldable (putValue v) bs
    <> putCont v k
putValue v (BLit (Map l)) = putAsMap v l
putValue v (BLit l) =
  putTag BLitT <> putBLit v l

getValue :: (PrimBase m) => GDeserial m (Value Reference)
getValue s@(v, _) =
  getTag >>= \case
    PartialT
      | Transfer vn <- v,
        vn < 4 -> do
          gr <- getGroupRef
          getList getWord64be >>= assertEmptyUnboxed
          bs <- getList (getValue s)
          pure $ Partial gr bs
      | otherwise -> do
          gr <- getGroupRef
          vs <- getList (getValue s)
          pure $ Partial gr vs
    DataT
      | Transfer vn <- v,
        vn < 4 -> do
          r <- getReference
          w <- getWord64be
          getList getWord64be >>= assertEmptyUnboxed
          vs <- getList (getValue s)
          pure $ Data r w vs
      | otherwise -> do
          r <- getReference
          w <- getWord64be
          vs <- getList (getValue s)
          pure $ Data r w vs
    ContT
      | Transfer vn <- v,
        vn < 4 -> do
          getList getWord64be >>= assertEmptyUnboxed
          bs <- getList (getValue s)
          k <- getCont s
          pure $ Cont bs k
      | otherwise -> do
          bs <- getList (getValue s)
          k <- getCont s
          pure $ Cont bs k
    BLitT -> BLit <$> getBLit s
  where
    assertEmptyUnboxed :: (PrimBase m) => [a] -> Get m ()
    assertEmptyUnboxed [] = pure ()
    assertEmptyUnboxed _ = exn [] "getValue: unboxed values no longer supported"
{-# SPECIALIZE getValue :: DeserialIO (Value Reference) #-}
{-# SPECIALIZE getValue :: DeserialST s (Value Reference) #-}

putCont :: Version -> Cont Reference -> Builder
putCont _ KE = putTag KET
putCont v (Mark a rs ds k) =
  putTag MarkT
    <> BU.word64BE a
    <> putFoldable putReference rs
    <> putMapping putReference (putValue v) ds
    <> putCont v k
putCont v (Push f n gr k) =
  putTag PushT
    <> BU.word64BE f
    <> BU.word64BE n
    <> putGroupRef gr
    <> putCont v k

getCont :: (PrimBase m) => GDeserial m (Cont Reference)
getCont s@(v, _) =
  getTag >>= \case
    KET -> pure KE
    MarkT
      | Transfer vn <- v,
        vn < 4 -> do
          getWord64be >>= assert0 "unboxed arg size"
          ba <- getWord64be
          refs <- getList getReference
          vals <- getMapping getReference (getValue s)
          cont <- getCont s
          pure $ Mark ba refs vals cont
      | otherwise ->
          Mark
            <$> getWord64be
            <*> getList getReference
            <*> getMapping getReference (getValue s)
            <*> getCont s
    PushT
      | Transfer vn <- v,
        vn < 4 -> do
          getWord64be >>= assert0 "unboxed frame size"
          bf <- getWord64be
          getWord64be >>= assert0 "unboxed arg size"
          ba <- getWord64be
          gr <- getGroupRef
          cont <- getCont s
          pure $ Push bf ba gr cont
      | otherwise ->
          Push
            <$> getWord64be
            <*> getWord64be
            <*> getGroupRef
            <*> getCont s
  where
    assert0 _name 0 = pure ()
    assert0 name n = exn [] $ "getCont: malformed intermediate term. Expected " <> name <> " to be 0, but got " <> show n
{-# SPECIALIZE getCont :: DeserialIO (Cont Reference) #-}
{-# SPECIALIZE getCont :: DeserialST s (Cont Reference) #-}

deserializeCode :: ByteString -> IO (Either String (Referenced Code))
deserializeCode bs = runGetCatchIO go bs
  where
    go =
      getWord32be >>= \case
        n
          | n == 4 -> CodeV4.getCodeWithHeader
          | 1 <= n && n < 4 ->
              Plain <$> getCode (Transfer n, False)
          | otherwise ->
              fail $ "deserializeGroup: unknown version: " ++ show n

-- Boolean argument determines whether ForeignFunc occurrences are
-- allowed to be serialized. For interchange, this should be False.
serializeCode :: Bool -> Referenced Code -> ByteString
serializeCode fops (dereference -> co) =
  L.toStrict . BU.toLazyByteString $ putVersion <> putCode fops co
  where
    putVersion = BU.word32BE codeVersion

serializeCodeWithVersion ::
  Word64 -> Bool -> Referenced Code -> IO (Either String L.ByteString)
serializeCodeWithVersion v fops rco
  | v == 4 =
      enreference rco >>= \(tys, tms, co) ->
        pure . Right . BU.toLazyByteString $
          BU.word32BE 4 <> CodeV4.putCodeWithHeader tys tms fops co
  | v == 3 =
      pure . Right . BU.toLazyByteString $
        BU.word32BE 3 <> putCode fops (dereference rco)
  | otherwise =
      pure . Left $ "unsupported code serialization version: " ++ show v
  where
    enreference (WithRefs tys tms co) = pure (tys, tms, co)
    enreference (Plain co) =
      runStateT (canonicalizeRefs co) emptyCST
        <&> \(co, CST _ _ _ tys tms) -> (toList tys, toList tms, co)

-- | Serializes a `SuperGroup` for rehashing.
--
-- Expected as arguments are some code, and the `Reference` that
-- refers to it. In particular, if the code refers to itself by
-- reference, or if the code is part of a mututally-recursive set of
-- definitions (which have a common hash), the reference used as part
-- of that (mutual) recursion must be supplied.
--
-- Using that reference, we find all references in the code to that
-- connected component. In the resulting byte string, those references
-- are instead replaced by positions in a listing of the connected
-- component. This means that the byte string is independent of the
-- hash used for the self reference. Only the order matters (which is
-- determined by the `Reference`). Then the bytes can be re-hashed to
-- establish a new hash for the connected component. This operation
-- should be idempotent as long as the indexing is preserved.
--
-- Supplying a `Builtin` reference is not supported. Such code
-- shouldn't be subject to rehashing.
serializeGroupForRehash ::
  (Var v) =>
  Reference ->
  SuperGroup Reference v ->
  L.ByteString
serializeGroupForRehash (Builtin _) _ =
  error "serializeForRehash: builtin reference"
serializeGroupForRehash (Derived h _) sg =
  BU.toLazyByteString $ putGroup refrep False sg
  where
    f r@(Derived h' i) | h == h' = Just (r, i)
    f _ = Nothing
    refrep = Map.fromList . mapMaybe f $ groupTermLinks sg

getVersionedValue :: (PrimBase m) => Get m (Referenced Value)
getVersionedValue =
  getWord32be >>= \case
    n
      | n < 1 -> fail $ "deserializeValue: unknown version: " ++ show n
      | n < 3 -> fail $ "deserializeValue: unsupported version: " ++ show n
      | n <= 4 -> Plain <$> getValue (Transfer n, False)
      | n == 5 -> ValueV5.getValueWithHeader
      | otherwise -> fail $ "deserializeValue: unknown version: " ++ show n
{-# SPECIALIZE getVersionedValue :: Get IO (Referenced Value) #-}
{-# SPECIALIZE getVersionedValue :: Get (ST s) (Referenced Value) #-}

deserializeValue :: ByteString -> IO (Either String (Referenced Value))
deserializeValue bs = runGetCatchIO getVersionedValue bs

serializeValue :: Referenced Value -> ByteString
serializeValue (dereference -> v) =
  L.toStrict . BU.toLazyByteString $
    putVersion <> putValue (Transfer valueVersion) v
  where
    putVersion = BU.word32BE valueVersion

serializeValueWithVersion ::
  Word64 -> Referenced Value -> IO L.ByteString
serializeValueWithVersion v rval
  | v == 5 = case rval of
      WithRefs tys tms x -> v5ser tys tms x
      Plain x -> do
        (x, CST _ _ _ tys tms) <-
          runStateT (canonicalizeRefs x) emptyCST
        v5ser (toList tys) (toList tms) x
  | v < 5,
    n <- fromIntegral v =
      pure . BU.toLazyByteString $
        BU.word32BE n
          <> putValue (Transfer n) (dereference rval)
  | otherwise =
      die [] $ "Value.serialize.versioned: unrecognized version: " ++ show v
  where
    v5ser tys tms x =
      pure $ ValueV5.versionedValueBytes tys tms x

-- This serializer is used exclusively for hashing unison values.
-- For this reason, it doesn't prefix the string with the current
-- version, so that only genuine changes in the way things are
-- serialized will change hashes.
--
-- The 4 prefix is used because we were previously including the
-- version in the hash, so to maintain the same hashes, we need to
-- include the extra bytes that were previously there.
--
-- Additionally, any major serialization changes should consider
-- retaining this representation as much as possible, even if it
-- becomes a separate format, because there is no need to parse from
-- the hash serialization, just generate and hash it.
serializeValueForHash :: Value Reference -> L.ByteString
serializeValueForHash v =
  BU.toLazyByteString (putPrefix <> putValue (Hash 4) v)
  where
    putPrefix = BU.word32BE 4

-- Gets a SuperGroup with the current code version. Used for
-- interpreter state serialization in U.R.Interface.
getGroupCurrent :: (PrimBase m, Var v) => Get m (SuperGroup Reference v)
getGroupCurrent = getGroup (Transfer codeVersion, False)

type GDeserial m a = (Version, Bool) -> Get m a

type DeserialIO a = (Version, Bool) -> Get IO a

type DeserialST s a = (Version, Bool) -> Get (ST s) a

-- Convert value version numbers to code version numbers
valueToCode :: Version -> Version
valueToCode v
  | Hash n <- v = Hash $ tweak n
  | Transfer n <- v = Transfer $ tweak n
  where
    tweak n
      | n > 2 = n - 1
      | otherwise = n

valueVersion :: Word32
valueVersion = 4

codeVersion :: Word32
codeVersion = 3

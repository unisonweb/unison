module Unison.Runtime.ANF.Serialize.CodeV4 where

import Control.Monad
import Data.Binary.Get qualified as BGet
import Data.Binary.Put qualified as BPut
import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Put
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Map.Strict as Map (lookup)
import Data.Serialize.Get qualified as SGet
import Data.Serialize.Put qualified as SPut
import Data.Word (Word64)
import GHC.Stack
import Unison.ABT.Normalized (Term (..))
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.ANF.POp as ANF
import Unison.Runtime.ANF.Serialize.Tags
import Unison.Runtime.Exception
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.Referenced
import Unison.Runtime.Serialize hiding
  ( getReferent,
    putReferent,
  )
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Type (ANFBlank), Var (..))
import Prelude hiding (getChar, putChar)

pushCtx :: [v] -> [v] -> [v]
pushCtx us vs = reverse us ++ vs

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

putIndex :: (MonadPut m) => Word64 -> m ()
putIndex = putVarInt
{-# INLINE putIndex #-}

getIndex :: (MonadGet m) => m Word64
getIndex = getVarInt
{-# INLINE getIndex #-}

putVar :: (MonadPut m) => (Eq v) => [v] -> v -> m ()
putVar ctx v
  | Just i <- index ctx v = putIndex i
  | otherwise = exn [] "putVar: variable not in context"

getVar :: (MonadGet m) => [v] -> m v
getVar ctx = deindex ctx <$> getIndex

putArgs :: (MonadPut m) => (Eq v) => [v] -> [v] -> m ()
putArgs ctx is = putFoldable (putVar ctx) is

getArgs :: (MonadGet m) => [v] -> m [v]
getArgs ctx = getList (getVar ctx)

putCCs :: (MonadPut m) => [Mem] -> m ()
putCCs ccs = putLength n *> traverse_ putCC ccs
  where
    n = length ccs
    putCC UN = putWord8 0
    putCC BX = putWord8 1

getCCs :: (MonadGet m) => m [Mem]
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
  (MonadPut m) =>
  (Var v) =>
  Bool ->
  SuperGroup RefNum v ->
  m ()
putGroup fops (Rec bs e) =
  putLength n
    *> traverse_ (putComb fops ctx) cs
    *> putComb fops ctx e
  where
    n = length us
    (us, cs) = unzip bs
    ctx = pushCtx us []

getGroup ::
  (MonadGet m) =>
  (Var v) =>
  m (SuperGroup RefNum v)
getGroup = do
  l <- getLength
  let n = fromIntegral l
      vs = getFresh <$> take l [0 ..]
      ctx = pushCtx vs []
  cs <- replicateM l (getComb ctx n)
  Rec (zip vs cs) <$> getComb ctx n

putCode :: (MonadPut m) => Bool -> (Code RefNum) -> m ()
putCode fops (CodeRep g c) =
  putGroup fops g *> putCacheability c

getCode :: (MonadGet m) => m (Code RefNum)
getCode = CodeRep <$> getGroup <*> getCacheability

putCodeWithHeader ::
  (MonadPut m) => [Reference] -> [Reference] -> Bool -> Code RefNum -> m ()
putCodeWithHeader tyrs tmrs fops co =
  putFoldable putReference tyrs
    *> putFoldable putReference tmrs
    *> putCode fops co
{-# SPECIALIZE putCodeWithHeader ::
  [Reference] -> [Reference] -> Bool -> Code RefNum -> BPut.Put
  #-}
{-# SPECIALIZE putCodeWithHeader ::
  [Reference] -> [Reference] -> Bool -> Code RefNum -> SPut.Put
  #-}

getCodeWithHeader :: (MonadGet m) => m (Referenced Code)
getCodeWithHeader = do
  tyl <- getLength
  tys <- replicateM tyl getReference
  tml <- getLength
  tms <- replicateM tml getReference
  co <- getCode
  pure (WithRefs tys tms co)
{-# SPECIALIZE getCodeWithHeader :: BGet.Get (Referenced Code) #-}
{-# SPECIALIZE getCodeWithHeader :: SGet.Get (Referenced Code) #-}

putCacheability :: (MonadPut m) => Cacheability -> m ()
putCacheability Uncacheable = putWord8 0
putCacheability Cacheable = putWord8 1

getCacheability :: (MonadGet m) => m Cacheability
getCacheability =
  getWord8 >>= \case
    0 -> pure Uncacheable
    1 -> pure Cacheable
    n -> exn [] $ "getBLit: unrecognized cacheability byte: " ++ show n

putComb ::
  (MonadPut m) =>
  (Var v) =>
  Bool ->
  [v] ->
  SuperNormal RefNum v ->
  m ()
putComb fops ctx (Lambda ccs (TAbss us e)) =
  putCCs ccs *> putNormal fops (pushCtx us ctx) e

getFresh :: (Var v) => Word64 -> v
getFresh n = freshenId n $ typed ANFBlank

getComb ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m (SuperNormal RefNum v)
getComb ctx frsh0 = do
  ccs <- getCCs
  let us = zipWith (\_ -> getFresh) ccs [frsh0 ..]
      frsh = frsh0 + fromIntegral (length ccs)
  Lambda ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh

putNormal ::
  (MonadPut m) =>
  (Var v) =>
  Bool ->
  [v] ->
  ANormal RefNum v ->
  m ()
putNormal fops ctx tm = case tm of
  TVar v -> putTag VarT *> putVar ctx v
  TFrc v -> putTag ForceT *> putVar ctx v
  TApp f as -> putTag AppT *> putFunc ctx f *> putArgs ctx as
  THnd rs nh _ah e ->
    putTag HandleT
      *> putFoldable putRefNum rs
      *> putVar ctx nh
      *> putNormal fops ctx e
  TShift r v e ->
    putTag ShiftT
      *> putRefNum r
      *> putNormal fops (v : ctx) e
  TMatch v bs ->
    putTag MatchT
      *> putVar ctx v
      *> putBranches fops ctx bs
  TLit l -> putTag LitT *> putLit l
  TBLit l -> putTag BxLitT *> putLit l
  TName v (Left r) as e ->
    putTag NameRefT
      *> putRefNum r
      *> putArgs ctx as
      *> putNormal fops (v : ctx) e
  TName v (Right u) as e ->
    putTag NameVarT
      *> putVar ctx u
      *> putArgs ctx as
      *> putNormal fops (v : ctx) e
  TLets Direct us ccs l e ->
    putTag LetDirT
      *> putCCs ccs
      *> putNormal fops ctx l
      *> putNormal fops (pushCtx us ctx) e
  TLets (Indirect w) us ccs l e ->
    putTag LetIndT
      *> putWord16be w
      *> putCCs ccs
      *> putNormal fops ctx l
      *> putNormal fops (pushCtx us ctx) e
  v -> exn [] $ "putNormal: malformed term\n" ++ show v

getNormal ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m (ANormal RefNum v)
getNormal ctx frsh0 =
  getTag >>= \case
    VarT -> TVar <$> getVar ctx
    ForceT -> TFrc <$> getVar ctx
    AppT -> TApp <$> getFunc ctx <*> getArgs ctx
    HandleT ->
      THnd
        <$> getList getRefNum
        <*> getVar ctx
        <*> pure Nothing
        <*> getNormal ctx frsh0
    ShiftT ->
      flip TShift v
        <$> getRefNum
        <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    MatchT -> TMatch <$> getVar ctx <*> getBranches ctx frsh0
    LitT -> TLit <$> getLit
    BxLitT -> TBLit <$> getLit
    NameRefT ->
      TName v . Left
        <$> getRefNum
        <*> getArgs ctx
        <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    NameVarT ->
      TName v . Right
        <$> getVar ctx
        <*> getArgs ctx
        <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    LetDirT -> do
      ccs <- getCCs
      let l = length ccs
          frsh = frsh0 + fromIntegral l
          us = getFresh <$> take l [frsh0 ..]
      TLets Direct us ccs
        <$> getNormal ctx frsh0
        <*> getNormal (pushCtx us ctx) frsh
    LetIndT -> do
      w <- getWord16be
      ccs <- getCCs
      let l = length ccs
          frsh = frsh0 + fromIntegral l
          us = getFresh <$> take l [frsh0 ..]
      TLets (Indirect w) us ccs
        <$> getNormal ctx frsh0
        <*> getNormal (pushCtx us ctx) frsh

putFunc ::
  (MonadPut m) =>
  (Var v) =>
  [v] ->
  Func RefNum v ->
  m ()
putFunc ctx f = case f of
  FVar v -> putTag FVarT *> putVar ctx v
  FComb r -> putTag FCombT *> putRefNum r
  FCont v -> putTag FContT *> putVar ctx v
  FCon r c -> putTag FConT *> putRefNum r *> putCTag c
  FReq r c -> putTag FReqT *> putRefNum r *> putCTag c
  FPrim (Left p) -> putTag FPrimT *> putPOp p
  FPrim (Right f) -> putTag FForeignT *> putFOp f

getFunc :: (MonadGet m, Var v) => [v] -> m (Func RefNum v)
getFunc ctx =
  getTag >>= \case
    FVarT -> FVar <$> getVar ctx
    FCombT -> FComb <$> getRefNum
    FContT -> FCont <$> getVar ctx
    FConT -> FCon <$> getRefNum <*> getCTag
    FReqT -> FReq <$> getRefNum <*> getCTag
    FPrimT -> FPrim . Left <$> getPOp
    FForeignT -> FPrim . Right <$> getFOp

-- Note: this numbering is derived, and so not particularly stable.
-- However, foreign functions are not serialized for interchange. This
-- is for serializing optimization information for standalaone
-- programs.
putFOp :: (MonadPut m) => ForeignFunc -> m ()
putFOp = putVarInt . fromEnum

getFOp :: (MonadGet m) => m ForeignFunc
getFOp = toEnum <$> getVarInt

putPOp :: (MonadPut m) => POp -> m ()
putPOp op
  | Just w <- Map.lookup op pop2word = putWord16be w
  | otherwise = exn [] $ "putPOp: unknown POp: " ++ show op

getPOp :: (MonadGet m) => m POp
getPOp =
  getWord16be >>= \w -> case Map.lookup w word2pop of
    Just op -> pure op
    Nothing -> exn [] "getPOp: unknown enum code"

putLit :: (MonadPut m) => Lit RefNum -> m ()
putLit = \case
  I i -> putTag IT *> putInt i
  N n -> putTag NT *> putNat n
  F f -> putTag FT *> putFloat f
  T t -> putTag TT *> putText (Util.Text.toText t)
  C c -> putTag CT *> putChar c
  LM r -> putTag LMT *> putNumberedReferent r
  LY r -> putTag LYT *> putRefNum r

getLit :: (MonadGet m) => m (Lit RefNum)
getLit =
  getTag >>= \case
    IT -> I <$> getInt
    NT -> N <$> getNat
    FT -> F <$> getFloat
    TT -> T . Util.Text.fromText <$> getText
    CT -> C <$> getChar
    LMT -> LM <$> getNumberedReferent
    LYT -> LY <$> getRefNum

putBranches ::
  (MonadPut m) =>
  (Var v) =>
  Bool ->
  [v] ->
  Branched RefNum (ANormal RefNum v) ->
  m ()
putBranches fops ctx bs = case bs of
  MatchEmpty -> putTag MEmptyT
  MatchIntegral m df -> do
    putTag MIntT
    putEnumMap putWord64be (putNormal fops ctx) m
    putMaybe df $ putNormal fops ctx
  MatchText m df -> do
    putTag MTextT
    putMap (putText . Util.Text.toText) (putNormal fops ctx) m
    putMaybe df $ putNormal fops ctx
  MatchRequest m (TAbs v df) -> do
    putTag MReqT
    putMapping
      putRefNum
      (putEnumMap putCTag (putCase fops ctx))
      m
    putNormal fops (v : ctx) df
  MatchData r m df -> do
    putTag MDataT
    putRefNum r
    putEnumMap putCTag (putCase fops ctx) m
    putMaybe df $ putNormal fops ctx
  MatchSum m -> do
    putTag MSumT
    putEnumMap putWord64be (putCase fops ctx) m
  MatchNumeric r m df -> do
    putTag MNumT
    putRefNum r
    putEnumMap putWord64be (putNormal fops ctx) m
    putMaybe df $ putNormal fops ctx
  _ -> exn [] "putBranches: malformed intermediate term"

getBranches ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m (Branched RefNum (ANormal RefNum v))
getBranches ctx frsh0 =
  getTag >>= \case
    MEmptyT -> pure MatchEmpty
    MIntT ->
      MatchIntegral
        <$> getEnumMap getWord64be (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MTextT ->
      MatchText
        <$> getMap (Util.Text.fromText <$> getText) (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MReqT ->
      MatchRequest
        <$> getMapping
          getRefNum
          (getEnumMap getCTag (getCase ctx frsh0))
        <*> (TAbs v <$> getNormal (v : ctx) (frsh0 + 1))
      where
        v = getFresh frsh0
    MDataT ->
      MatchData
        <$> getRefNum
        <*> getEnumMap getCTag (getCase ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MSumT -> MatchSum <$> getEnumMap getWord64be (getCase ctx frsh0)
    MNumT ->
      MatchNumeric
        <$> getRefNum
        <*> getEnumMap getWord64be (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)

putCase ::
  (MonadPut m) =>
  (Var v) =>
  Bool ->
  [v] ->
  ([Mem], ANormal RefNum v) ->
  m ()
putCase fops ctx (ccs, (TAbss us e)) =
  putCCs ccs *> putNormal fops (pushCtx us ctx) e

getCase ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m ([Mem], ANormal RefNum v)
getCase ctx frsh0 = do
  ccs <- getCCs
  let l = length ccs
      frsh = frsh0 + fromIntegral l
      us = getFresh <$> take l [frsh0 ..]
  (,) ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh

putCTag :: (MonadPut m) => CTag -> m ()
putCTag c = putVarInt $ fromEnum c

getCTag :: (MonadGet m) => m CTag
getCTag = toEnum <$> getVarInt

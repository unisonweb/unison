module Unison.Runtime.ANF.Serialize.CodeV4 where

import Control.Monad
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.Functor ((<&>))
import Data.Map.Strict as Map (lookup)
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
import Unison.Runtime.Serialize.Get
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

putIndex :: Word64 -> Builder
putIndex = putVarInt
{-# INLINE putIndex #-}

getIndex :: (PrimBase m) => Get m Word64
getIndex = getVarInt
{-# INLINE getIndex #-}

putVar :: (Eq v) => [v] -> v -> Builder
putVar ctx v
  | Just i <- index ctx v = putIndex i
  | otherwise = exn [] "putVar: variable not in context"

getVar :: (PrimBase m) => [v] -> Get m v
getVar ctx = deindex ctx <$> getIndex
{-# INLINE getVar #-}

putArgs :: (Eq v) => [v] -> [v] -> Builder
putArgs ctx is = putFoldable (putVar ctx) is

getArgs :: (PrimBase m) => [v] -> Get m [v]
getArgs ctx = getList (getVar ctx)
{-# INLINE getArgs #-}

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
{-# INLINE getCCs #-}

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
  Bool ->
  SuperGroup RefNum v ->
  Builder
putGroup fops (Rec bs e) =
  putLength n
    <> foldMap (putComb fops ctx) cs
    <> putComb fops ctx e
  where
    n = length us
    (us, cs) = unzip bs
    ctx = pushCtx us []

getGroup ::
  (PrimBase m) =>
  (Var v) =>
  Get m (SuperGroup RefNum v)
getGroup = do
  l <- getLength
  let n = fromIntegral l
      vs = getFresh <$> take l [0 ..]
      ctx = pushCtx vs []
  cs <- replicateM l (getComb ctx n)
  Rec (zip vs cs) <$> getComb ctx n
{-# INLINABLE getGroup #-}

putCode :: Bool -> (Code RefNum) -> Builder
putCode fops (CodeRep g c) =
  putGroup fops g <> putCacheability c

getCode :: (PrimBase m) => Get m (Code RefNum)
getCode = CodeRep <$> getGroup <*> getCacheability
{-# INLINABLE getCode #-}

putCodeWithHeader ::
  [Reference] -> [Reference] -> Bool -> Code RefNum -> Builder
putCodeWithHeader tyrs tmrs fops co =
  putFoldable putReference tyrs
    <> putFoldable putReference tmrs
    <> putCode fops co

getCodeWithHeader :: (PrimBase m) => Get m (Referenced Code)
getCodeWithHeader = do
  tyl <- getLength
  tys <- replicateM tyl getReference
  tml <- getLength
  tms <- replicateM tml getReference
  co <- getCode
  pure (WithRefs tys tms co)
{-# INLINABLE getCodeWithHeader #-}

putCacheability :: Cacheability -> Builder
putCacheability Uncacheable = BU.word8 0
putCacheability Cacheable = BU.word8 1

getCacheability :: (PrimBase m) => Get m Cacheability
getCacheability =
  getWord8 >>= \case
    0 -> pure Uncacheable
    1 -> pure Cacheable
    n -> exn [] $ "getBLit: unrecognized cacheability byte: " ++ show n
{-# INLINE getCacheability #-}

putComb ::
  (Var v) =>
  Bool ->
  [v] ->
  SuperNormal RefNum v ->
  Builder
putComb fops ctx (Lambda ccs (TAbss us e)) =
  putCCs ccs <> putNormal fops (pushCtx us ctx) e

getFresh :: (Var v) => Word64 -> v
getFresh n = freshenId n $ typed ANFBlank

getComb ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  Get m (SuperNormal RefNum v)
getComb ctx frsh0 = do
  ccs <- getCCs
  let us = zipWith (\_ -> getFresh) ccs [frsh0 ..]
      frsh = frsh0 + fromIntegral (length ccs)
  Lambda ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh
{-# INLINABLE getComb #-}

putNormal ::
  (Var v) =>
  Bool ->
  [v] ->
  ANormal RefNum v ->
  Builder
putNormal fops ctx tm = case tm of
  TVar v -> putTag VarT <> putVar ctx v
  TFrc v -> putTag ForceT <> putVar ctx v
  TApp f as -> putTag AppT <> putFunc ctx f <> putArgs ctx as
  THnd rs nh _ah e ->
    putTag HandleT
      <> putFoldable putRefNum rs
      <> putVar ctx nh
      <> putNormal fops ctx e
  TShift r v e ->
    putTag ShiftT
      <> putRefNum r
      <> putNormal fops (v : ctx) e
  TMatch v bs ->
    putTag MatchT
      <> putVar ctx v
      <> putBranches fops ctx bs
  TLit l -> putTag LitT <> putLit l
  TBLit l -> putTag BxLitT <> putLit l
  TName v (Left r) as e ->
    putTag NameRefT
      <> putRefNum r
      <> putArgs ctx as
      <> putNormal fops (v : ctx) e
  TName v (Right u) as e ->
    putTag NameVarT
      <> putVar ctx u
      <> putArgs ctx as
      <> putNormal fops (v : ctx) e
  TLets Direct us ccs l e ->
    putTag LetDirT
      <> putCCs ccs
      <> putNormal fops ctx l
      <> putNormal fops (pushCtx us ctx) e
  TLets (Indirect w) us ccs l e ->
    putTag LetIndT
      <> BU.word16BE w
      <> putCCs ccs
      <> putNormal fops ctx l
      <> putNormal fops (pushCtx us ctx) e
  v -> exn [] $ "putNormal: malformed term\n" ++ show v

getNormal ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  Get m (ANormal RefNum v)
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
{-# INLINABLE getNormal #-}

putFunc ::
  (Var v) =>
  [v] ->
  Func RefNum v ->
  Builder
putFunc ctx f = case f of
  FVar v -> putTag FVarT <> putVar ctx v
  FComb r -> putTag FCombT <> putRefNum r
  FCont v -> putTag FContT <> putVar ctx v
  FCon r c -> putTag FConT <> putRefNum r <> putCTag c
  FReq r c -> putTag FReqT <> putRefNum r <> putCTag c
  FPrim (Left p) -> putTag FPrimT <> putPOp p
  FPrim (Right f) -> putTag FForeignT <> putFOp f

getFunc :: (PrimBase m, Var v) => [v] -> Get m (Func RefNum v)
getFunc ctx =
  getTag >>= \case
    FVarT -> FVar <$> getVar ctx
    FCombT -> FComb <$> getRefNum
    FContT -> FCont <$> getVar ctx
    FConT -> FCon <$> getRefNum <*> getCTag
    FReqT -> FReq <$> getRefNum <*> getCTag
    FPrimT -> FPrim . Left <$> getPOp
    FForeignT -> FPrim . Right <$> getFOp
{-# INLINABLE getFunc #-}

-- Note: this numbering is derived, and so not particularly stable.
-- However, foreign functions are not serialized for interchange. This
-- is for serializing optimization information for standalaone
-- programs.
putFOp :: ForeignFunc -> Builder
putFOp = putVarInt . fromEnum

getFOp :: (PrimBase m) => Get m ForeignFunc
getFOp = toEnum <$> getVarInt
{-# INLINE getFOp #-}

putPOp :: POp -> Builder
putPOp op
  | Just w <- Map.lookup op pop2word = BU.word16BE w
  | otherwise = exn [] $ "putPOp: unknown POp: " ++ show op

getPOp :: (PrimBase m) => Get m POp
getPOp =
  getWord16be >>= \w -> case Map.lookup w word2pop of
    Just op -> pure op
    Nothing -> exn [] "getPOp: unknown enum code"
{-# INLINE getPOp #-}

putLit :: Lit RefNum -> Builder
putLit = \case
  I i -> putTag IT <> putInt i
  N n -> putTag NT <> putNat n
  F f -> putTag FT <> putFloat f
  T t -> putTag TT <> putText (Util.Text.toText t)
  C c -> putTag CT <> putChar c
  LM r -> putTag LMT <> putNumberedReferent r
  LY r -> putTag LYT <> putRefNum r

getLit :: (PrimBase m) => Get m (Lit RefNum)
getLit =
  getTag >>= \case
    IT -> I <$> getInt
    NT -> N <$> getNat
    FT -> F <$> getFloat
    TT -> T . Util.Text.fromText <$> getText
    CT -> C <$> getChar
    LMT -> LM <$> getNumberedReferent
    LYT -> LY <$> getRefNum
{-# INLINABLE getLit #-}

putBranches ::
  (Var v) =>
  Bool ->
  [v] ->
  Branched RefNum (ANormal RefNum v) ->
  Builder
putBranches fops ctx bs = case bs of
  MatchEmpty -> putTag MEmptyT
  MatchIntegral m df ->
    putTag MIntT
      <> putEnumMap BU.word64BE (putNormal fops ctx) m
      <> putMaybe df (putNormal fops ctx)
  MatchText m df ->
    putTag MTextT
      <> putMap (putText . Util.Text.toText) (putNormal fops ctx) m
      <> putMaybe df (putNormal fops ctx)
  MatchRequest m (TAbs v df) ->
    putTag MReqT
      <> putMapping
        putRefNum
        (putEnumMap putCTag (putCase fops ctx))
        m
      <> putNormal fops (v : ctx) df
  MatchData r m df ->
    putTag MDataT
      <> putRefNum r
      <> putEnumMap putCTag (putCase fops ctx) m
      <> putMaybe df (putNormal fops ctx)
  MatchSum m ->
    putTag MSumT
      <> putEnumMap BU.word64BE (putCase fops ctx) m
  MatchNumeric r m df ->
    putTag MNumT
      <> putRefNum r
      <> putEnumMap BU.word64BE (putNormal fops ctx) m
      <> putMaybe df (putNormal fops ctx)
  _ -> exn [] "putBranches: malformed intermediate term"

getBranches ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  Get m (Branched RefNum (ANormal RefNum v))
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
{-# INLINABLE getBranches #-}

putCase ::
  (Var v) =>
  Bool ->
  [v] ->
  ([Mem], ANormal RefNum v) ->
  Builder
putCase fops ctx (ccs, (TAbss us e)) =
  putCCs ccs <> putNormal fops (pushCtx us ctx) e

getCase ::
  (PrimBase m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  Get m ([Mem], ANormal RefNum v)
getCase ctx frsh0 = do
  ccs <- getCCs
  let l = length ccs
      frsh = frsh0 + fromIntegral l
      us = getFresh <$> take l [frsh0 ..]
  (,) ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh
{-# INLINABLE getCase #-}

putCTag :: CTag -> Builder
putCTag c = putVarInt $ fromEnum c

getCTag :: (PrimBase m) => Get m CTag
getCTag = toEnum <$> getVarInt
{-# INLINE getCTag #-}

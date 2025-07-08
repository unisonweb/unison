
module Unison.Runtime.ANF.Serialize.CodeV4 where

import Control.Monad
import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Put
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Map as Map (Map, fromList, lookup)
import Data.Word (Word16, Word64)
import GHC.Stack
import Unison.ABT.Normalized (Term (..))
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.ANF.Serialize.Tags
import Unison.Runtime.Exception
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.Serialize
import Unison.Var (Type (ANFBlank), Var (..))
import Unison.Util.Text qualified as Util.Text
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
deindex [] _ = exn "deindex: bad index"
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
  | otherwise = exn "putVar: variable not in context"

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
      _ -> exn "getCCs: bad calling convention"

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
  Map Reference Word64 ->
  Bool ->
  SuperGroup v ->
  m ()
putGroup refrep fops (Rec bs e) =
  putLength n
    *> traverse_ (putComb refrep fops ctx) cs
    *> putComb refrep fops ctx e
  where
    n = length us
    (us, cs) = unzip bs
    ctx = pushCtx us []

getGroup ::
  (MonadGet m) =>
  (Var v) =>
  m (SuperGroup v)
getGroup = do
  l <- getLength
  let n = fromIntegral l
      vs = getFresh <$> take l [0 ..]
      ctx = pushCtx vs []
  cs <- replicateM l (getComb ctx n)
  Rec (zip vs cs) <$> getComb ctx n

putCode :: (MonadPut m) => Bool -> Code -> m ()
putCode fops (CodeRep g c) = putGroup mempty fops g *> putCacheability c

getCode :: (MonadGet m) => m Code
getCode = CodeRep <$> getGroup <*> getCacheability

putCacheability :: (MonadPut m) => Cacheability -> m ()
putCacheability Uncacheable = putWord8 0
putCacheability Cacheable = putWord8 1

getCacheability :: (MonadGet m) => m Cacheability
getCacheability =
  getWord8 >>= \case
    0 -> pure Uncacheable
    1 -> pure Cacheable
    n -> exn $ "getBLit: unrecognized cacheability byte: " ++ show n

putComb ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  SuperNormal v ->
  m ()
putComb refrep fops ctx (Lambda ccs (TAbss us e)) =
  putCCs ccs *> putNormal refrep fops (pushCtx us ctx) e

getFresh :: (Var v) => Word64 -> v
getFresh n = freshenId n $ typed ANFBlank

getComb ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m (SuperNormal v)
getComb ctx frsh0 = do
  ccs <- getCCs
  let us = zipWith (\_ -> getFresh) ccs [frsh0 ..]
      frsh = frsh0 + fromIntegral (length ccs)
  Lambda ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh

putNormal ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  ANormal v ->
  m ()
putNormal refrep fops ctx tm = case tm of
  TVar v -> putTag VarT *> putVar ctx v
  TFrc v -> putTag ForceT *> putVar ctx v
  TApp f as -> putTag AppT *> putFunc refrep ctx f *> putArgs ctx as
  THnd rs nh _ah e ->
    putTag HandleT
      *> putRefs rs
      *> putVar ctx nh
      *> putNormal refrep fops ctx e
  TShift r v e ->
    putTag ShiftT *> putReference r *> putNormal refrep fops (v : ctx) e
  TMatch v bs ->
    putTag MatchT
      *> putVar ctx v
      *> putBranches refrep fops ctx bs
  TLit l -> putTag LitT *> putLit l
  TBLit l -> putTag BxLitT *> putLit l
  TName v (Left r) as e ->
    putTag NameRefT
      *> pr
      *> putArgs ctx as
      *> putNormal refrep fops (v : ctx) e
    where
      pr
        | Just w <- Map.lookup r refrep = putWord64be w
        | otherwise = putReference r
  TName v (Right u) as e ->
    putTag NameVarT
      *> putVar ctx u
      *> putArgs ctx as
      *> putNormal refrep fops (v : ctx) e
  TLets Direct us ccs l e ->
    putTag LetDirT
      *> putCCs ccs
      *> putNormal refrep fops ctx l
      *> putNormal refrep fops (pushCtx us ctx) e
  TLets (Indirect w) us ccs l e ->
    putTag LetIndT
      *> putWord16be w
      *> putCCs ccs
      *> putNormal refrep fops ctx l
      *> putNormal refrep fops (pushCtx us ctx) e
  v -> exn $ "putNormal: malformed term\n" ++ show v

getNormal ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m (ANormal v)
getNormal ctx frsh0 =
  getTag >>= \case
    VarT -> TVar <$> getVar ctx
    ForceT -> TFrc <$> getVar ctx
    AppT -> TApp <$> getFunc ctx <*> getArgs ctx
    HandleT ->
      THnd
        <$> getRefs
        <*> getVar ctx
        <*> pure Nothing
        <*> getNormal ctx frsh0
    ShiftT ->
      flip TShift v <$> getReference <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    MatchT -> TMatch <$> getVar ctx <*> getBranches ctx frsh0
    LitT -> TLit <$> getLit
    BxLitT -> TBLit <$> getLit
    NameRefT ->
      TName v . Left
        <$> getReference
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
  Map Reference Word64 ->
  [v] ->
  Func v ->
  m ()
putFunc refrep ctx f = case f of
  FVar v -> putTag FVarT *> putVar ctx v
  FComb r
    | Just w <- Map.lookup r refrep -> putTag FCombT *> putWord64be w
    | otherwise -> putTag FCombT *> putReference r
  FCont v -> putTag FContT *> putVar ctx v
  FCon r c -> putTag FConT *> putReference r *> putCTag c
  FReq r c -> putTag FReqT *> putReference r *> putCTag c
  FPrim (Left p) -> putTag FPrimT *> putPOp p
  FPrim (Right f) -> putTag FForeignT *> putFOp f

getFunc :: (MonadGet m, Var v) => [v] -> m (Func v)
getFunc ctx =
  getTag >>= \case
    FVarT -> FVar <$> getVar ctx
    FCombT -> FComb <$> getReference
    FContT -> FCont <$> getVar ctx
    FConT -> FCon <$> getReference <*> getCTag
    FReqT -> FReq <$> getReference <*> getCTag
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
  | otherwise = exn $ "putPOp: unknown POp: " ++ show op

getPOp :: (MonadGet m) => m POp
getPOp =
  getWord16be >>= \w -> case Map.lookup w word2pop of
    Just op -> pure op
    Nothing -> exn "getPOp: unknown enum code"

pOpCode :: POp -> Word16
pOpCode op = case op of
  ADDI -> 0
  SUBI -> 1
  MULI -> 2
  DIVI -> 3
  SGNI -> 4
  NEGI -> 5
  MODI -> 6
  POWI -> 7
  SHLI -> 8
  SHRI -> 9
  INCI -> 10
  DECI -> 11
  LEQI -> 12
  EQLI -> 13
  ADDN -> 14
  SUBN -> 15
  MULN -> 16
  DIVN -> 17
  MODN -> 18
  TZRO -> 19
  LZRO -> 20
  POWN -> 21
  SHLN -> 22
  SHRN -> 23
  ANDN -> 24
  IORN -> 25
  XORN -> 26
  COMN -> 27
  INCN -> 28
  DECN -> 29
  LEQN -> 30
  EQLN -> 31
  ADDF -> 32
  SUBF -> 33
  MULF -> 34
  DIVF -> 35
  MINF -> 36
  MAXF -> 37
  LEQF -> 38
  EQLF -> 39
  POWF -> 40
  EXPF -> 41
  SQRT -> 42
  LOGF -> 43
  LOGB -> 44
  ABSF -> 45
  CEIL -> 46
  FLOR -> 47
  TRNF -> 48
  RNDF -> 49
  COSF -> 50
  ACOS -> 51
  COSH -> 52
  ACSH -> 53
  SINF -> 54
  ASIN -> 55
  SINH -> 56
  ASNH -> 57
  TANF -> 58
  ATAN -> 59
  TANH -> 60
  ATNH -> 61
  ATN2 -> 62
  CATT -> 63
  TAKT -> 64
  DRPT -> 65
  SIZT -> 66
  UCNS -> 67
  USNC -> 68
  EQLT -> 69
  LEQT -> 70
  PAKT -> 71
  UPKT -> 72
  CATS -> 73
  TAKS -> 74
  DRPS -> 75
  SIZS -> 76
  CONS -> 77
  SNOC -> 78
  IDXS -> 79
  BLDS -> 80
  VWLS -> 81
  VWRS -> 82
  SPLL -> 83
  SPLR -> 84
  PAKB -> 85
  UPKB -> 86
  TAKB -> 87
  DRPB -> 88
  IDXB -> 89
  SIZB -> 90
  FLTB -> 91
  CATB -> 92
  ITOF -> 93
  NTOF -> 94
  ITOT -> 95
  NTOT -> 96
  TTOI -> 97
  TTON -> 98
  TTOF -> 99
  FTOT -> 100
  FORK -> 101
  EQLU -> 102
  CMPU -> 103
  EROR -> 104
  PRNT -> 105
  INFO -> 106
  POPC -> 107
  MISS -> 108
  CACH -> 109
  LKUP -> 110
  LOAD -> 111
  CVLD -> 112
  SDBX -> 113
  VALU -> 114
  TLTT -> 115
  TRCE -> 116
  ATOM -> 117
  TFRC -> 118
  DBTX -> 119
  IXOT -> 120
  IXOB -> 121
  SDBL -> 122
  SDBV -> 123
  CAST -> 124
  ANDI -> 125
  IORI -> 126
  XORI -> 127
  COMI -> 128
  DRPN -> 129
  TRNC -> 130
  REFN -> 131
  REFR -> 132
  REFW -> 133
  RCAS -> 134
  RRFC -> 135
  TIKR -> 136
  LESI -> 137
  NEQI -> 138
  LESN -> 139
  NEQN -> 140
  LESF -> 141
  NEQF -> 142
  LEQU -> 143
  LESU -> 144
  NOTB -> 145
  ANDB -> 146
  IORB -> 147

pOpAssoc :: [(POp, Word16)]
pOpAssoc = map (\op -> (op, pOpCode op)) [minBound .. maxBound]

pop2word :: Map POp Word16
pop2word = fromList pOpAssoc

word2pop :: Map Word16 POp
word2pop = fromList $ swap <$> pOpAssoc
  where
    swap (x, y) = (y, x)

putLit :: (MonadPut m) => Lit -> m ()
putLit (I i) = putTag IT *> putInt i
putLit (N n) = putTag NT *> putNat n
putLit (F f) = putTag FT *> putFloat f
putLit (T t) = putTag TT *> putText (Util.Text.toText t)
putLit (C c) = putTag CT *> putChar c
putLit (LM r) = putTag LMT *> putReferent r
putLit (LY r) = putTag LYT *> putReference r

getLit :: (MonadGet m) => m Lit
getLit =
  getTag >>= \case
    IT -> I <$> getInt
    NT -> N <$> getNat
    FT -> F <$> getFloat
    TT -> T . Util.Text.fromText <$> getText
    CT -> C <$> getChar
    LMT -> LM <$> getReferent
    LYT -> LY <$> getReference

putRefs :: (MonadPut m) => [Reference] -> m ()
putRefs rs = putFoldable putReference rs

getRefs :: (MonadGet m) => m [Reference]
getRefs = getList getReference

putBranches ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  Branched (ANormal v) ->
  m ()
putBranches refrep fops ctx bs = case bs of
  MatchEmpty -> putTag MEmptyT
  MatchIntegral m df -> do
    putTag MIntT
    putEnumMap putWord64be (putNormal refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  MatchText m df -> do
    putTag MTextT
    putMap (putText . Util.Text.toText) (putNormal refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  MatchRequest m (TAbs v df) -> do
    putTag MReqT
    putMap putReference (putEnumMap putCTag (putCase refrep fops ctx)) m
    putNormal refrep fops (v : ctx) df
  MatchData r m df -> do
    putTag MDataT
    putReference r
    putEnumMap putCTag (putCase refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  MatchSum m -> do
    putTag MSumT
    putEnumMap putWord64be (putCase refrep fops ctx) m
  MatchNumeric r m df -> do
    putTag MNumT
    putReference r
    putEnumMap putWord64be (putNormal refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  _ -> exn "putBranches: malformed intermediate term"

getBranches ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m (Branched (ANormal v))
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
        <$> getMap getReference (getEnumMap getCTag (getCase ctx frsh0))
        <*> (TAbs v <$> getNormal (v : ctx) (frsh0 + 1))
      where
        v = getFresh frsh0
    MDataT ->
      MatchData
        <$> getReference
        <*> getEnumMap getCTag (getCase ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MSumT -> MatchSum <$> getEnumMap getWord64be (getCase ctx frsh0)
    MNumT ->
      MatchNumeric
        <$> getReference
        <*> getEnumMap getWord64be (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)

putCase ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  Bool ->
  [v] ->
  ([Mem], ANormal v) ->
  m ()
putCase refrep fops ctx (ccs, (TAbss us e)) =
  putCCs ccs *> putNormal refrep fops (pushCtx us ctx) e

getCase ::
  (MonadGet m) =>
  (Var v) =>
  [v] ->
  Word64 ->
  m ([Mem], ANormal v)
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

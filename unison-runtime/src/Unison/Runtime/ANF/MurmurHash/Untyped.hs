module Unison.Runtime.ANF.MurmurHash.Untyped where

import Data.ByteString.Short qualified as SBS
import Data.Digest.Murmur64
  ( Hash64,
    Hashable64 (..),
    combine,
    hash64,
    hash64AddInt,
  )
import Data.Map.Strict qualified as M
import Data.Map.Strict.Internal qualified as M
import Data.Text qualified as DT
import Data.Word
import Numeric.Natural (Natural)
import Unison.ABT.Normalized (pattern TAbs, pattern TAbss)
import Unison.ConstructorReference
import Unison.ConstructorType qualified as CT
import Unison.Hash (toShort)
import Unison.Reference
import Unison.ReferentPrime
import Unison.Runtime.ANF
import Unison.Runtime.ANF.POp
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Exception
import Unison.Runtime.Foreign.Function.Type
import Unison.Runtime.Referenced
import Unison.Runtime.Serialize (naturalToWord64s)
import Unison.Runtime.TypeTags (mapBinTag, mapTipTag)
import Unison.Util.Bytes (Bytes)
import Unison.Util.Bytes qualified as B
import Unison.Util.EnumContainers qualified as EC
import Unison.Util.Text as UT hiding (reverse, pattern Text)
import Unison.Var (Var)

hash64ValueUntyped :: Referenced Value -> Hash64
hash64ValueUntyped = hash64 . UV

newtype UntypedValue = UV (Referenced Value)

instance Hashable64 UntypedValue where
  hash64Add = \case
    UV (WithRefs tyrs tmrs val) ->
      hash64AddValue (hrefIndexed tyrs tmrs) val
    UV (Plain val) ->
      hash64AddValue hrefPlain val

data HRefs r = HRefs
  { hash64AddTypeRef :: r -> Hash64 -> Hash64,
    hash64AddTermRef :: r -> Hash64 -> Hash64
  }

hash64AddShort :: SBS.ShortByteString -> Hash64 -> Hash64
hash64AddShort = flip $ SBS.foldl (flip $ hash64AddInt . fromIntegral)

hash64AddRef :: Reference -> Hash64 -> Hash64
hash64AddRef (ReferenceBuiltin tx) h =
  DT.foldl' (flip hash64Add) (hash64AddInt 1 h) tx
hash64AddRef (ReferenceDerived (Id bs n)) h =
  hash64Add n . hash64AddShort (toShort bs) $ hash64AddInt 2 h

hrefPlain :: HRefs Reference
hrefPlain = HRefs hash64AddRef hash64AddRef

hrefIndexed :: [Reference] -> [Reference] -> HRefs RefNum
hrefIndexed tyrs tmrs = HRefs (ix tyhs) (ix tmhs)
  where
    tyhs = PA.arrayFromList $ hash64AddRef <$> tyrs
    tmhs = PA.arrayFromList $ hash64AddRef <$> tmrs

    ix arr (RefNum i) = PA.indexArray arr i

pushCtx :: [v] -> [v] -> [v]
pushCtx new old = reverse new ++ old

hash64AddTermRefs :: HRefs r -> [r] -> Hash64 -> Hash64
hash64AddTermRefs rf = flip $ foldl' (flip $ hash64AddTypeRef rf)

hash64AddTypeRefs :: HRefs r -> [r] -> Hash64 -> Hash64
hash64AddTypeRefs rf = flip $ foldl' (flip $ hash64AddTypeRef rf)

hash64AddValue :: (Show r) => HRefs r -> Value r -> Hash64 -> Hash64
hash64AddValue rs = \case
  Partial gr vals ->
    hash64AddInt 1
      `combine` hash64AddGroupRef rs gr
      `combine` hash64AddValues rs vals
  Data _ t vals ->
    hash64AddInt 2
      `combine` hash64Add t
      `combine` hash64AddValues rs vals
  Cont vals k ->
    hash64AddInt 3
      `combine` hash64AddValues rs vals
      `combine` hash64AddCont rs k
  BLit (Map assocs) ->
    hash64AddUMap rs (M.fromDistinctAscList assocs)
  BLit lit ->
    hash64AddInt 4 `combine` hash64AddBLit rs lit

hash64AddGroupRef :: HRefs r -> GroupRef r -> Hash64 -> Hash64
hash64AddGroupRef rs (GR i k) =
  hash64AddTermRef rs i `combine` hash64Add k

hash64AddValues ::
  (Show r) => (Foldable f) => HRefs r -> f (Value r) -> Hash64 -> Hash64
hash64AddValues rs vs h = foldl' (flip $ hash64AddValue rs) h vs

hash64AddCont :: HRefs r -> Cont r -> Hash64 -> Hash64
hash64AddCont rs = \case
  KE -> hash64AddInt 1
  Mark n _ps _vs k ->
    hash64AddInt 2
      `combine` hash64Add n
      `combine` hash64AddCont rs k
  Push m n g k ->
    hash64AddInt 3
      `combine` hash64Add m
      `combine` hash64Add n
      `combine` hash64AddGroupRef rs g
      `combine` hash64AddCont rs k

hash64AddReferent :: HRefs r -> Referent' r -> Hash64 -> Hash64
hash64AddReferent rs (Ref' i) =
  hash64AddInt 1
    `combine` hash64AddTermRef rs i
hash64AddReferent rs (Con' (ConstructorReference i j) ct) =
  hash64AddInt 2
    `combine` hash64AddTypeRef rs i
    `combine` hash64Add j
    `combine` hash64AddInt k
  where
    k = case ct of
      CT.Data -> 1
      CT.Effect -> 2

hash64AddWord8 :: Word8 -> Hash64 -> Hash64
hash64AddWord8 = hash64AddInt . fromIntegral

hash64AddByteArray :: PA.ByteArray -> Hash64 -> Hash64
hash64AddByteArray bs = go 0
  where
    sz = PA.sizeofByteArray bs
    go i !h
      | i >= sz = h
      | b <- PA.indexByteArray bs i = go (i + 1) (hash64AddWord8 b h)

hash64AddDouble :: Double -> Hash64 -> Hash64
hash64AddDouble d = hash64AddInt i
  where
    i = PA.indexByteArray (PA.byteArrayFromList [d]) 0

-- Hash an arbitrary precision Integer by hashing sign + magnitude as Word64 chunks
hash64AddInteger :: Integer -> Hash64 -> Hash64
hash64AddInteger i h =
  let sign = if i >= 0 then 0 else 1
      chunks = naturalToWord64s (fromInteger (abs i))
   in foldl' (flip hash64Add) (hash64AddInt sign h) chunks

-- Hash an arbitrary precision Natural by hashing as Word64 chunks
hash64AddNatural :: Natural -> Hash64 -> Hash64
hash64AddNatural n h =
  let chunks = naturalToWord64s n
   in foldl' (flip hash64Add) h chunks

hash64AddBLit :: (Show r) => HRefs r -> BLit r -> Hash64 -> Hash64
hash64AddBLit rs = \case
  Text tx ->
    hash64AddInt 1 `combine` UT.hash64AddText tx
  List vs ->
    hash64AddInt 2 `combine` hash64AddValues rs vs
  TmLink rn ->
    hash64AddInt 3 `combine` hash64AddReferent rs rn
  TyLink i ->
    hash64AddInt 4 `combine` hash64AddTypeRef rs i
  Bytes bs ->
    hash64AddInt 5 `combine` B.hash64AddBytes bs
  Quote v ->
    hash64AddInt 6 `combine` hash64AddValue rs v
  Code co ->
    hash64AddInt 7 `combine` hash64AddCode rs co
  BArr bs ->
    hash64AddInt 8 `combine` hash64AddByteArray bs
  Arr vs ->
    hash64AddInt 9 `combine` hash64AddValues rs vs
  Pos w ->
    hash64AddInt 10 `combine` hash64Add w
  Neg w ->
    hash64AddInt 11 `combine` hash64Add w
  Char c ->
    hash64AddInt 12 `combine` hash64Add c
  Float d ->
    hash64AddInt 13 `combine` hash64AddDouble d
  Map _ ->
    exn [] "hash64AddBLit: encountered Map, should be impossible"
  BigInt i ->
    hash64AddInt 14 `combine` hash64AddInteger i
  BigNat n ->
    hash64AddInt 15 `combine` hash64AddNatural n

hash64AddCode :: (Show r) => HRefs r -> Code r -> Hash64 -> Hash64
hash64AddCode rs (CodeRep sg _) = hash64AddGroup rs sg

hash64AddGroup ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  SuperGroup r v ->
  Hash64 ->
  Hash64
hash64AddGroup rs (Rec bs bd) h =
  hash64AddSuper rs ctx (foldl' (hash64AddSuper rs ctx) h cs) bd
  where
    (us, cs) = unzip bs
    ctx = reverse us

hash64AddSuper ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  [v] ->
  Hash64 ->
  SuperNormal r v ->
  Hash64
hash64AddSuper rs ctx h (Lambda (length -> n) bd) =
  hash64AddNormal rs ctx bd $ hash64AddInt n h

-- Note: seemingly reversed order to add the left-most parts first.
hash64AddNormal ::
  (Show r) => (Var v) => HRefs r -> [v] -> ANormal r v -> Hash64 -> Hash64
hash64AddNormal rs ctx = \case
  TLets _ vs (length -> n) bn bd ->
    hash64AddInt 1
      `combine` hash64AddInt n
      `combine` hash64AddNormal rs ctx bn
      `combine` hash64AddNormal rs (pushCtx vs ctx) bd
  TName v rf as bd ->
    hash64AddInt 2
      `combine` hash64AddEither (hash64AddTermRef rs) (hash64AddVar ctx) rf
      `combine` hash64AddVars ctx as
      `combine` hash64AddNormal rs (pushCtx [v] ctx) bd
  TLit l ->
    hash64AddInt 3
      `combine` hash64AddLit rs l
  TBLit l ->
    hash64AddInt 4
      `combine` hash64AddLit rs l
  TMatch sc brs ->
    hash64AddInt 5
      `combine` hash64AddVar ctx sc
      `combine` hash64AddBranches rs ctx brs
  TShift p v bd ->
    hash64AddInt 6
      `combine` hash64AddTypeRef rs p
      `combine` hash64AddNormal rs (pushCtx [v] ctx) bd
  THnd is hn ha bd ->
    hash64AddInt 7
      `combine` hash64AddTypeRefs rs is
      `combine` hash64AddVar ctx hn
      `combine` hash64AddMaybe (hash64AddVar ctx) ha
      `combine` hash64AddNormal rs ctx bd
  TApp fn as ->
    hash64AddInt 8
      `combine` hash64AddFunc rs ctx fn
      `combine` hash64AddVars ctx as
  TFrc v ->
    hash64AddInt 9
      `combine` hash64AddVar ctx v
  TVar v ->
    hash64AddInt 10
      `combine` hash64AddVar ctx v
  TAbs v (TAbss vs body) ->
    hash64AddNormal rs (pushCtx (v : vs) ctx) body
  v -> exn [] ("hash64AddNormal: unsupported value: " ++ show v)

hash64AddVar :: (Var v) => [v] -> v -> Hash64 -> Hash64
hash64AddVar ctx v h
  | Just i <- index 0 ctx = hash64AddInt i h
  | otherwise = exn [] "hash64AddVar: variable not in context"
  where
    index !_ [] = Nothing
    index !n (u : us)
      | v == u = Just n
      | otherwise = index (n + 1) us

hash64AddVars :: (Var v) => [v] -> [v] -> Hash64 -> Hash64
hash64AddVars ctx = flip $ foldl' (flip $ hash64AddVar ctx)

hash64AddFunc :: (Var v) => HRefs r -> [v] -> Func r v -> Hash64 -> Hash64
hash64AddFunc rf ctx = \case
  FVar v ->
    hash64AddInt 1
      `combine` hash64AddVar ctx v
  FComb i ->
    hash64AddInt 2
      `combine` hash64AddTermRef rf i
  FCont v ->
    hash64AddInt 3
      `combine` hash64AddVar ctx v
  FCon i t ->
    hash64AddInt 4
      `combine` hash64AddTermRef rf i
      `combine` hash64Add (rawTag t)
  FReq i t ->
    hash64AddInt 5
      `combine` hash64AddTermRef rf i
      `combine` hash64Add (rawTag t)
  FPrim ins ->
    hash64AddInt 6
      `combine` hash64AddEither hash64AddPOp hash64AddForeign ins

hash64AddLit :: HRefs r -> Lit r -> Hash64 -> Hash64
hash64AddLit rs = \case
  I i -> hash64AddInt 1 `combine` hash64AddInt (fromIntegral i)
  N n -> hash64AddInt 2 `combine` hash64Add n
  F d -> hash64AddInt 3 `combine` hash64AddDouble d
  T t -> hash64AddInt 4 `combine` UT.hash64AddText t
  C c -> hash64AddInt 5 `combine` hash64Add c
  LM rn -> hash64AddInt 6 `combine` hash64AddReferent rs rn
  LY i -> hash64AddInt 7 `combine` hash64AddTypeRef rs i

hash64AddBranches ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  [v] ->
  Branched r (ANormal r v) ->
  Hash64 ->
  Hash64
hash64AddBranches rs ctx = \case
  MatchEmpty -> hash64AddInt 1
  MatchIntegral bs df ->
    hash64AddInt 2
      `combine` hash64AddEMap (hash64AddNAssoc rs ctx) bs
      `combine` hash64AddMaybe (hash64AddNormal rs ctx) df
  MatchText bs df ->
    hash64AddInt 3
      `combine` hash64AddMap (hash64AddTAssoc rs ctx) bs
      `combine` hash64AddMaybe (hash64AddNormal rs ctx) df
  MatchRequest bs pur ->
    hash64AddInt 4
      `combine` hash64AddAssocs (hash64AddRAssoc rs ctx) bs
      `combine` hash64AddNormal rs ctx pur
  MatchData _ bs df ->
    hash64AddInt 5
      `combine` hash64AddEMap (hash64AddDAssoc rs ctx) bs
      `combine` hash64AddMaybe (hash64AddNormal rs ctx) df
  MatchSum bs ->
    hash64AddInt 6
      `combine` hash64AddEMap (hash64AddSAssoc rs ctx) bs
  MatchNumeric _ bs df ->
    hash64AddInt 7
      `combine` hash64AddEMap (hash64AddNAssoc rs ctx) bs
      `combine` hash64AddMaybe (hash64AddNormal rs ctx) df
  MatchBytes bs df ->
    hash64AddInt 8
      `combine` hash64AddMap (hash64AddYAssoc rs ctx) bs
      `combine` hash64AddMaybe (hash64AddNormal rs ctx) df

hash64AddNAssoc ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  [v] ->
  Word64 ->
  ANormal r v ->
  Hash64 ->
  Hash64
hash64AddNAssoc rs ctx n tm =
  hash64Add n `combine` hash64AddNormal rs ctx tm

hash64AddTAssoc ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  [v] ->
  UT.Text ->
  ANormal r v ->
  Hash64 ->
  Hash64
hash64AddTAssoc rs ctx t tm =
  UT.hash64AddText t `combine` hash64AddNormal rs ctx tm

hash64AddYAssoc ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  [v] ->
  Bytes ->
  ANormal r v ->
  Hash64 ->
  Hash64
hash64AddYAssoc rs ctx b tm =
  B.hash64AddBytes b `combine` hash64AddNormal rs ctx tm

hash64AddDAssoc ::
  (Show r) =>
  (Var v) =>
  HRefs r ->
  [v] ->
  CTag ->
  ([Mem], ANormal r v) ->
  Hash64 ->
  Hash64
hash64AddDAssoc rs ctx (rawTag -> t) (length -> n, tm) =
  hash64Add t
    `combine` hash64AddInt n
    `combine` hash64AddNormal rs ctx tm

hash64AddSAssoc ::
  (Show r) => (Var v) => HRefs r -> [v] -> Word64 -> ([Mem], ANormal r v) -> Hash64 -> Hash64
hash64AddSAssoc rs ctx t (length -> n, tm) =
  hash64Add t
    `combine` hash64AddInt n
    `combine` hash64AddNormal rs ctx tm

hash64AddRAssoc ::
  (Show r) => (Var v) => HRefs r -> [v] -> r -> EC.EnumMap CTag ([Mem], ANormal r v) -> Hash64 -> Hash64
hash64AddRAssoc rs ctx i bs =
  hash64AddTypeRef rs i
    `combine` hash64AddEMap addInner bs
  where
    addInner t (length -> n, tm) =
      hash64Add (rawTag t)
        `combine` hash64AddInt n
        `combine` hash64AddNormal rs ctx tm

hash64AddPOp :: POp -> Hash64 -> Hash64
hash64AddPOp p = hash64AddInt . fromIntegral $ pOpCode p

hash64AddForeign :: ForeignFunc -> Hash64 -> Hash64
hash64AddForeign _f = id

hash64AddEither ::
  (a -> Hash64 -> Hash64) ->
  (b -> Hash64 -> Hash64) ->
  Either a b ->
  Hash64 ->
  Hash64
hash64AddEither l r = \case
  Left x -> l x . hash64AddInt 1
  Right y -> r y . hash64AddInt 2

hash64AddMaybe :: (a -> Hash64 -> Hash64) -> Maybe a -> Hash64 -> Hash64
hash64AddMaybe h = \case
  Nothing -> hash64AddInt 1
  Just x -> hash64AddInt 2 `combine` h x

rot :: (a -> b -> c -> d) -> c -> a -> b -> d
rot f z x y = f x y z

hash64AddAssocs ::
  (k -> v -> Hash64 -> Hash64) -> [(k, v)] -> Hash64 -> Hash64
hash64AddAssocs f = flip $ foldl' (flip $ uncurry f)

hash64AddEMap ::
  (EC.EnumKey k) =>
  (k -> v -> Hash64 -> Hash64) ->
  EC.EnumMap k v ->
  Hash64 ->
  Hash64
hash64AddEMap f = flip $ EC.foldlWithKey (rot f)

hash64AddMap ::
  (k -> v -> Hash64 -> Hash64) -> M.Map k v -> Hash64 -> Hash64
hash64AddMap f = flip $ M.foldlWithKey' (rot f)

-- Serializes a map as if it were a unison data type
hash64AddUMap ::
  (Show r) => HRefs r -> M.Map (Value r) (Value r) -> Hash64 -> Hash64
hash64AddUMap rs m h = case m of
  M.Tip ->
    hash64Add (maskTags mapTipTag) $
      hash64AddInt 2 h -- data type
  M.Bin sz k v l r ->
    hash64AddUMap rs r
      . hash64AddUMap rs l
      . hash64AddValue rs v
      . hash64AddValue rs k
      . hash64AddInt sz
      . hash64Add (maskTags mapBinTag)
      $ hash64AddInt 2 h

hash64AddAssoc ::
  (Hashable64 k) =>
  (k -> Hash64 -> Hash64) ->
  (v -> Hash64 -> Hash64) ->
  (k -> v -> Hash64 -> Hash64)
hash64AddAssoc f g k v = g v . f k

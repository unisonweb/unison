
module Unison.Runtime.ANF.Serialize.ValueV5
  ( getValueWithHeader,
    putValueWithHeader
  ) where

import Control.Monad (replicateM)
import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Put
import Data.Primitive.Array
  (Array, arrayFromListN, sizeofArray, indexArray)
import GHC.IsList qualified (fromList)
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.ANF.Serialize.Tags
import Unison.Runtime.Canonicalizer
import Unison.Runtime.Exception
import Unison.Runtime.Serialize
  hiding
    ( getConstructorReference
    , putConstructorReference
    , getReference
    , putReference
    , getReferent
    , putReferent
    )
import Unison.Runtime.Serialize qualified as SER
import Unison.Util.Text qualified as Util.Text
import Prelude hiding (getChar, putChar)

import Unison.ConstructorReference
import Unison.Referent (Referent, pattern Con, pattern Ref)

import Data.Binary.Get qualified as BGet
import Data.Binary.Put qualified as BPut
import Data.Serialize.Get qualified as SGet
import Data.Serialize.Put qualified as SPut

import Unison.Runtime.ANF.Serialize.CodeV4

-- types, terms
type GetRefLookup = (Array Reference, Array Reference)
type PutRefLookup = (CanonMap Reference Int, CanonMap Reference Int)

lookupRef :: Monad m => Array Reference -> Int -> m Reference
lookupRef arr i
  | 0 <= i && i < sizeofArray arr = pure $ indexArray arr i
  | otherwise = exn $ "lookupRef: index out of bounds: " ++ show i

putReference ::
  (MonadPut m) => CanonMap Reference Int -> Reference -> m ()
putReference cm r
  | Just i <- unsafeLookup r cm = putVarInt i
  | otherwise = exn $ "could not serialize reference: " ++ show r

getReference :: (MonadGet m) => Array Reference -> m Reference
getReference refm = getVarInt >>= lookupRef refm

putReferent ::
  (MonadPut m) => PutRefLookup -> Referent -> m ()
putReferent (tys, tms) = \case
  Ref r -> do
    putWord8 0
    putReference tms r
  Con r ct -> do
    putWord8 1
    putConstructorReference tys r
    putConstructorType ct

getReferent :: (MonadGet m) => GetRefLookup -> m Referent
getReferent (tys, tms) = do
  tag <- getWord8
  case tag of
    0 -> Ref <$> getReference tms
    1 -> Con <$> getConstructorReference tys <*> getConstructorType
    _ -> unknownTag "getReferent" tag

putConstructorReference ::
  (MonadPut m) => CanonMap Reference Int -> ConstructorReference -> m ()
putConstructorReference tys (ConstructorReference r i) = do
  putReference tys r
  putLength i

getConstructorReference ::
  (MonadGet m) => Array Reference -> m ConstructorReference
getConstructorReference tys =
  ConstructorReference <$> getReference tys <*> getLength

putGroupRef :: (MonadPut m) => CanonMap Reference Int -> GroupRef -> m ()
putGroupRef tms (GR r i) =
  putReference tms r *> putWord64be i

getGroupRef :: (MonadGet m) => Array Reference -> m GroupRef
getGroupRef tms = GR <$> getReference tms <*> getWord64be

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
--
-- -----------------
--
-- Version 5 departs significantly from previous versions to try to
-- optimize the format. The two major changes are:
--
--   1. Using variable width integers everywhere to avoid
--      unnecessarily padded values for tags and such
--   2. Pulling references out into a header section so that each is
--      only serialized once, and most reference occurrences are just
--      numbers.
--
-- this second change reduces the size of the format considerably, as
-- most of the serialized representation previously was repeated
-- occurrences of the 64 byte hashes.
--
-- At the moment, the reference change is implemented by using stable
-- names, and expecting canonically constructed values. This was done
-- to avoid a significant refactoring effort, but should be able to be
-- reworked in the future.
putValue :: (MonadPut m) => PutRefLookup -> Value -> m ()
putValue pref@(tys, tms) = \case
  Partial gr vs ->
    putTag PartialT
      *> putGroupRef tms gr
      *> putFoldable (putValue pref) vs
  Data r t vs ->
    putTag DataT
      *> putReference tys r
      *> putVarInt t
      *> putFoldable (putValue pref) vs
  Cont bs k ->
    putTag ContT
      *> putFoldable (putValue pref) bs
      *> putCont pref k
  BLit l -> putTag BLitT *> putBLit pref l
{-# SPECIALIZE putValue :: PutRefLookup -> Value -> BPut.Put #-}
{-# SPECIALIZE putValue :: PutRefLookup -> Value -> SPut.Put #-}

getValue :: (MonadGet m) => GetRefLookup -> m Value
getValue gref@(tys, tms) =
  getTag >>= \case
    PartialT -> do
      gr <- getGroupRef tms
      vs <- getList (getValue gref)
      pure $ Partial gr vs
    DataT -> do
      r <- getReference tys
      w <- getVarInt
      vs <- getList (getValue gref)
      pure $ Data r w vs
    ContT -> do
      bs <- getList (getValue gref)
      k <- getCont gref
      pure $ Cont bs k
    BLitT -> BLit <$> getBLit gref
{-# SPECIALIZE getValue :: GetRefLookup -> BGet.Get Value #-}
{-# SPECIALIZE getValue :: GetRefLookup -> SGet.Get Value #-}

putCont :: (MonadPut m) => PutRefLookup -> Cont -> m ()
putCont pref@(tys, tms) = \case
  KE -> putTag KET
  Mark a rs ds k ->
    putTag MarkT
      *> putVarInt a
      *> putFoldable (putReference tys) rs
      *> putMap (putReference tys) (putValue pref) ds
      *> putCont pref k
  Push f n gr k ->
    putTag PushT
      *> putVarInt f
      *> putVarInt n
      *> putGroupRef tms gr
      *> putCont pref k
{-# SPECIALIZE putCont :: PutRefLookup -> Cont -> BPut.Put #-}
{-# SPECIALIZE putCont :: PutRefLookup -> Cont -> SPut.Put #-}

getCont :: (MonadGet m) => GetRefLookup -> m Cont
getCont gref@(tys, tms) =
  getTag >>= \case
    KET -> pure KE
    MarkT ->
      Mark
        <$> getVarInt
        <*> getList (getReference tys)
        <*> getMap (getReference tys) (getValue gref)
        <*> getCont gref
    PushT ->
      Push
        <$> getVarInt
        <*> getVarInt
        <*> getGroupRef tms
        <*> getCont gref
{-# SPECIALIZE getCont :: GetRefLookup -> BGet.Get Cont #-}
{-# SPECIALIZE getCont :: GetRefLookup -> SGet.Get Cont #-}

putBLit :: (MonadPut m) => PutRefLookup -> BLit -> m ()
putBLit pref@(tys, _) = \case
  Text t -> putTag TextT *> putText (Util.Text.toText t)
  List s -> putTag ListT *> putFoldable (putValue pref) s
  TmLink r -> putTag TmLinkT *> putReferent pref r
  TyLink r -> putTag TyLinkT *> putReference tys r
  Bytes b -> putTag BytesT *> putBytes b
  Quote vl -> putTag QuoteT *> putValue pref vl
  Code (CodeRep sg ch) ->
    putTag tag *> putGroup mempty False sg
    where
      tag
        | Cacheable <- ch = CachedCodeT
        | otherwise = CodeT
  BArr a -> putTag BArrT *> putByteArray a
  Pos n -> putTag PosT *> putPositive n
  Neg n -> putTag NegT *> putPositive n
  Char c -> putTag CharT *> putChar c
  Float d -> putTag FloatT *> putFloat d
  Arr a -> putTag ArrT *> putFoldable (putValue pref) a
{-# SPECIALIZE putBLit :: PutRefLookup -> BLit -> BPut.Put #-}
{-# SPECIALIZE putBLit :: PutRefLookup -> BLit -> SPut.Put #-}

getBLit :: (MonadGet m) => GetRefLookup -> m BLit
getBLit gref@(tys, _) =
  getTag >>= \case
    TextT -> Text . Util.Text.fromText <$> getText
    ListT -> List <$> getSeq (getValue gref)
    TmLinkT -> TmLink <$> getReferent gref
    TyLinkT -> TyLink <$> getReference tys
    BytesT -> Bytes <$> getBytes
    QuoteT -> Quote <$> getValue gref
    CodeT ->
      Code . flip CodeRep Uncacheable <$> getGroup
    BArrT -> BArr <$> getByteArray
    PosT -> Pos <$> getPositive
    NegT -> Neg <$> getPositive
    CharT -> Char <$> getChar
    FloatT -> Float <$> getFloat
    ArrT -> Arr . GHC.IsList.fromList <$> getList (getValue gref)
    CachedCodeT -> Code . flip CodeRep Cacheable <$> getGroup
{-# SPECIALIZE getBLit :: GetRefLookup -> BGet.Get BLit #-}
{-# SPECIALIZE getBLit :: GetRefLookup -> SGet.Get BLit #-}

putValueWithHeader ::
  (MonadPut m) => [Reference] -> [Reference] -> Value -> m ()
putValueWithHeader tyrs tmrs v =
  putFoldable SER.putReference tyrs *>
  putFoldable SER.putReference tmrs *>
    putValue (fromListByIndex tyrs, fromListByIndex tmrs) v
{-# SPECIALIZE putValueWithHeader ::
      [Reference] -> [Reference] -> Value -> BPut.Put #-}
{-# SPECIALIZE putValueWithHeader ::
      [Reference] -> [Reference] -> Value -> SPut.Put #-}

getValueWithHeader :: (MonadGet m) => m (Referenced Value)
getValueWithHeader = do
  tyl <- getLength
  tys <- replicateM tyl SER.getReference
  tml <- getLength
  tms <- replicateM tml SER.getReference
  v <- getValue (arrayFromListN tyl tys, arrayFromListN tml tms)
  pure (WithRefs tys tms v)
{-# SPECIALIZE getValueWithHeader :: BGet.Get (Referenced Value) #-}
{-# SPECIALIZE getValueWithHeader :: SGet.Get (Referenced Value) #-}


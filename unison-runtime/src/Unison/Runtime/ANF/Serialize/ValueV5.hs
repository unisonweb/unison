
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

type GetRefLookup = Array Reference
type PutRefLookup = CanonMap Reference Int

lookupRef :: Monad m => GetRefLookup -> Int -> m Reference
lookupRef arr i
  | 0 <= i && i < sizeofArray arr = pure $ indexArray arr i
  | otherwise = exn $ "lookupRef: index out of bounds: " ++ show i

putReference ::
  (MonadPut m) => PutRefLookup -> Reference -> m ()
putReference cm r
  | Just i <- unsafeLookup r cm = putVarInt i
  | otherwise = exn $ "could not serialize reference: " ++ show r

getReference :: (MonadGet m) => GetRefLookup -> m Reference
getReference refm = getVarInt >>= lookupRef refm

putReferent ::
  (MonadPut m) => PutRefLookup -> Referent -> m ()
putReferent refm = \case
  Ref r -> do
    putWord8 0
    putReference refm r
  Con r ct -> do
    putWord8 1
    putConstructorReference refm r
    putConstructorType ct

getReferent :: (MonadGet m) => GetRefLookup -> m Referent
getReferent refm = do
  tag <- getWord8
  case tag of
    0 -> Ref <$> getReference refm
    1 -> Con <$> getConstructorReference refm <*> getConstructorType
    _ -> unknownTag "getReferent" tag

putHeader :: (MonadPut m) => [Reference] -> m ()
putHeader = putFoldable SER.putReference

putConstructorReference ::
  (MonadPut m) => PutRefLookup -> ConstructorReference -> m ()
putConstructorReference refm (ConstructorReference r i) = do
  putReference refm r
  putLength i

getConstructorReference ::
  (MonadGet m) => GetRefLookup -> m ConstructorReference
getConstructorReference refm =
  ConstructorReference <$> getReference refm <*> getLength

putGroupRef :: (MonadPut m) => PutRefLookup -> GroupRef -> m ()
putGroupRef refm (GR r i) =
  putReference refm r *> putWord64be i

getGroupRef :: (MonadGet m) => GetRefLookup -> m GroupRef
getGroupRef refm = GR <$> getReference refm <*> getWord64be

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
putValue refm (Partial gr vs) =
  putTag PartialT
    *> putGroupRef refm gr
    *> putFoldable (putValue refm) vs
putValue refm (Data r t vs) =
  putTag DataT
    *> putReference refm r
    *> putVarInt t
    *> putFoldable (putValue refm) vs
putValue refm (Cont bs k) =
  putTag ContT
    *> putFoldable (putValue refm) bs
    *> putCont refm k
putValue refm (BLit l) =
  putTag BLitT *> putBLit refm l
{-# SPECIALIZE putValue :: PutRefLookup -> Value -> BPut.Put #-}
{-# SPECIALIZE putValue :: PutRefLookup -> Value -> SPut.Put #-}

getValue :: (MonadGet m) => GetRefLookup -> m Value
getValue refm =
  getTag >>= \case
    PartialT -> do
      gr <- getGroupRef refm
      vs <- getList (getValue refm)
      pure $ Partial gr vs
    DataT -> do
      r <- getReference refm
      w <- getVarInt
      vs <- getList (getValue refm)
      pure $ Data r w vs
    ContT -> do
      bs <- getList (getValue refm)
      k <- getCont refm
      pure $ Cont bs k
    BLitT -> BLit <$> getBLit refm
{-# SPECIALIZE getValue :: GetRefLookup -> BGet.Get Value #-}
{-# SPECIALIZE getValue :: GetRefLookup -> SGet.Get Value #-}

putCont :: (MonadPut m) => PutRefLookup -> Cont -> m ()
putCont _ KE = putTag KET
putCont refm (Mark a rs ds k) =
  putTag MarkT
    *> putVarInt a
    *> putFoldable (putReference refm) rs
    *> putMap (putReference refm) (putValue refm) ds
    *> putCont refm k
putCont refm (Push f n gr k) =
  putTag PushT
    *> putVarInt f
    *> putVarInt n
    *> putGroupRef refm gr
    *> putCont refm k
{-# SPECIALIZE putCont :: PutRefLookup -> Cont -> BPut.Put #-}
{-# SPECIALIZE putCont :: PutRefLookup -> Cont -> SPut.Put #-}

getCont :: (MonadGet m) => GetRefLookup -> m Cont
getCont refm =
  getTag >>= \case
    KET -> pure KE
    MarkT ->
      Mark
        <$> getVarInt
        <*> getList (getReference refm)
        <*> getMap (getReference refm) (getValue refm)
        <*> getCont refm
    PushT ->
      Push
        <$> getVarInt
        <*> getVarInt
        <*> getGroupRef refm
        <*> getCont refm
{-# SPECIALIZE getCont :: GetRefLookup -> BGet.Get Cont #-}
{-# SPECIALIZE getCont :: GetRefLookup -> SGet.Get Cont #-}

putBLit :: (MonadPut m) => PutRefLookup -> BLit -> m ()
putBLit _ (Text t) = putTag TextT *> putText (Util.Text.toText t)
putBLit refm (List s) = putTag ListT *> putFoldable (putValue refm) s
putBLit refm (TmLink r) = putTag TmLinkT *> putReferent refm r
putBLit refm (TyLink r) = putTag TyLinkT *> putReference refm r
putBLit _ (Bytes b) = putTag BytesT *> putBytes b
putBLit refm (Quote vl) = putTag QuoteT *> putValue refm vl
putBLit _ (Code (CodeRep sg ch)) =
  putTag tag *> putGroup mempty False sg
  where
    tag
      | Cacheable <- ch = CachedCodeT
      | otherwise = CodeT
putBLit _ (BArr a) = putTag BArrT *> putByteArray a
putBLit _ (Pos n) = putTag PosT *> putPositive n
putBLit _ (Neg n) = putTag NegT *> putPositive n
putBLit _ (Char c) = putTag CharT *> putChar c
putBLit _ (Float d) = putTag FloatT *> putFloat d
putBLit refm (Arr a) = putTag ArrT *> putFoldable (putValue refm) a
{-# SPECIALIZE putBLit :: PutRefLookup -> BLit -> BPut.Put #-}
{-# SPECIALIZE putBLit :: PutRefLookup -> BLit -> SPut.Put #-}

getBLit :: (MonadGet m) => GetRefLookup -> m BLit
getBLit refm =
  getTag >>= \case
    TextT -> Text . Util.Text.fromText <$> getText
    ListT -> List <$> getSeq (getValue refm)
    TmLinkT -> TmLink <$> getReferent refm
    TyLinkT -> TyLink <$> getReference refm
    BytesT -> Bytes <$> getBytes
    QuoteT -> Quote <$> getValue refm
    CodeT ->
      Code . flip CodeRep Uncacheable <$> getGroup
    BArrT -> BArr <$> getByteArray
    PosT -> Pos <$> getPositive
    NegT -> Neg <$> getPositive
    CharT -> Char <$> getChar
    FloatT -> Float <$> getFloat
    ArrT -> Arr . GHC.IsList.fromList <$> getList (getValue refm)
    CachedCodeT -> Code . flip CodeRep Cacheable <$> getGroup
{-# SPECIALIZE getBLit :: GetRefLookup -> BGet.Get BLit #-}
{-# SPECIALIZE getBLit :: GetRefLookup -> SGet.Get BLit #-}

putValueWithHeader :: (MonadPut m) => [Reference] -> Value -> m ()
putValueWithHeader rs v = putHeader rs *> putValue (fromListByIndex rs) v
{-# SPECIALIZE putValueWithHeader :: [Reference] -> Value -> BPut.Put #-}
{-# SPECIALIZE putValueWithHeader :: [Reference] -> Value -> SPut.Put #-}

getValueWithHeader :: (MonadGet m) => m (Referenced Value)
getValueWithHeader = do
  l <- getLength
  h <- replicateM l SER.getReference
  v <- getValue (arrayFromListN l h)
  pure (WithRefs h v)
{-# SPECIALIZE getValueWithHeader :: BGet.Get (Referenced Value) #-}
{-# SPECIALIZE getValueWithHeader :: SGet.Get (Referenced Value) #-}


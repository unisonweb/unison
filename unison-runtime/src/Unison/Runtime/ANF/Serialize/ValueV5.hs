module Unison.Runtime.ANF.Serialize.ValueV5
  ( getValueWithHeader,
    putValueWithHeader,
    putVersionedValue,
    versionedValueBytes,
  )
where

import Control.Monad (replicateM)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.ByteString.Lazy qualified as L
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.ANF.Serialize.CodeV4
import Unison.Runtime.ANF.Serialize.Tags
import Unison.Runtime.Referenced
import Unison.Runtime.Serialize hiding
  ( getConstructorReference,
    getReference,
    getReferent,
    putConstructorReference,
    putReference,
    putReferent,
  )
import Unison.Runtime.Serialize qualified as SER
import Unison.Runtime.Serialize.Get
import Unison.Util.Text qualified as Util.Text
import Prelude hiding (getChar, putChar)

putGroupRef :: GroupRef RefNum -> Builder
putGroupRef (GR r i) = putRefNum r <> putVarInt i
{-# INLINE putGroupRef #-}

getGroupRef :: (PrimBase m) => Get m (GroupRef RefNum)
getGroupRef = GR <$> getRefNum <*> getVarInt
{-# INLINE getGroupRef #-}

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
putValue :: Value RefNum -> Builder
putValue = \case
  Partial gr vs ->
    putTag PartialT
      <> putGroupRef gr
      <> putFoldable putValue vs
  Data r t vs ->
    putTag DataT
      <> putRefNum r
      <> putVarInt t
      <> putFoldable putValue vs
  Cont bs k ->
    putTag ContT
      <> putFoldable putValue bs
      <> putCont k
  BLit l -> putTag BLitT <> putBLit l

getValue :: (PrimBase m) => Get m (Value RefNum)
getValue =
  getTag >>= \case
    PartialT -> do
      gr <- getGroupRef
      vs <- getList getValue
      pure $ Partial gr vs
    DataT -> do
      r <- getRefNum
      w <- getVarInt
      vs <- getList getValue
      pure $ Data r w vs
    ContT -> do
      bs <- getList getValue
      k <- getCont
      pure $ Cont bs k
    BLitT -> BLit <$> getBLit
{-# INLINEABLE getValue #-}

putCont :: Cont RefNum -> Builder
putCont = \case
  KE -> putTag KET
  Mark a rs ds k ->
    putTag MarkT
      <> putVarInt a
      <> putFoldable putRefNum rs
      <> putMapping putRefNum putValue ds
      <> putCont k
  Push f n gr k ->
    putTag PushT
      <> putVarInt f
      <> putVarInt n
      <> putGroupRef gr
      <> putCont k

getCont :: (PrimBase m) => Get m (Cont RefNum)
getCont =
  getTag >>= \case
    KET -> pure KE
    MarkT ->
      Mark
        <$> getVarInt
        <*> getList getRefNum
        <*> getMapping getRefNum getValue
        <*> getCont
    PushT ->
      Push
        <$> getVarInt
        <*> getVarInt
        <*> getGroupRef
        <*> getCont
{-# INLINEABLE getCont #-}

putBLit :: BLit RefNum -> Builder
putBLit = \case
  Text t -> putTag TextT <> putText (Util.Text.toText t)
  List s -> putTag ListT <> putFoldable putValue s
  TmLink r -> putTag TmLinkT <> putNumberedReferent r
  TyLink r -> putTag TyLinkT <> putRefNum r
  Bytes b -> putTag BytesT <> putBytes b
  Quote vl -> putTag QuoteT <> putValue vl
  Code (CodeRep sg ch) ->
    putTag tag <> putGroup False sg
    where
      tag
        | Cacheable <- ch = CachedCodeT
        | otherwise = CodeT
  BArr a -> putTag BArrT <> putByteArray a
  Pos n -> putTag PosT <> putPositive n
  Neg n -> putTag NegT <> putPositive n
  Char c -> putTag CharT <> putChar c
  Float d -> putTag FloatT <> putFloat d
  Arr a -> putTag ArrT <> putFoldable putValue a
  Map m -> putTag MapT <> putMapping putValue putValue m
  BigInt i -> putTag BigIntT <> putInteger i
  BigNat n -> putTag BigNatT <> putNatural n

getBLit :: (PrimBase m) => Get m (BLit RefNum)
getBLit =
  getTag >>= \case
    TextT -> Text . Util.Text.fromText <$> getText
    ListT -> List <$> getSeq getValue
    TmLinkT -> TmLink <$> getNumberedReferent
    TyLinkT -> TyLink <$> getRefNum
    BytesT -> Bytes <$> getBytes
    QuoteT -> Quote <$> getValue
    CodeT ->
      Code . flip CodeRep Uncacheable <$> getGroup
    BArrT -> BArr <$> getByteArray
    PosT -> Pos <$> getPositive
    NegT -> Neg <$> getPositive
    CharT -> Char <$> getChar
    FloatT -> Float <$> getFloat
    ArrT -> Arr <$> getArray getValue
    CachedCodeT -> Code . flip CodeRep Cacheable <$> getGroup
    MapT -> Map <$> getMapping getValue getValue
    BigIntT -> BigInt <$> getInteger
    BigNatT -> BigNat <$> getNatural
{-# INLINEABLE getBLit #-}

putValueWithHeader ::
  [Reference] -> [Reference] -> Value RefNum -> Builder
putValueWithHeader tyrs tmrs v =
  putFoldable SER.putReference tyrs
    <> putFoldable SER.putReference tmrs
    <> putValue v

putVersionedValue ::
  [Reference] -> [Reference] -> Value RefNum -> Builder
putVersionedValue tyrs tmrs v =
  BU.word32BE 5 <> putValueWithHeader tyrs tmrs v

versionedValueBytes ::
  [Reference] -> [Reference] -> Value RefNum -> L.ByteString
versionedValueBytes tyrs tmrs v =
  BU.toLazyByteString $ putVersionedValue tyrs tmrs v

getValueWithHeader :: (PrimBase m) => Get m (Referenced Value)
getValueWithHeader = do
  tyl <- getLength
  tys <- replicateM tyl SER.getReference
  tml <- getLength
  tms <- replicateM tml SER.getReference
  v <- getValue
  pure (WithRefs tys tms v)
{-# INLINEABLE getValueWithHeader #-}

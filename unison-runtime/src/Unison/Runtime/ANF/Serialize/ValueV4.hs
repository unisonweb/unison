
module Unison.Runtime.ANF.Serialize.ValueV4 where


import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Put
import GHC.IsList qualified (fromList)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.ANF.Serialize.Tags
import Unison.Runtime.Serialize
import Unison.Util.Text qualified as Util.Text
import Prelude hiding (getChar, putChar)

import Data.Binary.Get qualified as BGet
import Data.Binary.Put qualified as BPut
import Data.Serialize.Get qualified as SGet
import Data.Serialize.Put qualified as SPut

import Unison.Runtime.ANF.Serialize.CodeV3

putGroupRef :: (MonadPut m) => GroupRef -> m ()
putGroupRef (GR r i) =
  putReference r *> putWord64be i

getGroupRef :: (MonadGet m) => m GroupRef
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
putValue :: (MonadPut m) => Value -> m ()
putValue (Partial gr vs) =
  putTag PartialT
    *> putGroupRef gr
    *> putFoldable putValue vs
putValue (Data r t vs) =
  putTag DataT
    *> putReference r
    *> putWord64be t
    *> putFoldable putValue vs
putValue (Cont bs k) =
  putTag ContT
    *> putFoldable putValue bs
    *> putCont k
putValue (BLit l) =
  putTag BLitT *> putBLit l
{-# SPECIALIZE putValue :: Value -> BPut.Put #-}
{-# SPECIALIZE putValue :: Value -> SPut.Put #-}

getValue :: (MonadGet m) => m Value
getValue =
  getTag >>= \case
    PartialT -> do
      gr <- getGroupRef
      vs <- getList getValue
      pure $ Partial gr vs
    DataT -> do
      r <- getReference
      w <- getWord64be
      vs <- getList getValue
      pure $ Data r w vs
    ContT -> do
      bs <- getList getValue
      k <- getCont
      pure $ Cont bs k
    BLitT -> BLit <$> getBLit
{-# SPECIALIZE getValue :: BGet.Get Value #-}
{-# SPECIALIZE getValue :: SGet.Get Value #-}

putCont :: (MonadPut m) => Cont -> m ()
putCont KE = putTag KET
putCont (Mark a rs ds k) =
  putTag MarkT
    *> putWord64be a
    *> putFoldable putReference rs
    *> putMap putReference putValue ds
    *> putCont k
putCont (Push f n gr k) =
  putTag PushT
    *> putWord64be f
    *> putWord64be n
    *> putGroupRef gr
    *> putCont k
{-# SPECIALIZE putCont :: Cont -> BPut.Put #-}
{-# SPECIALIZE putCont :: Cont -> SPut.Put #-}

getCont :: (MonadGet m) => m Cont
getCont =
  getTag >>= \case
    KET -> pure KE
    MarkT ->
      Mark
        <$> getWord64be
        <*> getList getReference
        <*> getMap getReference getValue
        <*> getCont
    PushT ->
      Push
        <$> getWord64be
        <*> getWord64be
        <*> getGroupRef
        <*> getCont
{-# SPECIALIZE getCont :: BGet.Get Cont #-}
{-# SPECIALIZE getCont :: SGet.Get Cont #-}

putBLit :: (MonadPut m) => BLit -> m ()
putBLit (Text t) = putTag TextT *> putText (Util.Text.toText t)
putBLit (List s) = putTag ListT *> putFoldable putValue s
putBLit (TmLink r) = putTag TmLinkT *> putReferent r
putBLit (TyLink r) = putTag TyLinkT *> putReference r
putBLit (Bytes b) = putTag BytesT *> putBytes b
putBLit (Quote vl) = putTag QuoteT *> putValue vl
putBLit (Code (CodeRep sg ch)) =
  putTag tag *> putGroup mempty False sg
  where
    -- Hashing treats everything as uncacheable for consistent
    -- results.
    tag
      | Cacheable <- ch = CachedCodeT
      | otherwise = CodeT
putBLit (BArr a) = putTag BArrT *> putByteArray a
putBLit (Pos n) = putTag PosT *> putPositive n
putBLit (Neg n) = putTag NegT *> putPositive n
putBLit (Char c) = putTag CharT *> putChar c
putBLit (Float d) = putTag FloatT *> putFloat d
putBLit (Arr a) = putTag ArrT *> putFoldable putValue a
{-# SPECIALIZE putBLit :: BLit -> BPut.Put #-}
{-# SPECIALIZE putBLit :: BLit -> SPut.Put #-}

getBLit :: (MonadGet m) => m BLit
getBLit =
  getTag >>= \case
    TextT -> Text . Util.Text.fromText <$> getText
    ListT -> List <$> getSeq getValue
    TmLinkT -> TmLink <$> getReferent
    TyLinkT -> TyLink <$> getReference
    BytesT -> Bytes <$> getBytes
    QuoteT -> Quote <$> getValue
    CodeT ->
      Code . flip CodeRep Uncacheable <$> getGroup
    BArrT -> BArr <$> getByteArray
    PosT -> Pos <$> getPositive
    NegT -> Neg <$> getPositive
    CharT -> Char <$> getChar
    FloatT -> Float <$> getFloat
    ArrT -> Arr . GHC.IsList.fromList <$> getList getValue
    CachedCodeT -> Code . flip CodeRep Cacheable <$> getGroup
{-# SPECIALIZE getBLit :: BGet.Get BLit #-}
{-# SPECIALIZE getBLit :: SGet.Get BLit #-}

{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Unison.Codebase.Serialization.V0 where

import Data.List (elemIndex)
import Data.Bits (Bits)
import Data.Bytes.Get as R
import Data.Bytes.Put as W
import Data.Bytes.Serial (serialize, deserialize)
import Data.Bytes.Signed (Unsigned)
import Data.Bytes.VarInt (VarInt(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Unison.Reference (Reference)
import qualified Data.ByteString as B
import qualified Unison.Hash as Hash
import Unison.Hash (Hash)
import qualified Unison.ABT as ABT
-- import qualified Data.Text as Text
import qualified Unison.Reference as Reference

-- About this format:
--
-- Finalized: No
--
-- If Finalized: Yes, don't modify this file in a way that affects serialized form.
-- Instead, create a new file, V(n + 1).
-- This ensures that we have a well-defined serialized form and can read
-- and write old versions.
--

unknownTag :: (MonadGet m, Show a) => String -> a -> m x
unknownTag msg tag =
  fail $ "unknown tag " ++ show tag ++
         " while deserializing: " ++ msg

putLength ::
  (MonadPut m, Integral n, Integral (Unsigned n),
   Bits n, Bits (Unsigned n))
  => n -> m ()
putLength = serialize . VarInt

getLength ::
  (MonadGet m, Integral n, Integral (Unsigned n),
   Bits n, Bits (Unsigned n))
  => m n
getLength = unVarInt <$> deserialize

putText :: MonadPut m => Text -> m ()
putText text = do
  let bs = encodeUtf8 text
  putLength $ B.length bs
  putByteString bs

getText :: MonadGet m => m Text
getText = do
  len <- getLength
  bs <- getBytes len
  pure $ decodeUtf8 bs

putHash :: MonadPut m => Hash -> m ()
putHash h = do
  let bs = Hash.toBytes h
  putLength (B.length bs)
  putByteString bs

getHash :: MonadGet m => m Hash
getHash = do
  len <- getLength
  bs <- getBytes len
  pure $ Hash.fromBytes bs

putReference :: MonadPut m => Reference -> m ()
putReference r = case r of
  Reference.Builtin name -> do
    putWord8 0
    putText name
  Reference.Derived hash -> do
    putWord8 1
    putHash hash

getReference :: MonadGet m => m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Reference.Builtin <$> getText
    1 -> Reference.Derived <$> getHash
    _ -> unknownTag "Reference" tag

putABT
  :: (MonadPut m, Foldable f, Functor f, Ord v)
  => (v -> m ())
  -> (a -> m ())
  -> (forall x . (x -> m ()) -> f x -> m ())
  -> ABT.Term f v a
  -> m ()
putABT putVar putA putF abt = go (ABT.annotateBound'' abt) where
  go (ABT.Term _ (a, env) abt) = putA a *> case abt of
    ABT.Var v      -> putWord8 0 *> putVarRef env v
    ABT.Tm f       -> putWord8 1 *> putF go f
    ABT.Abs v body -> putWord8 2 *> putVar v *> go body
    ABT.Cycle body -> putWord8 3 *> go body

  putVarRef env v = case v `elemIndex` env of
    Nothing -> putWord8 0 *> putVar v
    Just i  -> putWord8 1 *> putLength i


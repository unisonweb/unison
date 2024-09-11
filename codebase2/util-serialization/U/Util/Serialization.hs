{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Util.Serialization where

import Control.Applicative (liftA3)
import Control.Monad (foldM, replicateM, replicateM_, when)
import Data.Bits (Bits, clearBit, setBit, shiftL, shiftR, testBit, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bytes.Get (MonadGet, getByteString, getBytes, getWord8, remaining, runGetS, skip)
import Data.Bytes.Put (MonadPut, putByteString, putWord8, runPutS)
import Data.Bytes.VarInt (VarInt (VarInt))
import Data.Foldable (Foldable (toList), traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Debug.Trace (traceM)
import GHC.Word (Word64)
import Prelude hiding (readFile, writeFile)

type Get a = forall m. (MonadGet m) => m a

type Put a = forall m. (MonadPut m) => a -> m ()

debug :: Bool
debug = False

putBytes :: Put a -> a -> ByteString
putBytes put a = runPutS (put a)

putVarInt :: (MonadPut m, Integral a, Bits a) => a -> m ()
putVarInt n
  | n < 0x80 = putWord8 $ fromIntegral n
  | otherwise = do
      putWord8 $ setBit (fromIntegral n) 7
      putVarInt $ shiftR n 7
{-# INLINE putVarInt #-}

getVarInt :: (MonadGet m, Num b, Bits b) => m b
getVarInt = getWord8 >>= getVarInt
  where
    getVarInt :: (MonadGet m, Num b, Bits b) => Word8 -> m b
    getVarInt n
      | testBit n 7 = do
          VarInt m <- getWord8 >>= getVarInt
          return $ shiftL m 7 .|. clearBit (fromIntegral n) 7
      | otherwise = return $ fromIntegral n
    {-# INLINE getVarInt #-}
{-# INLINE getVarInt #-}

putText :: (MonadPut m) => Text -> m ()
putText text = do
  let bs = encodeUtf8 text
  putVarInt $ BS.length bs
  putByteString bs

getText :: (MonadGet m) => m Text
getText = do
  len <- getVarInt
  bs <- BS.copy <$> getBytes len
  pure $ decodeUtf8 bs

skipText :: (MonadGet m) => m ()
skipText = skip =<< getVarInt

putFoldable ::
  (Foldable f, MonadPut m) => (a -> m ()) -> f a -> m ()
putFoldable putA as = do
  putVarInt (length as)
  traverse_ putA as

getList :: (MonadGet m) => m a -> m [a]
getList getA = do
  length <- getListLength
  replicateM length getA

getListLength :: (MonadGet m) => m Int
getListLength =
  getVarInt

getVector :: (MonadGet m) => m a -> m (Vector a)
getVector getA = do
  length <- getVarInt
  Vector.replicateM length getA

skipVector :: (MonadGet m) => m a -> m ()
skipVector getA = do
  length <- getVarInt
  replicateM_ length getA

getSequence :: (MonadGet m) => m a -> m (Seq a)
getSequence getA = do
  length <- getVarInt
  Seq.replicateM length getA

getSet :: (MonadGet m, Ord a) => m a -> m (Set a)
getSet getA = do
  length <- getVarInt
  -- avoid materializing intermediate list
  foldM (\s ma -> Set.insert <$> ma <*> pure s) mempty (replicate length getA)

putMap :: (MonadPut m) => (a -> m ()) -> (b -> m ()) -> Map a b -> m ()
putMap putA putB m = putFoldable (putPair putA putB) (Map.toList m)

addToExistingMap :: (MonadGet m, Ord a) => m a -> m b -> Map a b -> m (Map a b)
addToExistingMap getA getB map = do
  length <- getVarInt
  -- avoid materializing intermediate list
  foldM
    (\s (ma, mb) -> Map.insert <$> ma <*> mb <*> pure s)
    map
    (replicate length (getA, getB))

getMap :: (MonadGet m, Ord a) => m a -> m b -> m (Map a b)
getMap getA getB = addToExistingMap getA getB mempty

getFramedByteString :: (MonadGet m) => m ByteString
getFramedByteString = getVarInt >>= getByteString

getRemainingByteString :: (MonadGet m) => m ByteString
getRemainingByteString = fromIntegral <$> remaining >>= getByteString

getFramed :: (MonadGet m) => Get a -> m a
getFramed get =
  getFramedByteString >>= either fail pure . runGetS get

putFramedByteString :: (MonadPut m) => ByteString -> m ()
putFramedByteString bs = do
  putVarInt (BS.length bs)
  putByteString bs

putFramed :: (MonadPut m) => Put a -> a -> m ()
putFramed put a = do
  -- 1. figure out the length `len` of serialized `a`
  -- 2. Put the length `len`
  -- 3. Put `a`
  let bs = putBytes put a
  when debug $ traceM $ "putFramed " ++ (show $ BS.length bs) ++ " bytes: " ++ show bs
  putVarInt (BS.length bs)
  putByteString bs

skipFramed :: (MonadGet m) => m ()
skipFramed = do
  len <- getVarInt
  skip len

putFramedArray :: (MonadPut m, Foldable f) => Put a -> f a -> m ()
putFramedArray put (toList -> as) = do
  let bss = fmap (putBytes put) as
  let lengths = fmap BS.length bss
  let offsets = scanl (+) 0 lengths
  putFoldable putVarInt offsets
  traverse_ putByteString bss

getFramedArray :: (MonadGet m) => m a -> m (Vector a)
getFramedArray getA = do
  offsets :: [Int] <- getList getVarInt
  let count = length offsets - 1
  Vector.replicateM count getA

lengthFramedArray :: (MonadGet m) => m Word64
lengthFramedArray = (\offsetsLen -> offsetsLen - 1) <$> getVarInt

unsafeFramedArrayLookup :: (MonadGet m) => m a -> Int -> m a
unsafeFramedArrayLookup getA index = do
  offsets <- getVector getVarInt
  skip (Vector.unsafeIndex offsets index)
  getA

putPair :: (MonadPut m) => (a -> m ()) -> (b -> m ()) -> (a, b) -> m ()
putPair putA putB (a, b) = putA a *> putB b

getPair :: (MonadGet m) => m a -> m b -> m (a, b)
getPair = liftA2 (,)

getTuple3 :: (MonadGet m) => m a -> m b -> m c -> m (a, b, c)
getTuple3 = liftA3 (,,)

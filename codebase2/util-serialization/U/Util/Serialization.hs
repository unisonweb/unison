{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module U.Util.Serialization where

import Control.Monad (replicateM)
import Data.Bits ((.|.), Bits, clearBit, setBit, shiftL, shiftR, testBit)
import Data.ByteString (ByteString, readFile, writeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short (ShortByteString)
import Data.Bytes.Get (MonadGet, getByteString, getBytes, getWord8, runGetS, skip)
import Data.Bytes.Put (MonadPut, putByteString, putWord8, runPutS)
import Data.Bytes.VarInt (VarInt (VarInt))
import Data.Foldable (Foldable (toList), traverse_)
import Data.List.Extra (dropEnd)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import System.FilePath (takeDirectory)
import UnliftIO (MonadIO, liftIO)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import Prelude hiding (readFile, writeFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative (Applicative(liftA2))

type Get a = forall m. MonadGet m => m a

type Put a = forall m. MonadPut m => a -> m ()

-- todo: do we use this?
data Format a = Format
  { get :: Get a,
    put :: Put a
  }

getFromBytes :: Get a -> ByteString -> Maybe a
getFromBytes getA bytes =
  case runGetS getA bytes of Left _ -> Nothing; Right a -> Just a

getFromFile :: MonadIO m => Get a -> FilePath -> m (Maybe a)
getFromFile getA file = do
  b <- doesFileExist file
  if b then getFromBytes getA <$> liftIO (readFile file) else pure Nothing

getFromFile' :: MonadIO m => Get a -> FilePath -> m (Either String a)
getFromFile' getA file = do
  b <- doesFileExist file
  if b
    then runGetS getA <$> liftIO (readFile file)
    else pure . Left $ "No such file: " ++ file

putBytes :: Put a -> a -> ByteString
putBytes put a = runPutS (put a)

putWithParentDirs :: MonadIO m => Put a -> FilePath -> a -> m ()
putWithParentDirs putA file a = do
  createDirectoryIfMissing True (takeDirectory file)
  liftIO . writeFile file $ putBytes putA a

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

putText :: MonadPut m => Text -> m ()
putText text = do
  let bs = encodeUtf8 text
  putVarInt $ BS.length bs
  putByteString bs

getText :: MonadGet m => m Text
getText = do
  len <- getVarInt
  bs <- BS.copy <$> getBytes len
  pure $ decodeUtf8 bs

skipText :: MonadGet m => m ()
skipText = skip =<< getVarInt

putShortText :: MonadPut m => ShortText -> m ()
putShortText text = do
  let sbs = TS.toShortByteString text
  putVarInt $ BSS.length sbs
  putShortByteString sbs

getShortText :: MonadGet m => m ShortText
getShortText = do
  len <- getVarInt
  sbs <- getShortByteString len
  pure $ TSU.fromShortByteStringUnsafe sbs

-- | the `binary` package has a native version of this,
--  which may be more efficient by a constant factor
putShortByteString :: MonadPut m => ShortByteString -> m ()
putShortByteString = putByteString . BSS.fromShort

-- | the `binary` package has a native version of this,
--  which may be more efficient by a constant factor
getShortByteString :: MonadGet m => Int -> m ShortByteString
getShortByteString len = BSS.toShort <$> getByteString len

putFoldable ::
  (Foldable f, MonadPut m) => (a -> m ()) -> f a -> m ()
putFoldable putA as = do
  putVarInt (length as)
  traverse_ putA as

getList :: MonadGet m => m a -> m [a]
getList getA = do
  length <- getVarInt
  replicateM length getA

getVector :: MonadGet m => m a -> m (Vector a)
getVector getA = do
  length <- getVarInt
  Vector.replicateM length getA

getSequence :: MonadGet m => m a -> m (Seq a)
getSequence getA = do
  length <- getVarInt
  Seq.replicateM length getA

getFramed :: MonadGet m => Get a -> m (Maybe a)
getFramed get = do
  size <- getVarInt
  bytes <- getByteString size
  pure $ getFromBytes get bytes

putFramed :: MonadPut m => Put a -> a -> m ()
putFramed put a = do
  -- 1. figure out the length `len` of serialized `a`
  -- 2. Put the length `len`
  -- 3. Put `a`
  let bs = putBytes put a
  putVarInt (BS.length bs)
  putByteString bs

skipFramed :: MonadGet m => m ()
skipFramed = do
  len <- getVarInt
  skip len

putFramedArray :: (MonadPut m, Foldable f) => Put a -> f a -> m ()
putFramedArray put (toList -> as) = do
  let bss = fmap (putBytes put) as
  let lengths = fmap BS.length bss
  let offsets = scanl (+) 0 (dropEnd 1 lengths)
  putFoldable putVarInt offsets
  traverse_ putByteString bss

getFramedArray :: MonadGet m => m a -> m (Vector a)
getFramedArray getA = do
  offsets :: [Int] <- getList getVarInt
  let count = length offsets - 1
  Vector.replicateM count getA

-- | Look up a 0-based index in a framed array, O(num array elements),
--  because it reads the start indices for all elements first.
--  This could be skipped if the indices had a fixed size instead of varint
framedArrayLookup :: MonadGet m => Get a -> Int -> m (Maybe a)
framedArrayLookup getA index = do
  offsets <- getVector getVarInt
  if index > Vector.length offsets
    then pure Nothing
    else do
      skip (Vector.unsafeIndex offsets index)
      Just <$> getA

unsafeFramedArrayLookup :: MonadGet m => Get a -> Int -> m a
unsafeFramedArrayLookup getA index = do
  offsets <- getVector getVarInt
  skip (Vector.unsafeIndex offsets index)
  getA

putMap :: MonadPut m => (a -> m ()) -> (b -> m ()) -> Map a b -> m ()
putMap putA putB m = putFoldable (putPair putA putB) (Map.toList m)

getMap :: (MonadGet m, Ord a) => m a -> m b -> m (Map a b)
getMap getA getB = Map.fromList <$> getList (getPair getA getB)

putPair :: MonadPut m => (a -> m ()) -> (b -> m ()) -> (a,b) -> m ()
putPair putA putB (a,b) = putA a *> putB b

getPair :: MonadGet m => m a -> m b -> m (a,b)
getPair = liftA2 (,)

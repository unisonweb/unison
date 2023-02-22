{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Util.Serialization where

import Control.Applicative (Applicative (liftA2), liftA3)
import Control.Monad (foldM, replicateM, when)
import Data.Bits (Bits, clearBit, setBit, shiftL, shiftR, testBit, (.|.))
import Data.ByteString (ByteString, readFile, writeFile)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
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
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Debug.Trace (traceM)
import GHC.Word (Word64)
import System.FilePath (takeDirectory)
import UnliftIO (MonadIO, liftIO)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import Prelude hiding (readFile, writeFile)

type Get a = forall m. (MonadGet m) => m a

type Put a = forall m. (MonadPut m) => a -> m ()

-- todo: do we use this?
data Format a = Format
  { get :: Get a,
    put :: Put a
  }

debug :: Bool
debug = False

getFromBytes :: Get a -> ByteString -> Maybe a
getFromBytes getA bytes =
  case runGetS getA bytes of Left _ -> Nothing; Right a -> Just a

getFromFile :: (MonadIO m) => Get a -> FilePath -> m (Maybe a)
getFromFile getA file = do
  b <- doesFileExist file
  if b then getFromBytes getA <$> liftIO (readFile file) else pure Nothing

getFromFile' :: (MonadIO m) => Get a -> FilePath -> m (Either String a)
getFromFile' getA file = do
  b <- doesFileExist file
  if b
    then runGetS getA <$> liftIO (readFile file)
    else pure . Left $ "No such file: " ++ file

putBytes :: Put a -> a -> ByteString
putBytes put a = runPutS (put a)

putWithParentDirs :: (MonadIO m) => Put a -> FilePath -> a -> m ()
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

putShortText :: (MonadPut m) => ShortText -> m ()
putShortText text = do
  let sbs = TS.toShortByteString text
  putVarInt $ BSS.length sbs
  putShortByteString sbs

getShortText :: (MonadGet m) => m ShortText
getShortText = do
  len <- getVarInt
  sbs <- getShortByteString len
  pure $ TSU.fromShortByteStringUnsafe sbs

-- | the `binary` package has a native version of this,
--  which may be more efficient by a constant factor
putShortByteString :: (MonadPut m) => ShortByteString -> m ()
putShortByteString = putByteString . BSS.fromShort

-- | the `binary` package has a native version of this,
--  which may be more efficient by a constant factor
getShortByteString :: (MonadGet m) => Int -> m ShortByteString
getShortByteString len = BSS.toShort <$> getByteString len

putFoldable ::
  (Foldable f, MonadPut m) => (a -> m ()) -> f a -> m ()
putFoldable putA as = do
  putVarInt (length as)
  traverse_ putA as

getList :: (MonadGet m) => m a -> m [a]
getList getA = do
  length <- getVarInt
  replicateM length getA

getVector :: (MonadGet m) => m a -> m (Vector a)
getVector getA = do
  length <- getVarInt
  Vector.replicateM length getA

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

-- | Look up a 0-based index in a framed array, O(num array elements),
--  because it reads the start indices for all elements first.
--  This could be skipped if the indices had a fixed size instead of varint
lookupFramedArray :: (MonadGet m) => m a -> Int -> m (Maybe a)
lookupFramedArray getA index = do
  offsets <- getVector getVarInt
  if index > Vector.length offsets - 1
    then pure Nothing
    else do
      skip (Vector.unsafeIndex offsets index)
      Just <$> getA

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

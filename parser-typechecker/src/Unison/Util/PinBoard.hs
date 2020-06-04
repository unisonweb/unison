{-# LANGUAGE OverloadedStrings #-}

module Unison.Util.PinBoard
  ( PinBoard,
    new,
    pin,

    -- * For debugging
    debugDump,
  )
where

import Control.Concurrent.MVar
import Data.Foldable (find)
import Data.Functor.Compose
import Data.Hashable (Hashable, hash)
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Tuple (swap)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Unison.Prelude hiding (empty)

-- | A "pin board" is a place to pin values, with the following properties:
--
--   * Pinned values aren't kept alive by the pin board, they might be garbage collected at any time.
--   * If you try to pin a value that's already pinned (per its Eq instance), the pinned one will be returned instead.
newtype PinBoard a
  = PinBoard (MVar (IntMap (Bucket a)))

new :: IO (PinBoard a)
new =
  PinBoard <$> newMVar IntMap.empty

pin :: (Eq a, Hashable a) => PinBoard a -> a -> IO a
pin (PinBoard boardVar) x =
  modifyMVar boardVar \board -> pin_ board x

pin_ :: forall a. (Eq a, Hashable a) => IntMap (Bucket a) -> a -> IO (IntMap (Bucket a), a)
pin_ board x =
  swap <$> getCompose (IntMap.alterF (fmap Just . Compose . maybe miss hit) (hash x) board)
  where
    miss :: IO (a, Bucket a)
    miss = do
      bucket <- bucketAdd emptyBucket x
      pure (x, bucket)
    hit :: Bucket a -> IO (a, Bucket a)
    hit bucket =
      bucketFind bucket x >>= \case
        (Nothing, bucket') -> do
          bucket'' <- bucketAdd bucket' x
          pure (x, bucket'')
        (Just y, bucket') -> pure (y, bucket')

debugDump :: (a -> Text) -> PinBoard a -> IO ()
debugDump f (PinBoard boardVar) = do
  board <- readMVar boardVar
  contents <- traverse bucketToList (toList board)
  Text.putStrLn (Text.unlines ("PinBoard" : map (("  " <>) . f) (concat contents)))

-- | A bucket of values. Semantically, it's a set, but differs in a few ways:
--
--   * It has a very limited API.
--   * It doesn't keep the values contained within alive; they might be garbage collected at any time.
--   * Looking up a value mutates the bucket in IO; specifically, it drops all values that have been garbage collected.
newtype Bucket a
  = Bucket [Weak a] -- Invariant: values are non-empty lists

-- | An empty bucket.
emptyBucket :: Bucket a
emptyBucket =
  Bucket []

-- | Add a value to a bucket.
bucketAdd :: Bucket a -> a -> IO (Bucket a)
bucketAdd (Bucket weaks) x = do
  weak <- mkWeakPtr x Nothing
  pure (Bucket (weak : weaks))

-- | Look up a value in a bucket, per its Eq instance.
bucketFind :: Eq a => Bucket a -> a -> IO (Maybe a, Bucket a)
bucketFind (Bucket weaks) x = do
  (ys, weaks') <- unzip <$> dereferenceWeaks weaks
  pure (find (== x) ys, Bucket weaks')
  where

bucketToList :: Bucket a -> IO [a]
bucketToList (Bucket weaks) =
  map fst <$> dereferenceWeaks weaks

-- Dereference a list of weak pointers, returning the alive ones along with their values.
dereferenceWeaks :: [Weak a] -> IO [(a, Weak a)]
dereferenceWeaks =
  mapMaybeM \w -> fmap (,w) <$> deRefWeak w

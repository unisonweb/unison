{-# LANGUAGE OverloadedStrings #-}

-- | A utility type for saving memory in the presence of many duplicate ByteStrings, etc. If you have data that may be
-- a redundant duplicate, try pinning it to a pin board, and use the result of that operation instead.
--
--   Without a pin board:
--
--     x ───── "38dce848c8c829c62"
--     y ───── "38dce848c8c829c62"
--     z ───── "d2518f260535b927b"
--
--   With a pin board:
--
--     x ───── "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
--   ... and after x is garbage collected:
--
--             "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
--   ... and after y is garbage collected:
--
--                                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
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

-- | A "pin board" is a place to pin values; semantically, it's a set, but differs in a few ways:
--
--   * Pinned values aren't kept alive by the pin board, they might be garbage collected at any time.
--   * If you try to pin a value that's already pinned (per its Eq instance), the pinned one will be returned
--     instead.
--   * It has a small API: just 'new' and 'pin'.
newtype PinBoard a
  = PinBoard (MVar (IntMap (Bucket a)))

new :: IO (PinBoard a)
new =
  PinBoard <$> newMVar IntMap.empty

pin :: forall a. (Eq a, Hashable a) => PinBoard a -> a -> IO a
pin (PinBoard boardVar) x =
  modifyMVar boardVar \board ->
    swap <$> getCompose (IntMap.alterF alter n board)
  where
    -- Pin to pin board at a hash key: either there's nothing there (ifMiss), or there's a nonempty bucket (ifHit).
    alter :: Maybe (Bucket a) -> Compose IO ((,) a) (Maybe (Bucket a))
    alter =
      Compose . maybe ifMiss ifHit
    -- Pin a new value: create a new singleton bucket.
    ifMiss :: IO (a, Maybe (Bucket a))
    ifMiss =
      (x,) . Just <$> newBucket x finalizer
    -- Possibly pin a new value: if it already exists in the bucket, return that one instead. Otherwise, insert it.
    ifHit :: Bucket a -> IO (a, Maybe (Bucket a))
    ifHit bucket =
      bucketFind bucket x >>= \case
        -- The bucket fully compacted down to nothingness.
        Nothing -> ifMiss
        -- Hash collision: the bucket has things in it, but none are the given value. Insert.
        Just (Nothing, bucket') -> (x,) . Just <$> bucketAdd bucket' x finalizer
        -- The thing being inserted already exists; return it.
        Just (Just y, bucket') -> pure (y, Just bucket')
    -- When each thing pinned here is garbage collected, compact its bucket.
    finalizer :: IO ()
    finalizer = do
      putStrLn ("Finalizing " ++ show n)
      modifyMVar_ boardVar (IntMap.alterF (maybe (pure Nothing) bucketCompact) n)
    n :: Int
    n =
      hash x

debugDump :: (a -> Text) -> PinBoard a -> IO ()
debugDump f (PinBoard boardVar) = do
  board <- readMVar boardVar
  contents <- traverse bucketToList (toList board)
  Text.putStrLn (Text.unlines ("PinBoard" : map (("  " <>) . f) (concat contents)))

-- | A bucket of weak pointers to different values that all share a hash.
newtype Bucket a
  = Bucket [Weak a] -- Invariant: non-empty list

-- | A singleton bucket.
newBucket :: a -> IO () -> IO (Bucket a)
newBucket =
  bucketAdd (Bucket [])

-- | Add a value to a bucket.
bucketAdd :: Bucket a -> a -> IO () -> IO (Bucket a)
bucketAdd (Bucket weaks) x finalizer = do
  weak <- mkWeakPtr x (Just finalizer)
  pure (Bucket (weak : weaks))

-- | Drop all garbage-collected values from a bucket. If none remain, returns Nothing.
bucketCompact :: Bucket a -> IO (Maybe (Bucket a))
bucketCompact (Bucket weaks) = do
  bucketFromList . map snd <$> dereferenceWeaks weaks

-- | Look up a value in a bucket per its Eq instance, and compact the bucket.
bucketFind :: Eq a => Bucket a -> a -> IO (Maybe (Maybe a, Bucket a))
bucketFind (Bucket weaks) x = do
  (ys, weaks') <- unzip <$> dereferenceWeaks weaks
  pure ((find (== x) ys,) <$> bucketFromList weaks')

bucketFromList :: [Weak a] -> Maybe (Bucket a)
bucketFromList = \case
  [] -> Nothing
  weaks -> Just (Bucket weaks)

bucketToList :: Bucket a -> IO [a]
bucketToList (Bucket weaks) =
  map fst <$> dereferenceWeaks weaks

-- Dereference a list of weak pointers, returning the alive ones along with their values.
dereferenceWeaks :: [Weak a] -> IO [(a, Weak a)]
dereferenceWeaks =
  mapMaybeM \w -> fmap (,w) <$> deRefWeak w

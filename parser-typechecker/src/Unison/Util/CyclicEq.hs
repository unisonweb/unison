{-# Language BangPatterns #-}
{-# Language Strict #-}
{-# Language StrictData #-}
{-# Language DoAndIfThenElse #-}

module Unison.Util.CyclicEq where

import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import Data.Vector (Vector)
import qualified Data.HashTable.IO as HT
import qualified Data.Mutable as M
import qualified Data.Vector as V

data HashTable k v =
  HashTable {
    table :: BasicHashTable k v,
    sizeRef  :: M.IOPRef Int
  }

class CyclicEq a where
  -- Map from `Ref` ID to position in the stream
  -- If a ref is encountered again, we use its mapped ID
  cyclicEq :: HashTable Int Int -> HashTable Int Int -> a -> a -> IO Bool

new :: Int -> IO (HashTable k v)
new size = do
  t <- HT.newSized size
  r <- M.newRef 0
  pure (HashTable t r)

lookup :: (Hashable k, Eq k) => k -> HashTable k v -> IO (Maybe v)
lookup k t = HT.lookup (table t) k

insert :: (Hashable k, Eq k) => k -> v -> HashTable k v -> IO ()
insert k v t = do
  HT.insert (table t) k v
  M.modifyRef (sizeRef t) (1 +)

size :: HashTable k v -> IO Int
size h = M.readRef (sizeRef h)

insertEnd :: (Hashable k, Eq k) => k -> HashTable k Int -> IO ()
insertEnd k t = do
  n <- size t
  insert k n t

instance CyclicEq a => CyclicEq [a] where
  cyclicEq h1 h2 (x:xs) (y:ys) = cyclicEq h1 h2 x y >>= \b ->
    if b then cyclicEq h1 h2 xs ys
    else pure False
  cyclicEq _ _ [] [] = pure True
  cyclicEq _ _ _ _   = pure False

instance CyclicEq a => CyclicEq (Vector a) where
  cyclicEq h1 h2 xs ys =
    if V.length xs /= V.length ys then pure False
    else go 0 h1 h2 xs ys
    where
    go !i !h1 !h2 !xs !ys =
      if i >= V.length xs then pure True
      else do
        b <- cyclicEq h1 h2 (xs V.! i) (ys V.! i)
        if b then go (i + 1) h1 h2 xs ys
        else pure False

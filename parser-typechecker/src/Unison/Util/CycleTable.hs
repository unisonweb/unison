module Unison.Util.CycleTable where

import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HT
import qualified Data.Mutable as M

-- A hash table along with a unique number which gets incremented on
-- each insert. This is used as an implementation detail by `CyclicEq`,
-- `CyclicOrd`, etc to be able to compare, hash, or serialize cyclic structures.

data CycleTable k v =
  CycleTable {
    table :: BasicHashTable k v,
    sizeRef  :: M.IOPRef Int
  }

new :: Int -> IO (CycleTable k v)
new size = do
  t <- HT.newSized size
  r <- M.newRef 0
  pure (CycleTable t r)

lookup :: (Hashable k, Eq k) => k -> CycleTable k v -> IO (Maybe v)
lookup k t = HT.lookup (table t) k

insert :: (Hashable k, Eq k) => k -> v -> CycleTable k v -> IO ()
insert k v t = do
  HT.insert (table t) k v
  M.modifyRef (sizeRef t) (1 +)

size :: CycleTable k v -> IO Int
size h = M.readRef (sizeRef h)

insertEnd :: (Hashable k, Eq k) => k -> CycleTable k Int -> IO ()
insertEnd k t = do
  n <- size t
  insert k n t


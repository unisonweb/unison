module Unison.Util.Hashtable where

import Data.HashTable.IO (BasicHashTable)
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HT
import qualified Data.Mutable as M

data Hashtable k v =
  Hashtable {
    table :: BasicHashTable k v,
    sizeRef  :: M.IOPRef Int
  }

new :: Int -> IO (Hashtable k v)
new size = do
  t <- HT.newSized size
  r <- M.newRef 0
  pure (Hashtable t r)

lookup :: (Hashable k, Eq k) => k -> Hashtable k v -> IO (Maybe v)
lookup k t = HT.lookup (table t) k

insert :: (Hashable k, Eq k) => k -> v -> Hashtable k v -> IO ()
insert k v t = do
  HT.insert (table t) k v
  M.modifyRef (sizeRef t) (1 +)

size :: Hashtable k v -> IO Int
size h = M.readRef (sizeRef h)

insertEnd :: (Hashable k, Eq k) => k -> Hashtable k Int -> IO ()
insertEnd k t = do
  n <- size t
  insert k n t


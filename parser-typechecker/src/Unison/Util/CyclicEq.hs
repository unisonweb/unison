module Unison.Util.CyclicEq where

import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT

type HashTable k v = BasicHashTable k v

class CyclicEq a where
  -- Map from `Ref` ID to position in the stream
  -- If a ref is encountered again, we use its mapped ID
  cyclicEq :: HashTable Int Int -> IORef Int -> a -> a -> IO Bool


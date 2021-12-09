module Unison.Hashing.V2.Hashable
  ( Hashable,
    hash,
  )
where

import Data.Int (Int64)
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.BuildHashable as BuildHashable
import Data.Set (Set)

class Hashable t where
  hash :: t -> Hash

instance BuildHashable.Hashable a => Hashable [a] where
  hash = BuildHashable.hash

instance BuildHashable.Hashable a => Hashable (Set a) where
  hash = BuildHashable.hash

instance Hashable Int64 where
  hash = BuildHashable.hash

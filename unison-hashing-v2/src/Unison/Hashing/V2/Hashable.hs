module Unison.Hashing.V2.Hashable
  ( Hashable (..),
    hashFor,
    HashFor (..),
  )
where

import Data.Int (Int64)
import Data.Set (Set)
import Unison.Hash (Hash (..), HashFor (..))
import qualified Unison.Hashing.V2.Tokenizable as Tokenizable

-- | This typeclass provides a mechanism for obtaining a content-based hash for Unison types &
-- terms.
-- Be wary that Unison requires that these hashes be deterministic, any change to a Hashable
-- instance requires a full codebase migration and should not be taken lightly.
class Hashable t where
  hash :: t -> Hash

instance Tokenizable.Tokenizable a => Hashable [a] where
  hash = Tokenizable.hashTokenizable

instance Tokenizable.Tokenizable a => Hashable (Set a) where
  hash = Tokenizable.hashTokenizable

instance Hashable Int64 where
  hash = Tokenizable.hashTokenizable

hashFor :: Hashable t => t -> HashFor t
hashFor = HashFor . hash

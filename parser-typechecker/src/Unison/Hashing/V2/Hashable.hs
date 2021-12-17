module Unison.Hashing.V2.Hashable
  ( Hashable (..),
  )
where

import Data.Int (Int64)
import Data.Set (Set)
import Unison.Hash (Hash (..))
import qualified Unison.Hashing.V2.BuildHashable as BuildHashable

-- | This typeclass provides a mechanism for obtaining a content-based hash for Unison types &
-- terms.
-- Be wary that Unison requires that these hashes be deterministic, any change to a Hashable
-- instance requires a full codebase migration and should not be taken lightly.
class Hashable t where
  hash :: t -> Hash

instance BuildHashable.Tokenizable a => Hashable [a] where
  hash = BuildHashable.hashTokenizable

instance BuildHashable.Tokenizable a => Hashable (Set a) where
  hash = BuildHashable.hashTokenizable

instance Hashable Int64 where
  hash = BuildHashable.hashTokenizable

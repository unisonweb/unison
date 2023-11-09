-- | @NESet@ utilities.
module Unison.Util.NESet
  ( unionSet,
  )
where

import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet

unionSet :: Ord a => Set a -> NESet a -> NESet a
unionSet s n = NESet.withNonEmpty n (<> n) s

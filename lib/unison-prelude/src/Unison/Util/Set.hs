module Unison.Util.Set
  ( mapMaybe,
    symmetricDifference,
    Unison.Util.Set.traverse,
  )
where

import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference a b = (a `Set.difference` b) `Set.union` (b `Set.difference` a)

mapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap Set.fromList . Prelude.traverse f . Set.toList

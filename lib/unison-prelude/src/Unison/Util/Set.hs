module Unison.Util.Set
  ( difference1,
    mapMaybe,
    symmetricDifference,
    Unison.Util.Set.traverse,
  )
where

import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- | Set difference, but return @Nothing@ if the difference is empty.
difference1 :: Ord a => Set a -> Set a -> Maybe (Set a)
difference1 xs ys =
  if null zs then Nothing else Just zs
  where
    zs = Set.difference xs ys

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference a b = (a `Set.difference` b) `Set.union` (b `Set.difference` a)

mapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap Set.fromList . Prelude.traverse f . Set.toList

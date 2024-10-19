module Unison.Util.Set
  ( asSingleton,
    difference1,
    mapMaybe,
    symmetricDifference,
    Unison.Util.Set.traverse,
    Unison.Util.Set.for,
    flatMap,
    filterM,
    forMaybe,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Unison.Util.Monoid (foldMapM)

-- | Get the only member of a set, iff it's a singleton.
asSingleton :: Set a -> Maybe a
asSingleton xs =
  if Set.size xs == 1 then Just (Set.findMin xs) else Nothing

-- | Set difference, but return @Nothing@ if the difference is empty.
difference1 :: (Ord a) => Set a -> Set a -> Maybe (Set a)
difference1 xs ys =
  if null zs then Nothing else Just zs
  where
    zs = Set.difference xs ys

symmetricDifference :: (Ord a) => Set a -> Set a -> Set a
symmetricDifference a b = (a `Set.difference` b) `Set.union` (b `Set.difference` a)

mapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList

forMaybe :: (Ord b, Applicative f) => Set a -> (a -> f (Maybe b)) -> f (Set b)
forMaybe xs f =
  Prelude.traverse f (Set.toList xs) <&> \ys ->
    ys
      & Maybe.catMaybes
      & Set.fromList

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap Set.fromList . Prelude.traverse f . Set.toList

for :: (Ord b, Applicative f) => Set a -> (a -> f b) -> f (Set b)
for = flip Unison.Util.Set.traverse

flatMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
flatMap f = Set.unions . fmap f . Set.toList

filterM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterM p =
  foldMapM \x ->
    p x <&> \case
      False -> Set.empty
      True -> Set.singleton x

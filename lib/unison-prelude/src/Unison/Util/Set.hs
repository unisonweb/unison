module Unison.Util.Set
  ( asSingleton,
    difference1,
    differenceMap,
    foldCommutativeM,
    insertMaybe,
    intersects,
    mapMaybe,
    symmetricDifference,
    Unison.Util.Set.traverse,
    Unison.Util.Set.for,
    flatMap,
    filterM,
    forMaybe,
    thenInsert,
    thenInsertMaybe,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Internal qualified as Map.Internal (Map (..))
import Data.Map.Strict (Map)
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Internal qualified as Set.Internal (Set (..), merge)
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

-- | Like 'Set.difference', but the second argument is a map.
differenceMap :: (Ord k) => Set k -> Map k a -> Set k
differenceMap Set.Internal.Tip _ = Set.Internal.Tip
differenceMap x Map.Internal.Tip = x
differenceMap x (Map.Internal.Bin _ k _ yl yr)
  | Set.size zl + Set.size zr == Set.size x = x
  | otherwise = Set.Internal.merge zl zr
  where
    (xl, xr) = Set.split k x
    !zl = differenceMap xl yl
    !zr = differenceMap xr yr

-- | Fold a set strictly with a monadic "commutative" combining function that doesn't receive the elements in any
-- particular order.
foldCommutativeM :: (Monad m) => (a -> b -> m b) -> b -> Set a -> m b
foldCommutativeM f =
  let go !acc = \case
        Set.Internal.Bin _ x l r : xs -> do
          !acc1 <- f x acc
          go acc1 (l : r : xs)
        Set.Internal.Tip : xs -> go acc xs
        [] -> pure acc
   in \z xs -> go z [xs]

insertMaybe :: (Ord a) => Maybe a -> Set a -> Set a
insertMaybe mx xs =
  case mx of
    Just x -> Set.insert x xs
    Nothing -> xs

-- | Get whether two sets intersect.
intersects :: (Ord a) => Set a -> Set a -> Bool
intersects xs ys =
  not (Set.disjoint xs ys)

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

thenInsert :: (Ord a) => Set a -> a -> Set a
thenInsert xs x =
  Set.insert x xs

thenInsertMaybe :: (Ord a) => Set a -> Maybe a -> Set a
thenInsertMaybe xs = \case
  Just x -> Set.insert x xs
  Nothing -> xs

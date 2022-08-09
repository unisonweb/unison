{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Relation3 where

import Data.Function (on)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Semigroup (Sum (Sum, getSum))
import Data.Tuple.Extra (uncurry3)
import Unison.Prelude hiding (empty, toList)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R

data Relation3 a b c = Relation3
  { d1 :: Map a (Relation b c),
    d2 :: Map b (Relation a c),
    d3 :: Map c (Relation a b)
  }

instance (Eq a, Eq b, Eq c) => Eq (Relation3 a b c) where
  (==) = (==) `on` d1

instance (Ord a, Ord b, Ord c) => Ord (Relation3 a b c) where
  compare = comparing d1

instance (Show a, Show b, Show c) => Show (Relation3 a b c) where
  show = show . toList

d1s :: Relation3 a b c -> Set a
d1s = Map.keysSet . d1

d2s :: Relation3 a b c -> Set b
d2s = Map.keysSet . d2

d3s :: Relation3 a b c -> Set c
d3s = Map.keysSet . d3

-- | Project out a relation that only includes the 1st and 2nd dimensions.
d12 :: Relation3 a b c -> Relation a b
d12 Relation3 {d1, d2} =
  R.unsafeFromMultimaps (Map.map R.dom d1) (Map.map R.dom d2)

-- | Project out a relation that only includes the 1st and 3rd dimensions.
d13 :: Relation3 a b c -> Relation a c
d13 Relation3 {d1, d3} =
  R.unsafeFromMultimaps (Map.map R.ran d1) (Map.map R.dom d3)

-- | Project out a relation that only includes the 2nd and 3rd dimensions.
d23 :: Relation3 a b c -> Relation b c
d23 Relation3 {d2, d3} =
  R.unsafeFromMultimaps (Map.map R.ran d2) (Map.map R.ran d3)

filter ::
  (Ord a, Ord b, Ord c) =>
  ((a, b, c) -> Bool) ->
  Relation3 a b c ->
  Relation3 a b c
filter f = fromList . Prelude.filter f . toList

mapD1 :: (Ord a, Ord a', Ord b, Ord c) => (a -> a') -> Relation3 a b c -> Relation3 a' b c
mapD1 f Relation3 {d1, d2, d3} =
  Relation3
    { d1 = Map.mapKeysWith R.union f d1,
      d2 = Map.map (R.mapDom f) d2,
      d3 = Map.map (R.mapDom f) d3
    }

-- | Like 'mapD1', but takes a function that must be monotonic; i.e. @compare x y == compare (f x) (f y)@.
mapD1Monotonic :: (Ord a, Ord a', Ord b, Ord c) => (a -> a') -> Relation3 a b c -> Relation3 a' b c
mapD1Monotonic f Relation3 {d1, d2, d3} =
  Relation3
    { d1 = Map.mapKeysMonotonic f d1,
      d2 = Map.map (R.mapDomMonotonic f) d2,
      d3 = Map.map (R.mapDomMonotonic f) d3
    }

mapD2 :: (Ord a, Ord b, Ord b', Ord c) => (b -> b') -> Relation3 a b c -> Relation3 a b' c
mapD2 f Relation3 {d1, d2, d3} =
  Relation3
    { d1 = Map.map (R.mapDom f) d1,
      d2 = Map.mapKeysWith R.union f d2,
      d3 = Map.map (R.mapRan f) d3
    }

-- | Like 'mapD2', but takes a function that must be monotonic; i.e. @compare x y == compare (f x) (f y)@.
mapD2Monotonic :: (Ord a, Ord b, Ord b', Ord c) => (b -> b') -> Relation3 a b c -> Relation3 a b' c
mapD2Monotonic f Relation3 {d1, d2, d3} =
  Relation3
    { d1 = Map.map (R.mapDomMonotonic f) d1,
      d2 = Map.mapKeysMonotonic f d2,
      d3 = Map.map (R.mapRanMonotonic f) d3
    }

member :: (Ord a, Ord b, Ord c) => a -> b -> c -> Relation3 a b c -> Bool
member a b c = R.member b c . lookupD1 a

lookupD1 :: (Ord a, Ord b, Ord c) => a -> Relation3 a b c -> Relation b c
lookupD1 a = fromMaybe mempty . Map.lookup a . d1

lookupD2 :: (Ord a, Ord b, Ord c) => b -> Relation3 a b c -> Relation a c
lookupD2 b = fromMaybe mempty . Map.lookup b . d2

lookupD3 :: (Ord a, Ord b, Ord c) => c -> Relation3 a b c -> Relation a b
lookupD3 c = fromMaybe mempty . Map.lookup c . d3

size :: (Ord a, Ord b, Ord c) => Relation3 a b c -> Int
size = getSum . foldMap (Sum . R.size) . d1

toList :: Relation3 a b c -> [(a, b, c)]
toList = fmap (\(a, (b, c)) -> (a, b, c)) . toNestedList

toNestedList :: Relation3 a b c -> [(a, (b, c))]
toNestedList r3 =
  [ (a, bc) | (a, r2) <- Map.toList $ d1 r3, bc <- R.toList r2
  ]

nestD12 :: (Ord a, Ord b, Ord c) => Relation3 a b c -> Relation (a, b) c
nestD12 r = R.fromList [((a, b), c) | (a, b, c) <- toList r]

fromNestedDom :: (Ord a, Ord b, Ord c) => Relation (a, b) c -> Relation3 a b c
fromNestedDom = fromList . fmap (\((a, b), c) -> (a, b, c)) . R.toList

fromNestedRan :: (Ord a, Ord b, Ord c) => Relation a (b, c) -> Relation3 a b c
fromNestedRan = fromList . fmap (\(a, (b, c)) -> (a, b, c)) . R.toList

fromList :: (Ord a, Ord b, Ord c) => [(a, b, c)] -> Relation3 a b c
fromList xs = insertAll xs empty

empty :: (Ord a, Ord b, Ord c) => Relation3 a b c
empty = mempty

null :: Relation3 a b c -> Bool
null r = Map.null $ d1 r

insert,
  delete ::
    (Ord a, Ord b, Ord c) =>
    a ->
    b ->
    c ->
    Relation3 a b c ->
    Relation3 a b c
insert a b c Relation3 {..} =
  Relation3
    (Map.alter (ins b c) a d1)
    (Map.alter (ins a c) b d2)
    (Map.alter (ins a b) c d3)
  where
    ins x y = Just . R.insert x y . fromMaybe mempty

insertAll,
  deleteAll ::
    Foldable f =>
    Ord a =>
    Ord b =>
    Ord c =>
    f (a, b, c) ->
    Relation3 a b c ->
    Relation3 a b c
insertAll f r = foldl' (\r x -> uncurry3 insert x r) r f
deleteAll f r = foldl' (\r x -> uncurry3 delete x r) r f

-- | Compute the difference of two relations.
difference :: (Ord a, Ord b, Ord c) => Relation3 a b c -> Relation3 a b c -> Relation3 a b c
difference (Relation3 a1 b1 c1) (Relation3 a2 b2 c2) =
  Relation3
    (Map.differenceWith R.difference1 a1 a2)
    (Map.differenceWith R.difference1 b1 b2)
    (Map.differenceWith R.difference1 c1 c2)

-- | @union x y@ computes the union of @x@ and @y@.
union :: (Ord a, Ord b, Ord c) => Relation3 a b c -> Relation3 a b c -> Relation3 a b c
union (Relation3 a1 b1 c1) (Relation3 a2 b2 c2) =
  Relation3
    (Map.unionWith R.union a1 a2)
    (Map.unionWith R.union b1 b2)
    (Map.unionWith R.union c1 c2)

delete a b c Relation3 {..} =
  Relation3
    (Map.alter (del b c) a d1)
    (Map.alter (del a c) b d2)
    (Map.alter (del a b) c d3)
  where
    del _ _ Nothing = Nothing
    del x y (Just r) =
      let r' = R.delete x y r
       in if r' == mempty then Nothing else Just r'

instance (Ord a, Ord b, Ord c) => Semigroup (Relation3 a b c) where
  s1 <> s2 = Relation3 d1' d2' d3'
    where
      d1' = Map.unionWith (<>) (d1 s1) (d1 s2)
      d2' = Map.unionWith (<>) (d2 s1) (d2 s2)
      d3' = Map.unionWith (<>) (d3 s1) (d3 s2)

instance (Ord a, Ord b, Ord c) => Monoid (Relation3 a b c) where
  mempty = Relation3 mempty mempty mempty

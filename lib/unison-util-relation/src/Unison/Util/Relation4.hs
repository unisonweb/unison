{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Relation4 where

import Data.Function (on)
import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Semigroup (Sum (Sum, getSum))
import Unison.Prelude hiding (empty, toList)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R
import Unison.Util.Relation3 (Relation3 (Relation3))
import qualified Unison.Util.Relation3 as R3
import Prelude

data Relation4 a b c d = Relation4
  { d1 :: Map a (Relation3 b c d),
    d2 :: Map b (Relation3 a c d),
    d3 :: Map c (Relation3 a b d),
    d4 :: Map d (Relation3 a b c)
  }

instance (Eq a, Eq b, Eq c, Eq d) => Eq (Relation4 a b c d) where
  (==) = (==) `on` d1

instance (Ord a, Ord b, Ord c, Ord d) => Ord (Relation4 a b c d) where
  compare = comparing d1

instance (Show a, Show b, Show c, Show d) => Show (Relation4 a b c d) where
  show = show . toList

size :: (Ord a, Ord b, Ord c, Ord d) => Relation4 a b c d -> Int
size = getSum . foldMap (Sum . R3.size) . d1

toNestedList :: Relation4 a b c d -> [(a, (b, (c, d)))]
toNestedList r4 =
  [ (a, bcd)
    | (a, r3) <- Map.toList $ d1 r4,
      bcd <- R3.toNestedList r3
  ]

toList :: Relation4 a b c d -> [(a, b, c, d)]
toList = fmap (\(a, (b, (c, d))) -> (a, b, c, d)) . toNestedList

empty :: (Ord a, Ord b, Ord c, Ord d) => Relation4 a b c d
empty = mempty

null :: Relation4 a b c d -> Bool
null r = Map.null $ d1 r

fromList :: (Ord a, Ord b, Ord c, Ord d) => [(a, b, c, d)] -> Relation4 a b c d
fromList xs = insertAll xs empty

filter :: (Ord a, Ord b, Ord c, Ord d) => ((a, b, c, d) -> Bool) -> Relation4 a b c d -> Relation4 a b c d
filter f = fromList . Prelude.filter f . toList

memberD13 :: (Ord a, Ord c) => a -> c -> Relation4 a b c d -> Bool
memberD13 a c r4 =
  case Map.lookup a (d1 r4) of
    Nothing -> False
    Just r3 -> R3.memberD2 c r3

selectD3 ::
  (Ord a, Ord b, Ord c, Ord d) =>
  c ->
  Relation4 a b c d ->
  Relation4 a b c d
selectD3 c r =
  fromList [(a, b, c, d) | (a, b, d) <- maybe [] R3.toList $ Map.lookup c (d3 r)]

selectD34 ::
  (Ord a, Ord b, Ord c, Ord d) =>
  c ->
  d ->
  Relation4 a b c d ->
  Relation4 a b c d
selectD34 c d r =
  fromList
    [ (a, b, c, d)
      | (a, b) <-
          maybe
            []
            (maybe [] R.toList . Map.lookup d . R3.d3)
            (Map.lookup c (d3 r))
    ]

restrict34d12 ::
  (Ord a, Ord b, Ord c, Ord d) =>
  (c, d) ->
  Relation4 a b c d ->
  Relation a b
restrict34d12 (c, d) Relation4 {d3} =
  fromMaybe R.empty do
    abd <- Map.lookup c d3
    Map.lookup d (R3.d3 abd)

keys :: Relation4 a b c d -> (Set a, Set b, Set c, Set d)
keys Relation4 {d1, d2, d3, d4} =
  (Map.keysSet d1, Map.keysSet d2, Map.keysSet d3, Map.keysSet d4)

lookupD1 :: (Ord a, Ord b, Ord c, Ord d) => a -> Relation4 a b c d -> Relation3 b c d
lookupD1 a = fromMaybe mempty . Map.lookup a . d1

lookupD2 :: (Ord a, Ord b, Ord c, Ord d) => b -> Relation4 a b c d -> Relation3 a c d
lookupD2 b = fromMaybe mempty . Map.lookup b . d2

d1set :: Ord a => Relation4 a b c d -> Set a
d1set = Map.keysSet . d1

d12 :: (Ord a, Ord b) => Relation4 a b c d -> Relation a b
d12 = R.fromMultimap . fmap (Map.keysSet . R3.d1) . d1

d13 :: (Ord a, Ord c) => Relation4 a b c d -> Relation a c
d13 = R.fromMultimap . fmap (Map.keysSet . R3.d2) . d1

d34 :: (Ord c, Ord d) => Relation4 a b c d -> Relation c d
d34 = R.fromMultimap . fmap (Map.keysSet . R3.d3) . d3

-- | Project out a relation that only includes the 1st, 2nd, and 4th dimensions.
d124 :: (Ord a, Ord b, Ord c, Ord d) => Relation4 a b c d -> Relation3 a b d
d124 Relation4 {d1, d2, d4} =
  Relation3
    { d1 = Map.map R3.d13 d1,
      d2 = Map.map R3.d13 d2,
      d3 = Map.map R3.d12 d4
    }

-- | Project out a relation that only includes the 2nd, 3rd, and 4th dimensions.
d234 :: (Ord a, Ord b, Ord c, Ord d) => Relation4 a b c d -> Relation3 b c d
d234 Relation4 {d2, d3, d4} =
  Relation3
    { d1 = Map.map R3.d23 d2,
      d2 = Map.map R3.d23 d3,
      d3 = Map.map R3.d23 d4
    }

-- todo: make me faster
d12s :: (Ord a, Ord b) => Relation4 a b c d -> [(a, b)]
d12s = nubOrd . fmap (\(a, (b, _)) -> (a, b)) . toNestedList

d3s :: Relation4 a b c d -> Set c
d3s = Map.keysSet . d3

d4s :: Relation4 a b c d -> Set d
d4s = Map.keysSet . d4

-- e.g. Map.toList (d1 r) >>= \(a, r3) -> (a,) <$> Map.keys (R3.d1 r3)

insert,
  delete ::
    (Ord a, Ord b, Ord c, Ord d) =>
    a ->
    b ->
    c ->
    d ->
    Relation4 a b c d ->
    Relation4 a b c d
insert a b c d Relation4 {..} =
  Relation4
    (Map.alter (ins b c d) a d1)
    (Map.alter (ins a c d) b d2)
    (Map.alter (ins a b d) c d3)
    (Map.alter (ins a b c) d d4)
  where
    ins x y z = Just . R3.insert x y z . fromMaybe mempty
delete a b c d Relation4 {..} =
  Relation4
    (Map.alter (del b c d) a d1)
    (Map.alter (del a c d) b d2)
    (Map.alter (del a b d) c d3)
    (Map.alter (del a b c) d d4)
  where
    del _ _ _ Nothing = Nothing
    del x y z (Just r) =
      let r' = R3.delete x y z r
       in if r' == mempty then Nothing else Just r'

mapD2 :: (Ord a, Ord b, Ord b', Ord c, Ord d) => (b -> b') -> Relation4 a b c d -> Relation4 a b' c d
mapD2 f Relation4 {d1, d2, d3, d4} =
  Relation4
    { d1 = Map.map (R3.mapD1 f) d1,
      d2 = Map.mapKeysWith R3.union f d2,
      d3 = Map.map (R3.mapD2 f) d3,
      d4 = Map.map (R3.mapD2 f) d4
    }

-- | Like 'mapD2', but takes a function that must be monotonic; i.e. @compare x y == compare (f x) (f y)@.
mapD2Monotonic :: (Ord a, Ord b, Ord b', Ord c, Ord d) => (b -> b') -> Relation4 a b c d -> Relation4 a b' c d
mapD2Monotonic f Relation4 {d1, d2, d3, d4} =
  Relation4
    { d1 = Map.map (R3.mapD1Monotonic f) d1,
      d2 = Map.mapKeysMonotonic f d2,
      d3 = Map.map (R3.mapD2Monotonic f) d3,
      d4 = Map.map (R3.mapD2Monotonic f) d4
    }

insertAll ::
  Foldable f =>
  Ord a =>
  Ord b =>
  Ord c =>
  Ord d =>
  f (a, b, c, d) ->
  Relation4 a b c d ->
  Relation4 a b c d
insertAll f r = foldl' (\r x -> uncurry4 insert x r) r f

instance (Ord a, Ord b, Ord c, Ord d) => Semigroup (Relation4 a b c d) where
  s1 <> s2 = Relation4 d1' d2' d3' d4'
    where
      d1' = Map.unionWith (<>) (d1 s1) (d1 s2)
      d2' = Map.unionWith (<>) (d2 s1) (d2 s2)
      d3' = Map.unionWith (<>) (d3 s1) (d3 s2)
      d4' = Map.unionWith (<>) (d4 s1) (d4 s2)

instance (Ord a, Ord b, Ord c, Ord d) => Monoid (Relation4 a b c d) where
  mempty = Relation4 mempty mempty mempty mempty

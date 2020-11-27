{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Relation4 where

--import qualified Data.Set as Set

import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import Data.Semigroup (Sum (Sum, getSum))
import qualified Unison.Hashable as H
import Unison.Prelude hiding (empty, toList)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R
import Unison.Util.Relation3 (Relation3)
import qualified Unison.Util.Relation3 as R3
import Util (uncurry4)
import Prelude

data Relation4 a b c d = Relation4
  { d1 :: Map a (Relation3 b c d),
    d2 :: Map b (Relation3 a c d),
    d3 :: Map c (Relation3 a b d),
    d4 :: Map d (Relation3 a b c)
  }
  deriving (Eq, Ord)

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

fromList :: (Ord a, Ord b, Ord c, Ord d) => [(a, b, c, d)] -> Relation4 a b c d
fromList xs = insertAll xs empty

filter :: (Ord a, Ord b, Ord c, Ord d) => ((a, b, c, d) -> Bool) -> Relation4 a b c d -> Relation4 a b c d
filter f = fromList . Prelude.filter f . toList

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

d1set :: Ord a => Relation4 a b c d -> Set a
d1set = Map.keysSet . d1

d12 :: (Ord a, Ord b) => Relation4 a b c d -> Relation a b
d12 = R.fromMultimap . fmap (Map.keysSet . R3.d1) . d1

d34 :: (Ord c, Ord d) => Relation4 a b c d -> Relation c d
d34 = R.fromMultimap . fmap (Map.keysSet . R3.d3) . d3

-- todo: make me faster
d12s :: (Ord a, Ord b) => Relation4 a b c d -> [(a, b)]
d12s = nubOrd . fmap (\(a, (b, _)) -> (a, b)) . toNestedList

--e.g. Map.toList (d1 r) >>= \(a, r3) -> (a,) <$> Map.keys (R3.d1 r3)

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

mapD2 ::
  (Ord a, Ord b, Ord b', Ord c, Ord d) =>
  (b -> b') ->
  Relation4 a b c d ->
  Relation4 a b' c d
mapD2 f = fromList . fmap (\(a, b, c, d) -> (a, f b, c, d)) . toList

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
  (<>) = mappend

instance (Ord a, Ord b, Ord c, Ord d) => Monoid (Relation4 a b c d) where
  mempty = Relation4 mempty mempty mempty mempty
  s1 `mappend` s2 = Relation4 d1' d2' d3' d4'
    where
      d1' = Map.unionWith (<>) (d1 s1) (d1 s2)
      d2' = Map.unionWith (<>) (d2 s1) (d2 s2)
      d3' = Map.unionWith (<>) (d3 s1) (d3 s2)
      d4' = Map.unionWith (<>) (d4 s1) (d4 s2)

instance
  (H.Hashable d1, H.Hashable d2, H.Hashable d3, H.Hashable d4) =>
  H.Hashable (Relation4 d1 d2 d3 d4)
  where
  tokens s = [H.accumulateToken $ toNestedList s]

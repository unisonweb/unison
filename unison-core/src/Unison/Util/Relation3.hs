{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Relation3 where

import qualified Data.Map as Map
import Data.Semigroup (Sum (Sum, getSum))
import Data.Tuple.Extra (uncurry3)
import qualified Unison.Hashable as H
import Unison.Prelude hiding (empty, toList)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R

data Relation3 a b c = Relation3
  { d1 :: Map a (Relation b c),
    d2 :: Map b (Relation a c),
    d3 :: Map c (Relation a b)
  }
  deriving (Eq, Ord)

instance (Show a, Show b, Show c) => Show (Relation3 a b c) where
  show = show . toList

d1s :: Relation3 a b c -> Set a
d1s = Map.keysSet . d1

d2s :: Relation3 a b c -> Set b
d2s = Map.keysSet . d2

d3s :: Relation3 a b c -> Set c
d3s = Map.keysSet . d3

filter ::
  (Ord a, Ord b, Ord c) =>
  ((a, b, c) -> Bool) ->
  Relation3 a b c ->
  Relation3 a b c
filter f = fromList . Prelude.filter f . toList

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

difference ::
  (Ord a, Ord b, Ord c) =>
  Relation3 a b c ->
  Relation3 a b c ->
  Relation3 a b c
difference a b = deleteAll (Unison.Util.Relation3.toList b) a

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
  (<>) = mappend

instance (Ord a, Ord b, Ord c) => Monoid (Relation3 a b c) where
  mempty = Relation3 mempty mempty mempty
  s1 `mappend` s2 = Relation3 d1' d2' d3'
    where
      d1' = Map.unionWith (<>) (d1 s1) (d1 s2)
      d2' = Map.unionWith (<>) (d2 s1) (d2 s2)
      d3' = Map.unionWith (<>) (d3 s1) (d3 s2)

instance
  (H.Hashable d1, H.Hashable d2, H.Hashable d3) =>
  H.Hashable (Relation3 d1 d2 d3)
  where
  tokens s = [H.accumulateToken $ toNestedList s]

{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Star3 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Prelude
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R

-- Represents a set of (fact, d1, d2, d3), but indexed using a star schema so
-- it can be efficiently queried from any of the dimensions.
data Star3 fact d1 d2 d3 = Star3
  { fact :: Set fact,
    d1 :: Relation fact d1,
    d2 :: Relation fact d2,
    d3 :: Relation fact d3
  }
  deriving (Eq, Ord, Show)

-- Produce the cross-product across all the dimensions
toList ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  Star3 fact d1 d2 d3 ->
  [(fact, d1, d2, d3)]
toList s =
  [ (f, x, y, z) | f <- Set.toList (fact s), x <- Set.toList (R.lookupDom f (d1 s)), y <- Set.toList (R.lookupDom f (d2 s)), z <- Set.toList (R.lookupDom f (d3 s))
  ]

-- `difference a b` contains only the facts from `a` that are absent from `b`
-- or differ along any of the dimensions `d1..d3`.
difference ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
difference a b = Star3 facts d1s d2s d3s
  where
    d1s = R.difference (d1 a) (d1 b)
    d2s = R.difference (d2 a) (d2 b)
    d3s = R.difference (d3 a) (d3 b)
    facts = R.dom d1s <> R.dom d2s <> R.dom d3s

d23s ::
  (Ord fact, Ord d2, Ord d3) =>
  Star3 fact d1 d2 d3 ->
  [(fact, d2, d3)]
d23s s =
  [ (f, x, y) | f <- Set.toList (fact s), x <- Set.toList (R.lookupDom f (d2 s)), y <- Set.toList (R.lookupDom f (d3 s))
  ]

d23s' ::
  (Ord fact, Ord d2, Ord d3) =>
  Star3 fact d1 d2 d3 ->
  [(d2, d3)]
d23s' s =
  [ (x, y) | f <- Set.toList (fact s), x <- Set.toList (R.lookupDom f (d2 s)), y <- Set.toList (R.lookupDom f (d3 s))
  ]

d12s ::
  (Ord fact, Ord d1, Ord d2) =>
  Star3 fact d1 d2 d3 ->
  [(fact, d1, d2)]
d12s s =
  [ (f, x, y) | f <- Set.toList (fact s), x <- Set.toList (R.lookupDom f (d1 s)), y <- Set.toList (R.lookupDom f (d2 s))
  ]

d13s ::
  (Ord fact, Ord d1, Ord d3) =>
  Star3 fact d1 d2 d3 ->
  [(fact, d1, d3)]
d13s s =
  [ (f, x, y) | f <- Set.toList (fact s), x <- Set.toList (R.lookupDom f (d1 s)), y <- Set.toList (R.lookupDom f (d3 s))
  ]

mapD1 :: (Ord fact, Ord d1, Ord d1a) => (d1 -> d1a) -> Star3 fact d1 d2 d3 -> Star3 fact d1a d2 d3
mapD1 f s = s {d1 = R.mapRan f (d1 s)}

mapD2 :: (Ord fact, Ord d2, Ord d2a) => (d2 -> d2a) -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2a d3
mapD2 f s = s {d2 = R.mapRan f (d2 s)}

mapD3 :: (Ord fact, Ord d3, Ord d3a) => (d3 -> d3a) -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2 d3a
mapD3 f s = s {d3 = R.mapRan f (d3 s)}

fromList ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  [(fact, d1, d2, d3)] ->
  Star3 fact d1 d2 d3
fromList = foldl' (flip insert) mempty

selectFact ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  Set fact ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
selectFact fs s = Star3 fact' d1' d2' d3'
  where
    fact' = Set.intersection fs (fact s)
    d1' = fs R.<| d1 s
    d2' = fs R.<| d2 s
    d3' = fs R.<| d3 s

select1D3 ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  d3 ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
select1D3 = selectD3 . Set.singleton

selectD3 ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  Set d3 ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
selectD3 d3s s = Star3 fact' d1' d2' d3'
  where
    fact' = Set.intersection (R.dom d3') (fact s)
    d1' = R.dom d3' R.<| d1 s
    d2' = R.dom d3' R.<| d2 s
    d3' = d3 s R.|> d3s

-- Deletes tuples of the form (fact, d1, _, _).
-- If no other (fact, dk, _, _) tuples exist for any other dk, then
-- `fact` is removed from the `fact` set and from the other dimensions as well,
-- that is, (fact, d1) is treated as a primary key.
deletePrimaryD1 ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  (fact, d1) ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
deletePrimaryD1 (f, x) s =
  let d1' = R.delete f x (d1 s)
      otherX = R.lookupDom f d1'
   in if Set.null otherX
        then Star3 (Set.delete f (fact s)) d1' (R.deleteDom f (d2 s)) (R.deleteDom f (d3 s))
        else s {d1 = d1'}

lookupD1 :: (Ord fact, Ord d1) => d1 -> Star3 fact d1 d2 d3 -> Set fact
lookupD1 x s = R.lookupRan x (d1 s)

insertD1 ::
  (Ord fact, Ord d1) =>
  (fact, d1) ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
insertD1 (f, x) s =
  s
    { fact = Set.insert f (fact s),
      d1 = R.insert f x (d1 s)
    }

memberD1 :: (Ord fact, Ord d1) => (fact, d1) -> Star3 fact d1 d2 d3 -> Bool
memberD1 (f, x) s = R.member f x (d1 s)

memberD2 :: (Ord fact, Ord d2) => (fact, d2) -> Star3 fact d1 d2 d3 -> Bool
memberD2 (f, x) s = R.member f x (d2 s)

memberD3 :: (Ord fact, Ord d3) => (fact, d3) -> Star3 fact d1 d2 d3 -> Bool
memberD3 (f, x) s = R.member f x (d3 s)

insert ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  (fact, d1, d2, d3) ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
insert (f, d1i, d2i, d3i) s = Star3 fact' d1' d2' d3'
  where
    fact' = Set.insert f (fact s)
    d1' = R.insert f d1i (d1 s)
    d2' = R.insert f d2i (d2 s)
    d3' = R.insert f d3i (d3 s)

insertD23 ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  (fact, d2, d3) ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
insertD23 (f, x, y) s = Star3 fact' (d1 s) d2' d3'
  where
    fact' = Set.insert f (fact s)
    d2' = R.insert f x (d2 s)
    d3' = R.insert f y (d3 s)

deleteD3 ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  (fact, d3) ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
deleteD3 (f, x) s = garbageCollect f (Star3 (fact s) (d1 s) (d2 s) d3')
  where
    d3' = R.delete f x (d3 s)

deleteD2 ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  (fact, d2) ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
deleteD2 (f, x) s = garbageCollect f (Star3 (fact s) (d1 s) d2' (d3 s))
  where
    d2' = R.delete f x (d2 s)

-- | Given a possibly-invalid Star3, which may contain the given fact in its fact set that are not related to any d1,
-- d2, or d3, return a valid Star3, with this fact possibly removed.
garbageCollect :: (Ord fact) => fact -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2 d3
garbageCollect f star =
  star
    { fact =
        if R.memberDom f (d1 star) || R.memberDom f (d2 star) || R.memberDom f (d3 star)
          then fact star
          else Set.delete f (fact star)
    }

deleteFact ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  Set fact ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
deleteFact facts Star3 {..} =
  Star3
    (fact `Set.difference` facts)
    (facts R.<|| d1)
    (facts R.<|| d2)
    (facts R.<|| d3)

-- Efficiently replace facts with those in the provided `Map`.
-- The `apply` function can be used to add other dimensions
-- in the same traversal. It is given `apply old new s` where
-- s is the current `Star` being accumulated.
--
-- Currently used by update propagation but likely useful for
-- other bulk rewriting of namespaces.
replaceFacts ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  (fact -> fact -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2 d3) ->
  Map fact fact ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
replaceFacts apply m s =
  let -- the intersection of `fact` and the replacement keys is often small,
      -- so we compute that first (which can happen in O(size of intersection))
      -- rather than iterating over one or the other
      replaceable = Map.keysSet m `Set.intersection` fact s
      go s old = apply old new $ replaceFact old new s
        where
          new = Map.findWithDefault old old m
   in foldl' go s replaceable

replaceFact ::
  (Ord fact, Ord d1, Ord d2, Ord d3) =>
  fact ->
  fact ->
  Star3 fact d1 d2 d3 ->
  Star3 fact d1 d2 d3
replaceFact f f' s@Star3 {..} =
  if Set.notMember f fact
    then s
    else
      Star3
        (Set.insert f' . Set.delete f $ fact)
        (R.replaceDom f f' d1)
        (R.replaceDom f f' d2)
        (R.replaceDom f f' d3)

instance (Ord fact, Ord d1, Ord d2, Ord d3) => Semigroup (Star3 fact d1 d2 d3) where
  s1 <> s2 = Star3 fact' d1' d2' d3'
    where
      fact' = fact s1 <> fact s2
      d1' = d1 s1 <> d1 s2
      d2' = d2 s1 <> d2 s2
      d3' = d3 s1 <> d3 s2

instance (Ord fact, Ord d1, Ord d2, Ord d3) => Monoid (Star3 fact d1 d2 d3) where
  mempty = Star3 mempty mempty mempty mempty

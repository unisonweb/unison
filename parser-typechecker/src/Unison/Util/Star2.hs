{-# LANGUAGE RecordWildCards #-}

module Unison.Util.Star2
  ( Star2 (Star2),
    fact,
    insertD1,
    insertD2,
    deleteD1,
    deleteD2,
    deleteFact,
    deletePrimaryD1,
    d1,
    d2,
    difference,
    lookupD1,
    mapD2,
    memberD1,
    replaceFacts,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Prelude
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as R

-- Represents a set of (fact, d1, d2, d2), but indexed using a star schema so
-- it can be efficiently queried from any of the dimensions.
data Star2 fact d1 d2 = Star2
  { fact :: Set fact,
    d1 :: Relation fact d1,
    d2 :: Relation fact d2
  }
  deriving (Eq, Ord, Show)

-- Produce the cross-product across all the dimensions

-- `difference a b` contains only the facts from `a` that are absent from `b`
-- or differ along any of the dimensions `d1..d2`.
difference ::
  (Ord fact, Ord d1, Ord d2) =>
  Star2 fact d1 d2 ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
difference a b = Star2 facts d1s d2s
  where
    d1s = R.difference (d1 a) (d1 b)
    d2s = R.difference (d2 a) (d2 b)
    facts = R.dom d1s <> R.dom d2s

mapD2 :: (Ord fact, Ord d2, Ord d2a) => (d2 -> d2a) -> Star2 fact d1 d2 -> Star2 fact d1 d2a
mapD2 f s = s {d2 = R.mapRan f (d2 s)}

-- Deletes tuples of the form (fact, d1, _, _).
-- If no other (fact, dk, _, _) tuples exist for any other dk, then
-- `fact` is removed from the `fact` set and from the other dimensions as well,
-- that is, (fact, d1) is treated as a primary key.
deletePrimaryD1 ::
  (Ord fact, Ord d1, Ord d2) =>
  (fact, d1) ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
deletePrimaryD1 (f, x) s =
  let d1' = R.delete f x (d1 s)
      otherX = R.lookupDom f d1'
   in if Set.null otherX
        then Star2 (Set.delete f (fact s)) d1' (R.deleteDom f (d2 s))
        else s {d1 = d1'}

-- Deletes tuples of the form (_, d1, _, _).
deleteD1 ::
  (Ord fact, Ord d1, Ord d2) =>
  d1 ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
deleteD1 x s =
  let d1' = R.deleteRan x (d1 s)
      deadFacts = R.lookupRan x (d1 s)
      newFacts = Set.difference (fact s) deadFacts
      d2' = R.subtractDom deadFacts (d2 s)
   in Star2
        newFacts
        d1'
        d2'

lookupD1 :: (Ord fact, Ord d1) => d1 -> Star2 fact d1 d2 -> Set fact
lookupD1 x s = R.lookupRan x (d1 s)

insertD1 ::
  (Ord fact, Ord d1) =>
  (fact, d1) ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
insertD1 (f, x) s =
  s
    { fact = Set.insert f (fact s),
      d1 = R.insert f x (d1 s)
    }

insertD2 ::
  (Ord fact, Ord d2) =>
  (fact, d2) ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
insertD2 (f, x) s =
  s
    { fact = Set.insert f (fact s),
      d2 = R.insert f x (d2 s)
    }

memberD1 :: (Ord fact, Ord d1) => (fact, d1) -> Star2 fact d1 d2 -> Bool
memberD1 (f, x) s = R.member f x (d1 s)

deleteD2 ::
  (Ord fact, Ord d1, Ord d2) =>
  (fact, d2) ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
deleteD2 (f, x) s = garbageCollect f (Star2 (fact s) (d1 s) d2')
  where
    d2' = R.delete f x (d2 s)

-- | Given a possibly-invalid Star2, which may contain the given fact in its fact set that are not related to any d1,
-- d2, or d2, return a valid Star2, with this fact possibly removed.
garbageCollect :: (Ord fact) => fact -> Star2 fact d1 d2 -> Star2 fact d1 d2
garbageCollect f star =
  star
    { fact =
        if R.memberDom f (d1 star) || R.memberDom f (d2 star)
          then fact star
          else Set.delete f (fact star)
    }

deleteFact ::
  (Ord fact, Ord d1, Ord d2) =>
  Set fact ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
deleteFact facts Star2 {..} =
  Star2
    (fact `Set.difference` facts)
    (facts R.<|| d1)
    (facts R.<|| d2)

-- Efficiently replace facts with those in the provided `Map`.
-- The `apply` function can be used to add other dimensions
-- in the same traversal. It is given `apply old new s` where
-- s is the current `Star` being accumulated.
--
-- Currently used by update propagation but likely useful for
-- other bulk rewriting of namespaces.
replaceFacts ::
  (Ord fact, Ord d1, Ord d2) =>
  (fact -> fact -> Star2 fact d1 d2 -> Star2 fact d1 d2) ->
  Map fact fact ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
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
  (Ord fact, Ord d1, Ord d2) =>
  fact ->
  fact ->
  Star2 fact d1 d2 ->
  Star2 fact d1 d2
replaceFact f f' s@Star2 {..} =
  if Set.notMember f fact
    then s
    else
      Star2
        (Set.insert f' . Set.delete f $ fact)
        (R.replaceDom f f' d1)
        (R.replaceDom f f' d2)

instance (Ord fact, Ord d1, Ord d2) => Semigroup (Star2 fact d1 d2) where
  s1 <> s2 = Star2 fact' d1' d2'
    where
      fact' = fact s1 <> fact s2
      d1' = d1 s1 <> d1 s2
      d2' = d2 s1 <> d2 s2

instance (Ord fact, Ord d1, Ord d2) => Monoid (Star2 fact d1 d2) where
  mempty = Star2 mempty mempty mempty

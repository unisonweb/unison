module Unison.Util.Star3 where

import Data.List (foldl')
import Data.Set (Set)
import Unison.Util.Relation (Relation)
import qualified Data.Set as Set
import qualified Unison.Hashable as H
import qualified Unison.Util.Relation as R

-- Represents a set of (fact, d1, d2, d3), but indexed using a star schema so
-- it can be efficiently quried from any of the dimensions.
data Star3 fact d1 d2 d3
  = Star3 { fact :: Set fact
          , d1 :: Relation fact d1
          , d2 :: Relation fact d2
          , d3 :: Relation fact d3 } deriving (Eq,Ord,Show)

toList :: (Ord fact, Ord d1, Ord d2, Ord d3)
       => Star3 fact d1 d2 d3
       -> [(fact, d1, d2, d3)]
toList s = [ (f, x, y, z) | f <- Set.toList (fact s)
                          , x <- Set.toList (R.lookupDom f (d1 s))
                          , y <- Set.toList (R.lookupDom f (d2 s))
                          , z <- Set.toList (R.lookupDom f (d3 s)) ]

d23s :: (Ord fact, Ord d2, Ord d3)
     => Star3 fact d1 d2 d3
     -> [(fact, d2, d3)]
d23s s = [ (f, x, y) | f <- Set.toList (fact s)
                     , x <- Set.toList (R.lookupDom f (d2 s))
                     , y <- Set.toList (R.lookupDom f (d3 s)) ]

d23s' :: (Ord fact, Ord d2, Ord d3)
      => Star3 fact d1 d2 d3
      -> [(d2, d3)]
d23s' s = [ (x, y) | f <- Set.toList (fact s)
                   , x <- Set.toList (R.lookupDom f (d2 s))
                   , y <- Set.toList (R.lookupDom f (d3 s)) ]

d12s :: (Ord fact, Ord d1, Ord d2)
     => Star3 fact d1 d2 d3
     -> [(fact, d1, d2)]
d12s s = [ (f, x, y) | f <- Set.toList (fact s)
                     , x <- Set.toList (R.lookupDom f (d1 s))
                     , y <- Set.toList (R.lookupDom f (d2 s)) ]

d13s :: (Ord fact, Ord d1, Ord d3)
     => Star3 fact d1 d2 d3
     -> [(fact, d1, d3)]
d13s s = [ (f, x, y) | f <- Set.toList (fact s)
                     , x <- Set.toList (R.lookupDom f (d1 s))
                     , y <- Set.toList (R.lookupDom f (d3 s)) ]

mapD1 :: (Ord fact, Ord d1, Ord d1a) => (d1 -> d1a) -> Star3 fact d1 d2 d3 -> Star3 fact d1a d2 d3
mapD1 f s = s { d1 = R.mapRan f (d1 s) }

mapD2 :: (Ord fact, Ord d2, Ord d2a) => (d2 -> d2a) -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2a d3
mapD2 f s = s { d2 = R.mapRan f (d2 s) }

mapD3 :: (Ord fact, Ord d3, Ord d3a) => (d3 -> d3a) -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2 d3a
mapD3 f s = s { d3 = R.mapRan f (d3 s) }

fromList :: (Ord fact, Ord d1, Ord d2, Ord d3)
         => [(fact, d1, d2, d3)] -> Star3 fact d1 d2 d3
fromList = foldl' (flip insert) mempty

selectFact
  :: (Ord fact, Ord d1, Ord d2, Ord d3)
  => Set fact
  -> Star3 fact d1 d2 d3
  -> Star3 fact d1 d2 d3
selectFact fs s = Star3 fact' d1' d2' d3' where
  fact' = Set.intersection fs (fact s)
  d1'   = fs R.<| d1 s
  d2'   = fs R.<| d2 s
  d3'   = fs R.<| d3 s


-- Deletes tuples of the form (fact, d1, _, _).
-- If no other (fact, dk, _, _) tuples exist for any other dk, then
-- `fact` is removed from the `fact` set and from the other dimensions as well,
-- that is, (fact, d1) is treated as a primary key.
deletePrimaryD1 :: (Ord fact, Ord d1, Ord d2, Ord d3)
         => (fact, d1) -> Star3 fact d1 d2 d3 -> Star3 fact d1 d2 d3
deletePrimaryD1 (f, x) s = let
  d1' = R.delete f x (d1 s)
  otherX = R.lookupDom f d1'
  in if Set.null otherX then
       Star3 (Set.delete f (fact s)) d1' (R.deleteDom f (d2 s)) (R.deleteDom f (d3 s))
     else s { d1 = d1' }

lookupD1 :: (Ord fact, Ord d1) => d1 -> Star3 fact d1 d2 d3 -> Set fact
lookupD1 x s = R.lookupRan x (d1 s)

insertD1
  :: (Ord fact, Ord d1)
  => (fact, d1)
  -> Star3 fact d1 d2 d3
  -> Star3 fact d1 d2 d3
insertD1 (f,x) s = s { fact = Set.insert f (fact s)
                     , d1   = R.insert f x (d1 s) }

memberD1 :: (Ord fact, Ord d1) => (fact,d1) -> Star3 fact d1 d2 d3 -> Bool
memberD1 (f, x) s = R.member f x (d1 s)

memberD2 :: (Ord fact, Ord d2) => (fact,d2) -> Star3 fact d1 d2 d3 -> Bool
memberD2 (f, x) s = R.member f x (d2 s)

memberD3 :: (Ord fact, Ord d3) => (fact,d3) -> Star3 fact d1 d2 d3 -> Bool
memberD3 (f, x) s = R.member f x (d3 s)

insert :: (Ord fact, Ord d1, Ord d2, Ord d3)
       => (fact, d1, d2, d3)
       -> Star3 fact d1 d2 d3
       -> Star3 fact d1 d2 d3
insert (f, d1i, d2i, d3i) s = Star3 fact' d1' d2' d3' where
  fact' = Set.insert f (fact s)
  d1'   = R.insert f d1i (d1 s)
  d2'   = R.insert f d2i (d2 s)
  d3'   = R.insert f d3i (d3 s)

insertD23 :: (Ord fact, Ord d1, Ord d2, Ord d3)
          => (fact, d2, d3)
          -> Star3 fact d1 d2 d3
          -> Star3 fact d1 d2 d3
insertD23 (f, x, y) s = Star3 fact' (d1 s) d2' d3' where
  fact' = Set.insert f (fact s)
  d2'   = R.insert f x (d2 s)
  d3'   = R.insert f y (d3 s)

deleteD23 :: (Ord fact, Ord d1, Ord d2, Ord d3)
          => (fact, d2, d3)
          -> Star3 fact d1 d2 d3
          -> Star3 fact d1 d2 d3
deleteD23 (f, x, y) s = Star3 (fact s) (d1 s) d2' d3' where
  d2' = R.delete f x (d2 s)
  d3' = R.delete f y (d3 s)

instance (Ord fact, Ord d1, Ord d2, Ord d3) => Semigroup (Star3 fact d1 d2 d3) where
  (<>) = mappend

instance (Ord fact, Ord d1, Ord d2, Ord d3) => Monoid (Star3 fact d1 d2 d3) where
  mempty = Star3 mempty mempty mempty mempty
  s1 `mappend` s2 = Star3 fact' d1' d2' d3' where
    fact' = fact s1 <> fact s2
    d1'   = d1 s1 <> d1 s2
    d2'   = d2 s1 <> d2 s2
    d3'   = d3 s1 <> d3 s2

instance (H.Hashable fact, H.Hashable d1, H.Hashable d2, H.Hashable d3)
       => H.Hashable (Star3 fact d1 d2 d3) where
  tokens s =
    [ H.accumulateToken (fact s)
    , H.accumulateToken (d1 s)
    , H.accumulateToken (d2 s)
    , H.accumulateToken (d3 s) ]

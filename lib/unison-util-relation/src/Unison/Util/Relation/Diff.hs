module Unison.Util.Relation.Diff
  ( relationMapDiff
  , Diff(..)
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Given two diff-able values l and r; this structure contains each of
-- * (l - r)
-- * (r - l)
-- * (l `intersection` r)
-- * (l `union` r)
--
-- Fields are lazy to avoid paying for fields you don't use.
data Diff a = Diff
  { leftOnly     :: a
  , rightOnly    :: a
  , intersection :: a
  , union        :: a
  } deriving (Functor)

instance (Semigroup a) => Semigroup (Diff a) where
  Diff l r i u <> Diff l' r' i' u' = Diff (l <> l') (r <> r') (i <> i') (u <> u')

instance (Monoid a) => Monoid (Diff a) where
  mempty = Diff mempty mempty mempty mempty

-- | Compute a 'Diff' from two ascending lists.
-- Lists must be in ascending order, this isn't checked.
-- O(n + m)
ascListDifferences :: Ord a => [a] -> [a] -> Diff [a]
ascListDifferences a b =
  case (a, b) of
    ([], []) -> mempty
    (l, []) -> Diff l [] [] l
    ([], r) -> Diff [] r [] r
    (l:ls, r:rs) ->
      case compare l r of
        EQ -> Diff [] [] [l] [l] <> ascListDifferences ls rs
        LT -> Diff [l] [] [] [l] <> ascListDifferences ls (r:rs)
        GT -> Diff [] [r] [] [r] <> ascListDifferences (l:ls) rs

-- | Computes the total 'Diff' of two Sets
-- O(n + m)
setDifferences :: (Ord a) => Set a -> Set a -> Diff (Set a)
setDifferences l r = Set.fromDistinctAscList <$> ascListDifferences (Set.toAscList l) (Set.toAscList r)

-- | A helper for 'relationMapDiff'.
-- Computes the diff of two ordered key-value maps in ascending order.
-- The resulting lists are sorted in ascending order.
-- O(n + m)
mapDifferences' :: (Ord k, Ord a) => [(k, Set a)] -> [(k, Set a)] -> Diff [(k, Set a)]
mapDifferences' a b =
  case (a, b) of
    ([], []) -> mempty
    (l, []) -> Diff l [] [] l
    ([], r) -> Diff [] r [] r
    (l@(lk, las):ls, r@(rk, ras):rs) ->
      case compare lk rk of
        EQ -> let d = setDifferences las ras
               in Diff [(lk, leftOnly d)] [(rk, rightOnly d)] [(lk, intersection d)] [(lk, union d)] <> mapDifferences' ls rs
        LT -> Diff [l] [] [] [l] <> mapDifferences' ls (r:rs)
        GT -> Diff [] [r] [] [r] <> mapDifferences' (l:ls) rs

-- diffThings :: (Ord k, Ord a, Ord b) => (a -> b) -> [a] -> [a] -> Diff [a]
-- diffThings compareOn a b =
--   case (a, b) of
--     ([], []) -> mempty
--     (l, []) -> Diff l [] [] l
--     ([], r) -> Diff [] r [] r
--     (l@(lk, las):ls, r@(rk, ras):rs) ->
--       case compare lk rk of
--         EQ -> let d = setDifferences las ras
--                in Diff [(lk, leftOnly d)] [(rk, rightOnly d)] [(lk, intersection d)] [(lk, union d)] <> mapDifferences' ls rs
--         LT -> Diff [l] [] [] [l] <> mapDifferences' ls (r:rs)
--         GT -> Diff [] [r] [] [r] <> mapDifferences' (l:ls) rs

-- | Computes the total 'Diff' of two relation maps.
-- Time of (relationMapDiff x y) is:
-- O(n + m)
-- where n and m represent the total number of contained 'a's inside x and y respectively.
relationMapDiff :: (Ord k, Ord a) => Map k (Set a) -> Map k (Set a) -> Diff (Map k (Set a))
relationMapDiff a b = Map.fromDistinctAscList . filter (not . null . snd) <$> mapDifferences' (Map.toAscList a) (Map.toAscList b)

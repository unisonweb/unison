module Unison.Util.List where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Safe (headMay)

multimap :: Foldable f => Ord k => f (k, v) -> Map k [v]
multimap kvs =
  -- preserve the order of the values from the original list
  reverse <$> foldl' step Map.empty kvs
  where
  step m (k,v) = Map.insertWith (++) k [v] m

groupBy :: (Foldable f, Ord k) => (v -> k) -> f v -> Map k [v]
groupBy f vs = reverse <$> foldl' step Map.empty vs
  where step m v = Map.insertWith (++) (f v) [v] m

-- returns the subset of `f a` which maps to unique `b`s.
-- prefers earlier copies, if many `a` map to some `b`.
uniqueBy, nubOrdOn :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy f as = wrangle' (toList as) Set.empty where
  wrangle' [] _ = []
  wrangle' (a:as) seen =
    if Set.member b seen
    then wrangle' as seen
    else a : wrangle' as (Set.insert b seen)
    where b = f a
nubOrdOn = uniqueBy

-- prefers later copies
uniqueBy' :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy' f = reverse . uniqueBy f . reverse . toList

safeHead :: Foldable f => f a -> Maybe a
safeHead = headMay . toList

validate :: (Semigroup e, Foldable f) => (a -> Either e b) -> f a -> Either e [b]
validate f as = case partitionEithers (f <$> toList as) of
  ([], bs) -> Right bs
  (e:es, _) -> Left (foldl' (<>) e es)

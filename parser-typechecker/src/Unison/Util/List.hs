module Unison.Util.List where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

multimap :: Ord k => [(k,v)] -> Map k [v]
multimap kvs =
  -- preserve the order of the values from the original list
  reverse <$> foldl' step Map.empty kvs
  where
  step m (k,v) = Map.insertWith (++) k [v] m

-- prefers earlier copies
uniqueBy :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy f as = wrangle' (toList as) Set.empty where
  wrangle' [] _ = []
  wrangle' (a:as) seen =
    if Set.member b seen
    then wrangle' as seen
    else a : wrangle' as (Set.insert b seen)
    where b = f a

-- prefers later copies
uniqueBy' :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy' f = reverse . uniqueBy f . reverse . toList


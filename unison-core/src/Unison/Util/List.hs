module Unison.Util.List where

import Unison.Prelude

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.Extra as List

multimap :: Foldable f => Ord k => f (k, v) -> Map k [v]
multimap kvs =
  -- preserve the order of the values from the original list
  reverse <$> foldl' step Map.empty kvs
  where
  step m (k,v) = Map.insertWith (++) k [v] m

groupBy :: (Foldable f, Ord k) => (v -> k) -> f v -> Map k [v]
groupBy f vs = reverse <$> foldl' step Map.empty vs
  where step m v = Map.insertWith (++) (f v) [v] m

-- | group _consecutive_ elements by a key.
-- e.g.
-- >>> groupMap (\n -> (odd n, show n)) [1, 3, 4, 6, 7]
-- [(True,["1","3"]),(False,["4","6"]),(True,["7"])]
groupMap :: (Foldable f, Eq k) => (a -> (k, b)) -> f a -> [(k, [b])]
groupMap f xs =
  xs
  & toList
  & fmap f
  & List.groupOn fst
  -- head is okay since groupOn only returns populated lists.
  <&> \grp -> (fst . head $ grp, snd <$> grp)

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

-- Intercalate a list with separators determined by inspecting each
-- adjacent pair.
intercalateMapWith :: (a -> a -> b) -> (a -> b) -> [a] -> [b]
intercalateMapWith sep f xs  = result where
  xs'   = map f xs
  pairs = filter (\p -> length p == 2) $ map (take 2) $ List.tails xs
  seps  = flip map pairs $ \case
    x1 : x2 : _ -> sep x1 x2
    _           -> error "bad list length"
  paired = zipWith (\sep x -> [sep, x]) seps (drop 1 xs')
  result = take 1 xs' ++ mconcat paired

-- Take runs of consecutive occurrences of r within a list,
-- and in each run, overwrite all but the first occurrence of r with w.
quenchRuns :: Eq a => a -> a -> [a] -> [a]
quenchRuns r w = reverse . go False r w [] where
  go inRun r w acc = \case
    [] -> acc
    h : tl ->
      if h == r
      then go True r w ((if inRun then w else r) : acc) tl
      else go False r w (h : acc) tl

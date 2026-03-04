module Unison.Util.List where

import Control.Arrow ((&&&))
import Data.Either.Validation (eitherToValidation, validationToEither)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Prelude

multimap :: (Foldable f) => (Ord k) => f (k, v) -> Map k (NonEmpty v)
multimap = foldr step Map.empty
  where
    step (k, v) = Map.insertWith (<>) k (pure v)

groupBy :: (Foldable f, Ord k) => (v -> k) -> f v -> Map k (NonEmpty v)
groupBy f = foldr step Map.empty
  where
    step v = Map.insertWith (<>) (f v) (pure v)

groupOn :: (Foldable f, Eq k) => (a -> k) -> f a -> [(NonEmpty a)]
groupOn f = NE.groupBy ((==) `on2` f)
  where
    (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

-- | group _consecutive_ elements by a key.
-- e.g.
-- >>> groupMap (\n -> (odd n, show n)) [1, 3, 4, 6, 7]
-- [(True,["1","3"]),(False,["4","6"]),(True,["7"])]
groupMap :: (Foldable f, Functor f, Eq k) => (a -> (k, b)) -> f a -> [(k, NonEmpty b)]
groupMap f = fmap (fst . NE.head &&& fmap snd) . groupOn fst . fmap f

-- returns the subset of `f a` which maps to unique `b`s.
-- prefers earlier copies, if many `a` map to some `b`.
uniqueBy, nubOrdOn :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy f as = wrangle' (toList as) Set.empty
  where
    wrangle' [] _ = []
    wrangle' (a : as) seen =
      if Set.member b seen
        then wrangle' as seen
        else a : wrangle' as (Set.insert b seen)
      where
        b = f a
nubOrdOn = uniqueBy

-- prefers later copies
uniqueBy' :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy' f = reverse . uniqueBy f . reverse . toList

safeHead :: (Foldable f) => f a -> Maybe a
safeHead = headMay . toList

validate :: (Semigroup e, Traversable f) => (a -> Either e b) -> f a -> Either e (f b)
validate f = validationToEither . traverse (eitherToValidation . f)

-- Intercalate a list with separators determined by inspecting each
-- adjacent pair.
intercalateMapWith :: (a -> a -> b) -> (a -> b) -> [a] -> [b]
intercalateMapWith sep f xs = result
  where
    xs' = map f xs
    pairs = filter (\p -> length p == 2) $ map (take 2) $ List.tails xs
    seps = flip map pairs $ \case
      x1 : x2 : _ -> sep x1 x2
      _ -> error "bad list length"
    paired = zipWith (\sep x -> [sep, x]) seps (drop 1 xs')
    result = take 1 xs' ++ mconcat paired

-- Take runs of consecutive occurrences of r within a list,
-- and in each run, overwrite all but the first occurrence of r with w.
quenchRuns :: (Eq a) => a -> a -> [a] -> [a]
quenchRuns r w = reverse . go False r w []
  where
    go inRun r w acc = \case
      [] -> acc
      h : tl ->
        if h == r
          then go True r w ((if inRun then w else r) : acc) tl
          else go False r w (h : acc) tl

-- | Finds the longest shared path prefix of two paths.
-- Returns (shared prefix, path to first location from shared prefix, path to second location from shared prefix)
--
-- >>> splitOnLongestCommonPrefix ["a", "b", "x"] ["a", "b", "c"]
-- (["a","b"],["x"],["c"])
--
-- >>> splitOnLongestCommonPrefix [] ["a", "b", "c"]
-- ([],[],["a","b","c"])
splitOnLongestCommonPrefix :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
splitOnLongestCommonPrefix as bs =
  case (as, bs) of
    ([], _) -> ([], as, bs)
    (_, []) -> ([], as, bs)
    (x : xs, y : ys)
      | x == y ->
          let (prefix, ra, rb) = splitOnLongestCommonPrefix xs ys
           in (x : prefix, ra, rb)
      | otherwise -> ([], as, bs)

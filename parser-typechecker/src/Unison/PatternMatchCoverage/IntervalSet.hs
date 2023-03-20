module Unison.PatternMatchCoverage.IntervalSet
  ( IntervalSet,
    empty,
    singleton,
    fromList,
    insert,
    delete,
    difference,
    intersection,
    complement,
    null,
    member,
    extractSingleton,
    intersectIntervals,
    map,
    foldr,
    lookupMin,
    lookupMax,
  )
where

import Data.Coerce (coerce)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Prelude hiding (foldr, map, null)
import qualified Prelude

newtype IntervalSet = IntervalSet {unIntervalSet :: IntMap Int}
  deriving stock (Show, Eq, Ord)

empty :: IntervalSet
empty = IntervalSet IntMap.empty

singleton :: (Int, Int) -> IntervalSet
singleton x = insert x empty

lookupMin :: IntervalSet -> Maybe Int
lookupMin = fmap fst . IntMap.lookupMin . unIntervalSet

lookupMax :: IntervalSet -> Maybe Int
lookupMax = fmap snd . IntMap.lookupMax . unIntervalSet

member :: Int -> IntervalSet -> Bool
member i is =
  case splitLookupLE i is of
    (_, m, _) -> case m of
      Nothing -> False
      Just (_, ub) -> i <= ub

foldr :: (Int -> Int -> b -> b) -> b -> IntervalSet -> b
foldr f z = IntMap.foldrWithKey f z . unIntervalSet

map :: ((Int, Int) -> (Int, Int)) -> IntervalSet -> IntervalSet
map f = IntervalSet . foldr phi IntMap.empty
  where
    phi k v b = let (k', v') = f (k, v) in IntMap.insert k' v' b

-- | insert inclusive bounds interval into set
insert :: (Int, Int) -> IntervalSet -> IntervalSet
insert i@(lb, ub) is
  | nullInterval i = is
  | otherwise =
      case splitLookupLE lb is of
        (smaller, m1, xs) ->
          case splitLookupLE ub xs of
            (_, m2, larger) ->
              IntervalSet $
                IntMap.unions
                  [ unIntervalSet smaller,
                    unIntervalSet $ fromList (maybeToList m1 ++ [i] ++ maybeToList m2),
                    unIntervalSet larger
                  ]

delete :: (Int, Int) -> IntervalSet -> IntervalSet
delete i@(lb, ub) is
  | nullInterval i = is
  | otherwise =
      case splitLookupLE lb is of
        (smaller, m1, xs) ->
          case splitLookupLE ub xs of
            (_, m2, larger) ->
              IntervalSet $
                IntMap.unions
                  [ unIntervalSet smaller,
                    case m1 of
                      Nothing -> IntMap.empty
                      Just j -> IntMap.fromList (catMaybes (Prelude.map (intersectIntervals j =<<) [upTo lb, downTo ub])),
                    fromMaybe IntMap.empty do
                      j <- m2
                      aboveDelete <- downTo ub
                      uncurry IntMap.singleton <$> intersectIntervals aboveDelete j,
                    unIntervalSet larger
                  ]

complement :: IntervalSet -> IntervalSet
complement (IntervalSet m) = fromAscList . (\xs -> Prelude.foldr phi z xs Nothing) . IntMap.toAscList $ m
  where
    phi (lb, ub) b mprevUb =
      case mprevUb of
        Nothing -> case upTo lb of
          Nothing -> b (Just ub)
          Just x -> x : b (Just ub)
        Just lastUb ->
          let !lbPred = safeAdd lb (-1)
              !lastUbSucc = safeAdd lastUb 1
              proposedInterval = (lastUbSucc, lbPred)
           in case nullInterval proposedInterval of
                True -> b (Just ub)
                False -> proposedInterval : b (Just ub)
    z = \case
      Nothing -> [(0, maxBound)]
      Just prev -> case downTo prev of
        Nothing -> []
        Just x -> [x]

intersection :: IntervalSet -> IntervalSet -> IntervalSet
intersection a b = difference a (complement b)

null :: IntervalSet -> Bool
null = IntMap.null . unIntervalSet

extractSingleton :: IntervalSet -> Maybe Int
extractSingleton (IntervalSet m) = case IntMap.toList m of
  [(lb, ub)]
    | lb == ub -> Just lb
  _ -> Nothing

-- | add two integers, sticking to a bound if it would overflow
safeAdd :: Int -> Int -> Int
safeAdd a b =
  let c = a + b
   in case a > 0 && b > 0 of
        True -> case c < 0 of
          True -> maxBound
          False -> c
        False -> case a < 0 && b < 0 of
          True -> case c >= 0 of
            True -> minBound
            False -> c
          False -> c

difference :: IntervalSet -> IntervalSet -> IntervalSet
difference x (IntervalSet y) = IntMap.foldlWithKey' (\b k v -> delete (k, v) b) x y

-- | the interval [0, lb)
upTo :: Int -> Maybe (Int, Int)
upTo lb = case lb <= 0 of
  True -> Nothing
  False -> Just (0, safeAdd lb (-1))

-- | the interval (ub, maxBound]
downTo :: Int -> Maybe (Int, Int)
downTo ub = case ub == maxBound of
  True -> Nothing
  False -> Just (safeAdd ub 1, maxBound)

nullInterval :: (Int, Int) -> Bool
nullInterval (lb, ub) = ub < lb

-- | merge a list sorted on the lower bound ascending
fromAscList :: [(Int, Int)] -> IntervalSet
fromAscList = IntervalSet . IntMap.fromAscList . mergeOverlappingAscList

fromList :: [(Int, Int)] -> IntervalSet
fromList = fromAscList . sortOn fst . filter (not . nullInterval)

intersectIntervals :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersectIntervals a b
  | doOverlap a b =
      let !lb = on max fst a b
          !ub = on min snd a b
       in Just (lb, ub)
  | otherwise = Nothing

mergeOverlappingAscList :: [(Int, Int)] -> [(Int, Int)]
mergeOverlappingAscList = \case
  x0 : x1 : xs -> case doOverlap x0 x1 of
    True -> spanIntervals x0 x1 : mergeOverlappingAscList xs
    False -> x0 : x1 : mergeOverlappingAscList xs
  [x] -> [x]
  [] -> []

doOverlap :: (Int, Int) -> (Int, Int) -> Bool
doOverlap (lb0, ub0) (lb1, ub1)
  | ub0 >= lb1 && lb0 <= ub1 = True
  | otherwise = False

spanIntervals :: (Int, Int) -> (Int, Int) -> (Int, Int)
spanIntervals (lb0, ub0) (lb1, ub1) =
  let !lb = min lb0 lb1
      !ub = max ub0 ub1
   in (lb, ub)

splitLookupLE :: Int -> IntervalSet -> (IntervalSet, Maybe (Int, Int), IntervalSet)
splitLookupLE k (IntervalSet m) =
  coerce case IntMap.splitLookup k m of
    (smaller, Just v, larger) -> (smaller, Just (k, v), larger)
    (smaller, Nothing, larger) -> case IntMap.maxViewWithKey smaller of
      Just ((k, v), smaller) -> (smaller, Just (k, v), larger)
      Nothing -> (smaller, Nothing, larger)

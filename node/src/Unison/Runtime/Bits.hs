{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language Rank2Types #-}

module Unison.Runtime.Bits where

import Data.Tuple (swap)
import Data.List
import Unison.Runtime.Unfold (Unfold)
import qualified Unison.Runtime.Unfold as U

newtype Bits = Bits { bitstream :: Unfold Bit } deriving (Eq,Ord,Show)

data Bit = Zero | One | Both deriving (Eq,Ord,Show)

matches :: Bit -> Bool -> Bool
matches Both _ = True
matches Zero False = True
matches One True = True
matches _ _ = False

type Score = Double

from01s :: [Int] -> Bits
from01s bs = fromList (map f bs) where
  f 0 = Zero
  f 1 = One
  f i = error ("from01s: must be 0 or 1, got " ++ show i)

fromList :: [Bit] -> Bits
fromList bs = Bits (U.fromList bs)

toList :: Bits -> [Bit]
toList (Bits bs) = U.toList bs

-- | Achieves maximum value of n/2 when both `zeros` and `ones` are n/2.
-- As distribution is more skewed toward either bit, score approaches 0.
-- Satisfies: `score n n 0 == 0`, `score n 0 n == 0`, `score n 0 0 == 0`.
-- There is a linear penalty if zeros + ones < n. So `score 10 4 4` will
-- be less than `score 10 5 5`.
score :: Double -> Double -> Double -> Score
score n zeros ones =
  let p0 = zeros / n; p1 = ones / n
  in p0 * (n - zeros) + p1 * (n - ones)

bitCounts' :: (Double -> Double -> Bool) -> [Bit] -> (Double,Double)
bitCounts' halt bs = go 0 0 bs where
  go !zeros !ones [] = (zeros, ones)
  go !zeros !ones (b:bs)
    | halt zeros ones = (zeros, ones)
    | otherwise = case b of
      Zero -> go (zeros + 1) ones bs
      One -> go zeros (ones + 1) bs
      Both -> go (zeros + 1) (ones + 1) bs

mostSignificantBit :: [Bits] -> Maybe (Int, Score)
mostSignificantBit bs = go (Nothing,0) (U.columns (map bitstream bs)) where
  n = fromIntegral (length bs)
  lengthGT xs n = not (null (dropWhile (\(_,m) -> m <= n) (xs `zip` [1..])))
  value = maybe 0 snd
  rem z o = (n-z) `min` (n-o)
  maxPossible z o = score n (z `max` o) (m + ((n/2 - m) `min` rem z o)) where
    m = z `min` o
  stop best z o | z `max` o > n/2 = maxPossible z o <= best
  stop _ _ _ = False
  go (!best,!_) [] = best
  go (!best,!i) (bs:tl)
    | not (lengthGT bs (value best * 2)) = best
    | otherwise = case bitCounts' (stop (value best)) bs of
      (z,o) -> go (if s > value best then Just (i, s) else best, i + 1) tl
        where s = score n z o

bitCounts :: [Bits] -> [(Int,Int)]
bitCounts bs = sums (map bitstream bs) where
  sumCol = foldl' step (0,0) where
    step (z,o) b = case b of Zero -> (z+1,o); One -> (z,o+1); Both -> (z+1,o+1)
  sums [] = []
  sums bs =
    let (col, bs') = unzip [ (b, tl) | Just (b, tl) <- map U.uncons bs ]
    in (if null bs' then [] else sumCol col : sums bs')

mostSignificantBits :: [Bits] -> [(Int,Score)]
mostSignificantBits bs = go (map rank $ bitCounts bs) where
  rank = let n = fromIntegral (length bs)
         in \(zeros, ones) -> score n (fromIntegral zeros) (fromIntegral ones)
  go ranks = map swap $ sortBy (flip compare) (ranks `zip` [0..])

sample :: [Bits]
sample =
  [ from01s[1,0]
  , from01s[1,0]
  , from01s[1,1,0,1]
  , from01s[1,1,1,1]
  , from01s[1,1,0,1]
  , from01s[1,1,0,1,1]
  , from01s[1,1,1,1]
  , from01s[1,1,0,1]
  , from01s[1,1,1,1]
  ]

sampleMsb :: Maybe (Int,Score)
sampleMsb = mostSignificantBit sample

{-# Language GADTs #-}
{-# Language TypeOperators #-}

module Unison.Runtime.Vector where

import Unison.Prelude
-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

data Vec a where
  Ints    :: UV.Vector Int -> Vec Int
  Words   :: UV.Vector Word -> Vec Word
  Bytes   :: UV.Vector Word8 -> Vec Word8
  Bools   :: UV.Vector Bool -> Vec Bool
  Pair    :: Vec a -> Vec b -> Vec (a, b)
  Units   :: Int -> Vec ()
  -- to get kth index, get the kth element of the Vector Int:
  --   if negative, it's `Left`, value is found in 1-based index into the `Vec a`
  --   if positive, it's `Right`, value is found in 1-based index into the `Vec b`
  Eithers :: UV.Vector Int -> Vec a -> Vec b -> Vec (Either a b)

data Tensor ix a = Tensor { bounds :: ix, at :: Vec ix -> Vec (ix, a) }

_1 :: Vec (a,b) -> Vec a
_1 (Pair a _) = a

_2 :: Vec (a,b) -> Vec b
_2 (Pair _ b) = b

compose :: Tensor ix ix2 -> Tensor ix2 a -> Tensor ix a
compose v1 v2 = Tensor (bounds v1) at' where
  at' ix = let ix2s = at v1 ix
               as   = at v2 (_2 ix2s)
           -- not quite right since the second `at` may drop
           -- further elements - want to do some inner-join kind of thing here
           -- maybe have at return (Vec a, Vec Int) of indices dropped
           in Pair (_1 ix2s) (_2 as)

{-
data Vector ix a = Vector { bounds :: ix, at :: ix -> Maybe a }

-- data Vector ix a = Vector { bounds :: ix, at :: UV.Vector ix -> UV.Vector (ix, a) }
-- fromSum :: (Unbox a, Unbox b) => [Either a b] -> Vector (() :. Int) (Either a b)
-- fromList :: Unbox a => [a] -> Vector (() :. Int) a

data a :. b = a :. b

unsnoc :: (a :. b) -> a
unsnoc (a :. _) = a

fromList :: [a] -> Vector (() :. Int) a
fromList as =
  Vector (() :. V.length vs) (\(() :. i) -> vs V.!? i)
  where vs = V.fromList as

compose :: Vector ix ix2 -> Vector ix2 a -> Vector ix a
compose v1 v2 = Vector (bounds v1) at' where
  at' ix = at v2 =<< at v1 ix

replicate :: Ord i => i -> Vector ix a -> Vector (ix :. i) a
replicate imax v = Vector (bounds v :. imax) at' where
  at' (ix :. i) | i <= imax = at v ix
                | otherwise = Nothing

scalar :: a -> Vector () a
scalar a = Vector () (const $ Just a)

pmap :: (a -> Maybe b) -> Vector ix a -> Vector ix b
pmap f v = Vector (bounds v) (\ix -> at v ix >>= f)

instance Functor (Vector ix) where
  fmap f = pmap (Just . f)

plus, minus, times :: (Ord ix, Num a) => Vector ix a -> Vector ix a -> Vector ix a
v1 `plus` v2 = Vector (bounds v1 `min` bounds v2) at' where
  at' ix = (+) <$> at v1 ix <*> at v2 ix
v1 `minus` v2 = Vector (bounds v1 `min` bounds v2) at' where
  at' ix = (-) <$> at v1 ix <*> at v2 ix
v1 `times` v2 = Vector (bounds v1 `min` bounds v2) at' where
  at' ix = (*) <$> at v1 ix <*> at v2 ix

prefixSum :: Num a => Vector (ix :. Int) a -> Vector (ix :. Int) a
prefixSum _v = undefined

sum :: Num a => Vector (ix :. Int) a -> Vector ix a
sum v = Vector (unsnoc (bounds v)) at' where
  at' ix = Just $ foldl' (+) 0 [ a | i <- [0..maxi], Just a <- [at v (ix :. i)] ]
  maxi = case bounds v of
    _ :. i -> i
-}

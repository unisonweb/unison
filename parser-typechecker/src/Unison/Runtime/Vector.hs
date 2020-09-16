{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language BangPatterns #-}

module Unison.Runtime.Vector where

import Prelude hiding (length)
import Unison.Prelude
-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Bit as B
import qualified Data.Vector.Unboxed.Mutable as MV

data Vec a where
  V :: Type a -> UV.Vector a -> Vec a
  Zip :: Vec a -> Vec b -> Vec (a,b)
  -- to get kth index, get the kth element of the Vector Int:
  --   if negative, it's `Left`, value is found in 1-based index into the `Vec a`
  --   if positive, it's `Right`, value is found in 1-based index into the `Vec b`
  Eithers :: UV.Vector Int -> Vec a -> Vec b -> Vec (Either a b)
  -- to get kth index, get kth element of the Vector Int, if it's -1, it's `Nothing`
  -- otherwise, it's the index into the `Vec a`
  Maybes  :: UV.Vector Int -> Vec a -> Vec (Maybe a)

data Type a where
  Int :: Type Int
  Word :: Type Word
  Byte :: Type Word8
  Bit :: Type B.Bit
  Double :: Type Double

data Vector ix a = Vector { bounds :: ix, at :: Vec ix -> Vec (ix, a) }

unsafeIndex :: Int -> Vec a -> a
unsafeIndex !i v = case v of
  V t v -> case t of
    Int -> UV.unsafeIndex v i
    Double -> UV.unsafeIndex v i
    Word -> UV.unsafeIndex v i
    Byte -> UV.unsafeIndex v i
    Bit -> UV.unsafeIndex v i
  Zip v1 v2 -> (unsafeIndex i v1, unsafeIndex i v2)
  Maybes inds elems ->
    let i' = UV.unsafeIndex inds i
    in if i' < 0 then Nothing
       else           Just $ unsafeIndex i' elems
  Eithers inds lefts rights ->
    let i' = UV.unsafeIndex inds i
    in if i' < 0 then Left  $ unsafeIndex (abs i' - 1) lefts
       else           Right $ unsafeIndex (i' - 1    ) rights

pick :: UV.Vector Int -> Vec a -> Vec a
pick inds v = case v of
  V t v -> V t $ case t of
    Int -> UV.backpermute v inds
    Double -> UV.backpermute v inds
    Word -> UV.backpermute v inds
    Byte -> UV.backpermute v inds
    Bit -> UV.backpermute v inds
  Zip v1 v2 -> Zip (pick inds v1) (pick inds v2)
  -- todo: this needs to do something more interesting
  Eithers is lefts rights -> Eithers (UV.backpermute is inds) lefts rights
  Maybes is elems ->
    let is' = UV.backpermute is inds
        elems' = pick (UV.filter (>= 0) is') elems
        step (cur,_) i = if i >= 0 then (cur,cur) else (cur,-1)
    in Maybes (UV.map snd $ UV.postscanl' step (0,0) is') elems'

length :: Vec a -> Int
length v = case v of
  V t v -> case t of
    Int -> UV.length v
    Double -> UV.length v
    Word -> UV.length v
    Byte -> UV.length v
    Bit -> UV.length v
  Zip v1 v2 -> length v1 `min` length v2
  Eithers v _ _ -> UV.length v
  Maybes v _ -> UV.length v

unsafeCompareAt :: Int -> Int -> Vec a -> Vec a -> Ordering
unsafeCompareAt i j v v2 = case (v,v2) of
  (V t v, V _ v2) -> case t of
    Int -> UV.unsafeIndex v i `compare` UV.unsafeIndex v2 j
    Double -> UV.unsafeIndex v i `compare` UV.unsafeIndex v2 j
    Word -> UV.unsafeIndex v i `compare` UV.unsafeIndex v2 j
    Byte -> UV.unsafeIndex v i `compare` UV.unsafeIndex v2 j
    Bit -> UV.unsafeIndex v i `compare` UV.unsafeIndex v2 j
  (Zip v1a v2a, Zip v1b v2b) -> case unsafeCompareAt i j v1a v1b of
    EQ -> unsafeCompareAt i j v2a v2b
    c -> c
  (Maybes indsa elemsa, Maybes indsb elemsb) ->
    let i' = UV.unsafeIndex indsa i
        j' = UV.unsafeIndex indsb i
    in if i' >= 0 && j' >= 0 then
         unsafeCompareAt i' j' elemsa elemsb
       else if i' < 0 then LT
       else if j' < 0 then GT
       else EQ
  (Eithers indsa leftsa rightsa, Eithers indsb leftsb rightsb) ->
    let i' = UV.unsafeIndex indsa i
        j' = UV.unsafeIndex indsb i
    in if i' > 0 && j' > 0 then
         unsafeCompareAt (i' - 1) (j' - 1) leftsa leftsb
       else if i' < 0 && j' < 0 then
         unsafeCompareAt (abs i' - 1) (abs j' - 1) rightsa rightsb
       else if i' < 0 then LT
       else GT
  _ -> error "impossible"

innerJoin :: Ord b => Vec (a,b) -> Vec (b,c) -> Vec (a,(b,c))
innerJoin _v1 _v2 = undefined
  -- conceptually: iterate through v1, v2 `b` column, collect up
  -- the matching indices for each, then `pick` from each
  -- undefined
  -- where
  -- (inds1, inds2) = go 0 0
  -- go i j = if

_1 :: Vec (a,b) -> Vec a
_1 (Zip a _) = a
_1 _ = error "impossible"

_2 :: Vec (a,b) -> Vec b
_2 (Zip _ b) = b
_2 _ = error "impossible"

compose :: Ord ix2 => Vector ix ix2 -> Vector ix2 a -> Vector ix a
compose v1 v2 = Vector (bounds v1) at' where
  at' ix = let
    ix2s = at v1 ix        -- [(ix,ix2)]
    as   = at v2 (_2 ix2s) -- [(ix2,a)]
    in if length as == length ix2s then
         Zip (_1 ix2s) (_2 as)
       else let Zip ix (Zip _ a) = innerJoin ix2s as
            in  Zip ix a
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

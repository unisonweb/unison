{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language BangPatterns #-}

module Unison.Runtime.Vector where

import Prelude as P hiding (length)
import qualified Data.Massiv.Array as A

import Unison.Prelude
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as UV
import qualified Data.Bit as B
-- import qualified Data.Vector.Unboxed.Mutable as MV

data Vec ix a where
  U :: A.Unbox a => Type a -> A.Array A.U ix a -> Vec ix a

data Type a where
  Int :: Type Int
  Word :: Type Word
  Byte :: Type Word8
  Bit :: Type B.Bit
  Double :: Type Double

at1 :: Int -> Vec A.Ix1 a -> a
at1 !i (U _ v) = A.index' v i

at :: (A.Unbox ix, A.Index ix) => ix -> Vec ix a -> a
at !i (U _ v) = A.index' v i

size :: (A.Unbox ix, A.Index ix) => Vec ix a -> A.Sz ix
size v = case v of U _ v -> A.size v

size1 :: Vec A.Ix1 a -> Int
size1 v = case size v of A.Sz i -> i

toIndexedList :: (A.Unbox ix, A.Index ix) => Vec ix a -> [(ix,a)]
toIndexedList v = case v of
  U _ v -> A.toList $ A.imap (\i e -> (i,e)) v

indices :: (A.Unbox ix, A.Index ix) => Vec ix a -> [ix]
indices = map fst . toIndexedList

toList :: (A.Unbox ix, A.Index ix) => Vec ix a -> [a]
toList = map snd . toIndexedList

compareAt :: (A.Unbox ix, A.Index ix) => ix -> ix -> Vec ix a -> Vec ix a -> Ordering
compareAt i j v v2 = case (v,v2) of
  (U t v, U _ v2) -> case t of
    Int    -> A.index' v i `compare` A.index' v2 j
    Double -> A.index' v i `compare` A.index' v2 j
    Word   -> A.index' v i `compare` A.index' v2 j
    Byte   -> A.index' v i `compare` A.index' v2 j
    Bit    -> A.index' v i `compare` A.index' v2 j

class Vectorizable a where
  fromList :: [a] -> Vec A.Ix1 a

pick :: (A.Unbox ix, A.Index ix) => Vec ix ix -> Vec ix a -> Vec ix a
pick inds v = case v of
  U t v -> U t $ case t of
    Double -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Word   -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Int    -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Byte   -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Bit    -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v

instance (A.Unbox ix, A.Index ix, Ord ix) => Eq (Vec ix a) where
  v1 == v2 = compare v1 v2 == EQ

instance (A.Unbox ix, A.Index ix, Ord ix) => Ord (Vec ix a) where
  v1 `compare` v2 =
    case dropWhile (== EQ) [ compareAt i j v1 v2 | (i,j) <- indices v1 `P.zip` indices v2 ] of
      [] -> size v1 `compare` size v2
      hd : _ -> hd

instance Vectorizable Int where fromList vs = U Int (A.fromList A.Seq vs)
instance Vectorizable Double where fromList vs = U Double (A.fromList A.Seq vs)
instance Vectorizable Word where fromList vs = U Word (A.fromList A.Seq vs)
instance Vectorizable Word8 where fromList vs = U Byte (A.fromList A.Seq vs)
instance Vectorizable B.Bit where fromList vs = U Bit (A.fromList A.Seq vs)

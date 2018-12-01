{-# Language BangPatterns #-}
{-# Language MagicHash #-} -- used for unsafe pointer equality

module Unison.Runtime.SparseVector where

import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as B
import qualified GHC.Exts as Exts
import qualified Data.Vector.Unboxed           as UV

-- Denotes a `Nat -> Maybe a`.
-- Representation is a `Vector a` along with a bitset
-- that encodes the index of each element.
-- Ex: `[(1,a), (5,b)]` is encoded as (100010, [a,b])
data SparseVector bits a
  = SparseVector { indices :: !bits
                 , elements :: !(UV.Vector a) }

map :: (UV.Unbox a, UV.Unbox b) => (a -> b) -> SparseVector bits a -> SparseVector bits b
map f v = v { elements = UV.map f (elements v) }

isSupersetOf :: B.FiniteBits bits => bits -> bits -> Bool
isSupersetOf b1 b2 = (b1 .&. b2) == b2

mask :: (UV.Unbox a, B.FiniteBits bits)
     => bits -> SparseVector bits a -> SparseVector bits a
mask bits a =
  if indices' == bits then a
  else SparseVector indices' $ UV.create $ do
        vec <- MUV.new (B.popCount indices')
        go vec (indices a) bits 0 0
  where
   indices' = indices a .&. bits
   eas = elements a
   go !out !indAs !indBs !i !k =
     if indAs == B.zeroBits || indBs == B.zeroBits then pure out
     else let
       (!a1, !b1) = (B.countTrailingZeros indAs, B.countTrailingZeros indBs)
       in if a1 == b1 then do
           MUV.write out k (eas UV.! (i + a1))
           go out (indAs `B.shiftR` (a1 + 1)) (indBs `B.shiftR` (b1 + 1))
                  (i + 1) (k + 1)
          else if a1 < b1 then
            go out (indAs `B.shiftR` (a1 + 1)) indBs
                   (i + 1) k
          else
            go out indAs (indBs `B.shiftR` (b1 + 1)) i k

zipWith
 :: (UV.Unbox a, UV.Unbox b, UV.Unbox c, B.FiniteBits bits)
 => (a -> b -> c)
 -> SparseVector bits a
 -> SparseVector bits b
 -> SparseVector bits c
zipWith f a b =
 if indices a `eq` indices b || indices a == indices b then
   SparseVector (indices a) (UV.zipWith f (elements a) (elements b))
 else let
   indices' = indices a .&. indices b
   (eas, ebs) = (elements a, elements b)
   go !out !indAs !indBs !i !j !k =
     if indAs == B.zeroBits || indBs == B.zeroBits then pure out
     else let
       a1 = B.countTrailingZeros indAs
       b1 = B.countTrailingZeros indBs
       in
         if a1 == b1 then do
           MUV.write out k (f (eas UV.! (i + a1)) (ebs UV.! (j + a1)))
           go out (indAs `B.shiftR` (a1 + 1))
                  (indBs `B.shiftR` (b1 + 1))
                  (i + 1) (j + 1) (k + 1)
         else if a1 < b1 then
           go out (indAs `B.shiftR` (a1 + 1))
                  indBs
                  (i + 1) j k
         else
           go out indAs
                  (indBs `B.shiftR` (b1 + 1))
                  i (j + 1) k
   in SparseVector indices' $ UV.create $ do
        vec <- MUV.new (B.popCount indices')
        go vec (indices a) (indices b) 0 0 0

-- Finds the index of the least significant 1 bit.
findFirst1 :: B.FiniteBits b => b -> Int
findFirst1 b = 1 + B.countTrailingZeros b

-- Pointer equality a la Scala.
eq :: a -> a -> Bool
eq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y Exts.==# 1#)
{-# INLINE eq #-}

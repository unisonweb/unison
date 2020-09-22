{-# Language BangPatterns #-}
{-# Language Strict #-}
{-# Language StrictData #-}

module Unison.Util.CyclicEq where

import Unison.Prelude

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Unison.Util.CycleTable as CT

{-
 Typeclass used for comparing potentially cyclic types for equality.
 Cyclic types may refer to themselves indirectly, so something is needed to
 prevent an infinite loop in these cases. The basic idea: when a subexpression
 is first examined, its "id" (represented as some `Int`) may be added to the
 mutable hash table along with its position. The next time that same id is
 encountered, it will be compared based on this position.
 -}
class CyclicEq a where
  -- Map from `Ref` ID to position in the stream
  -- If a ref is encountered again, we use its mapped ID
  cyclicEq :: CT.CycleTable Int Int -> CT.CycleTable Int Int -> a -> a -> IO Bool

bothEq' :: (Eq a, CyclicEq b) => CT.CycleTable Int Int -> CT.CycleTable Int Int
  -> a -> a -> b -> b -> IO Bool
bothEq' h1 h2 a1 a2 b1 b2 =
  if a1 == a2 then cyclicEq h1 h2 b1 b2
  else pure False

bothEq ::
  (CyclicEq a, CyclicEq b) => CT.CycleTable Int Int -> CT.CycleTable Int Int
  -> a -> a -> b -> b -> IO Bool
bothEq h1 h2 a1 a2 b1 b2 = cyclicEq h1 h2 a1 a2 >>= \b ->
  if b then cyclicEq h1 h2 b1 b2
  else pure False

instance CyclicEq a => CyclicEq [a] where
  cyclicEq h1 h2 (x:xs) (y:ys) = bothEq h1 h2 x y xs ys
  cyclicEq _ _ [] [] = pure True
  cyclicEq _ _ _ _   = pure False

instance CyclicEq a => CyclicEq (S.Seq a) where
  cyclicEq h1 h2 xs ys =
    if S.length xs == S.length ys then cyclicEq h1 h2 (toList xs) (toList ys)
    else pure False

instance CyclicEq a => CyclicEq (Vector a) where
  cyclicEq h1 h2 xs ys =
    if V.length xs /= V.length ys then pure False
    else go 0 h1 h2 xs ys
    where
    go !i !h1 !h2 !xs !ys =
      if i >= V.length xs then pure True
      else do
        b <- cyclicEq h1 h2 (xs V.! i) (ys V.! i)
        if b then go (i + 1) h1 h2 xs ys
        else pure False

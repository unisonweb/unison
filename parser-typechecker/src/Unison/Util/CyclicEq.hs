{-# Language BangPatterns #-}
{-# Language Strict #-}
{-# Language StrictData #-}
{-# Language DoAndIfThenElse #-}

module Unison.Util.CyclicEq where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Unison.Util.Hashtable as HT

class CyclicEq a where
  -- Map from `Ref` ID to position in the stream
  -- If a ref is encountered again, we use its mapped ID
  cyclicEq :: HT.Hashtable Int Int -> HT.Hashtable Int Int -> a -> a -> IO Bool

bothEq' :: (Eq a, CyclicEq b) => HT.Hashtable Int Int -> HT.Hashtable Int Int
  -> a -> a -> b -> b -> IO Bool
bothEq' h1 h2 a1 a2 b1 b2 =
  if a1 == a2 then cyclicEq h1 h2 b1 b2
  else pure False

bothEq ::
  (CyclicEq a, CyclicEq b) => HT.Hashtable Int Int -> HT.Hashtable Int Int
  -> a -> a -> b -> b -> IO Bool
bothEq h1 h2 a1 a2 b1 b2 = cyclicEq h1 h2 a1 a2 >>= \b ->
  if b then cyclicEq h1 h2 b1 b2
  else pure False

instance CyclicEq a => CyclicEq [a] where
  cyclicEq h1 h2 (x:xs) (y:ys) = bothEq h1 h2 x y xs ys
  cyclicEq _ _ [] [] = pure True
  cyclicEq _ _ _ _   = pure False

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

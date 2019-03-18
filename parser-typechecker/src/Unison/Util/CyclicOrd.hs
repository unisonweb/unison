{-# Language BangPatterns #-}
{-# Language Strict #-}
{-# Language StrictData #-}
{-# Language DoAndIfThenElse #-}

module Unison.Util.CyclicOrd where

import Data.Vector (Vector)
import Unison.Util.Hashtable (Hashtable)
import qualified Data.Vector as V
import qualified Unison.Util.Hashtable as HT

class CyclicOrd a where
  -- Map from `Ref` ID to position in the stream
  -- If a ref is encountered again, we use its mapped ID
  cyclicOrd :: Hashtable Int Int -> Hashtable Int Int -> a -> a -> IO Ordering

bothOrd' ::
  (Ord a, CyclicOrd b) => HT.Hashtable Int Int -> HT.Hashtable Int Int
  -> a -> a -> b -> b -> IO Ordering
bothOrd' h1 h2 a1 a2 b1 b2 = case compare a1 a2 of
  EQ -> cyclicOrd h1 h2 b1 b2
  c -> pure c

bothOrd ::
  (CyclicOrd a, CyclicOrd b) => HT.Hashtable Int Int -> HT.Hashtable Int Int
  -> a -> a -> b -> b -> IO Ordering
bothOrd h1 h2 a1 a2 b1 b2 = cyclicOrd h1 h2 a1 a2 >>= \b ->
  if b == EQ then cyclicOrd h1 h2 b1 b2
  else pure b

instance CyclicOrd a => CyclicOrd [a] where
  cyclicOrd h1 h2 (x:xs) (y:ys) = bothOrd h1 h2 x y xs ys
  cyclicOrd _ _ [] []  = pure EQ
  cyclicOrd _ _ [] _   = pure LT
  cyclicOrd _ _ _ []   = pure GT

instance CyclicOrd a => CyclicOrd (Vector a) where
  cyclicOrd h1 h2 xs ys = go 0 h1 h2 xs ys
    where
    go !i !h1 !h2 !xs !ys =
      if i >= V.length xs && i >= V.length ys then pure EQ
      else if i >= V.length xs then pure LT
      else if i >= V.length ys then pure GT
      else do
        b <- cyclicOrd h1 h2 (xs V.! i) (ys V.! i)
        if b == EQ then go (i + 1) h1 h2 xs ys
        else pure b

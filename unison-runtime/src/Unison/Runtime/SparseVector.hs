{-# LANGUAGE BangPatterns #-}
-- used for unsafe pointer equality
{-# LANGUAGE MagicHash #-}

module Unison.Runtime.SparseVector where

import Data.Vector.Unboxed qualified as UV
import Prelude hiding (unzip)

-- Denotes a `Nat -> Maybe a`.
-- Representation is a `Vector a` along with a bitset
-- that encodes the index of each element.
-- Ex: `[(1,a), (5,b)]` is encoded as (100010, [a,b])
data SparseVector bits a = SparseVector
  { indices :: !bits,
    elements :: !(UV.Vector a)
  }

-- todo: instance (UV.Unbox a, B.FiniteBits bits, Num n)
--   => Num (SparseVector bits n)

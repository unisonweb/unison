{-# LANGUAGE GADTs #-}

module Unison.Runtime.Vector where

import Data.Vector.Unboxed qualified as UV
import Unison.Prelude

-- A `Vec a` denotes a `Nat -> Maybe a`
data Vec a where
  Scalar :: a -> Vec a
  Vec :: (UV.Unbox a) => UV.Vector a -> Vec a
  Pair :: Vec a -> Vec b -> Vec (a, b)
  Choose :: Vec Bool -> Vec a -> Vec a -> Vec a
  Mux :: Vec Nat -> Vec (Vec a) -> Vec a

-- todo: maybe make representation `(UV.Vector Nat -> UnboxedMap Nat a, Bound)`
-- `UnboxedMap Nat a = (UV.Vector Nat, UV.Vector a)`
-- UnboxedMap Nat could be implemented as an `UArray`
-- `Bound` is Nat, max possible index
-- then easy to implement `+`, `-`, etc

type Nat = Word64

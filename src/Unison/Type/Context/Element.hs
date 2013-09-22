{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Type.Context.Element where

import Control.Lens
import Data.Foldable

-- Kind used to tag contexts
data T = Complete | Incomplete

-- | Elements of an algorithmic context, indexed by the `T` kind,
-- which indicates whether the context contains unsolved existentials.
-- An `Element Complete` cannot be `Existential`.
-- The `sa` type parameter is the type of solved existentials (a `Monotype`)
-- and the `a` type parameter is the type of other annotations (a `Polytype`).
data Element (t :: T) sa a v where
  Universal :: v -> Element t sa a v            -- | ^ `v` is universally quantified
  Existential :: v -> Element Incomplete sa a v -- | ^ `v` existential and unsolved
  Solved :: v -> sa -> Element t sa a v         -- | ^ `v` is solved to some monotype, `sa`
  Ann :: v -> a -> Element t sa a v             -- | ^ `v` has type `a`, which may be quantified
  Marker :: v -> Element t sa a v               -- | ^ used for scoping somehow

instance Functor (Element t sa a) where
  fmap f e = case e of
    Universal v -> Universal (f v)
    Existential v -> Existential (f v)
    Solved v sa -> Solved (f v) sa
    Ann v a -> Ann (f v) a
    Marker v -> Marker (f v)

_Universal :: Simple Prism (Element t sa a v) v
_Universal = prism Universal go where
  go (Universal v) = Right v
  go e = Left e

_Existential :: Simple Prism (Element Incomplete sa a v) v
_Existential = prism Existential go where
  go (Existential v) = Right v
  go e = Left e

_Solved :: Simple Prism (Element t sa a v) (v, sa)
_Solved = prism (uncurry Solved) go where
  go (Solved v t) = Right (v, t)
  go e = Left e

_Ann :: Simple Prism (Element t sa a v) (v, a)
_Ann = prism (uncurry Ann) go where
  go (Ann v t) = Right (v, t)
  go e = Left e

_Marker :: Simple Prism (Element t sa a v) v
_Marker = prism Marker go where
  go (Marker v) = Right v
  go e = Left e

deriving instance (Eq sa, Eq a, Eq v) => Eq (Element t sa a v)
deriving instance (Show sa, Show a, Show v) => Show (Element t sa a v)
deriving instance (Ord sa, Ord a, Ord v) => Ord (Element t sa a v)

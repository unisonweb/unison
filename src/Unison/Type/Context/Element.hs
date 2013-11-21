{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Type.Context.Element where

import Control.Lens
import qualified Unison.Syntax.Type as T
import qualified Unison.Syntax.Var as V

-- | Elements of an algorithmic context, indexed by the `T` kind,
-- which indicates whether the context contains unsolved existentials.
-- An `Element Complete` cannot be `Existential`.
-- The `sa` type parameter is the type of solved existentials (a `Monotype`)
-- and the `a` type parameter is the type of other annotations (a `Polytype`).
data Element sa a v where
  Universal :: v -> Element sa a v            -- | ^ `v` is universally quantified
  Existential :: v -> Element sa a v          -- | ^ `v` existential and unsolved
  Solved :: v -> sa -> Element sa a v         -- | ^ `v` is solved to some monotype, `sa`
  Ann :: v -> a -> Element sa a v             -- | ^ `v` has type `a`, which may be quantified
  Marker :: v -> Element sa a v               -- | ^ used for scoping somehow

instance Functor (Element sa a) where
  fmap f e = case e of
    Universal v -> Universal (f v)
    Existential v -> Existential (f v)
    Solved v sa -> Solved (f v) sa
    Ann v a -> Ann (f v) a
    Marker v -> Marker (f v)

(===) :: Eq v => TElement c k v -> TElement c k v -> Bool
Existential v === Existential v2 | v == v2 = True
Universal v === Universal v2 | v == v2 = True
Marker v === Marker v2 | v == v2 = True
_ === _ = False

(!==) :: Eq v => TElement c k v -> TElement c k v -> Bool
e1 !== e2 = not (e1 === e2)

type TElement c k v =
  Element (T.Monotype c k (V.Var v)) (T.Type c k (V.Var v)) (V.Var v)

_Universal :: Simple Prism (Element sa a v) v
_Universal = prism Universal go where
  go (Universal v) = Right v
  go e = Left e

_Existential :: Simple Prism (Element sa a v) v
_Existential = prism Existential go where
  go (Existential v) = Right v
  go e = Left e

_Solved :: Simple Prism (Element sa a v) (v, sa)
_Solved = prism (uncurry Solved) go where
  go (Solved v t) = Right (v, t)
  go e = Left e

_Ann :: Simple Prism (Element sa a v) (v, a)
_Ann = prism (uncurry Ann) go where
  go (Ann v t) = Right (v, t)
  go e = Left e

_Marker :: Simple Prism (Element sa a v) v
_Marker = prism Marker go where
  go (Marker v) = Right v
  go e = Left e

deriving instance (Eq sa, Eq a, Eq v) => Eq (Element sa a v)
deriving instance (Show sa, Show a, Show v) => Show (Element sa a v)
deriving instance (Ord sa, Ord a, Ord v) => Ord (Element sa a v)

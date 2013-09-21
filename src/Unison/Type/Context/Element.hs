{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Type.Context.Element where

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

deriving instance (Eq sa, Eq a, Eq v) => Eq (Element t sa a v)
deriving instance (Show sa, Show a, Show v) => Show (Element t sa a v)
deriving instance (Ord sa, Ord a, Ord v) => Ord (Element t sa a v)

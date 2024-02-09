module Unison.KindInference.Constraint.Unsolved
  ( Constraint (..),
    starProv,
    prov,
    loc,
  )
where

import Control.Lens (Lens, Lens', Traversal)
import Unison.KindInference.Constraint.Provenance (Provenance)
import Unison.KindInference.Constraint.Provenance qualified as Provenance

-- | Unsolved constraints
--
-- These are produced during constraint generation and given as input
-- to the constraint solver.
data Constraint uv v loc starProv
  = -- | An IsType constraint may arise from generation or from the
    -- solver. During generation the provenance is always a real
    -- source code location, but the solver defaults unconstrained
    -- kind vars to Star.
    IsType uv (starProv v loc)
  | IsArr uv (Provenance v loc) uv uv
  | IsAbility uv (Provenance v loc)
  | Unify (Provenance v loc) uv uv
  deriving stock (Show, Eq, Ord)

starProv ::
  Traversal
    (Constraint uv v loc prov)
    (Constraint uv v loc prov')
    (prov v loc)
    (prov' v loc)
starProv f = \case
  IsType x l -> IsType x <$> f l
  IsAbility x l -> pure (IsAbility x l)
  IsArr s l a b -> pure (IsArr s l a b)
  Unify l a b -> pure (Unify l a b)
{-# INLINE starProv #-}

prov ::
  Lens
    (Constraint uv v loc Provenance)
    (Constraint uv v loc' Provenance)
    (Provenance v loc)
    (Provenance v loc')
prov f = \case
  IsType x l -> IsType x <$> f l
  IsAbility x l -> IsAbility x <$> f l
  IsArr s l a b -> (\x -> IsArr s x a b) <$> f l
  Unify l a b -> (\x -> Unify x a b) <$> f l
{-# INLINE prov #-}

loc :: Lens' (Constraint uv v loc Provenance) loc
loc = prov . Provenance.loc
{-# INLINE loc #-}

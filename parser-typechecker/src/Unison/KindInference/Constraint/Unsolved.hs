module Unison.KindInference.Constraint.Unsolved
  ( Constraint (..),
    starProv,
    prov,
    loc,
  )
where

import Control.Lens (Traversal, Lens, Lens')
import Unison.KindInference.Constraint.Provenance (Provenance)
import Unison.KindInference.Constraint.Provenance qualified as Provenance

-- | Unsolved constraints
data Constraint uv v loc starProv
  = -- | An IsStar constraint may arise from generation or from the
    -- solver. During generation the provenance is always a real
    -- source code location, but the solver defaults unconstrained
    -- kind vars to Star.
    IsStar uv (starProv v loc)
  | IsArr uv (Provenance v loc) uv uv
  | IsEffect uv (Provenance v loc)
  | Unify (Provenance v loc) uv uv
  deriving stock (Show, Eq, Ord)

starProv ::
  Traversal
    (Constraint uv v loc prov)
    (Constraint uv v loc prov')
    (prov v loc)
    (prov' v loc)
starProv f = \case
  IsStar x l -> IsStar x <$> f l
  IsEffect x l -> pure (IsEffect x l)
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
  IsStar x l -> IsStar x <$> f l
  IsEffect x l -> IsEffect x <$> f l
  IsArr s l a b -> (\x -> IsArr s x a b) <$> f l
  Unify l a b -> (\x -> Unify x a b) <$> f l
{-# INLINE prov #-}

loc :: Lens' (Constraint uv v loc Provenance) loc
loc = prov . Provenance.loc
{-# INLINE loc #-}

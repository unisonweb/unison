module Unison.KindInference.Constraint.Solved
  ( Constraint (..),
    prov,
    loc,
  )
where

import Control.Lens (Traversal, Traversal')
import Unison.KindInference.Constraint.Provenance (Provenance)
import Unison.KindInference.Constraint.Provenance qualified as Provenance
import Unison.KindInference.Constraint.TypeProvenance (TypeProvenance)
import Unison.KindInference.Constraint.TypeProvenance qualified as TP

-- | Solved constraints
--
-- These constraints are associated with unification variables during
-- kind inference.
data Constraint uv v loc
  = IsType (TypeProvenance v loc)
  | IsAbility (Provenance v loc)
  | IsArr (Provenance v loc) uv uv
  deriving stock (Show, Eq, Ord)

prov ::
  Traversal
    (Constraint uv v loc)
    (Constraint uv v loc')
    (Provenance v loc)
    (Provenance v loc')
prov f = \case
  IsType x -> IsType <$> TP.prov f x
  IsAbility x -> IsAbility <$> f x
  IsArr l a b -> (\x -> IsArr x a b) <$> f l
{-# INLINE prov #-}

loc :: Traversal' (Constraint uv v loc) loc
loc = prov . Provenance.loc
{-# INLINE loc #-}

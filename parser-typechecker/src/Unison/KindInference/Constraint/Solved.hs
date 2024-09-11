module Unison.KindInference.Constraint.Solved
  ( Constraint (..),
  )
where

import Unison.KindInference.Constraint.Provenance (Provenance)
import Unison.KindInference.Constraint.TypeProvenance (TypeProvenance)

-- | Solved constraints
--
-- These constraints are associated with unification variables during
-- kind inference.
data Constraint uv v loc
  = IsType (TypeProvenance v loc)
  | IsAbility (Provenance v loc)
  | IsArr (Provenance v loc) uv uv
  deriving stock (Show, Eq, Ord)

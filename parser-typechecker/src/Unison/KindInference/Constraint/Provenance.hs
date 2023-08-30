module Unison.KindInference.Constraint.Provenance
  ( Provenance (..),
    loc,
  )
where

import Control.Lens (Lens')
import Unison.KindInference.Constraint.Context (ConstraintContext (..))

-- | A tag for a source span indicating where it came from. This is
-- helpful so that we correctly propagate the problematic source span
-- through the solver.
data Provenance v loc
  = Provenance !(ConstraintContext v loc) !loc
  deriving stock (Show, Eq, Ord)

loc :: Lens' (Provenance v loc) loc
loc f = \case
  Provenance ctx x -> Provenance ctx <$> f x

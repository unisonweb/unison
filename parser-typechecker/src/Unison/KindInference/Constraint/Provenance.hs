module Unison.KindInference.Constraint.Provenance
  ( Provenance (..),
    loc,
  )
where

import Control.Lens (Lens')
import Unison.KindInference.Constraint.Context (ConstraintContext (..))

-- | A tag for a source span and context indicating where the
-- generated constraint came from. This is helpful to propagate the
-- constraint context through the solver for user-facing error
-- messages.
data Provenance v loc
  = Provenance !(ConstraintContext v loc) !loc
  deriving stock (Show, Eq, Ord)

loc :: Lens' (Provenance v loc) loc
loc f = \case
  Provenance ctx x -> Provenance ctx <$> f x
{-# INLINE loc #-}

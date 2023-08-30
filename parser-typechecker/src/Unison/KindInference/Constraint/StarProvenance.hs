module Unison.KindInference.Constraint.StarProvenance
  ( StarProvenance (..),
    prov,
  )
where

import Control.Lens (Traversal)
import Unison.KindInference.Constraint.Provenance (Provenance)

-- | Provenance of an @IsStar@ constraint, which can arise by default
-- if a type is unconstrained.
data StarProvenance v loc
  = NotDefault (Provenance v loc)
  | Default
  deriving stock (Show, Eq, Ord)

prov ::
  Traversal
    (StarProvenance v loc)
    (StarProvenance v loc')
    (Provenance v loc)
    (Provenance v loc')
prov f = \case
  Default -> pure Default
  NotDefault p -> NotDefault <$> f p
{-# INLINE prov #-}

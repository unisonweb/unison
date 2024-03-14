module Unison.Merge.TwoWay
  ( TwoWay (..),
  )
where

import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Unison.Prelude

data TwoWay a = TwoWay
  { alice :: !a,
    bob :: !a
  }
  deriving stock (Functor, Generic)
  deriving (Monoid, Semigroup) via (GenericSemigroupMonoid (TwoWay a))

instance Applicative TwoWay where
  pure x = TwoWay x x
  TwoWay f g <*> TwoWay x y = TwoWay (f x) (g y)

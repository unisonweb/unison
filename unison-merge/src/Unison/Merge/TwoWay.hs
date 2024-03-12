module Unison.Merge.TwoWay
  ( TwoWay (..),
  )
where

import Unison.Prelude

data TwoWay a = TwoWay
  { alice :: !a,
    bob :: !a
  }
  deriving stock (Functor, Generic)

instance Applicative TwoWay where
  pure x = TwoWay x x
  TwoWay f g <*> TwoWay x y = TwoWay (f x) (g y)

instance Monoid a => Monoid (TwoWay a) where
  mempty = TwoWay mempty mempty

instance Semigroup a => Semigroup (TwoWay a) where
  TwoWay ax bx <> TwoWay ay by = TwoWay (ax <> ay) (bx <> by)

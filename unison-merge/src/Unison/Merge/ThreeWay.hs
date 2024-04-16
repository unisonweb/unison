module Unison.Merge.ThreeWay
  ( ThreeWay (..),
    forgetLca,
  )
where

import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude

data ThreeWay a = ThreeWay
  { lca :: !a,
    alice :: !a,
    bob :: !a
  }
  deriving stock (Foldable, Functor, Generic, Traversable)

instance Applicative ThreeWay where
  pure x = ThreeWay x x x
  ThreeWay f g h <*> ThreeWay x y z = ThreeWay (f x) (g y) (h z)

forgetLca :: ThreeWay a -> TwoWay a
forgetLca ThreeWay {alice, bob} =
  TwoWay {alice, bob}

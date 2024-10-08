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

forgetLca :: ThreeWay a -> TwoWay a
forgetLca ThreeWay {alice, bob} =
  TwoWay {alice, bob}

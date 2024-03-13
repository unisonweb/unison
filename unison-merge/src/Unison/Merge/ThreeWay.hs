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
  deriving stock (Functor, Generic)

forgetLca :: ThreeWay a -> TwoWay a
forgetLca ThreeWay {alice, bob} =
  TwoWay {alice, bob}

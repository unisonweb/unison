module Unison.Merge.ThreeWay
  ( ThreeWay (..),
    forgetLca,
    toTwoOrThreeWay,
    toUpdated,
    GThreeWay (..),
    gforgetLca,
    gfromTwoWay,
    gtoThreeWay,
    gtoUpdated,
  )
where

import Unison.Merge.Internal.Types (GThreeWay (..), ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (GUpdated (..), Updated)

forgetLca :: ThreeWay a -> TwoWay a
forgetLca ThreeWay {alice, bob} =
  TwoWay {alice, bob}

toTwoOrThreeWay :: ThreeWay a -> TwoOrThreeWay a
toTwoOrThreeWay ThreeWay {alice, bob, lca} =
  TwoOrThreeWay {alice, bob, lca = Just lca}

toUpdated :: ThreeWay a -> TwoWay (Updated a)
toUpdated =
  gtoUpdated . toG

toG :: ThreeWay a -> GThreeWay a a
toG ThreeWay {lca, alice, bob} =
  GThreeWay {lca, alice, bob}

gforgetLca :: GThreeWay a b -> TwoWay b
gforgetLca GThreeWay {alice, bob} =
  TwoWay {alice, bob}

gfromTwoWay :: a -> TwoWay b -> GThreeWay a b
gfromTwoWay lca TwoWay {alice, bob} =
  GThreeWay {lca, alice, bob}

gtoThreeWay :: GThreeWay a a -> ThreeWay a
gtoThreeWay GThreeWay {lca, alice, bob} =
  ThreeWay {lca, alice, bob}

gtoUpdated :: GThreeWay a b -> TwoWay (GUpdated a b)
gtoUpdated GThreeWay {lca, alice, bob} =
  TwoWay
    { alice = Updated {old = lca, new = alice},
      bob = Updated {old = lca, new = bob}
    }

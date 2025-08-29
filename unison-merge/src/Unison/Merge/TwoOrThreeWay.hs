module Unison.Merge.TwoOrThreeWay
  ( TwoOrThreeWay (..),
    forgetLca,
    toThreeWay,
  )
where

import Unison.Merge.Internal.Types (ThreeWay (..), TwoOrThreeWay (..), TwoWay (..))
import Unison.Prelude

forgetLca :: TwoOrThreeWay a -> TwoWay a
forgetLca TwoOrThreeWay {alice, bob} =
  TwoWay {alice, bob}

toThreeWay :: a -> TwoOrThreeWay a -> ThreeWay a
toThreeWay x TwoOrThreeWay {alice, bob, lca} =
  ThreeWay {alice, bob, lca = fromMaybe x lca}

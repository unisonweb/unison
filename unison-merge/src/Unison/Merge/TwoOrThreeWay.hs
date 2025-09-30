module Unison.Merge.TwoOrThreeWay
  ( TwoOrThreeWay (..),
    forgetLca,
    toThreeWay,
    toThreeWayA,
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

toThreeWayA :: (Applicative f) => f a -> TwoOrThreeWay a -> f (ThreeWay a)
toThreeWayA x y =
  case y.lca of
    Just lca -> pure (g lca)
    Nothing -> g <$> x
  where
    g lca =
      ThreeWay {alice = y.alice, bob = y.bob, lca}

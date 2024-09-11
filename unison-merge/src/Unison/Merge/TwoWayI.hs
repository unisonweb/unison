module Unison.Merge.TwoWayI
  ( TwoWayI (..),
    forgetBoth,
    who_,
  )
where

import Control.Lens (Lens')
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude
import Prelude hiding (zipWith)

-- | "Two-way inclusive".
data TwoWayI a = TwoWayI
  { alice :: a,
    bob :: a,
    both :: a
  }
  deriving stock (Foldable, Functor, Generic)
  deriving (Monoid, Semigroup) via (GenericSemigroupMonoid (TwoWayI a))

forgetBoth :: TwoWayI a -> TwoWay a
forgetBoth TwoWayI {alice, bob} =
  TwoWay {alice, bob}

who_ :: EitherWayI x -> Lens' (TwoWayI a) a
who_ = \case
  OnlyAlice _ -> #alice
  OnlyBob _ -> #bob
  AliceAndBob _ -> #both

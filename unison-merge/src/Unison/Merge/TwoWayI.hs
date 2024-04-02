module Unison.Merge.TwoWayI
  ( TwoWayI (..),
    forgetBoth,
  )
where

import Data.Semialign (Semialign, alignWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These (..))
import Data.Zip (Zip, zipWith)
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
  deriving (Semigroup) via (GenericSemigroupMonoid (TwoWayI a))

instance Applicative TwoWayI where
  pure x = TwoWayI x x x
  TwoWayI f g h <*> TwoWayI x y z = TwoWayI (f x) (g y) (h z)

instance Semialign TwoWayI where
  alignWith :: (These a b -> c) -> TwoWayI a -> TwoWayI b -> TwoWayI c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Zip TwoWayI where
  zipWith :: (a -> b -> c) -> TwoWayI a -> TwoWayI b -> TwoWayI c
  zipWith f (TwoWayI x1 x2 x3) (TwoWayI y1 y2 y3) =
    TwoWayI (f x1 y1) (f x2 y2) (f x3 y3)

forgetBoth :: TwoWayI a -> TwoWay a
forgetBoth TwoWayI {alice, bob} =
  TwoWay {alice, bob}

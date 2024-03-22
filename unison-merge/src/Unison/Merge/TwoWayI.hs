module Unison.Merge.TwoWayI
  ( TwoWayI (..),
  )
where

import Data.Semialign (Semialign, alignWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These (..))
import Data.Zip (Zip, zipWith)
import Unison.Prelude
import Prelude hiding (zipWith)

-- | "Two-way inclusive".
data TwoWayI a = TwoWayI
  { alice :: !a,
    bob :: !a,
    both :: !a
  }
  deriving stock (Foldable, Functor, Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid (TwoWayI a))

instance Semialign TwoWayI where
  alignWith :: (These a b -> c) -> TwoWayI a -> TwoWayI b -> TwoWayI c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Zip TwoWayI where
  zipWith :: (a -> b -> c) -> TwoWayI a -> TwoWayI b -> TwoWayI c
  zipWith f (TwoWayI x1 x2 x3) (TwoWayI y1 y2 y3) =
    TwoWayI (f x1 y1) (f x2 y2) (f x3 y3)

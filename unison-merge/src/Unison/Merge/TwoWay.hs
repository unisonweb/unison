module Unison.Merge.TwoWay
  ( TwoWay (..),
    swap,
    unzipMap,
  )
where

import Data.Semialign (Semialign, alignWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These (These))
import Data.Zip (Zip, unzipWith, zipWith)
import Unison.Prelude
import Prelude hiding (zipWith)

data TwoWay a = TwoWay
  { alice :: a,
    bob :: a
  }
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving (Monoid, Semigroup) via (GenericSemigroupMonoid (TwoWay a))

instance Applicative TwoWay where
  pure x = TwoWay x x
  TwoWay f g <*> TwoWay x y = TwoWay (f x) (g y)

instance Semialign TwoWay where
  alignWith :: (These a b -> c) -> TwoWay a -> TwoWay b -> TwoWay c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Zip TwoWay where
  zipWith :: (a -> b -> c) -> TwoWay a -> TwoWay b -> TwoWay c
  zipWith f (TwoWay x1 x2) (TwoWay y1 y2) =
    TwoWay (f x1 y1) (f x2 y2)

-- | Swap who's considered Alice and who's considered Bob. Usually nonsense, but sometimes what you need!
swap :: TwoWay a -> TwoWay a
swap (TwoWay x y) =
  TwoWay y x

-- | Unzip a @Map k (TwoWay v)@ into a @TwoWay (Map k v)@.
unzipMap :: Ord k => Map k (TwoWay v) -> TwoWay (Map k v)
unzipMap =
  fromPair . unzipWith (\TwoWay {alice, bob} -> (alice, bob))

fromPair :: (a, a) -> TwoWay a
fromPair (alice, bob) =
  TwoWay {alice, bob}

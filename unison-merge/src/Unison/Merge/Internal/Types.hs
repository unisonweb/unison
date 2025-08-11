-- | Internal types module to house types that would require mutual recursion at the module level if defined separately
module Unison.Merge.Internal.Types
  ( ThreeWay (..),
    TwoOrThreeWay (..),
    TwoWay (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Semialign (Semialign, Unzip, Zip, alignWith, unzipWith, zipWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These (..))
import Unison.Prelude
import Prelude hiding (zipWith)

data ThreeWay a = ThreeWay
  { lca :: !a,
    alice :: !a,
    bob :: !a
  }
  deriving stock (Foldable, Functor, Generic, Traversable)

instance Applicative ThreeWay where
  pure :: a -> ThreeWay a
  pure x =
    ThreeWay x x x

  (<*>) :: ThreeWay (a -> b) -> ThreeWay a -> ThreeWay b
  ThreeWay f g h <*> ThreeWay x y z =
    ThreeWay (f x) (g y) (h z)

instance Semialign ThreeWay where
  alignWith :: (These a b -> c) -> ThreeWay a -> ThreeWay b -> ThreeWay c
  alignWith f (ThreeWay a b c) (ThreeWay x y z) =
    ThreeWay (f (These a x)) (f (These b y)) (f (These c z))

instance Unzip ThreeWay where
  unzipWith :: (c -> (a, b)) -> ThreeWay c -> (ThreeWay a, ThreeWay b)
  unzipWith f (ThreeWay a b c) =
    let (i, x) = f a
        (j, y) = f b
        (k, z) = f c
     in (ThreeWay i j k, ThreeWay x y z)

instance Zip ThreeWay where
  zipWith :: (a -> b -> c) -> ThreeWay a -> ThreeWay b -> ThreeWay c
  zipWith f (ThreeWay a b c) (ThreeWay x y z) =
    ThreeWay (f a x) (f b y) (f c z)

data TwoOrThreeWay a = TwoOrThreeWay
  { lca :: Maybe a,
    alice :: a,
    bob :: a
  }
  deriving stock (Foldable, Functor, Generic, Traversable)

instance Applicative TwoOrThreeWay where
  pure :: a -> TwoOrThreeWay a
  pure x =
    TwoOrThreeWay (Just x) x x

  (<*>) :: TwoOrThreeWay (a -> b) -> TwoOrThreeWay a -> TwoOrThreeWay b
  TwoOrThreeWay f g h <*> TwoOrThreeWay x y z =
    TwoOrThreeWay (f <*> x) (g y) (h z)

data TwoWay a = TwoWay
  { alice :: a,
    bob :: a
  }
  deriving stock (Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (NFData)
  deriving (Monoid, Semigroup) via (GenericSemigroupMonoid (TwoWay a))

instance Applicative TwoWay where
  pure x = TwoWay x x
  TwoWay f g <*> TwoWay x y = TwoWay (f x) (g y)

instance Semialign TwoWay where
  alignWith :: (These a b -> c) -> TwoWay a -> TwoWay b -> TwoWay c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Unzip TwoWay where
  unzipWith :: (c -> (a, b)) -> TwoWay c -> (TwoWay a, TwoWay b)
  unzipWith f (TwoWay cx cy) =
    let (ax, bx) = f cx
        (ay, by) = f cy
     in (TwoWay ax ay, TwoWay bx by)

instance Zip TwoWay where
  zipWith :: (a -> b -> c) -> TwoWay a -> TwoWay b -> TwoWay c
  zipWith f (TwoWay x1 x2) (TwoWay y1 y2) =
    TwoWay (f x1 y1) (f x2 y2)

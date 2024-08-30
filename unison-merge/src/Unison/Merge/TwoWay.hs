module Unison.Merge.TwoWay
  ( TwoWay (..),
    bothWays,
    justTheTerms,
    justTheTypes,
    or,
    sequenceDefns,
    swap,
    twoWay,
    unzipMap,
    who_,
  )
where

import Control.Lens (Lens')
import Data.Semialign (Semialign, alignWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These (These))
import Data.Zip (Unzip, Zip, unzipWith, zipWith)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Prelude
import Unison.Util.Defns (Defns (..), DefnsF)
import Prelude hiding (or, zipWith)

data TwoWay a = TwoWay
  { alice :: a,
    bob :: a
  }
  deriving stock (Foldable, Functor, Generic, Show, Traversable)
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

bothWays :: a -> TwoWay a
bothWays x =
  TwoWay x x

justTheTerms :: TwoWay (Defns terms types) -> TwoWay terms
justTheTerms =
  fmap (view #terms)

justTheTypes :: TwoWay (Defns terms types) -> TwoWay types
justTheTypes =
  fmap (view #types)

or :: TwoWay Bool -> Bool
or =
  twoWay (||)

sequenceDefns :: TwoWay (Defns terms types) -> DefnsF TwoWay terms types
sequenceDefns defns =
  Defns (justTheTerms defns) (justTheTypes defns)

-- | Swap who's considered Alice and who's considered Bob. Usually nonsense, but sometimes what you need!
swap :: TwoWay a -> TwoWay a
swap (TwoWay x y) =
  TwoWay y x

twoWay :: (a -> a -> b) -> TwoWay a -> b
twoWay f TwoWay {alice, bob} =
  f alice bob

-- | Unzip a @Map k (TwoWay v)@ into a @TwoWay (Map k v)@.
unzipMap :: (Ord k) => Map k (TwoWay v) -> TwoWay (Map k v)
unzipMap =
  fromPair . unzipWith (\TwoWay {alice, bob} -> (alice, bob))

who_ :: EitherWay x -> Lens' (TwoWay a) a
who_ = \case
  Alice _ -> #alice
  Bob _ -> #bob

--

fromPair :: (a, a) -> TwoWay a
fromPair (alice, bob) =
  TwoWay {alice, bob}

module Unison.Merge.TwoWay
  ( TwoWay (..),
    bothWays,
    gtoThreeWay,
    justTheTerms,
    justTheTypes,
    or,
    sequenceDefns,
    swap,
    toThreeWay,
    toTwoOrThreeWay,
    twoWay,
    unzipMap,
    updatedToThreeWay,
    who_,
  )
where

import Control.Lens (Lens')
import Data.Zip (unzipWith)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.Internal.Types (GThreeWay (..), ThreeWay (..), TwoOrThreeWay (..), TwoWay (..))
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Prelude
import Unison.Util.Defns (Defns (..), DefnsF)
import Prelude hiding (or, zipWith)

bothWays :: a -> TwoWay a
bothWays x =
  TwoWay x x

gtoThreeWay :: a -> TwoWay b -> GThreeWay a b
gtoThreeWay lca TwoWay {alice, bob} =
  GThreeWay {lca, alice, bob}

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

toThreeWay :: a -> TwoWay a -> ThreeWay a
toThreeWay lca TwoWay {alice, bob} =
  ThreeWay {lca, alice, bob}

toTwoOrThreeWay :: Maybe a -> TwoWay a -> TwoOrThreeWay a
toTwoOrThreeWay lca TwoWay {alice, bob} =
  TwoOrThreeWay {lca, alice, bob}

twoWay :: (a -> a -> b) -> TwoWay a -> b
twoWay f TwoWay {alice, bob} =
  f alice bob

-- | Unzip a @Map k (TwoWay v)@ into a @TwoWay (Map k v)@.
unzipMap :: (Ord k) => Map k (TwoWay v) -> TwoWay (Map k v)
unzipMap =
  fromPair . unzipWith (\TwoWay {alice, bob} -> (alice, bob))

updatedToThreeWay :: (Semigroup a) => TwoWay (Updated a) -> ThreeWay a
updatedToThreeWay TwoWay {alice, bob} =
  ThreeWay
    { lca = alice.old <> bob.old,
      alice = alice.new,
      bob = bob.new
    }

who_ :: EitherWay x -> Lens' (TwoWay a) a
who_ = \case
  Alice _ -> #alice
  Bob _ -> #bob

--

fromPair :: (a, a) -> TwoWay a
fromPair (alice, bob) =
  TwoWay {alice, bob}

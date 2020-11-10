module U.Util.Set where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Traversable as T

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap Set.fromList . T.traverse f . Set.toList

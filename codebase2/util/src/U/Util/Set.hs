{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Util.Set where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Maybe as Maybe

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap Set.fromList . T.traverse f . Set.toList

mapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList

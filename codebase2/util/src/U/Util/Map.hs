{- ORMOLU_DISABLE -}
module U.Util.Map where

import qualified Data.Bifunctor as B
import qualified Data.Bitraversable as B
import Data.Map (Map)
import qualified Data.Map as Map

bimap :: Ord a' => (a -> a') -> (b -> b') -> Map a b -> Map a' b'
bimap fa fb = Map.fromList . map (B.bimap fa fb) . Map.toList

bitraverse :: (Applicative f, Ord a') => (a -> f a') -> (b -> f b') -> Map a b -> f (Map a' b')
bitraverse fa fb = fmap Map.fromList . traverse (B.bitraverse fa fb) . Map.toList

-- | @NEMap@ utilities.
module Unison.Util.NEMap
  ( fromSetA,
    fromSetM,
  )
where

import Data.Map.NonEmpty.Internal (NEMap (NEMap))
import Data.Set.NonEmpty.Internal (NESet (NESet))
import Unison.Util.Map qualified as Map

fromSetA :: Applicative f => (k -> f a) -> NESet k -> f (NEMap k a)
fromSetA f (NESet k ks) = NEMap k <$> f k <*> Map.fromSetA f ks

fromSetM :: Monad f => (k -> f a) -> NESet k -> f (NEMap k a)
fromSetM f (NESet k ks) = NEMap k <$> f k <*> Map.fromSetM f ks

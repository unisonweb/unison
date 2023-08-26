-- | @NEMap@ utilities.
module Unison.Util.NEMap
  ( fromSetM,
  )
where

import Data.Map.NonEmpty.Internal (NEMap (NEMap))
import Data.Set.NonEmpty.Internal (NESet (NESet))
import Unison.Util.Map qualified as Map

fromSetM :: Monad f => (k -> f a) -> NESet k -> f (NEMap k a)
fromSetM f (NESet k ks) = NEMap k <$> f k <*> Map.fromSetM f ks

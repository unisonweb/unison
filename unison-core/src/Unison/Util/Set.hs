module Unison.Util.Set where

import Data.Set

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference a b = (a `difference` b) `union` (b `difference` a)
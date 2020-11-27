{-# LANGUAGE ViewPatterns #-}

module Unison.Util.Set where

import Data.Set

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference a b = (a `difference` b) `union` (b `difference` a)

mapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybe f s = fromList [r | (f -> Just r) <- toList s]

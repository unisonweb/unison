{-# LANGUAGE DeriveFoldable #-}

module Unison.Codebase.Conflicted (Conflicted, map, one, asOne, conflicted, delete, singletonMap) where

import           Data.Foldable
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Prelude         hiding (map)
import           Unison.Hashable (Hashable)
import qualified Unison.Hashable as H

data Conflicted a = Conflicted (Set a) deriving (Eq, Foldable)

instance Ord a => Semigroup (Conflicted a) where
  Conflicted as <> Conflicted as2 = Conflicted (as `Set.union` as2)

map :: Ord b => (a -> b) -> Conflicted a -> Conflicted b
map f (Conflicted a) = Conflicted (Set.map f a)

delete :: Ord a => a -> Conflicted a -> Maybe (Conflicted a)
delete a (Conflicted as) = conflicted (Set.delete a as)

one :: a -> Conflicted a
one = Conflicted . Set.singleton

asOne :: Conflicted a -> Maybe a
asOne (Conflicted as)
  | Set.size as /= 1 = Nothing
  | otherwise        = Just $ Set.findMin as

conflicted :: Set a -> Maybe (Conflicted a)
conflicted set =
  if Set.null set then Nothing else Just (Conflicted set)

singletonMap :: Ord k => k -> v -> Map k (Conflicted v)
singletonMap k v = Map.singleton k (one v)


instance Hashable a => Hashable (Conflicted a) where
  tokens = H.tokens . toList

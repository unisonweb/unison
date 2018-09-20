{-# LANGUAGE DeriveFoldable #-}

module Unison.Codebase.Conflicted where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable
import Unison.Hashable

data Conflicted a = One a | Many (Set a)
  deriving (Eq, Foldable)

instance Ord a => Semigroup (Conflicted a) where
  One a <> One a2 = if a == a2 then One a else Many (Set.fromList [a,a2])
  One a <> Many as = Many (Set.insert a as)
  Many as <> One a = Many (Set.insert a as)
  Many as <> Many as2 = Many (as `Set.union` as2)

conflicted :: Set a -> Conflicted a
conflicted set
  | Set.size set == 1 = One (Set.findMin set)
  | otherwise         = Many set

instance Hashable a => Hashable (Conflicted a) where
  tokens = tokens . toList
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.SlurpComponent where

import Data.Set (Set)
import qualified Data.Set as Set

data SlurpComponent v =
  SlurpComponent { types :: Set v, terms :: Set v }
  deriving (Eq,Ord,Show)

difference :: Ord v => SlurpComponent v -> SlurpComponent v -> SlurpComponent v
difference c1 c2 = SlurpComponent types' terms' where
  types' = types c1 `Set.difference` types c2
  terms' = terms c1 `Set.difference` terms c2

intersection :: Ord v => SlurpComponent v -> SlurpComponent v -> SlurpComponent v
intersection c1 c2 = SlurpComponent types' terms' where
  types' = types c1 `Set.intersection` types c2
  terms' = terms c1 `Set.intersection` terms c2

instance Ord v => Semigroup (SlurpComponent v) where (<>) = mappend
instance Ord v => Monoid (SlurpComponent v) where
  mempty = SlurpComponent mempty mempty
  c1 `mappend` c2 = SlurpComponent (types c1 <> types c2)
                                   (terms c1 <> terms c2)

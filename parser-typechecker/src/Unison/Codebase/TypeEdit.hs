module Unison.Codebase.TypeEdit where

import Unison.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

toReference :: TypeEdit -> Maybe Reference
toReference (Replace r) = Just r
toReference Deprecate = Nothing

module Unison.Codebase.V1.Patch.TypeEdit where

import Unison.Codebase.V1.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

references :: TypeEdit -> [Reference]
references (Replace r) = [r]
references Deprecate = []

toReference :: TypeEdit -> Maybe Reference
toReference (Replace r) = Just r
toReference Deprecate     = Nothing

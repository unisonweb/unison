module Unison.Codebase.TypeEdit where

import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H
import Unison.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

references :: TypeEdit -> [Reference]
references (Replace r) = [r]
references Deprecate = []

instance Hashable TypeEdit where
  tokens (Replace r) = H.Tag 0 : H.tokens r
  tokens Deprecate = [H.Tag 1]

toReference :: TypeEdit -> Maybe Reference
toReference (Replace r) = Just r
toReference Deprecate = Nothing

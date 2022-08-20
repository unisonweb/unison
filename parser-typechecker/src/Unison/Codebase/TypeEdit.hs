module Unison.Codebase.TypeEdit where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Unison.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

references :: TypeEdit -> [Reference]
references (Replace r) = [r]
references Deprecate = []

toReference :: TypeEdit -> Maybe Reference
toReference (Replace r) = Just r
toReference Deprecate = Nothing

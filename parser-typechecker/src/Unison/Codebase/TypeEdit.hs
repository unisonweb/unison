module Unison.Codebase.TypeEdit where

import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H

data TypeEdit r = Replace r | Deprecate
  deriving (Eq, Ord, Show)

references :: TypeEdit r -> [r]
references (Replace r) = [r]
references Deprecate = []

instance Hashable r => Hashable (TypeEdit r) where
  tokens (Replace r) = H.Tag 0 : H.tokens r
  tokens Deprecate   = [H.Tag 1]

toReference :: TypeEdit r -> Maybe r
toReference (Replace r) = Just r
toReference Deprecate     = Nothing

module Unison.Hashing.V2.TypeEdit (TypeEdit (..)) where

import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Hashing.V2.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

instance Tokenizable TypeEdit where
  tokens (Replace r) = H.Tag 0 : H.tokens r
  tokens Deprecate = [H.Tag 1]

module Unison.Hashing.V2.TypeEdit (TypeEdit (..)) where

import Unison.Hashing.V2.BuildHashable (Tokenizable)
import qualified Unison.Hashing.V2.BuildHashable as H
import Unison.Hashing.V2.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

instance Tokenizable TypeEdit where
  tokens (Replace r) = H.Tag 0 : H.tokens r
  tokens Deprecate = [H.Tag 1]

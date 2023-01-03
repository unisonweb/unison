module Unison.Hashing.V2.TypeEdit (TypeEdit (..)) where

import Unison.Hashing.V2.Reference (Reference)
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as H

data TypeEdit
  = TypeEditReplace Reference
  | TypeEditDeprecate
  deriving (Eq, Ord, Show)

instance Tokenizable TypeEdit where
  tokens (TypeEditReplace r) = H.Tag 0 : H.tokens r
  tokens TypeEditDeprecate = [H.Tag 1]

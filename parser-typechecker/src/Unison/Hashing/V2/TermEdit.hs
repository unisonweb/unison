module Unison.Hashing.V2.TermEdit (TermEdit (..)) where

import Unison.Hashing.V2.Referent (Referent)
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import qualified Unison.Hashing.V2.Tokenizable as H

data TermEdit = Replace Referent | Deprecate
  deriving (Eq, Ord, Show)

instance Tokenizable TermEdit where
  tokens (Replace r) = [H.Tag 0] ++ H.tokens r
  tokens Deprecate = [H.Tag 1]

module Unison.Codebase.Patch (Patch (..)) where

import Unison.Codebase.TermEdit (TermEdit)
import Unison.Codebase.TypeEdit (TypeEdit)
import Unison.Reference (Reference)
import Unison.Util.Relation (Relation)
import Prelude hiding (head, read, subtract)

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit,
    _typeEdits :: Relation Reference TypeEdit
  }
  deriving (Eq, Ord, Show)

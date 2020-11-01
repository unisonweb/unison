module U.Codebase.Sqlite.Patch.Diff where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Sqlite.Patch.TermEdit (TermEdit)
import U.Codebase.Sqlite.Patch.TypeEdit (TypeEdit)
import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Sqlite.Referent (Referent)

data PatchDiff = PatchDiff
  { addedTermEdits :: Map Referent (Set TermEdit),
    addedTypeEdits :: Map Reference (Set TypeEdit),
    removedTermEdits :: Map Referent (Set TermEdit),
    removedTypeEdits :: Map Reference (Set TypeEdit)
  }
  deriving (Eq, Ord, Show)

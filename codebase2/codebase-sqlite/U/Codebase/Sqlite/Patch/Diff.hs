module U.Codebase.Sqlite.Patch.Diff where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Sqlite.DbId (PatchObjectId)
import U.Codebase.Sqlite.Patch.TermEdit (TermEdit)
import U.Codebase.Sqlite.Patch.TypeEdit (TypeEdit)
import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Sqlite.Referent (Referent)

data PatchDiff = PatchDiff
  { reference :: PatchObjectId,
    addedTermEdits :: Map Referent TermEdit,
    addedTypeEdits :: Map Reference TypeEdit,
    removedTermEdits :: Set Referent,
    removedTypeEdits :: Set Reference
  }
  deriving (Eq, Ord, Show)

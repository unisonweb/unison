module U.Codebase.Sqlite.Patch.Diff where

import Data.Map (Map)
import U.Codebase.Sqlite.Referent
import U.Codebase.Sqlite.Reference
import U.Codebase.Sqlite.Patch.TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit
import U.Codebase.Sqlite.DbId
import Data.Set (Set)

data PatchDiff = PatchDiff
  { reference :: PatchId
  , addedTermEdits :: Map Referent TermEdit
  , addedTypeEdits :: Map Reference TypeEdit
  , removedTermEdits :: Set Referent
  , removedTypeEdits :: Set Reference
  } deriving (Eq, Ord, Show)

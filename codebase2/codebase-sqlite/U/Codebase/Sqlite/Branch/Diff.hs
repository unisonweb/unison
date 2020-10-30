module U.Codebase.Sqlite.Branch.Diff where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Sqlite.DbId (BranchObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.Patch.Diff (PatchDiff)
import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Sqlite.Referent (Referent)

type NameSegment = TextId

type Metadata = Reference

data PatchOp = PatchRemove | PatchAdd PatchObjectId | PatchEdit PatchDiff

data AddRemove a = AddRemove {add :: Set a, remove :: Set a}

data Diff = Diff
  { reference :: BranchObjectId,
    terms :: Map NameSegment (AddRemove Referent),
    types :: Map NameSegment (AddRemove Reference),
    termMetadata :: Map NameSegment (Map Referent (AddRemove Metadata)),
    typeMetadata :: Map NameSegment (Map Reference (AddRemove Metadata)),
    patches :: Map NameSegment PatchOp
  }

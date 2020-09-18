module U.Codebase.Sqlite.Branch.Diff where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Reference
import U.Codebase.Sqlite.Referent
import U.Codebase.Sqlite.Patch.Diff

type NameSegment = TextId
type Metadata = Reference
data PatchOp = PatchRemove | PatchAdd | PatchEdit PatchDiff
data AddRemove a = AddRemove { add :: Set a, remove :: Set a }

data Diff = Diff
  { reference :: BranchId,
    terms :: Map NameSegment (AddRemove Referent),
    types :: Map NameSegment (AddRemove Reference),
    termMetadata :: Map NameSegment (Map Referent (AddRemove Metadata)),
    typeMetadata :: Map NameSegment (Map Reference (AddRemove Metadata)),
    patches :: Map NameSegment PatchOp
  }
  

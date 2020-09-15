module U.Codebase.Sqlite.Branch.Full where

import Data.Map (Map)
import U.Codebase.Sqlite.Referent
import U.Codebase.Sqlite.Reference
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Branch.MetadataSet

type NameSegment = TextId

data Branch = Branch
  { terms :: Map NameSegment (Map Referent MetadataSetFormat),
    types :: Map NameSegment (Map Reference MetadataSetFormat),
    patches :: Map NameSegment PatchId,
    children :: Map NameSegment BranchId
  }

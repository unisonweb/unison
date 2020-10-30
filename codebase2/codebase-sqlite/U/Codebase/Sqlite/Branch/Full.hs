module U.Codebase.Sqlite.Branch.Full where

import Data.Map (Map)
import U.Codebase.Sqlite.Branch.MetadataSet (MetadataSetFormat)
import U.Codebase.Sqlite.DbId (BranchObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Sqlite.Referent (Referent)

type NameSegment = TextId

data Branch = Branch
  { terms :: Map NameSegment (Map Referent MetadataSetFormat),
    types :: Map NameSegment (Map Reference MetadataSetFormat),
    patches :: Map NameSegment PatchObjectId,
    children :: Map NameSegment BranchObjectId
  }

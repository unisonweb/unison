module U.Codebase.Sqlite.Branch.Diff where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Sqlite.Branch.MetadataSet
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Reference
import U.Codebase.Sqlite.Referent

data Diff = Diff
  { reference :: BranchId,
    add :: DiffSlice,
    remove :: DiffSlice
  }

type NameSegment = TextId

data DiffSlice = DiffSlice
  { terms :: Map NameSegment (Set Referent),
    types :: Map NameSegment (Set Reference),
    termMetadata :: Map NameSegment (Map Referent MetadataSetFormat),
    typeMetadata :: Map NameSegment (Map Reference MetadataSetFormat)
  }

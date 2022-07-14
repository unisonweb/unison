module Unison.Codebase.SqliteCodebase.SyncEphemeral where

import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.DbId (SchemaVersion)
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import Unison.Hash (Hash)
import Unison.Prelude

data Dependencies = Dependencies
  { definitions :: Set Hash,
    branches :: Set Hash
  }

data Error
  = Sync22Error Sync22.Error
  | SrcWrongSchema SchemaVersion
  | DestWrongSchema SchemaVersion
  | DisappearingBranch CausalHash
  deriving stock (Show)
  deriving anyclass (Exception)

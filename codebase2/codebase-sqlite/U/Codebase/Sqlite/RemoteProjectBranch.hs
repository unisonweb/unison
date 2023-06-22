module U.Codebase.Sqlite.RemoteProjectBranch
  ( RemoteProjectBranch (..),
  )
where

import Network.URI (URI)
import Network.URI.Orphans.Sqlite ()
import U.Codebase.Sqlite.DbId (RemoteProjectBranchId, RemoteProjectId)
import Unison.Core.Orphans.Sqlite ()
import Unison.Core.Project (ProjectBranchName)
import Unison.Prelude
import Unison.Sqlite (FromRow, ToRow)

data RemoteProjectBranch = RemoteProjectBranch
  { projectId :: RemoteProjectId,
    branchId :: RemoteProjectBranchId,
    host :: URI,
    name :: ProjectBranchName
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)

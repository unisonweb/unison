module U.Codebase.Sqlite.RemoteProject
  ( RemoteProject (..),
  )
where

import Network.URI (URI)
import Network.URI.Orphans.Sqlite ()
import U.Codebase.Sqlite.DbId (RemoteProjectId)
import Unison.Core.Orphans.Sqlite ()
import Unison.Core.Project (ProjectName)
import Unison.Prelude
import Unison.Sqlite (FromRow, ToRow)

-- | A remote project.
data RemoteProject = RemoteProject
  { projectId :: RemoteProjectId,
    host :: URI,
    name :: ProjectName
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)

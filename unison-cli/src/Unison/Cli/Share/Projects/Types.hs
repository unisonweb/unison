-- | Types of "Unison.Cli.Share.Projects", put in their own module to avoid cyclic dependencies.
--
-- Refer to that module's documentation, and use it instead if you can. It re-exports these types.
module Unison.Cli.Share.Projects.Types
  ( RemoteProject (..),
    RemoteProjectBranch (..),
  )
where

import U.Codebase.Sqlite.DbId (RemoteProjectBranchId (..), RemoteProjectId (..))
import Unison.Prelude
import Unison.Project (ProjectBranchName, ProjectName)
import qualified Unison.Share.API.Hash as Share.API

-- | A remote project.
data RemoteProject = RemoteProject
  { projectId :: RemoteProjectId,
    projectName :: ProjectName
  }
  deriving stock (Eq, Generic, Show)

-- | A remote project branch.
data RemoteProjectBranch = RemoteProjectBranch
  { projectId :: RemoteProjectId,
    projectName :: ProjectName,
    branchId :: RemoteProjectBranchId,
    branchName :: ProjectBranchName,
    branchHead :: Share.API.HashJWT
  }
  deriving stock (Eq, Show, Generic)

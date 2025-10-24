module U.Codebase.Sqlite.ProjectBranch
  ( ProjectBranch (..),
    ProjectBranchRow (..),
  )
where

import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import Unison.Core.Orphans.Sqlite ()
import Unison.Core.Project (ProjectBranchName)
import Unison.Prelude
import Unison.Sqlite (FromRow, ToRow)

-- | A project branch.
data ProjectBranch = ProjectBranch
  { projectId :: !ProjectId,
    branchId :: !ProjectBranchId,
    name :: !ProjectBranchName,
    parentBranchId :: !(Maybe ProjectBranchId),
    isMerge :: !Bool,
    isUpdate :: !Bool,
    isUpgrade :: !Bool
  }
  deriving stock (Eq, Generic, Show)

data ProjectBranchRow = ProjectBranchRow
  { projectId :: !ProjectId,
    branchId :: !ProjectBranchId,
    name :: !ProjectBranchName,
    parentBranchId :: !(Maybe ProjectBranchId)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

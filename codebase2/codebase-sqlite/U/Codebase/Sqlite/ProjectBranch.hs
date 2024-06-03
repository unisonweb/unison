module U.Codebase.Sqlite.ProjectBranch
  ( ProjectBranch (..),
  )
where

import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId, ProjectId)
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
    causalHashId :: !CausalHashId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToRow, FromRow)

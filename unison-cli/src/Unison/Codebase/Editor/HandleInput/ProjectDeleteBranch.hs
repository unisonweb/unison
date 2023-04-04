-- | @project.delete-branch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectDeleteBranch
  ( handleProjectDeleteBranch,
  )
where

import Control.Lens ((^.))
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)

-- | Delete a project branch in the current project.
--
-- Currently, deleting a branch means deleting its `project_branch` row, then deleting its contents from the namespace.
-- Its children branches, if any, are reparented to their grandparent, if any.
--
-- If the user is on the branch that they're deleting, we have to cd somewhere; try these in order:
--
--   1. cd to parent branch, if it exists
--   2. cd to "main", if it exists
--   3. cd to loose code path `.`
handleProjectDeleteBranch :: ProjectBranchName -> Cli ()
handleProjectDeleteBranch branchName = do
  ProjectAndBranch currentProject currentBranch <- ProjectUtils.expectCurrentProjectBranch
  pure ()

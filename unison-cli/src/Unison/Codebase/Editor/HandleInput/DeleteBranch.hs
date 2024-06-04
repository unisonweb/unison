-- | @delete.branch@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteBranch
  ( handleDeleteBranch,
    doDeleteProjectBranch,
  )
where

import Data.Map.Strict qualified as Map
import Data.These (These (..))
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Witch (unsafeFrom)

-- | Delete a project branch.
--
-- Currently, deleting a branch means deleting its `project_branch` row, then deleting its contents from the namespace.
-- Its children branches, if any, are reparented to their grandparent, if any. You may delete the only branch in a
-- project.
handleDeleteBranch :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleDeleteBranch projectAndBranchNamesToDelete = do
  projectAndBranchToDelete <-
    ProjectUtils.expectProjectAndBranchByTheseNames
      case projectAndBranchNamesToDelete of
        ProjectAndBranch Nothing branch -> That branch
        ProjectAndBranch (Just project) branch -> These project branch

  maybeCurrentBranch <- ProjectUtils.getCurrentProjectBranch

  doDeleteProjectBranch projectAndBranchToDelete

  -- If the user is on the branch that they're deleting, we have to cd somewhere; try these in order:
  --
  --   1. cd to parent branch, if it exists
  --   2. cd to "main", if it exists
  --   3. cd to loose code path `.`
  whenJust maybeCurrentBranch \(currentProjectAndBranch, _restPath) ->
    when (ProjectUtils.justTheIds currentProjectAndBranch == ProjectUtils.justTheIds projectAndBranchToDelete) do
      newPath <-
        case projectAndBranchToDelete.branch.parentBranchId of
          Nothing ->
            let loadMain =
                  Queries.loadProjectBranchByName projectAndBranchToDelete.project.projectId (unsafeFrom @Text "main")
             in Cli.runTransaction loadMain <&> \case
                  Nothing -> Path.Absolute Path.empty
                  Just mainBranch -> ProjectUtils.projectBranchPath (ProjectUtils.justTheIds' mainBranch)
          Just parentBranchId ->
            pure $
              ProjectUtils.projectBranchPath
                (ProjectAndBranch projectAndBranchToDelete.project.projectId parentBranchId)
      Cli.cd newPath

-- | Delete a project branch and record an entry in the reflog.
doDeleteProjectBranch :: ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Cli ()
doDeleteProjectBranch projectAndBranch = do
  Cli.runTransaction do
    Queries.deleteProjectBranch projectAndBranch.project.projectId projectAndBranch.branch.branchId
  Cli.stepAt
    ("delete.branch " <> into @Text (ProjectUtils.justTheNames projectAndBranch))
    ( Path.unabsolute (ProjectUtils.projectBranchesPath projectAndBranch.project.projectId),
      over Branch.children (Map.delete (ProjectUtils.projectBranchSegment projectAndBranch.branch.branchId))
    )

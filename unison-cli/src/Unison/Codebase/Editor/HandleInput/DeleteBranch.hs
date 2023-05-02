-- | @delete.branch@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteBranch
  ( handleDeleteBranch,
  )
where

import Control.Lens (over, (^.))
import qualified Data.Map.Strict as Map
import Data.These (These (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Witch (unsafeFrom)

-- | Delete a project branch.
--
-- Currently, deleting a branch means deleting its `project_branch` row, then deleting its contents from the namespace.
-- Its children branches, if any, are reparented to their grandparent, if any. You may delete the only branch in a
-- project.
handleDeleteBranch :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleDeleteBranch projectAndBranchNames0 = do
  projectAndBranchNames <-
    ProjectUtils.hydrateNames
      case projectAndBranchNames0 of
        ProjectAndBranch Nothing branch -> That branch
        ProjectAndBranch (Just project) branch -> These project branch

  maybeCurrentBranch <- ProjectUtils.getCurrentProjectBranch

  deletedBranch <-
    Cli.runEitherTransaction do
      Queries.loadProjectBranchByNames (projectAndBranchNames ^. #project) (projectAndBranchNames ^. #branch) >>= \case
        Nothing -> pure (Left (Output.LocalProjectBranchDoesntExist projectAndBranchNames))
        Just branch -> do
          Queries.deleteProjectBranch (branch ^. #projectId) (branch ^. #branchId)
          pure (Right branch)

  let projectId = deletedBranch ^. #projectId

  Cli.stepAt
    "project.delete-branch"
    ( Path.unabsolute (ProjectUtils.projectBranchesPath projectId),
      \branchObject ->
        branchObject
          & over
            Branch.children
            (Map.delete (ProjectUtils.projectBranchSegment (deletedBranch ^. #branchId)))
    )

  -- If the user is on the branch that they're deleting, we have to cd somewhere; try these in order:
  --
  --   1. cd to parent branch, if it exists
  --   2. cd to "main", if it exists
  --   3. cd to loose code path `.`
  whenJust maybeCurrentBranch \(ProjectAndBranch _currentProject currentBranch) ->
    when (deletedBranch == currentBranch) do
      newPath <-
        case deletedBranch ^. #parentBranchId of
          Nothing ->
            Cli.runTransaction (Queries.loadProjectBranchByName projectId (unsafeFrom @Text "main")) <&> \case
              Nothing -> Path.Absolute Path.empty
              Just mainBranch -> ProjectUtils.projectBranchPath (ProjectAndBranch projectId (mainBranch ^. #branchId))
          Just parentBranchId -> pure (ProjectUtils.projectBranchPath (ProjectAndBranch projectId parentBranchId))
      Cli.cd newPath

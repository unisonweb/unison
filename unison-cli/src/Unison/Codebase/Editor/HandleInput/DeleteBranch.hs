-- | @delete.branch@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteBranch
  ( handleDeleteBranch,
    handleDeleteBranch2,
    doDeleteProjectBranch,
  )
where

import Control.Lens
import Data.List qualified as List
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), defaultBranchName)
import Unison.Sqlite qualified as Sqlite

-- | Delete a project branch.
--
-- Currently, deleting a branch means deleting its `project_branch` row, then deleting its contents from the namespace.
-- Its children branches, if any, are reparented to their grandparent, if any. You may delete the only branch in a
-- project.
handleDeleteBranch :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleDeleteBranch namesToDelete = do
  current <- Cli.getCurrentProjectPath
  toDelete <- ProjectUtils.resolveProjectBranchInProject current.project (namesToDelete & #branch %~ Just)
  handleDeleteBranch2 toDelete

-- | Like 'handleDeleteBranch2', but for when the branch name to delete is already resolved to a branch.
handleDeleteBranch2 :: ProjectAndBranch Project ProjectBranch -> Cli ()
handleDeleteBranch2 toDelete = do
  current <- Cli.getCurrentProjectPath

  -- If the user is on the branch that they're deleting, we have to cd somewhere; try these in order:
  --
  --   1. cd to parent branch, if it exists
  --   2. cd to "main", if it exists
  --   3. Any other branch in the codebase
  --   4. Create a new branch in the current project
  when (toDelete.branch.branchId == current.branch.branchId) do
    nextLocation <-
      Cli.runTransaction do
        maybeNextLocation <-
          runMaybeT $
            asum
              [ parentBranch toDelete.branch.projectId toDelete.branch.parentBranchId,
                findMainBranchInProjectExcept current.project.projectId toDelete.branch.branchId,
                -- Any branch in the codebase except the one we're deleting
                findAnyBranchInProjectExcept toDelete.branch.projectId toDelete.branch.branchId,
                findAnyBranchInCodebaseExcept toDelete.branch.projectId toDelete.branch.branchId
              ]
        case maybeNextLocation of
          Just nextLocation -> pure nextLocation
          Nothing -> createNewBranchInProjectExcept toDelete.project.name toDelete.branch.name

    Cli.switchProject nextLocation

  doDeleteProjectBranch toDelete
  where
    parentBranch :: ProjectId -> Maybe ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    parentBranch projectId mayParentBranchId = do
      parentBranchId <- hoistMaybe mayParentBranchId
      pure (ProjectAndBranch projectId parentBranchId)

    findMainBranchInProjectExcept :: ProjectId -> ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findMainBranchInProjectExcept projectId exceptBranchId = do
      branch <- MaybeT $ Queries.loadProjectBranchByName projectId defaultBranchName
      guard (branch.branchId /= exceptBranchId)
      pure (ProjectAndBranch projectId branch.branchId)

    findAnyBranchInProjectExcept :: ProjectId -> ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findAnyBranchInProjectExcept projectId exceptBranchId = do
      (someBranchId, _) <- MaybeT . fmap (List.find (\(branchId, _) -> branchId /= exceptBranchId)) $ Queries.loadAllProjectBranchesBeginningWith projectId Nothing
      pure (ProjectAndBranch projectId someBranchId)

    findAnyBranchInCodebaseExcept :: ProjectId -> ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findAnyBranchInCodebaseExcept exceptProjectId exceptBranchId = do
      (_, pbIds) <- MaybeT . fmap (List.find (\(_, ids) -> ids /= ProjectAndBranch exceptProjectId exceptBranchId)) $ Queries.loadAllProjectBranchNamePairs
      pure pbIds

    createNewBranchInProjectExcept :: ProjectName -> ProjectBranchName -> Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    createNewBranchInProjectExcept projectName = \case
      UnsafeProjectBranchName "main" -> do
        (_, emptyCausalHashId) <- Codebase.emptyCausalHash
        Ops.insertProjectAndBranch projectName (UnsafeProjectBranchName "main2") emptyCausalHashId
          <&> \(proj, branch) -> ProjectAndBranch proj.projectId branch.branchId
      _ -> do
        (_, emptyCausalHashId) <- Codebase.emptyCausalHash
        Ops.insertProjectAndBranch projectName (UnsafeProjectBranchName "main") emptyCausalHashId
          <&> \(proj, branch) -> ProjectAndBranch proj.projectId branch.branchId

-- | Delete a project branch and record an entry in the reflog.
doDeleteProjectBranch :: (HasCallStack) => ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Cli ()
doDeleteProjectBranch projectAndBranch = do
  Cli.runTransaction do
    Queries.deleteProjectBranch projectAndBranch.project.projectId projectAndBranch.branch.branchId

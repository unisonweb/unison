-- | @delete.branch@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteBranch
  ( handleDeleteBranch,
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
import Unison.Codebase.Editor.HandleInput.ProjectCreate
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

-- | Delete a project branch.
--
-- Currently, deleting a branch means deleting its `project_branch` row, then deleting its contents from the namespace.
-- Its children branches, if any, are reparented to their grandparent, if any. You may delete the only branch in a
-- project.
handleDeleteBranch :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleDeleteBranch projectAndBranchNamesToDelete = do
  ProjectPath currentProject currentBranch _ <- Cli.getCurrentProjectPath
  projectAndBranchToDelete@(ProjectAndBranch projectOfBranchToDelete branchToDelete) <- ProjectUtils.resolveProjectBranchInProject currentProject (projectAndBranchNamesToDelete & #branch %~ Just)

  -- If the user is on the branch that they're deleting, we have to cd somewhere; try these in order:
  --
  --   1. cd to parent branch, if it exists
  --   2. cd to "main", if it exists
  --   3. Any other branch in the codebase
  --   4. Create a new branch in the current project
  when (branchToDelete ^. #branchId == currentBranch ^. #branchId) do
    mayNextLocation <-
      Cli.runTransaction . runMaybeT $
        asum
          [ parentBranch (branchToDelete ^. #projectId) (branchToDelete ^. #parentBranchId),
            findMainBranchInProjectExcept (currentProject ^. #projectId) (branchToDelete ^. #branchId),
            -- Any branch in the codebase except the one we're deleting
            findAnyBranchInProjectExcept (branchToDelete ^. #projectId) (branchToDelete ^. #branchId),
            findAnyBranchInCodebaseExcept (branchToDelete ^. #projectId) (branchToDelete ^. #branchId),
            createNewBranchInProjectExcept projectOfBranchToDelete.name branchToDelete.name
          ]

    nextLoc <- mayNextLocation `whenNothing` projectCreate False Nothing
    Cli.switchProject nextLoc
  doDeleteProjectBranch projectAndBranchToDelete
  where
    parentBranch :: ProjectId -> Maybe ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    parentBranch projectId mayParentBranchId = do
      parentBranchId <- hoistMaybe mayParentBranchId
      pure (ProjectAndBranch projectId parentBranchId)

    findMainBranchInProjectExcept :: ProjectId -> ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findMainBranchInProjectExcept projectId exceptBranchId = do
      branch <- MaybeT $ Queries.loadProjectBranchByName projectId (unsafeFrom @Text "main")
      guard (branch ^. #branchId /= exceptBranchId)
      pure (ProjectAndBranch projectId (branch ^. #branchId))

    findAnyBranchInProjectExcept :: ProjectId -> ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findAnyBranchInProjectExcept projectId exceptBranchId = do
      (someBranchId, _) <- MaybeT . fmap (List.find (\(branchId, _) -> branchId /= exceptBranchId)) $ Queries.loadAllProjectBranchesBeginningWith projectId Nothing
      pure (ProjectAndBranch projectId someBranchId)

    findAnyBranchInCodebaseExcept :: ProjectId -> ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findAnyBranchInCodebaseExcept exceptProjectId exceptBranchId = do
      (_, pbIds) <- MaybeT . fmap (List.find (\(_, ids) -> ids /= ProjectAndBranch exceptProjectId exceptBranchId)) $ Queries.loadAllProjectBranchNamePairs
      pure pbIds

    createNewBranchInProjectExcept :: ProjectName -> ProjectBranchName -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    createNewBranchInProjectExcept projectName (UnsafeProjectBranchName "main") = lift $ do
      (_, emptyCausalHashId) <- Codebase.emptyCausalHash
      Ops.insertProjectAndBranch projectName (UnsafeProjectBranchName "main2") emptyCausalHashId
        <&> \(proj, branch) -> ProjectAndBranch proj.projectId branch.branchId
    createNewBranchInProjectExcept projectName _ = lift $ do
      (_, emptyCausalHashId) <- Codebase.emptyCausalHash
      Ops.insertProjectAndBranch projectName (UnsafeProjectBranchName "main") emptyCausalHashId
        <&> \(proj, branch) -> ProjectAndBranch proj.projectId branch.branchId

-- | Delete a project branch and record an entry in the reflog.
doDeleteProjectBranch :: (HasCallStack) => ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Cli ()
doDeleteProjectBranch projectAndBranch = do
  Cli.runTransaction do
    Queries.deleteProjectBranch projectAndBranch.project.projectId projectAndBranch.branch.branchId

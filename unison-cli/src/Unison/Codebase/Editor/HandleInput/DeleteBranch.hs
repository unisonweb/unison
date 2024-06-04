-- | @delete.branch@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteBranch
  ( handleDeleteBranch,
    doDeleteProjectBranch,
  )
where

import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.ProjectCreate
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
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
  projectAndBranchToDelete@(ProjectAndBranch _projectToDelete branchToDelete) <- ProjectUtils.resolveProjectBranch currentProject (projectAndBranchNamesToDelete & #branch %~ Just)
  doDeleteProjectBranch projectAndBranchToDelete

  -- If the user is on the branch that they're deleting, we have to cd somewhere; try these in order:
  --
  --   1. cd to parent branch, if it exists
  --   2. cd to "main", if it exists
  --   3. Any other branch in the codebase
  --   4. Create a dummy project and go to /main
  when (branchToDelete ^. #branchId == currentBranch ^. #branchId) do
    mayNextLocation <-
      Cli.runTransaction . runMaybeT $
        asum
          [ parentBranch (branchToDelete ^. #projectId) (branchToDelete ^. #parentBranchId),
            findMainBranchInProject (currentProject ^. #projectId),
            findAnyBranchInProject (currentProject ^. #projectId),
            findAnyBranchInCodebase,
            createDummyProject
          ]
    nextLoc <- mayNextLocation `whenNothing` projectCreate False Nothing
    Cli.switchProject nextLoc
  where
    parentBranch :: ProjectId -> Maybe ProjectBranchId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    parentBranch projectId mayParentBranchId = do
      parentBranchId <- hoistMaybe mayParentBranchId
      pure (ProjectAndBranch projectId parentBranchId)
    findMainBranchInProject :: ProjectId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findMainBranchInProject projectId = do
      branch <- MaybeT $ Queries.loadProjectBranchByName projectId (unsafeFrom @Text "main")
      pure (ProjectAndBranch projectId (branch ^. #branchId))

    findAnyBranchInProject :: ProjectId -> MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findAnyBranchInProject projectId = do
      (someBranchId, _) <- MaybeT . fmap listToMaybe $ Queries.loadAllProjectBranchesBeginningWith projectId Nothing
      pure (ProjectAndBranch projectId someBranchId)
    findAnyBranchInCodebase :: MaybeT Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    findAnyBranchInCodebase = do
      (_, pbIds) <- MaybeT . fmap listToMaybe $ Queries.loadAllProjectBranchNamePairs
      pure pbIds
    createDummyProject = error "TODO: create new branch or project if we  delete the last branch you're on."

-- | Delete a project branch and record an entry in the reflog.
doDeleteProjectBranch :: ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Cli ()
doDeleteProjectBranch projectAndBranch = do
  Cli.runTransaction do
    Queries.deleteProjectBranch projectAndBranch.project.projectId projectAndBranch.branch.branchId

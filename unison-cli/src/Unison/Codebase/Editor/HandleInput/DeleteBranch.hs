-- | @delete.branch@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteBranch
  ( handleDeleteBranch,
  )
where

import Control.Lens
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
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
handleDeleteBranch projectAndBranchNames = do
  ProjectPath currentProject currentBranch _ <- Cli.getCurrentProjectPath
  ProjectAndBranch _proj branchToDelete <- ProjectUtils.resolveProjectBranch currentProject (projectAndBranchNames & #branch %~ Just)
  Cli.runTransaction do
    Queries.deleteProjectBranch (branchToDelete ^. #projectId) (branchToDelete ^. #branchId)

  let projectId = branchToDelete ^. #projectId

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
          [ parentBranch projectId (branchToDelete ^. #parentBranchId),
            findMainBranchInProject projectId,
            findAnyBranchInProject projectId,
            findAnyBranchInCodebase
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

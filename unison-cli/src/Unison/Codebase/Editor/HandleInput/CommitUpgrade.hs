-- | @upgrade.commit@ handler.
module Unison.Codebase.Editor.HandleInput.CommitUpgrade
  ( handleCommitUpgrade,
  )
where

import U.Codebase.Sqlite.Project qualified
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.DeleteBranch qualified as DeleteBranch
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

handleCommitUpgrade :: Cli ()
handleCommitUpgrade = do
  (upgradeProjectAndBranch, _path) <- ProjectUtils.expectCurrentProjectBranch

  -- Assert that this is an "upgrade" branch, get its parent (which is the branch we were on when we ran `upgrade`),
  -- and switch to the parent.

  parentBranchId <-
    ProjectUtils.getUpgradeBranchParent upgradeProjectAndBranch.branch
      & onNothing (Cli.returnEarly Output.NoUpgradeInProgress)

  parentBranch <-
    Cli.runTransaction do
      parentBranch <- Queries.expectProjectBranch upgradeProjectAndBranch.project.projectId parentBranchId
      Queries.setMostRecentBranch parentBranch.projectId parentBranch.branchId
      pure parentBranch
  Cli.cd (ProjectUtils.projectBranchPath (ProjectAndBranch parentBranch.projectId parentBranch.branchId))

  -- Merge the upgrade branch into the parent

  Merge.doMergeLocalBranch
    TwoWay
      { alice = ProjectAndBranch upgradeProjectAndBranch.project parentBranch,
        bob = upgradeProjectAndBranch
      }

  -- Delete the upgrade branch

  DeleteBranch.doDeleteProjectBranch upgradeProjectAndBranch

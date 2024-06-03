-- | @upgrade.commit@ handler.
module Unison.Codebase.Editor.HandleInput.CommitUpgrade
  ( handleCommitUpgrade,
  )
where

import U.Codebase.Sqlite.Project qualified
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.DeleteBranch qualified as DeleteBranch
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge
import Unison.Codebase.Editor.HandleInput.ProjectSwitch qualified as ProjectSwitch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

-- Note: this implementation is similar to `merge.commit`.

handleCommitUpgrade :: Cli ()
handleCommitUpgrade = do
  (upgradeProjectAndBranch, _path) <- ProjectUtils.expectCurrentProjectBranch

  -- Assert that this is an "upgrade" branch and get its parent, which is the branch we were on when we ran `upgrade`.

  parentBranchId <-
    ProjectUtils.getUpgradeBranchParent upgradeProjectAndBranch.branch
      & onNothing (Cli.returnEarly Output.NoUpgradeInProgress)
  parentBranch <-
    Cli.runTransaction do
      Queries.expectProjectBranch upgradeProjectAndBranch.project.projectId parentBranchId

  let parentProjectAndBranch =
        ProjectAndBranch upgradeProjectAndBranch.project parentBranch

  -- Switch to the parent

  ProjectSwitch.switchToProjectBranch (ProjectUtils.justTheIds parentProjectAndBranch)

  -- Merge the upgrade branch into the parent

  Merge.doMergeLocalBranch
    TwoWay
      { alice = parentProjectAndBranch,
        bob = upgradeProjectAndBranch
      }

  -- Delete the upgrade branch

  DeleteBranch.doDeleteProjectBranch upgradeProjectAndBranch

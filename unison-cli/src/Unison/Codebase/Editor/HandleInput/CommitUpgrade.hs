-- | @upgrade.commit@ handler.
module Unison.Codebase.Editor.HandleInput.CommitUpgrade
  ( handleCommitUpgrade,
  )
where

import U.Codebase.Sqlite.Project qualified
import Unison.Cli.Monad (Cli)
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.ProjectSwitch qualified as ProjectSwitch
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

handleCommitUpgrade :: Cli ()
handleCommitUpgrade = do
  (projectAndBranch, _path) <- ProjectUtils.expectCurrentProjectBranch

  -- Assert that this is an "upgrade" branch and get its parent, which is the branch we were on when we ran `upgrade`.
  parentBranchId <-
    ProjectUtils.getUpgradeBranchParent projectAndBranch.branch
      & onNothing wundefined

  -- Switch to the parent
  ProjectSwitch.switchToProjectBranch projectAndBranch.project.projectId parentBranchId

  -- Merge in the upgrade branch
  wundefined

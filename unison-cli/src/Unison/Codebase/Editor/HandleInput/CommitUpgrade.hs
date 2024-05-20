-- | @upgrade.commit@ handler.
module Unison.Codebase.Editor.HandleInput.CommitUpgrade
  ( handleCommitUpgrade,
  )
where

import Data.Text qualified as Text
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.MergeTypes (MergeSource (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge
import Unison.Codebase.Editor.HandleInput.ProjectSwitch qualified as ProjectSwitch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

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

  (parentCausalHash, upgradeCausalHash, lcaCausalHash) <-
    Cli.runTransaction do
      parentCausalHash <- ProjectUtils.getProjectBranchCausalHash (ProjectUtils.justTheIds parentProjectAndBranch)
      upgradeCausalHash <- ProjectUtils.getProjectBranchCausalHash (ProjectUtils.justTheIds upgradeProjectAndBranch)
      lcaCausalHash <- Operations.lca parentCausalHash upgradeCausalHash
      pure (parentCausalHash, upgradeCausalHash, lcaCausalHash)

  Merge.doMerge
    Merge.MergeInfo
      { alice =
          Merge.AliceMergeInfo
            { causalHash = parentCausalHash,
              projectAndBranch = parentProjectAndBranch
            },
        bob =
          Merge.BobMergeInfo
            { causalHash = upgradeCausalHash,
              source = MergeSource'LocalProjectBranch (ProjectUtils.justTheNames upgradeProjectAndBranch)
            },
        lca =
          Merge.LcaMergeInfo
            { causalHash = lcaCausalHash
            },
        description = Text.pack (InputPattern.patternName InputPatterns.upgradeCommitInputPattern)
      }

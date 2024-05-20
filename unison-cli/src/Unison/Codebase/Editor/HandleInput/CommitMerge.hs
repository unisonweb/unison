-- | @merge.commit@ handler.
module Unison.Codebase.Editor.HandleInput.CommitMerge
  ( handleCommitMerge,
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
import Unison.Codebase.Editor.HandleInput.Update2 qualified as Update
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

-- Note: this implementation is similar to `upgrade.commit`.

handleCommitMerge :: Cli ()
handleCommitMerge = do
  (mergeProjectAndBranch, _path) <- ProjectUtils.expectCurrentProjectBranch

  -- Assert that this is a "merge" branch and get its parent, which is the branch we were on when we ran `merge`.

  parentBranchId <-
    ProjectUtils.getMergeBranchParent mergeProjectAndBranch.branch
      & onNothing (Cli.returnEarly Output.NoMergeInProgress)
  parentBranch <-
    Cli.runTransaction do
      Queries.expectProjectBranch mergeProjectAndBranch.project.projectId parentBranchId

  let parentProjectAndBranch =
        ProjectAndBranch mergeProjectAndBranch.project parentBranch

  -- Run `update`

  Update.handleUpdate2

  -- Switch to the parent

  ProjectSwitch.switchToProjectBranch (ProjectUtils.justTheIds parentProjectAndBranch)

  -- Merge the merge branch into the parent

  Merge.doMergeLocalBranch
    TwoWay
      { alice = parentProjectAndBranch,
        bob = mergeProjectAndBranch
      }

  -- Delete the merge branch

  DeleteBranch.doDeleteProjectBranch mergeProjectAndBranch

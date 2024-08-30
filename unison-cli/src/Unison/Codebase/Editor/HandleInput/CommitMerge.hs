-- | @merge.commit@ handler.
module Unison.Codebase.Editor.HandleInput.CommitMerge
  ( handleCommitMerge,
  )
where

import U.Codebase.Sqlite.Project qualified
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.DeleteBranch qualified as DeleteBranch
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))

-- Note: this implementation is similar to `upgrade.commit`.

handleCommitMerge :: Cli ()
handleCommitMerge = do
  mergeProjectAndBranch <- Cli.getCurrentProjectAndBranch

  -- Assert that this is a "merge" branch, get its parent (which is the branch we were on when we ran `merge`),
  -- and switch to the parent.

  parentBranchId <-
    ProjectUtils.getMergeBranchParent mergeProjectAndBranch.branch
      & onNothing (Cli.returnEarly Output.NoMergeInProgress)

  parentBranch <-
    Cli.runTransaction do
      parentBranch <- Queries.expectProjectBranch mergeProjectAndBranch.project.projectId parentBranchId
      pure parentBranch
  Cli.switchProject (ProjectAndBranch parentBranch.projectId parentBranch.branchId)

  -- Merge the merge branch into the parent

  Merge.doMergeLocalBranch
    TwoWay
      { alice = ProjectAndBranch mergeProjectAndBranch.project parentBranch,
        bob = mergeProjectAndBranch
      }

  -- Delete the merge branch

  DeleteBranch.doDeleteProjectBranch mergeProjectAndBranch

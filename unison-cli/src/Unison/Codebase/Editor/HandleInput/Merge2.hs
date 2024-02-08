module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge
  )
where

import Control.Monad.Reader (ask)
import Unison.Prelude
import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Unison.Merge.Database (MergeDatabase(..), makeMergeDatabase)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment, typecheckTerm)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Merge.Diff qualified as Merge

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let projectBranches = Merge.TwoWay {alice = aliceProjectBranch, bob = bobProjectBranch}
  let alicePath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (aliceProjectBranch ^. #branchId))
  let bobPath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (bobProjectBranch ^. #branchId))

  -- Create a bunch of cached database lookup functions
  Cli.Env {codebase} <- ask
  db@MergeDatabase {loadCausal} <- makeMergeDatabase codebase
  undefined

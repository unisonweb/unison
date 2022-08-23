module Unison.Codebase.Editor.HandleInput.MoveBranch where

import Data.Configurator ()
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import Unison.Prelude

-- | Moves a branch and its history from one location to another, and saves the new root
-- branch.
doMoveBranch :: forall r. Text -> Path.Absolute -> Path.Absolute -> Cli r ()
doMoveBranch actionDescription src dest = do
  srcBranch <- Cli.expectBranchAtPath' (Path.AbsolutePath' src)
  rootBranch <- Cli.getRootBranch
  let newRoot =
        rootBranch
          & Branch.modifyAt (Path.unabsolute src) (const Branch.empty)
          & Branch.modifyAt (Path.unabsolute dest) (const srcBranch)
  Cli.updateRoot newRoot actionDescription

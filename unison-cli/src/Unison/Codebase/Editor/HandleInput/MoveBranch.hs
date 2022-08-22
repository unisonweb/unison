module Unison.Codebase.Editor.HandleInput.MoveBranch where

import Data.Configurator ()
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import Unison.Prelude

doMoveBranch :: Text -> Maybe Path.Split' -> Path.Path' -> Cli r ()
doMoveBranch actionDescription maySrc dest' = do
  case maySrc of
    Nothing -> moveRoot dest'
    Just src -> moveNamespace (Path.unsplit' src) dest'
  where
    moveRoot dest' = do
      rootBranch <- Cli.getRootBranch
      destAbs <- Cli.resolvePath' dest'
      -- Just add the current root as a child of an empty root
      let newRoot = Branch.modifyAt (Path.unabsolute destAbs) (const rootBranch) Branch.empty
      Cli.updateRoot newRoot actionDescription
    moveNamespace src' dest' = do
      srcAbs <- Cli.resolvePath' src'
      destAbs <- Cli.resolvePath' dest'
      srcBranch <- Cli.expectBranchAtPath' src'
      rootBranch <- Cli.getRootBranch
      let newRoot =
            rootBranch
              & Branch.modifyAt (Path.unabsolute srcAbs) (const Branch.empty)
              & Branch.modifyAt (Path.unabsolute destAbs) (const srcBranch)
      Cli.updateRoot newRoot actionDescription

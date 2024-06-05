module Unison.Codebase.Editor.HandleInput.MoveBranch (doMoveBranch, moveBranchFunc) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Prelude

moveBranchFunc :: Path.Path' -> Path.Path' -> Cli (Maybe (PP.ProjectPath, Branch IO -> Branch IO))
moveBranchFunc src' dest' = do
  -- We currently only support moving within the same project branch.
  srcPP@(PP.ProjectPath proj projBranch srcAbs) <- Cli.resolvePath' src'
  PP.ProjectPath _ _ destAbs <- Cli.resolvePath' dest'
  destBranchExists <- Cli.branchExistsAtPath' dest'
  Cli.getMaybeBranchFromProjectPath srcPP >>= traverse \srcBranch -> do
    -- We want the move to appear as a single step in the root namespace, but we need to make
    -- surgical changes in both the root and the destination, so we make our modifications at the shared parent of
    -- those changes such that they appear as a single change in the root.
    let (changeRootPath, srcLoc, destLoc) = Path.longestPathPrefix (Path.unabsolute srcAbs) (Path.unabsolute destAbs)
    let doMove changeRoot =
          changeRoot
            & Branch.modifyAt srcLoc (const Branch.empty)
            & Branch.modifyAt destLoc (const srcBranch)
    if destBranchExists
      then Cli.respond (MovedOverExistingBranch dest')
      else pure ()
    pure (PP.ProjectPath proj projBranch $ Path.Absolute changeRootPath, doMove)

-- | Moves a branch and its history from one location to another, and saves the new root
-- branch.
doMoveBranch :: Text -> Path.Path' -> Path.Path' -> Cli ()
doMoveBranch actionDescription src' dest' = do
  moveBranchFunc src' dest' >>= \case
    Nothing -> Cli.respond (BranchNotFound src')
    Just (path, func) -> do
      _ <- Cli.updateAt actionDescription path func
      Cli.respond Success

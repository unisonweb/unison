module Unison.Codebase.Editor.HandleInput.History (handleHistory) where

import Data.Map qualified as Map
import U.Codebase.HashTags
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Causal qualified as Causal
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Path (Path')
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude

handleHistory :: Maybe Int -> Maybe Int -> BranchIdG Path' -> Cli.Cli ()
handleHistory resultsCap diffCap from = do
  branch <-
    case from of
      BranchAtSCH hash -> Cli.resolveShortCausalHash hash
      BranchAtPath path' -> do
        pp <- Cli.resolvePath' path'
        Cli.getBranchFromProjectPath pp
      BranchAtProjectPath pp -> Cli.getBranchFromProjectPath pp
  schLength <- Cli.runTransaction Codebase.branchHashLength
  history <- liftIO (doHistory schLength 0 branch [])
  Cli.respondNumbered history
  where
    doHistory :: Int -> Int -> Branch IO -> [(CausalHash, Names.Diff)] -> IO NumberedOutput
    doHistory schLength !n b acc =
      if maybe False (n >=) resultsCap
        then pure (History diffCap schLength acc (PageEnd (Branch.headHash b) n))
        else case Branch._history b of
          Causal.One {} -> pure (History diffCap schLength acc (EndOfLog $ Branch.headHash b))
          Causal.Merge _ _ _ tails ->
            pure (History diffCap schLength acc (MergeTail (Branch.headHash b) $ Map.keys tails))
          Causal.Cons _ _ _ tail -> do
            b' <- fmap Branch.Branch $ snd tail
            let elem = (Branch.headHash b, Branch.namesDiff b' b)
            doHistory schLength (n + 1) b' (elem : acc)

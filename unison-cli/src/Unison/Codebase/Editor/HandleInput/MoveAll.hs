module Unison.Codebase.Editor.HandleInput.MoveAll (handleMoveAll) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.HandleInput.MoveBranch (moveBranchFunc)
import Unison.Codebase.Editor.HandleInput.MoveTerm (moveTermSteps)
import Unison.Codebase.Editor.HandleInput.MoveType (moveTypeSteps)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified' qualified as HQ'
import Unison.Prelude

handleMoveAll :: Path.Path' -> Path.Path' -> Text -> Cli ()
handleMoveAll src' dest' description = do
  moveBranchFunc <- moveBranchFunc src' dest'
  moveTermTypeSteps <- case (,) <$> Path.toSplit' src' <*> Path.toSplit' dest' of
    Nothing -> pure []
    Just (fmap HQ'.NameOnly -> src, dest) -> do
      termSteps <- moveTermSteps src dest
      typeSteps <- moveTypeSteps src dest
      pure (termSteps ++ typeSteps)
  case (moveBranchFunc, moveTermTypeSteps) of
    (Nothing, []) -> Cli.respond (Output.MoveNothingFound src')
    (mupdates, steps) -> do
      Cli.updateAndStepAt description (maybeToList mupdates) steps
      Cli.respond Output.Success

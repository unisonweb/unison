module Unison.Codebase.Editor.HandleInput.Rename (handleRename) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.HandleInput.MoveBranch (moveBranchFunc)
import Unison.Codebase.Editor.HandleInput.MoveTerm (moveTermSteps)
import Unison.Codebase.Editor.HandleInput.MoveType (moveTypeSteps)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Prelude

-- | Rename changes only the final segment of a name.
-- For example, `rename foo.bar.baz Qux` produces `foo.bar.Qux`.
handleRename :: Path.Path' -> NameSegment -> Text -> Cli ()
handleRename src' newNameSeg description = do
  -- The destination path has the same parent as the source, but with the new name segment
  case Path.split src' of
    Nothing -> Cli.respond (Output.MoveNothingFound src')
    Just (parentPath, _oldNameSeg) -> do
      let dest' = Path.unsplit (parentPath, newNameSeg)
      -- Use the same logic as MoveAll, but construct the destination from the source parent + new segment
      moveBranchFunc <- moveBranchFunc False src' dest'
      moveTermTypeSteps <- case (,) <$> Path.split src' <*> Path.split dest' of
        Nothing -> pure []
        Just (HQ'.NameOnly -> src, dest) -> do
          termSteps <- moveTermSteps src dest
          typeSteps <- moveTypeSteps src dest
          pure (termSteps ++ typeSteps)
      case (moveBranchFunc, moveTermTypeSteps) of
        (Nothing, []) -> Cli.respond (Output.MoveNothingFound src')
        (mupdates, steps) -> do
          pp <- Cli.getCurrentProjectPath
          Cli.updateAndStepAt description (pp ^. #branch) (maybeToList mupdates) steps
          Cli.respond (Output.RenameResult src' dest')

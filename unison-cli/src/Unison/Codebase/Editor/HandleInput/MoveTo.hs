{-# OPTIONS_GHC -Wwarn=x-partial #-}

module Unison.Codebase.Editor.HandleInput.MoveTo (handleMoveTo) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Codebase.Editor.HandleInput.MoveBranch (moveBranchFunc)
import Unison.Codebase.Editor.HandleInput.MoveTerm (moveTermSteps)
import Unison.Codebase.Editor.HandleInput.MoveType (moveTypeSteps)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Prelude

-- | Move one or more items INTO a destination namespace.
-- For example:
--   `moveTo foo.bar baz` moves `foo.bar` to `baz.bar` (keeps the final segment)
--   `moveTo foo bar baz dest` moves all three into `dest.foo`, `dest.bar`, `dest.baz`
--
-- If multiple sources have the same final segment, we move the non-conflicting items
-- and report the conflict to the user.
handleMoveTo :: NonEmpty Path.Path' -> Path.Path' -> Text -> Cli ()
handleMoveTo sources dest' description = do
  -- Group sources by their final segment to detect conflicts
  let sourcesWithSegments :: [(Path.Path', Maybe (Path.Path', NameSegment))]
      sourcesWithSegments = [(src, Path.split src) | src <- NE.toList sources]

      -- Partition into sources with valid splits and those without
      (invalidSources, validSources) = foldr partitionValid ([], []) sourcesWithSegments
        where
          partitionValid (src, Nothing) (invalid, valid) = (src : invalid, valid)
          partitionValid (src, Just (parent, seg)) (invalid, valid) = (invalid, (src, parent, seg) : valid)

      -- Group valid sources by their final segment
      byFinalSegment :: Map.Map NameSegment [(Path.Path', Path.Path')]
      byFinalSegment =
        Map.fromListWith
          (++)
          [(seg, [(src, parent)]) | (src, parent, seg) <- validSources]

      -- Separate conflicting and non-conflicting sources
      (conflicting, nonConflicting) = Map.partition (\srcs -> length srcs > 1) byFinalSegment

  -- Report invalid sources (paths that can't be split, like the root)
  when (not (null invalidSources)) $
    Cli.respond $
      Output.MoveNothingFound (head invalidSources)

  -- Process non-conflicting sources
  let nonConflictingSources :: [(Path.Path', NameSegment)]
      nonConflictingSources = do
        (seg, srcs) <- Map.toList nonConflicting
        (src, _parent) <- srcs
        pure (src, seg)

  results <- forM nonConflictingSources $ \(src', seg) -> do
    -- Destination is dest'.seg
    let destPath = Path.descend dest' seg
    result <- moveSingleItem src' destPath
    pure (src', destPath, result)

  -- Collect all the steps and updates, and track what was actually moved
  let allBranchUpdates = catMaybes [mupdate | (_, _, (mupdate, _)) <- results]
      allSteps = concat [steps | (_, _, (_, steps)) <- results]
      -- Items that were actually moved (had branch updates or term/type steps)
      movedItems = [(src, dest) | (src, dest, (mupdate, steps)) <- results, isJust mupdate || not (null steps)]

  -- Perform the moves
  when (not (null allBranchUpdates) || not (null allSteps)) $ do
    pp <- Cli.getCurrentProjectPath
    Cli.updateAndStepAt description (pp ^. #branch) allBranchUpdates allSteps

  -- Build conflict info: just the source paths grouped by segment
  let conflictInfo :: [(NameSegment, [Path.Path'])]
      conflictInfo = [(seg, map fst srcs) | (seg, srcs) <- Map.toList conflicting]

  -- Report results
  if null conflicting
    then
      if null movedItems
        then Cli.respond (Output.MoveNothingFound (NE.head sources))
        else Cli.respond (Output.MoveToResult movedItems)
    else do
      -- Report the conflicts (and any items that were moved)
      Cli.respond $ Output.MoveToConflicts movedItems conflictInfo dest'

-- | Move a single item (term, type, and/or namespace) from src to dest
moveSingleItem ::
  Path.Path' ->
  Path.Path' ->
  Cli (Maybe (Path.Absolute, Branch IO -> Branch IO), [(Path.Absolute, Branch0 m -> Branch0 m)])
moveSingleItem src' dest' = do
  -- Move namespace if exists
  moveBranchResult <- moveBranchFunc False src' dest'

  -- Move term and type if they exist
  moveTermTypeSteps <- case (,) <$> Path.split src' <*> Path.split dest' of
    Nothing -> pure []
    Just (HQ'.NameOnly -> src, dest) -> do
      termSteps <- moveTermSteps src dest
      typeSteps <- moveTypeSteps src dest
      pure (termSteps ++ typeSteps)

  pure (moveBranchResult, moveTermTypeSteps)

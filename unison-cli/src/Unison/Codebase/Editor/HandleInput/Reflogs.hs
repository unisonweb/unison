-- | Helpers for working with various kinds of reflogs.
module Unison.Codebase.Editor.HandleInput.Reflogs (showProjectBranchReflog) where

import Control.Arrow ((&&&))
import Data.List qualified as List
import Data.Time (UTCTime)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reflog qualified as Reflog
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Prelude
import Unison.Codebase.ShortCausalHash qualified as SCH

showRootReflog :: Cli ()
showRootReflog = do
  let numEntriesToShow = 500
  (schLength, entries) <-
    Cli.runTransaction $
      (,) <$> Codebase.branchHashLength <*> Codebase.getDeprecatedRootReflog numEntriesToShow
  let moreEntriesToLoad = length entries == numEntriesToShow
  let expandedEntries = List.unfoldr expandEntries (entries, Nothing, moreEntriesToLoad)
  let (shortEntries, numberedEntries) =
        unzip $
          expandedEntries <&> \(time, hash, reason) ->
            let (exp, sa) = (SCH.fromHash schLength &&& SA.Namespace) hash
             in ((time, exp, reason), sa)
  Cli.setNumberedArgs numberedEntries
  Cli.respond $ ShowReflog shortEntries
  where
    expandEntries ::
      ([Reflog.Entry CausalHash Text], Maybe CausalHash, Bool) ->
      Maybe ((Maybe UTCTime, CausalHash, Text), ([Reflog.Entry CausalHash Text], Maybe CausalHash, Bool))
    expandEntries ([], Just expectedHash, moreEntriesToLoad) =
      if moreEntriesToLoad
        then Nothing
        else Just ((Nothing, expectedHash, "history starts here"), ([], Nothing, moreEntriesToLoad))
    expandEntries ([], Nothing, _moreEntriesToLoad) = Nothing
    expandEntries (entries@(Reflog.Entry {time, fromRootCausalHash, toRootCausalHash, reason} : rest), mayExpectedHash, moreEntriesToLoad) =
      Just $
        case mayExpectedHash of
          Just expectedHash
            | expectedHash == toRootCausalHash -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))
            -- Historical discontinuity, insert a synthetic entry
            | otherwise -> ((Nothing, toRootCausalHash, "(external change)"), (entries, Nothing, moreEntriesToLoad))
          -- No expectation, either because this is the most recent entry or
          -- because we're recovering from a discontinuity
          Nothing -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))

showProjectBranchReflog :: Maybe (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) -> Cli ()
showProjectBranchReflog = do
  let numEntriesToShow = 500
  (schLength, entries) <-
    Cli.runTransaction $
      (,) <$> Codebase.branchHashLength <*> Codebase.getDeprecatedRootReflog numEntriesToShow
  let moreEntriesToLoad = length entries == numEntriesToShow
  let expandedEntries = List.unfoldr expandEntries (entries, Nothing, moreEntriesToLoad)
  let (shortEntries, numberedEntries) =
        unzip $
          expandedEntries <&> \(time, hash, reason) ->
            let (exp, sa) = (SCH.fromHash schLength &&& SA.Namespace) hash
             in ((time, exp, reason), sa)
  Cli.setNumberedArgs numberedEntries
  Cli.respond $ ShowReflog shortEntries
  where
    expandEntries ::
      ([Reflog.Entry CausalHash Text], Maybe CausalHash, Bool) ->
      Maybe ((Maybe UTCTime, CausalHash, Text), ([Reflog.Entry CausalHash Text], Maybe CausalHash, Bool))
    expandEntries ([], Just expectedHash, moreEntriesToLoad) =
      if moreEntriesToLoad
        then Nothing
        else Just ((Nothing, expectedHash, "history starts here"), ([], Nothing, moreEntriesToLoad))
    expandEntries ([], Nothing, _moreEntriesToLoad) = Nothing
    expandEntries (entries@(Reflog.Entry {time, fromRootCausalHash, toRootCausalHash, reason} : rest), mayExpectedHash, moreEntriesToLoad) =
      Just $
        case mayExpectedHash of
          Just expectedHash
            | expectedHash == toRootCausalHash -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))
            -- Historical discontinuity, insert a synthetic entry
            | otherwise -> ((Nothing, toRootCausalHash, "(external change)"), (entries, Nothing, moreEntriesToLoad))
          -- No expectation, either because this is the most recent entry or
          -- because we're recovering from a discontinuity
          Nothing -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))

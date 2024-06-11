module Unison.Codebase.Editor.HandleInput.MoveType (doMoveType, moveTypeSteps) where

import Control.Lens (_2)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path, Path')
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified' qualified as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Prelude

moveTypeSteps :: (Path', HQ'.HQSegment) -> (Path', NameSegment) -> Cli [(Path, Branch0 m -> Branch0 m)]
moveTypeSteps src' dest' = do
  src <- Cli.resolveSplit' src'
  srcTypes <- Cli.getTypesAt src
  case Set.toList srcTypes of
    [] -> pure []
    _ : _ : _ -> do
      hqLength <- Cli.runTransaction Codebase.hashLength
      Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' Set.empty srcTypes)
    [srcType] -> do
      dest <- Cli.resolveSplit' dest'
      destTypes <- Cli.getTypesAt (Path.convert dest)
      when (not (Set.null destTypes)) do
        Cli.returnEarly (Output.TypeAlreadyExists dest' destTypes)
      let p = Path.convert src
      pure
        [ -- Mitchell: throwing away any hash-qualification here seems wrong!
          BranchUtil.makeDeleteTypeName (over _2 HQ'.toName p) srcType,
          BranchUtil.makeAddTypeName (Path.convert dest) srcType
        ]

doMoveType :: (Path', HQ'.HQSegment) -> (Path', NameSegment) -> Text -> Cli ()
doMoveType src' dest' description = do
  steps <- moveTypeSteps src' dest'
  when (null steps) do
    Cli.returnEarly (Output.TypeNotFound src')
  Cli.stepManyAt description steps
  Cli.respond Output.Success

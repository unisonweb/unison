module Unison.Codebase.Editor.HandleInput.MoveType (doMoveType) where

import Control.Lens (over, _2)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified' qualified as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Util.Set qualified as Set

doMoveType :: (Path', HQ'.HQSegment) -> (Path', NameSegment) -> Text -> Cli ()
doMoveType src' dest' description = do
  src <- Cli.resolveSplit' src'
  srcTypes <- Cli.getTypesAt src
  srcType <-
    Set.asSingleton srcTypes & onNothing do
      if Set.null srcTypes
        then Cli.returnEarly (Output.TypeNotFound src')
        else do
          hqLength <- Cli.runTransaction Codebase.hashLength
          Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' Set.empty srcTypes)
  dest <- Cli.resolveSplit' dest'
  destTypes <- Cli.getTypesAt (Path.convert dest)
  when (not (Set.null destTypes)) do
    Cli.returnEarly (Output.TypeAlreadyExists dest' destTypes)
  let p = Path.convert src
  srcMetadata <- do
    root0 <- Cli.getRootBranch0
    pure (BranchUtil.getTypeMetadataAt p srcType root0)
  Cli.stepManyAt
    description
    [ -- Mitchell: throwing away any hash-qualification here seems wrong!
      BranchUtil.makeDeleteTypeName (over _2 HQ'.toName p) srcType,
      BranchUtil.makeAddTypeName (Path.convert dest) srcType srcMetadata
    ]
  Cli.respond Output.Success

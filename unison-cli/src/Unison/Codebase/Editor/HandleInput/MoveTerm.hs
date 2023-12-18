module Unison.Codebase.Editor.HandleInput.MoveTerm (doMoveTerm, moveTermSteps) where

import Control.Lens (over, _2)
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

moveTermSteps :: (Path', HQ'.HQSegment) -> (Path', NameSegment) -> Cli [(Path, Branch0 m -> Branch0 m)]
moveTermSteps src' dest' = do
  src <- Cli.resolveSplit' src'
  srcTerms <- Cli.getTermsAt src
  case Set.toList srcTerms of
    [] -> pure []
    _:_:_ -> do
      hqLength <- Cli.runTransaction Codebase.hashLength
      Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' srcTerms Set.empty)
    [srcTerm] -> do
      dest <- Cli.resolveSplit' dest'
      destTerms <- Cli.getTermsAt (Path.convert dest)
      when (not (Set.null destTerms)) do
        Cli.returnEarly (Output.TermAlreadyExists dest' destTerms)
      let p = Path.convert src
      srcMetadata <- do
        root0 <- Cli.getRootBranch0
        pure (BranchUtil.getTermMetadataAt p srcTerm root0)
      pure
        [ -- Mitchell: throwing away any hash-qualification here seems wrong!
          BranchUtil.makeDeleteTermName (over _2 HQ'.toName p) srcTerm,
          BranchUtil.makeAddTermName (Path.convert dest) srcTerm srcMetadata
        ]

doMoveTerm :: (Path', HQ'.HQSegment) -> (Path', NameSegment) -> Text -> Cli ()
doMoveTerm src' dest' description = do
  steps <- moveTermSteps src' dest'
  when (null steps) do
    Cli.returnEarly (Output.TermNotFound src')
  Cli.stepManyAt description steps
  Cli.respond Output.Success

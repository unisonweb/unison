module Unison.Codebase.Editor.HandleInput.MoveTerm (doMoveTerm) where

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

doMoveTerm :: (Path', HQ'.HQSegment) -> (Path', NameSegment) -> Text -> Cli ()
doMoveTerm src' dest' description = do
  src <- Cli.resolveSplit' src'
  srcTerms <- Cli.getTermsAt src
  srcTerm <-
    Set.asSingleton srcTerms & onNothing do
      if Set.null srcTerms
        then Cli.returnEarly (Output.TermNotFound src')
        else do
          hqLength <- Cli.runTransaction Codebase.hashLength
          Cli.returnEarly (Output.DeleteNameAmbiguous hqLength src' srcTerms Set.empty)
  dest <- Cli.resolveSplit' dest'
  destTerms <- Cli.getTermsAt (Path.convert dest)
  when (not (Set.null destTerms)) do
    Cli.returnEarly (Output.TermAlreadyExists dest' destTerms)
  let p = Path.convert src
  srcMetadata <- do
    root0 <- Cli.getRootBranch0
    pure (BranchUtil.getTermMetadataAt p srcTerm root0)
  Cli.stepManyAt
    description
    [ -- Mitchell: throwing away any hash-qualification here seems wrong!
      BranchUtil.makeDeleteTermName (over _2 HQ'.toName p) srcTerm,
      BranchUtil.makeAddTermName (Path.convert dest) srcTerm srcMetadata
    ]
  Cli.respond Output.Success

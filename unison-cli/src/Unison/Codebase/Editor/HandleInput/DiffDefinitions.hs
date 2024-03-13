module Unison.Codebase.Editor.HandleInput.DiffDefinitions (diffTerms) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.Pretty qualified as P
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name)
import Unison.Server.NameSearch (NameSearch (..))
import Unison.Server.NameSearch qualified as NameSearch
import Unison.Server.NameSearch.FromNames qualified as NameSearch

diffTerms :: HQ'.HashQualified Name -> HQ'.HashQualified Name -> Cli ()
diffTerms fromTerm toTerm = do
  names <- Cli.currentNames
  pped <- Cli.prettyPrintEnvDeclFromNames names
  hqLength <- Cli.runTransaction Codebase.hashLength
  let NameSearch {termSearch} = NameSearch.makeNameSearch hqLength names
  fromRefs <- NameSearch.lookupRelativeHQRefs' termSearch NameSearch.ExactName fromTerm
  let renderedFrom = P.prettyTerm pped False False (fromTerm, _, _)
  _

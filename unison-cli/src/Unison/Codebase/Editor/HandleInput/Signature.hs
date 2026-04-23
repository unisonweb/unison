module Unison.Codebase.Editor.HandleInput.Signature
  ( handleSignature,
  )
where

import Control.Monad.Reader (ask)
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Codebase.Editor.Output
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Server.Backend qualified as Backend
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Server.QueryResult (QueryResult (..))
import Unison.Server.SearchResultPrime qualified as SR'
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Util.Pretty qualified as P

handleSignature :: NonEmpty (HQ.HashQualified Name) -> Cli ()
handleSignature hqNames = do
  Cli.Env {codebase} <- ask
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  let nameSearch = NameSearch.makeNameSearch 10 names
  let query = Set.fromList (toList hqNames)
  (misses, results) <- Cli.runTransaction do
    QueryResult {misses, hits} <- Backend.hqNameQuery codebase nameSearch Names.IncludeSuffixes query
    results <- Backend.loadSearchResults codebase hits
    pure (misses, results)
  let sigs =
        results >>= \case
          SR'.Tm name (Just typ) r _ ->
            TypePrinter.prettySignaturesCT suffixifiedPPE [(r, name, typ)]
          SR'.Tp' tr ->
            [Pretty.prettyTypeResultHeader' tr]
          _ -> []
  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  Cli.respond . DisplayDefinitions $ P.lines sigs

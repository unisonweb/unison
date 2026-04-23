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
import Unison.Codebase.Editor.Output
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Server.Backend qualified as Backend
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Server.QueryResult (QueryResult (..))
import Unison.Server.SearchResultPrime qualified as SR'
import Unison.Syntax.NamePrinter qualified as NP
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
  let renderEntry = \case
        SR'.Tm name (Just typ) _ _ ->
          let namePretty = P.syntaxToColor (NP.prettyHashQualified name)
              typePretty = TypePrinter.pretty suffixifiedPPE typ
           in namePretty <> P.newline <> P.indentN 2 (P.text ": " <> typePretty)
        SR'.Tp' (SR'.TypeResult' name _ r _) ->
          let namePretty = P.syntaxToColor (NP.prettyHashQualified name)
              tag = case r of
                Reference.Builtin {} -> P.lit "(builtin type)"
                Reference.DerivedId {} -> P.lit "(type)"
           in namePretty <> P.newline <> P.indentN 2 tag
        _ -> mempty
  let sigs = map renderEntry (filter (not . isMissing) results)
  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  Cli.respond . DisplayDefinitions $ P.sep P.newline sigs
  where
    isMissing = \case
      SR'.Tm _ Nothing _ _ -> True
      _ -> False

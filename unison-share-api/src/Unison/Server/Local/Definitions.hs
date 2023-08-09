module Unison.Server.Local.Definitions (prettyDefinitionsForHQName) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Map qualified as Map
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path)
import Unison.Codebase.Runtime qualified as Rt
import Unison.Debug qualified as Debug
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Server.Backend
import Unison.Server.Doc qualified as Doc
import Unison.Server.Local qualified as Local
import Unison.Server.NameSearch.FromNames (makeNameSearch)
import Unison.Server.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.Pretty (Width)

-- | Renders a definition for the given name or hash alongside its documentation.
prettyDefinitionsForHQName ::
  -- | The path representing the user's current perspective.
  -- Searches will be limited to definitions within this path, and names will be relative to
  -- this path.
  Path ->
  -- | The root branch to use
  V2Branch.CausalBranch Sqlite.Transaction ->
  Maybe Width ->
  -- | Whether to suffixify bindings in the rendered syntax
  Suffixify ->
  -- | Runtime used to evaluate docs. This should be sandboxed if run on the server.
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  -- | The name, hash, or both, of the definition to display.
  HQ.HashQualified Name ->
  Backend IO DefinitionDisplayResults
prettyDefinitionsForHQName perspective shallowRoot renderWidth suffixifyBindings rt codebase perspectiveQuery = do
  result <- liftIO . Codebase.runTransaction codebase $ do
    shallowBranch <- V2Causal.value shallowRoot
    Local.relocateToNameRoot perspective perspectiveQuery shallowBranch >>= \case
      Left err -> pure $ Left err
      Right (namesRoot, locatedQuery) -> do
        Debug.debugM Debug.Temp "prettyDefinitionsForHQName: " (namesRoot, locatedQuery)
        pure $ Right (shallowRoot, namesRoot, locatedQuery)
  (shallowRoot, namesRoot, query) <- either throwError pure result
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our perspective but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  -- ppe which returns names fully qualified to the current perspective,  not to the codebase root.
  let biases = maybeToList $ HQ.toName query
  hqLength <- liftIO $ Codebase.runTransaction codebase $ Codebase.hashLength
  (localNamesOnly, unbiasedPPED) <- scopedNamesForBranchHash codebase (Just shallowRoot) namesRoot
  let pped = PPED.biasTo biases unbiasedPPED
  -- Debug.debugM Debug.Temp "names: " (localNamesOnly)
  let nameSearch = makeNameSearch hqLength (NamesWithHistory.fromCurrentNames localNamesOnly)
  (DefinitionResults terms types misses) <- liftIO $ Codebase.runTransaction codebase do
    definitionsBySuffixes codebase nameSearch DontIncludeCycles [query]

  Debug.debugM Debug.Temp "terms: " terms
  Debug.debugM Debug.Temp "types: " types
  Debug.debugM Debug.Temp "misses: " misses
  let width = mayDefaultWidth renderWidth
  let docResults :: Name -> IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults name = do
        docRefs <- docsForDefinitionName codebase nameSearch name
        renderDocRefs pped width codebase rt docRefs

  let fqnPPE = PPED.unsuffixifiedPPE pped
  typeDefinitions <-
    ifor (typesToSyntax suffixifyBindings width pped types) \ref tp -> do
      let hqTypeName = PPE.typeNameOrHashOnly fqnPPE ref
      docs <- liftIO $ (maybe (pure []) docResults (HQ.toName hqTypeName))
      mkTypeDefinition codebase pped namesRoot shallowRoot width ref docs tp
  termDefinitions <-
    ifor (termsToSyntax suffixifyBindings width pped terms) \reference trm -> do
      let referent = Referent.Ref reference
      let hqTermName = PPE.termNameOrHashOnly fqnPPE referent
      docs <- liftIO $ (maybe (pure []) docResults (HQ.toName hqTermName))
      mkTermDefinition codebase pped namesRoot shallowRoot width reference docs trm
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $
    DefinitionDisplayResults
      renderedDisplayTerms
      renderedDisplayTypes
      renderedMisses

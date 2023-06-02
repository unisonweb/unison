-- | This module contains implementations of Backend methods which are specialized for Share.
-- We should likely move them to the Share repository eventually, but for now it's much easier
-- to ensure they're resilient to refactors and changes in the Backend API if they live here.
--
-- Perhaps we'll move them when the backing implementation switches to postgres.
module Unison.Server.Share.Definitions (definitionForHQName) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.NameLookups (PathSegments (..), ReversedName (..))
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.Codebase.SqliteCodebase.Conversions qualified as CV
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Debug qualified as Debug
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Sqlite qualified as PPESqlite
import Unison.Reference (TermReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Server.Backend hiding (renderDocRefs)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Doc qualified as Doc
import Unison.Server.NameSearch.Sqlite qualified as SqliteNameSearch
import Unison.Server.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.Pretty (Width)

-- | Renders a definition for the given name or hash alongside its documentation.
definitionForHQName ::
  -- | The path representing the user's current namesRoot.
  -- Searches will be limited to definitions within this path, and names will be relative to
  -- this path.
  Path ->
  -- | The root branch to use
  CausalHash ->
  Maybe Width ->
  -- | Whether to suffixify bindings in the rendered syntax
  Suffixify ->
  -- | Runtime used to evaluate docs. This should be sandboxed if run on the server.
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  -- | The name, hash, or both, of the definition to display.
  HQ.HashQualified Name ->
  Backend IO DefinitionDisplayResults
definitionForHQName perspective rootHash renderWidth suffixifyBindings rt codebase perspectiveQuery = do
  result <- liftIO . Codebase.runTransaction codebase $ do
    shallowRoot <- resolveCausalHashV2 (Just rootHash)
    shallowBranch <- V2Causal.value shallowRoot
    perspectiveQuery <- addNameIfHashOnly codebase perspective perspectiveQuery shallowRoot
    Backend.relocateToNameRoot perspective perspectiveQuery shallowBranch >>= \case
      Left err -> pure $ Left err
      Right (namesRoot, locatedQuery) -> pure $ Right (shallowRoot, namesRoot, locatedQuery)
  (shallowRoot, namesRoot, query) <- either throwError pure result
  Debug.debugM Debug.Server "definitionForHQName: (namesRoot, query)" (namesRoot, query)
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our namesRoot but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  -- ppe which returns names fully qualified to the current namesRoot,  not to the codebase root.
  let biases = maybeToList $ HQ.toName query
  let rootBranchHash = V2Causal.valueHash shallowRoot
  let ppedBuilder deps = fmap (PPED.biasTo biases) . liftIO . Codebase.runTransaction codebase $ PPESqlite.ppedForReferences rootBranchHash namesRoot deps
  (dr@(DefinitionResults terms types misses), nameSearch) <- liftIO $ Codebase.runTransaction codebase do
    nameSearch <- SqliteNameSearch.scopedNameSearch codebase rootBranchHash namesRoot
    dr <- definitionsBySuffixes codebase nameSearch DontIncludeCycles [query]
    pure (dr, nameSearch)
  Debug.debugM Debug.Server "definitionForHQName: found definitions" dr
  let width = mayDefaultWidth renderWidth
  let docResults :: Name -> Backend IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults name = do
        docRefs <- liftIO $ docsForDefinitionName codebase nameSearch name
        renderDocRefs ppedBuilder width codebase rt docRefs

  let drDeps = definitionResultsDependencies dr
  termAndTypePPED <- ppedBuilder drDeps
  let fqnTermAndTypePPE = PPED.unsuffixifiedPPE termAndTypePPED
  typeDefinitions <-
    ifor (typesToSyntax suffixifyBindings width termAndTypePPED types) \ref tp -> do
      let hqTypeName = PPE.typeNameOrHashOnly fqnTermAndTypePPE ref
      docs <- maybe (pure []) docResults (HQ.toName hqTypeName)
      mkTypeDefinition codebase termAndTypePPED namesRoot shallowRoot width ref docs tp
  termDefinitions <-
    ifor (termsToSyntax suffixifyBindings width termAndTypePPED terms) \reference trm -> do
      let referent = Referent.Ref reference
      let hqTermName = PPE.termNameOrHashOnly fqnTermAndTypePPE referent
      docs <- maybe (pure []) docResults (HQ.toName hqTermName)
      mkTermDefinition codebase termAndTypePPED namesRoot shallowRoot width reference docs trm
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $
    DefinitionDisplayResults
      renderedDisplayTerms
      renderedDisplayTypes
      renderedMisses

-- | A _hopefully_ temporary solution for the following problem:
--
-- When rendering definitions by-hash, we don't know which of the project's dependencies we
-- may be in, so we don't know which mount to use when rendering it.
-- So, first we do a breadth-first recursive search to find some name for that definition,
-- then we can use that name to find the mount and render just as we would if provided a name
-- up front.
addNameIfHashOnly :: Codebase m v a -> Path -> HQ.HashQualified Name -> V2Branch.CausalBranch Sqlite.Transaction -> Sqlite.Transaction (HQ.HashQualified Name)
addNameIfHashOnly codebase perspective hqQuery rootCausal = case hqQuery of
  HQ.HashOnly sh -> do
    let rootBranchHash = V2Causal.valueHash rootCausal
    let pathSegments = coerce $ Path.toList perspective
    let findTerm = do
          termRefs <- lift $ termReferentsByShortHash codebase sh
          termRefs
            & altMap \ref -> do
              MaybeT $ Ops.recursiveTermNameSearch pathSegments rootBranchHash (CV.referent1to2 ref)
    let findType = do
          typeRefs <- lift $ typeReferencesByShortHash sh
          typeRefs
            & altMap \ref -> do
              MaybeT $ Ops.recursiveTypeNameSearch pathSegments rootBranchHash (Cv.reference1to2 ref)
    mayReversedName <- runMaybeT $ findTerm <|> findType
    case mayReversedName of
      Nothing -> pure hqQuery
      Just (ReversedName reversedName) -> pure $ HQ.NameOnly (Name.fromReverseSegments $ coerce reversedName)
  _ -> pure hqQuery

renderDocRefs ::
  PPEDBuilder ->
  Width ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  [TermReference] ->
  Backend IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
renderDocRefs _ppedBuilder _width _codebase _rt [] = pure []
renderDocRefs ppedBuilder width codebase rt docRefs = do
  eDocs <- for docRefs \ref -> (ref,) <$> liftIO (Backend.evalDocRef rt codebase ref)
  let docDeps = foldMap (Doc.dependencies . snd) eDocs <> Set.fromList (LD.TermReference <$> docRefs)
  docsPPED <- ppedBuilder docDeps
  for eDocs \(ref, eDoc) -> do
    let name = bestNameForTerm @Symbol (PPED.suffixifiedPPE docsPPED) width (Referent.Ref ref)
    let hash = Reference.toText ref
    let renderedDoc = Doc.renderDoc docsPPED eDoc
    pure (name, hash, renderedDoc)

type PPEDBuilder = Set LD.LabeledDependency -> Backend IO PPED.PrettyPrintEnvDecl

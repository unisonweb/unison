module Unison.Server.Share.Definitions (definitionForHQName) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Projects as Projects
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Debug as Debug
import qualified Unison.HashQualified as HQ
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import Unison.NameSegment (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Sqlite as PPESqlite
import Unison.Reference (Reference, TermReference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Server.Backend hiding (renderDocRefs)
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.Doc as Doc
import qualified Unison.Server.NameSearch.Sqlite as SqliteNameSearch
import Unison.Server.Types
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.HashQualified as HQ (toText)
import qualified Unison.Syntax.HashQualified' as HQ' (toText)
import Unison.Util.AnnotatedText (AnnotatedText)
import Unison.Util.Pretty (Width)
import qualified Unison.Util.SyntaxText as UST

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
    relocateToProjectRoot perspective perspectiveQuery shallowBranch >>= \case
      Left err -> pure $ Left err
      Right (namesRoot, locatedQuery) -> pure $ Right (shallowRoot, namesRoot, locatedQuery)
  (shallowRoot, namesRoot, query) <- either throwError pure result
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our namesRoot but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  -- ppe which returns names fully qualified to the current namesRoot,  not to the codebase root.
  let biases = maybeToList $ HQ.toName query
  let rootBranchHash = V2Causal.valueHash shallowRoot
  let ppedBuilder deps = liftIO . Codebase.runTransaction codebase $ PPESqlite.ppedForReferences rootBranchHash namesRoot deps

  Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: building names search"
  let nameSearch = SqliteNameSearch.scopedNameSearch codebase rootBranchHash namesRoot
  (dr@(DefinitionResults terms types misses), branchAtPath) <- liftIO $ Codebase.runTransaction codebase do
    Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: starting search"
    dr <- definitionsBySuffixes codebase nameSearch DontIncludeCycles [query]
    Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: finished search"
    causalAtPath <- Codebase.getShallowCausalAtPath namesRoot (Just shallowRoot)
    branchAtPath <- V2Causal.value causalAtPath
    pure (dr, branchAtPath)
  Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: Done building definition results"
  let width = mayDefaultWidth renderWidth
  let docResults :: Name -> Backend IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults name = do
        docRefs <- liftIO $ docsForTermName codebase nameSearch name
        renderDocRefs ppedBuilder width codebase rt docRefs
  let mkTermDefinition ::
        PPED.PrettyPrintEnvDecl ->
        Reference ->
        DisplayObject
          (AnnotatedText (UST.Element Reference))
          (AnnotatedText (UST.Element Reference)) ->
        Backend IO TermDefinition
      mkTermDefinition termPPED r tm = do
        let referent = Referent.Ref r
        ts <- liftIO (Codebase.runTransaction codebase (Codebase.getTypeOfTerm codebase r))
        let hqTermName = PPE.termNameOrHashOnly fqnTermPPE referent
        let bn = bestNameForTerm @Symbol (PPED.suffixifiedPPE termPPED) width (Referent.Ref r)
        tag <-
          lift
            ( termEntryTag
                <$> termListEntry codebase branchAtPath (ExactName (NameSegment bn) (Cv.referent1to2 referent))
            )
        renderedDocs <- (maybe (pure []) docResults (HQ.toName hqTermName))
        mk renderedDocs ts bn tag
        where
          fqnTermPPE = PPED.unsuffixifiedPPE termPPED
          mk _ Nothing _ _ = throwError $ MissingSignatureForTerm r
          mk docs (Just typeSig) bn tag = do
            -- We don't ever display individual constructors (they're shown as part of their
            -- type), so term references are never constructors.
            let referent = Referent.Ref r
            pure $
              TermDefinition
                (HQ'.toText <$> PPE.allTermNames fqnTermPPE referent)
                bn
                tag
                (bimap mungeSyntaxText mungeSyntaxText tm)
                (formatSuffixedType termPPED width typeSig)
                docs
  let mkTypeDefinition ::
        ( PPED.PrettyPrintEnvDecl ->
          Reference ->
          DisplayObject
            (AnnotatedText (UST.Element Reference))
            (AnnotatedText (UST.Element Reference)) ->
          Backend IO TypeDefinition
        )
      mkTypeDefinition pped r tp = do
        let hqTypeName = PPE.typeNameOrHashOnly fqnPPE r
        let bn = bestNameForType @Symbol (PPED.suffixifiedPPE pped) width r
        Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: getting type tag"
        tag <-
          liftIO $ Codebase.runTransaction codebase do
            typeEntryTag <$> typeListEntry codebase branchAtPath (ExactName (NameSegment bn) r)
        Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: getting type tag"
        docs <- (maybe (pure []) docResults (HQ.toName hqTypeName))
        pure $
          TypeDefinition
            (HQ'.toText <$> PPE.allTypeNames fqnPPE r)
            bn
            tag
            (bimap mungeSyntaxText mungeSyntaxText tp)
            docs
        where
          fqnPPE = PPED.unsuffixifiedPPE pped

  let drDeps = definitionResultsDependencies $ Debug.debugLog Debug.Server "prettyDefinitionsForHQName: Computing definition result deps" dr
  Debug.debugLogM Debug.Server $ "prettyDefinitionsForHQName: building term and type pped. Num deps: " <> show (Set.size drDeps)
  termAndTypePPED <- PPED.biasTo biases <$> ppedBuilder drDeps
  Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: building type definitions"
  typeDefinitions <-
    Map.traverseWithKey (mkTypeDefinition termAndTypePPED) $
      typesToSyntax suffixifyBindings width termAndTypePPED types
  Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: building term definitions"
  termDefinitions <-
    Map.traverseWithKey (mkTermDefinition termAndTypePPED) $
      termsToSyntax suffixifyBindings width termAndTypePPED terms
  Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: Rendering definitions"
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $
    DefinitionDisplayResults
      renderedDisplayTerms
      renderedDisplayTypes
      renderedMisses

-- | Given an arbitrary query and perspective, find the project root the query belongs in,
-- then return that root and the query relocated to that project root.
relocateToProjectRoot :: Path -> HQ.HashQualified Name -> V2Branch.Branch Sqlite.Transaction -> Sqlite.Transaction (Either BackendError (Path, HQ.HashQualified Name))
relocateToProjectRoot perspective query rootBranch = do
  let queryLocation = HQ.toName query & maybe perspective \name -> perspective <> Path.fromName name
  -- Names should be found from the project root of the queried name
  (Projects.inferNamesRoot queryLocation rootBranch) >>= \case
    Nothing -> do
      Debug.debugLogM Debug.Temp "prettyDefinitionsForHQName: No project root inferred"
      pure $ Right (perspective, query)
    Just projectRoot ->
      case Debug.debug Debug.Temp "prettyDefinitionsForHQName: longestPathPrefix" $ Path.longestPathPrefix perspective projectRoot of
        -- The perspective is equal to the project root
        (_sharedPrefix, Path.Empty, Path.Empty) -> do
          Debug.debugM Debug.Temp "prettyDefinitionsForHQName: perspective == ProjectRoot" (perspective, query)
          pure $ Right (perspective, query)
        -- The perspective is _outside_ of the project containing the query
        (_sharedPrefix, Path.Empty, remainder) -> do
          -- Since the project root is lower down we need to strip the part of the prefix
          -- which is now redundant.

          pure . Right $ Debug.debug Debug.Temp "prettyDefinitionsForHQName: namesRoot outside of project" $ (projectRoot, query <&> \n -> fromMaybe n $ Path.unprefixName (Path.Absolute remainder) n)
        -- The namesRoot is _inside_ of the project containing the query
        (_sharedPrefix, remainder, Path.Empty) -> do
          -- Since the project is higher up, we need to prefix the query
          -- with the remainder of the path
          pure . Right $ Debug.debug Debug.Temp "prettyDefinitionsForHQName: project outside of namesRoot" $ (projectRoot, query <&> Path.prefixName (Path.Absolute remainder))
        -- The namesRoot and project root are disjoint, this shouldn't ever happen.
        (_, _, _) -> pure $ Left (DisjointProjectAndPerspective perspective projectRoot)

renderDocRefs ::
  PPEDBuilder ->
  Width ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  [TermReference] ->
  Backend IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
renderDocRefs _ppedBuilder _width _codebase _rt [] = pure []
renderDocRefs ppedBuilder width codebase rt docRefs = do
  Debug.debugLogM Debug.Server $ "Evaluating docs"
  eDocs <- for docRefs \ref -> (ref,) <$> liftIO (Backend.evalDocRef rt codebase ref)
  let docDeps = foldMap (Doc.dependencies . snd) eDocs <> Set.fromList (LD.TermReference <$> docRefs)
  Debug.debugLogM Debug.Server $ "Building pped for doc refs: " <> show (Set.size docDeps)
  docsPPED <- ppedBuilder docDeps
  Debug.debugLogM Debug.Server $ "Done building pped"
  for eDocs \(ref, eDoc) -> do
    let name = bestNameForTerm @Symbol (PPED.suffixifiedPPE docsPPED) width (Referent.Ref ref)
    let hash = Reference.toText ref
    let renderedDoc = Doc.renderDoc docsPPED eDoc
    pure (name, hash, renderedDoc)

type PPEDBuilder = Set LD.LabeledDependency -> Backend IO PPED.PrettyPrintEnvDecl

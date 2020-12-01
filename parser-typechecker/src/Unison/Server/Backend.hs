{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Backend where

import Control.Monad.Except (ExceptT (..), throwError)
import Data.Bifunctor (first)
import Data.List.Extra (sort)
import qualified Data.Map as Map
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.DataDeclaration as DD
import Unison.HashQualified' as HQ'
    ( fromName,
      fromNamedReference,
      fromNamedReferent,
      take,
      HQSegment )
import Unison.Name as Name ( unsafeFromText )
import Unison.NameSegment (NameSegment)
import qualified Unison.Names2 as Names
import Unison.Names3
  ( Names (..),
    Names0,
  )
import qualified Unison.Names3 as Names3
import Unison.Parser (Ann)
import Unison.Prelude
    ( toList, Generic, fromMaybe, for, MonadTrans(lift) )
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Type (Type)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Star3 as Star3
import Unison.Var (Var)

data ShallowListEntry v a
  = ShallowTermEntry Referent HQ'.HQSegment (Maybe (Type v a))
  | ShallowTypeEntry Reference HQ'.HQSegment
  | ShallowBranchEntry NameSegment Int -- number of child definitions
  | ShallowPatchEntry NameSegment
  deriving (Eq, Ord, Show, Generic)

data BackendError
  = NoSuchNamespace Path.Absolute
  | BadRootBranch Codebase.GetRootBranchError

type Backend m a = ExceptT BackendError m a

-- implementation detail of basicParseNames0 and basicPrettyPrintNames0
basicNames0' :: Branch m -> Path -> (Names0, Names0)
basicNames0' root path = (parseNames00, prettyPrintNames00)
  where
    root0 = Branch.head root
    currentBranch = fromMaybe Branch.empty $ Branch.getAt path root
    absoluteRootNames0 = Names3.makeAbsolute0 (Branch.toNames0 root0)
    currentBranch0 = Branch.head currentBranch
    currentPathNames0 = Branch.toNames0 currentBranch0
    -- all names, but with local names in their relative form only, rather
    -- than absolute; external names appear as absolute
    currentAndExternalNames0 =
      currentPathNames0
        `Names3.unionLeft0` absDot externalNames
      where
        absDot = Names.prefix0 (Name.unsafeFromText "")
        externalNames = rootNames `Names.difference` pathPrefixed currentPathNames0
        rootNames = Branch.toNames0 root0
        pathPrefixed = case path of
          Path.Path (toList -> []) -> id
          p -> Names.prefix0 (Path.toName p)
    -- parsing should respond to local and absolute names
    parseNames00 = currentPathNames0 <> absoluteRootNames0
    -- pretty-printing should use local names where available
    prettyPrintNames00 = currentAndExternalNames0

basicSuffixifiedNames :: Int -> Branch m -> Path -> PPE.PrettyPrintEnv
basicSuffixifiedNames hashLength root path =
  let names0 = basicPrettyPrintNames0 root path
   in PPE.suffixifiedPPE . PPE.fromNamesDecl hashLength $ Names names0 mempty

basicPrettyPrintNames0 :: Branch m -> Path -> Names0
basicPrettyPrintNames0 root = snd . basicNames0' root

loadReferentType ::
  (Applicative m, Var v) =>
  Codebase m v Ann ->
  Referent ->
  m (Maybe (Type v Ann))
loadReferentType codebase = \case
  Referent.Ref r -> Codebase.getTypeOfTerm codebase r
  Referent.Con r cid _ -> getTypeOfConstructor r cid
  where
    getTypeOfConstructor (Reference.DerivedId r) cid = do
      maybeDecl <- Codebase.getTypeDeclaration codebase r
      pure $ case maybeDecl of
        Nothing -> Nothing
        Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
    getTypeOfConstructor r cid =
      error $
        "Don't know how to getTypeOfConstructor "
          ++ show r
          ++ " "
          ++ show cid

findShallow ::
  (Monad m, Var v) =>
  Codebase m v Ann ->
  Path.Absolute ->
  Backend m [ShallowListEntry v Ann]
findShallow codebase path' = do
  let path = Path.unabsolute path'
  hashLength <- lift $ Codebase.hashLength codebase
  root <- ExceptT . (first BadRootBranch <$>) $ Codebase.getRootBranch codebase
  b0 <-
    maybe (throwError . NoSuchNamespace $ Path.Absolute path) pure $
      Branch.head
        <$> Branch.getAt path root
  let hqTerm b0 ns r =
        let refs = Star3.lookupD1 ns . Branch._terms $ b0
         in case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLength $ HQ'.fromNamedReferent ns r
      hqType b0 ns r =
        let refs = Star3.lookupD1 ns . Branch._types $ b0
         in case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLength $ HQ'.fromNamedReference ns r
      defnCount b =
        (R.size . Branch.deepTerms $ Branch.head b)
          + (R.size . Branch.deepTypes $ Branch.head b)
  termEntries <- for (R.toList . Star3.d1 $ Branch._terms b0) $ \(r, ns) -> do
    ot <- lift $ loadReferentType codebase r
    pure $ ShallowTermEntry r (hqTerm b0 ns r) ot
  let typeEntries =
        [ ShallowTypeEntry r (hqType b0 ns r)
          | (r, ns) <- R.toList . Star3.d1 $ Branch._types b0
        ]
      branchEntries =
        [ ShallowBranchEntry ns (defnCount b)
          | (ns, b) <- Map.toList $ Branch._children b0
        ]
      patchEntries =
        [ ShallowPatchEntry ns
          | (ns, (_h, _mp)) <- Map.toList $ Branch._edits b0
        ]
  pure . sort $ termEntries ++ typeEntries ++ branchEntries ++ patchEntries

typeReferencesByShortHash codebase sh = do
  fromCodebase <- Codebase.typeReferencesByPrefix codebase sh
  let fromBuiltins = Set.filter (\r -> sh == Reference.toShortHash r)
                                B.intrinsicTypeReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

termReferencesByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferencesByPrefix codebase sh
  let fromBuiltins = Set.filter (\r -> sh == Reference.toShortHash r)
                                B.intrinsicTermReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

termReferentsByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferentsByPrefix codebase sh
  let fromBuiltins = Set.map Referent.Ref $ Set.filter
        (\r -> sh == Reference.toShortHash r)
        B.intrinsicTermReferences
  pure (fromBuiltins <> Set.map (fmap Reference.DerivedId) fromCodebase)

hqNameQuery' doSuffixify hqs = do
  let (hqnames, hashes) = partition (isJust . HQ.toName) hqs
  termRefs <- filter (not . Set.null . snd) . zip hashes <$> traverse
    (eval . TermReferentsByShortHash)
    (catMaybes (HQ.toHash <$> hashes))
  typeRefs <- filter (not . Set.null . snd) . zip hashes <$> traverse
    (eval . TypeReferencesByShortHash)
    (catMaybes (HQ.toHash <$> hashes))
  parseNames0 <- makeHistoricalParsingNames $ Set.fromList hqnames
  let
    mkTermResult n r = SR.termResult (HQ'.fromHQ' n) r Set.empty
    mkTypeResult n r = SR.typeResult (HQ'.fromHQ' n) r Set.empty
    termResults =
      (\(n, tms) -> (n, toList $ mkTermResult n <$> toList tms)) <$> termRefs
    typeResults =
      (\(n, tps) -> (n, toList $ mkTypeResult n <$> toList tps)) <$> typeRefs
    parseNames = (if doSuffixify then Names3.suffixify else id) parseNames0
    resultss   = searchBranchExact hqLength parseNames hqnames
    missingRefs =
      [ x
      | x <- hashes
      , isNothing (lookup x termRefs) && isNothing (lookup x typeRefs)
      ]
    (misses, hits) =
      partition (\(_, results) -> null results) (zip hqs resultss)
    results =
      List.sort
        .   uniqueBy SR.toReferent
        $   (hits ++ termResults ++ typeResults)
        >>= snd
  pure (missingRefs ++ (fst <$> misses), results)

hqNameQuery = hqNameQuery' False

hqNameQuerySuffixify :: [HQ.HashQualified Name] -> QueryResult
hqNameQuerySuffixify = hqNameQuery' True

data DefinitionDisplayResults =
  DefinitionDisplayResults
    { termDefinitions :: Map Reference (DisplayObject SyntaxText)
    , typeDefinitions :: Map Reference (DisplayObject SyntaxText)
    , missingDefinitions :: HQ.HashQualified Name
    } deriving (Eq, Ord, Show, Generic)

definitionsBySuffixes
  :: Codebase m v Ann -> [HQ.HashQualified Name] -> m DefinitionResults
findBySuffixes codebase = do
  -- First find the hashes by name and note any query misses.
  (misses, results) <- hqNameQuerySuffixify query
  -- Now load the terms/types for those hashes.
  results'          <- loadSearchResults results
  let termTypes :: Map.Map Reference (Type v Ann)
      termTypes = Map.fromList
        [ (r, t) | SR'.Tm _ (Just t) (Referent.Ref r) _ <- results' ]
      (collatedTypes, collatedTerms) = collateReferences
        (mapMaybe SR'.tpReference results')
        (mapMaybe SR'.tmReferent results')
  -- load the `collatedTerms` and types into a Map Reference.Id Term/Type
  -- for later
  loadedDerivedTerms <-
    fmap (Map.fromList . catMaybes) . for (toList collatedTerms) $ \case
      Reference.DerivedId i -> fmap (i, ) <$> Codebase.getTerm codebase i
      Reference.Builtin{}   -> pure Nothing
  loadedDerivedTypes <-
    fmap (Map.fromList . catMaybes) . for (toList collatedTypes) $ \case
      Reference.DerivedId i ->
        fmap (i, ) <$> Codebase.getTypeDeclaration codebase i
      Reference.Builtin{} -> pure Nothing
  -- Populate DisplayObjects for the search results, in anticipation of
  -- rendering the definitions.
  loadedDisplayTerms :: Map Reference (DisplayObject (Term v Ann)) <-
    fmap Map.fromList . for (toList collatedTerms) $ \case
      r@(Reference.DerivedId i) -> do
        let tm = Map.lookup i loadedDerivedTerms
        -- We add a type annotation to the term using if it doesn't
        -- already have one that the user provided
        pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
          Nothing        -> MissingObject i
          Just (tm, typ) -> case tm of
            Term.Ann' _ _ -> UserObject tm
            _             -> UserObject (Term.ann (ABT.annotation tm) tm typ)
      r@(Reference.Builtin _) -> pure (r, BuiltinObject)
  let loadedDisplayTypes :: Map Reference (DisplayObject (DD.Decl v Ann))
      loadedDisplayTypes = Map.fromList . (`fmap` toList collatedTypes) $ \case
        r@(Reference.DerivedId i) ->
          (r, ) . maybe (MissingObject i) UserObject $ Map.lookup
            i
            loadedDerivedTypes
        r@(Reference.Builtin _) -> (r, BuiltinObject)
  let deps =
        foldMap SR'.labeledDependencies results'
          <> foldMap Term.labeledDependencies loadedDerivedTerms
  -- Now let's pretty-print
  printNames <- makePrintNamesFromLabeled' deps
  -- We might like to make sure that the user search terms get used as
  -- the names in the pretty-printer, but the current implementation
  -- doesn't.
  ppe        <- prettyPrintEnvDecl printNames
  let renderedDisplayTerms = termsToSyntax ppe loadedDisplayTerms
      renderedDisplayTypes = typesToSyntax ppe loadedDisplayTypes
  pure $ DefinitionDisplayResults renderedDisplayTerms
                                  renderedDisplayTypes
                                  misses

termsToSyntax
  :: Var v
  => Ord a
  => Applicative m
  => PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayObject (Term v a))
  -> Map
       Reference.Reference
       (DisplayObject (SyntaxText' ShortHash))
termsToSyntax ppe0 terms = Map.fromList . map go . Map.toList $ Map.mapKeys
  (first (PPE.termName ppeDecl . Referent.Ref) . dupe)
  terms
 where
  ppeDecl = PPE.unsuffixifiedPPE ppe0
  go ((n, r), dt) = (r, TermPrinter.prettyBinding (ppeBody r) n <$> dt)

typesToSyntax
  :: Var v
  => Ord a
  -> PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayObject (DD.Decl v a1))
  -> Map Reference.Reference (DisplayObject (SyntaxText' ShortHash))
typesToSyntax ppe0 types = map go . Map.toList $ Map.mapKeys
  (first (PPE.typeName ppeDecl) . dupe)
  types
 where
  go2 ((n, r), dt) =
    ( r
    , (\case
        Left  d -> DeclPrinter.prettyEffectDecl (ppeBody r) r n d
        Right d -> DeclPrinter.prettyDataDecl (ppeBody r) r n d
      )
      <$> dt
    )

loadSearchResults
  :: (Var v, Applicative m)
  => Codebase m v Ann
  -> [SR.SearchResult]
  -> m [SearchResult' v Ann]
loadSearchResults c = traverse loadSearchResult
 where
  loadSearchResult = \case
    SR.Tm (SR.TermResult name r aliases) -> do
      typ <- lift $ loadReferentType c r
      pure $ SR'.Tm name typ r aliases
    SR.Tp (SR.TypeResult name r aliases) -> do
      dt <- loadTypeDisplayObject c r
      pure $ SR'.Tp name dt r aliases

loadTypeDisplayObject
  :: Codebase m v Ann -> Reference -> m (DisplayObject (DD.Decl v Ann))
loadTypeDisplayObject c = \case
  Reference.Builtin _ -> pure BuiltinObject
  Reference.DerivedId id ->
    maybe (MissingObject id) UserObject <$> Codebase.getTypeDeclaration c id


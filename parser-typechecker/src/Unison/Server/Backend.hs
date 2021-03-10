{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Backend where

import           Control.Error.Util             ( (??) )
import           Control.Monad.Except           ( ExceptT(..)
                                                , throwError
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Tuple.Extra               ( dupe )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Unison.Builtin                as B
import qualified Unison.Builtin.Decls          as Decls
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Path           ( Path )
import           Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Metadata      as Metadata
import qualified Unison.Codebase.Path          as Path
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Server.SearchResult    as SR
import qualified Unison.Server.SearchResult'   as SR'
import qualified Unison.ABT                    as ABT
import           Unison.Term                    ( Term )
import qualified Unison.Term                   as Term
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import           Unison.Name                   as Name
                                                ( unsafeFromText )
import           Unison.NameSegment             ( NameSegment )
import qualified Unison.NameSegment            as NameSegment
import qualified Unison.Names2                 as Names
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.Names3                  ( Names(..)
                                                , Names0
                                                )
import qualified Unison.Names3                 as Names3
import           Unison.Parser                  ( Ann )
import           Unison.Prelude
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Util.Pretty            as Pretty
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Type                    ( Type )
import qualified Unison.Type                   as Type
import qualified Unison.Typechecker            as Typechecker
import qualified Unison.Util.Relation          as R
import qualified Unison.Util.Star3             as Star3
import           Unison.Var                     ( Var )
import           Unison.Server.Types
import           Unison.Server.QueryResult
import           Unison.Util.SyntaxText         ( SyntaxText )
import qualified Unison.Util.SyntaxText        as SyntaxText
import           Unison.Util.List               ( uniqueBy )
import           Unison.ShortHash
import qualified Unison.Codebase.ShortBranchHash
                                               as SBH
import           Unison.Codebase.ShortBranchHash
                                                ( ShortBranchHash )
import qualified Unison.TermPrinter            as TermPrinter
import qualified Unison.TypePrinter            as TypePrinter
import qualified Unison.DeclPrinter            as DeclPrinter
import           Unison.Util.Pretty             ( Width )
import qualified Data.Text                     as Text
import qualified Unison.Server.Syntax          as Syntax

data TermTag = Doc | Test
  deriving (Eq, Ord, Show, Generic)

data TypeTag = Ability | Data
  deriving (Eq, Ord, Show, Generic)

data ShallowListEntry v a
  = ShallowTermEntry Referent HQ'.HQSegment (Maybe (Type v a)) (Maybe TermTag)
  | ShallowTypeEntry Reference HQ'.HQSegment TypeTag
  -- The integer here represents the number of children
  | ShallowBranchEntry NameSegment ShortBranchHash Int
  | ShallowPatchEntry NameSegment
  deriving (Eq, Ord, Show, Generic)

listEntryName :: ShallowListEntry v a -> Text
listEntryName = \case
  ShallowTermEntry _ s _ _ -> HQ'.toText s
  ShallowTypeEntry   _ s _ -> HQ'.toText s
  ShallowBranchEntry n _ _ -> NameSegment.toText n
  ShallowPatchEntry n      -> NameSegment.toText n

data BackendError
  = NoSuchNamespace Path.Absolute
  | BadRootBranch Codebase.GetRootBranchError
  | CouldntExpandBranchHash ShortBranchHash
  | AmbiguousBranchHash ShortBranchHash (Set ShortBranchHash)
  | NoBranchForHash Branch.Hash
  | MissingSignatureForTerm Reference

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
          Path.Path (toList -> []) -> const mempty
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

basicParseNames0 :: Branch m -> Path -> Names0
basicParseNames0 root = fst . basicNames0' root

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

getRootBranch :: Functor m => Codebase m v Ann -> Backend m (Branch m)
getRootBranch =
  ExceptT . (first BadRootBranch <$>) . Codebase.getRootBranch

-- List the immediate children of a namespace
findShallow
  :: (Monad m, Var v)
  => Codebase m v Ann
  -> Path.Absolute
  -> Backend m [ShallowListEntry v Ann]
findShallow codebase path' = do
  let path = Path.unabsolute path'
  root <- getRootBranch codebase
  let mayb = Branch.getAt path root
  case mayb of
    Nothing -> pure []
    Just b  -> findShallowInBranch codebase b

findShallowInBranch
  :: (Monad m, Var v)
  => Codebase m v Ann
  -> Branch m
  -> Backend m [ShallowListEntry v Ann]
findShallowInBranch codebase b = do
  hashLength <- lift $ Codebase.hashLength codebase
  let hqTerm b0 ns r =
        let refs = Star3.lookupD1 ns . Branch._terms $ b0
        in  case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLength $ HQ'.fromNamedReferent ns r
      hqType b0 ns r =
        let refs = Star3.lookupD1 ns . Branch._types $ b0
        in  case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLength $ HQ'.fromNamedReference ns r
      defnCount b =
        (R.size . Branch.deepTerms $ Branch.head b)
          + (R.size . Branch.deepTypes $ Branch.head b)
      b0 = Branch.head b
  termEntries <- for (R.toList . Star3.d1 $ Branch._terms b0) $ \(r, ns) -> do
    ot <- lift $ loadReferentType codebase r
    -- A term is a doc if its type conforms to the `Doc` type.
    let isDoc = case ot of
          Just t  -> Typechecker.isSubtype t $ Type.ref mempty Decls.docRef
          Nothing -> False
        -- A term is a test if it has a link of type `IsTest`.
        isTest =
          Metadata.hasMetadataWithType r (Decls.isTestRef) $ Branch._terms b0
        tag = if isDoc then Just Doc else if isTest then Just Test else Nothing
    pure $ ShallowTermEntry r (hqTerm b0 ns r) ot tag
  typeEntries <- for (R.toList . Star3.d1 $ Branch._types b0) $ \(r, ns) -> do
    -- The tag indicates whether the type is a data declaration or an ability.
    tag <- case Reference.toId r of
      Just r -> do
        decl <- lift $ Codebase.getTypeDeclaration codebase r
        pure $ case decl of
          Just (Left _) -> Ability
          _             -> Data
      _ -> pure Data
    pure $ ShallowTypeEntry r (hqType b0 ns r) tag
  let
    branchEntries =
      [ ShallowBranchEntry ns
                           (SBH.fullFromHash $ Branch.headHash b)
                           (defnCount b)
      | (ns, b) <- Map.toList $ Branch._children b0
      ]
    patchEntries =
      [ ShallowPatchEntry ns
      | (ns, (_h, _mp)) <- Map.toList $ Branch._edits b0
      ]
  pure
    .  List.sortOn listEntryName
    $  termEntries
    ++ typeEntries
    ++ branchEntries
    ++ patchEntries

termReferencesByShortHash
  :: Monad m => Codebase m v a -> ShortHash -> m (Set Reference)
typeReferencesByShortHash codebase sh = do
  fromCodebase <- Codebase.typeReferencesByPrefix codebase sh
  let fromBuiltins = Set.filter (\r -> sh == Reference.toShortHash r)
                                B.intrinsicTypeReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

typeReferencesByShortHash
  :: Monad m => Codebase m v a -> ShortHash -> m (Set Reference)
termReferencesByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferencesByPrefix codebase sh
  let fromBuiltins = Set.filter (\r -> sh == Reference.toShortHash r)
                                B.intrinsicTermReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

termReferentsByShortHash
  :: Monad m => Codebase m v a -> ShortHash -> m (Set Referent)
termReferentsByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferentsByPrefix codebase sh
  let fromBuiltins = Set.map Referent.Ref $ Set.filter
        (\r -> sh == Reference.toShortHash r)
        B.intrinsicTermReferences
  pure (fromBuiltins <> Set.map (fmap Reference.DerivedId) fromCodebase)

-- currentPathNames0 :: Path -> Names0
-- currentPathNames0 = Branch.toNames0 . Branch.head . Branch.getAt

getCurrentPrettyNames :: Path -> Branch m -> Names
getCurrentPrettyNames path root =
  Names (basicPrettyPrintNames0 root path) mempty

getCurrentParseNames :: Path -> Branch m -> Names
getCurrentParseNames path root = Names (basicParseNames0 root path) mempty

-- Any absolute names in the input which have `root` as a prefix
-- are converted to names relative to current path. All other names are
-- converted to absolute names. For example:
--
-- e.g. if currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names0 -> Names0
fixupNamesRelative root = Names3.map0 fixName where
  prefix = Path.toName $ Path.unabsolute root
  fixName n = if root == Path.absoluteEmpty
    then n
    else fromMaybe (Name.makeAbsolute n) (Name.stripNamePrefix prefix n)

-- | The output list (of lists) corresponds to the query list.
searchBranchExact
  :: Int -> Names -> [HQ.HashQualified Name] -> [[SR.SearchResult]]
searchBranchExact len names queries =
  let
    searchTypes :: HQ.HashQualified Name -> [SR.SearchResult]
    searchTypes query =
      -- a bunch of references will match a HQ ref.
      let refs = toList $ Names3.lookupHQType query names
          mayName r Nothing  = HQ'.fromNamedReference "" r
          mayName _ (Just n) = n
      in  refs <&> \r ->
            let hqNames = Names3.typeName len r names
            in
              let primaryName =
                    mayName r
                      . lastMay
                      . sortOn
                          (\n -> HQ.matchesNamedReference (HQ'.toName n) r query
                          )
                      $ toList hqNames
              in  let aliases = Set.delete primaryName hqNames
                  in  SR.typeResult primaryName r aliases
    searchTerms :: HQ.HashQualified Name -> [SR.SearchResult]
    searchTerms query =
      -- a bunch of references will match a HQ ref.
      let refs = toList $ Names3.lookupHQTerm query names
          mayName r Nothing  = HQ'.fromNamedReferent "" r
          mayName _ (Just n) = n
      in  refs <&> \r ->
            let hqNames = Names3.termName len r names
            in  let primaryName =
                        mayName r
                          . lastMay
                          . sortOn
                              (\n ->
                                HQ.matchesNamedReferent (HQ'.toName n) r query
                              )
                          $ toList hqNames
                in  let aliases = Set.delete primaryName hqNames
                    in  SR.termResult primaryName r aliases
  in
    [ searchTypes q <> searchTerms q | q <- queries ]

hqNameQuery'
  :: Monad m
  => Bool
  -> Maybe Path
  -> Branch m
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> m QueryResult
hqNameQuery' doSuffixify relativeTo root codebase hqs = do
  -- Split the query into hash-only and hash-qualified-name queries.
  let (hqnames, hashes) = List.partition (isJust . HQ.toName) hqs
  -- Find the terms with those hashes.
  termRefs <- filter (not . Set.null . snd) . zip hashes <$> traverse
    (termReferentsByShortHash codebase)
    (catMaybes (HQ.toHash <$> hashes))
  -- Find types with those hashes.
  typeRefs <- filter (not . Set.null . snd) . zip hashes <$> traverse
    (typeReferencesByShortHash codebase)
    (catMaybes (HQ.toHash <$> hashes))
  -- Now do the name queries.
  -- The hq-name search needs a hash-qualifier length
  hqLength <- Codebase.hashLength codebase
  -- We need to construct the names that we want to use / search by.
  let currentPath = fromMaybe Path.empty relativeTo
      parseNames0 = getCurrentParseNames currentPath root
      mkTermResult n r = SR.termResult (HQ'.fromHQ' n) r Set.empty
      mkTypeResult n r = SR.typeResult (HQ'.fromHQ' n) r Set.empty
      -- Transform the hash results a bit
      termResults =
        (\(n, tms) -> (n, toList $ mkTermResult n <$> toList tms)) <$> termRefs
      typeResults =
        (\(n, tps) -> (n, toList $ mkTypeResult n <$> toList tps)) <$> typeRefs
      -- Suffixify the names
      parseNames = (if doSuffixify then Names3.suffixify else id) parseNames0
      -- Now do the actual name query
      resultss   = searchBranchExact hqLength parseNames hqnames
      -- Handle query misses correctly
      missingRefs =
        [ x
        | x <- hashes
        , isNothing (lookup x termRefs) && isNothing (lookup x typeRefs)
        ]
      (misses, hits) =
        List.partition (\(_, results) -> null results) (zip hqs resultss)
      -- Gather the results
      results =
        List.sort
          .   uniqueBy SR.toReferent
          $   (hits ++ termResults ++ typeResults)
          >>= snd
  pure $ QueryResult (missingRefs ++ (fst <$> misses)) results

hqNameQuery
  :: Monad m
  => Maybe Path
  -> Branch m
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> m QueryResult
hqNameQuery = hqNameQuery' False

hqNameQuerySuffixify
  :: Monad m
  => Maybe Path
  -> Branch m
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> m QueryResult
hqNameQuerySuffixify = hqNameQuery' True

-- TODO: Move this to its own module
data DefinitionResults v =
  DefinitionResults
    { termResults :: Map Reference (DisplayObject (Term v Ann))
    , typeResults :: Map Reference (DisplayObject (DD.Decl v Ann))
    , noResults :: [HQ.HashQualified Name]
    }

-- Separates type references from term references and returns types and terms,
-- respectively. For terms that are constructors, turns them into their data
-- types.
collateReferences
  :: Foldable f
  => Foldable g
  => f Reference -- types requested
  -> g Referent -- terms requested, including ctors
  -> (Set Reference, Set Reference)
collateReferences (toList -> types) (toList -> terms) =
  let terms' = [ r | Referent.Ref r <- terms ]
      types' = [ r | Referent.Con r _ _ <- terms ]
  in  (Set.fromList types' <> Set.fromList types, Set.fromList terms')

expandShortBranchHash
  :: Monad m => Codebase m v a -> ShortBranchHash -> Backend m Branch.Hash
expandShortBranchHash codebase hash = do
  hashSet <- lift $ Codebase.branchHashesByPrefix codebase hash
  len     <- lift $ Codebase.branchHashLength codebase
  case Set.toList hashSet of
    []  -> throwError $ CouldntExpandBranchHash hash
    [h] -> pure h
    _ ->
      throwError . AmbiguousBranchHash hash $ Set.map (SBH.fromHash len) hashSet

prettyType
  :: Var v
  => Width
  -> PPE.PrettyPrintEnvDecl
  -> Type v Ann
  -> Syntax.SyntaxText
prettyType width ppe =
  mungeSyntaxText . Pretty.render width . TypePrinter.pretty0
    (PPE.suffixifiedPPE ppe)
    mempty
    (-1)

mungeSyntaxText
  :: Functor g => g (SyntaxText.Element Reference) -> g Syntax.Element
mungeSyntaxText = fmap Syntax.convertElement

prettyDefinitionsBySuffixes
  :: forall v m
   . Monad m
  => Var v
  => Maybe Path
  -> Maybe Branch.Hash
  -> Maybe Width
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> Backend m DefinitionDisplayResults
prettyDefinitionsBySuffixes relativeTo root renderWidth codebase query = do
  branch                               <- resolveBranchHash root codebase
  DefinitionResults terms types misses <- definitionsBySuffixes relativeTo
                                                                branch
                                                                codebase
                                                                query
  hqLength <- lift $ Codebase.hashLength codebase
  -- We might like to make sure that the user search terms get used as
  -- the names in the pretty-printer, but the current implementation
  -- doesn't.
  let
    printNames = getCurrentPrettyNames (fromMaybe Path.empty relativeTo) branch
    parseNames = getCurrentParseNames (fromMaybe Path.empty relativeTo) branch
    ppe        = PPE.fromNamesDecl hqLength printNames
    width      = mayDefault renderWidth
    termFqns :: Map Reference (Set Text)
    termFqns = Map.mapWithKey f terms
     where
      f k _ =
        R.lookupRan (Referent.Ref' k)
          . R.filterDom (\n -> "." `Text.isPrefixOf` n && n /= ".")
          . R.mapDom Name.toText
          . Names.terms
          $ currentNames parseNames
    typeFqns :: Map Reference (Set Text)
    typeFqns = Map.mapWithKey f types
     where
      f k _ =
        R.lookupRan k
          . R.filterDom (\n -> "." `Text.isPrefixOf` n && n /= ".")
          . R.mapDom Name.toText
          . Names.types
          $ currentNames parseNames
    flatten = Set.toList . fromMaybe Set.empty
    mkTermDefinition r tm = mk =<< lift (Codebase.getTypeOfTerm codebase r)
     where
      mk Nothing = throwError $ MissingSignatureForTerm r
      mk (Just typeSig) =
        pure
          . TermDefinition
              (flatten $ Map.lookup r termFqns)
              ( Text.pack
              . Pretty.render width
              . fmap SyntaxText.toPlain
              . TermPrinter.pretty0 @v (PPE.suffixifiedPPE ppe)
                                       TermPrinter.emptyAc
              $ Term.ref mempty r
              )
              (fmap mungeSyntaxText tm)
          $ prettyType width ppe typeSig
    mkTypeDefinition r tp =
      TypeDefinition
          (flatten $ Map.lookup r typeFqns)
          ( Text.pack
          . Pretty.render width
          . fmap SyntaxText.toPlain
          . TypePrinter.pretty0 @v (PPE.suffixifiedPPE ppe) mempty (-1)
          $ Type.ref () r
          )
        $ fmap mungeSyntaxText tp
    typeDefinitions =
      Map.mapWithKey mkTypeDefinition $ typesToSyntax width ppe types
  termDefinitions <- Map.traverseWithKey mkTermDefinition
    $ termsToSyntax width ppe terms
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $ DefinitionDisplayResults renderedDisplayTerms
                                  renderedDisplayTypes
                                  renderedMisses

resolveBranchHash
  :: Monad m => Maybe Branch.Hash -> Codebase m v Ann -> Backend m (Branch m)
resolveBranchHash h codebase = case h of
  Nothing    -> getRootBranch codebase
  Just bhash -> do
    mayBranch <- lift $ Codebase.getBranchForHash codebase bhash
    mayBranch ?? NoBranchForHash bhash

definitionsBySuffixes
  :: forall m v
   . Monad m
  => Var v
  => Maybe Path
  -> Branch m
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> Backend m (DefinitionResults v)
definitionsBySuffixes relativeTo branch codebase query = do
  -- First find the hashes by name and note any query misses.
  QueryResult misses results <- lift
    $ hqNameQuerySuffixify relativeTo branch codebase query
  -- Now load the terms/types for those hashes.
  results' <- lift $ loadSearchResults codebase results
  let termTypes :: Map.Map Reference (Type v Ann)
      termTypes = Map.fromList
        [ (r, t) | SR'.Tm _ (Just t) (Referent.Ref r) _ <- results' ]
      (collatedTypes, collatedTerms) = collateReferences
        (mapMaybe SR'.tpReference results')
        (mapMaybe SR'.tmReferent results')
  -- load the `collatedTerms` and types into a Map Reference.Id Term/Type
  -- for later
  loadedDerivedTerms <-
    lift $ fmap (Map.fromList . catMaybes) . for (toList collatedTerms) $ \case
      Reference.DerivedId i -> fmap (i, ) <$> Codebase.getTerm codebase i
      Reference.Builtin{}   -> pure Nothing
  loadedDerivedTypes <-
    lift $ fmap (Map.fromList . catMaybes) . for (toList collatedTypes) $ \case
      Reference.DerivedId i ->
        fmap (i, ) <$> Codebase.getTypeDeclaration codebase i
      Reference.Builtin{} -> pure Nothing
  -- Populate DisplayObjects for the search results, in anticipation of
  -- rendering the definitions.
  loadedDisplayTerms <- fmap Map.fromList . for (toList collatedTerms) $ \case
    r@(Reference.DerivedId i) -> do
      let tm = Map.lookup i loadedDerivedTerms
      -- We add a type annotation to the term using if it doesn't
      -- already have one that the user provided
      pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
        Nothing        -> MissingObject $ Reference.idToShortHash i
        Just (tm, typ) -> case tm of
          Term.Ann' _ _ -> UserObject tm
          _             -> UserObject (Term.ann (ABT.annotation tm) tm typ)
    r@(Reference.Builtin _) -> pure (r, BuiltinObject)
  let loadedDisplayTypes = Map.fromList . (`fmap` toList collatedTypes) $ \case
        r@(Reference.DerivedId i) ->
          (r, )
            . maybe (MissingObject $ Reference.idToShortHash i) UserObject
            $ Map.lookup i loadedDerivedTypes
        r@(Reference.Builtin _) -> (r, BuiltinObject)
  pure $ DefinitionResults loadedDisplayTerms loadedDisplayTypes misses

termsToSyntax
  :: Var v
  => Ord a
  => Int
  -> PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayObject (Term v a))
  -> Map Reference.Reference (DisplayObject SyntaxText)
termsToSyntax width ppe0 terms =
  Map.fromList . map go . Map.toList $ Map.mapKeys
    (first (PPE.termName ppeDecl . Referent.Ref) . dupe)
    terms
 where
  ppeBody r = PPE.declarationPPE ppe0 r
  ppeDecl = PPE.unsuffixifiedPPE ppe0
  go ((n, r), dt) =
    (r, Pretty.render width . TermPrinter.prettyBinding (ppeBody r) n <$> dt)

typesToSyntax
  :: Var v
  => Ord a
  => Int
  -> PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayObject (DD.Decl v a))
  -> Map Reference.Reference (DisplayObject SyntaxText)
typesToSyntax width ppe0 types =
  Map.fromList $ map go . Map.toList $ Map.mapKeys
    (first (PPE.typeName ppeDecl) . dupe)
    types
 where
  ppeBody r = PPE.declarationPPE ppe0 r
  ppeDecl = PPE.unsuffixifiedPPE ppe0
  go ((n, r), dt) =
    ( r
    , (\case
        Left d ->
          Pretty.render width $ DeclPrinter.prettyEffectDecl (ppeBody r) r n d
        Right d ->
          Pretty.render width $ DeclPrinter.prettyDataDecl (ppeBody r) r n d
      )
      <$> dt
    )

loadSearchResults
  :: (Var v, Applicative m)
  => Codebase m v Ann
  -> [SR.SearchResult]
  -> m [SR'.SearchResult' v Ann]
loadSearchResults c = traverse loadSearchResult
 where
  loadSearchResult = \case
    SR.Tm (SR.TermResult name r aliases) -> do
      typ <- loadReferentType c r
      pure $ SR'.Tm name typ r aliases
    SR.Tp (SR.TypeResult name r aliases) -> do
      dt <- loadTypeDisplayObject c r
      pure $ SR'.Tp name dt r aliases

loadTypeDisplayObject
  :: Applicative m
  => Codebase m v Ann
  -> Reference
  -> m (DisplayObject (DD.Decl v Ann))
loadTypeDisplayObject c = \case
  Reference.Builtin _ -> pure BuiltinObject
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> Codebase.getTypeDeclaration c id


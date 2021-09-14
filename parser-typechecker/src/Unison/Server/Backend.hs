{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Backend where

import Control.Lens (_2, over)
import Control.Error.Util ((??),hush)
import Control.Monad.Except
  ( ExceptT (..),
    throwError,
  )
import Data.Bifunctor (first,bimap)
import Data.List.Extra (nubOrd)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (dupe)
import qualified Text.FuzzyFind as FZF
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as B
import qualified Unison.Builtin.Decls as Decls
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.Runtime.IOSource as DD
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch, Branch0)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.Causal (RawHash(RawHash))
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash,
  )
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DeclPrinter
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.Name as Name
  ( unsafeFromText,
  )
import qualified Unison.Name as Name
import qualified Unison.NamePrinter as NP
import Unison.NameSegment (NameSegment(..))
import qualified Unison.NameSegment as NameSegment
import qualified Unison.Names2 as Names
import Unison.Names3
  ( Names (..),
    Names0,
  )
import qualified Unison.Names3 as Names3
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Server.QueryResult
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.Server.Syntax as Syntax
import Unison.Server.Types
import Unison.ShortHash
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TermPrinter
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Typechecker as Typechecker
import Unison.Util.List (uniqueBy)
import Unison.Util.Pretty (Width)
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Star3 as Star3
import qualified Unison.Util.SyntaxText as UST
import Unison.Var (Var)
import qualified Unison.Server.Doc as Doc
import qualified Unison.Codebase.Editor.DisplayObject as DisplayObject
import qualified Unison.WatchKind as WK
import qualified Unison.PrettyPrintEnv.Util as PPE

type SyntaxText = UST.SyntaxText' Reference

data ShallowListEntry v a
  = ShallowTermEntry (TermEntry v a)
  | ShallowTypeEntry TypeEntry
  | -- The integer here represents the number of children
    ShallowBranchEntry NameSegment ShortBranchHash Int
  | ShallowPatchEntry NameSegment
  deriving (Eq, Ord, Show, Generic)

listEntryName :: ShallowListEntry v a -> Text
listEntryName = \case
  ShallowTermEntry (TermEntry _ s _ _) -> HQ'.toText s
  ShallowTypeEntry (TypeEntry _ s _) -> HQ'.toText s
  ShallowBranchEntry n _ _ -> NameSegment.toText n
  ShallowPatchEntry n      -> NameSegment.toText n

data BackendError
  = NoSuchNamespace Path.Absolute
  | BadRootBranch Codebase.GetRootBranchError
  | CouldntExpandBranchHash ShortBranchHash
  | AmbiguousBranchHash ShortBranchHash (Set ShortBranchHash)
  | NoBranchForHash Branch.Hash
  | CouldntLoadBranch Branch.Hash
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

data TermEntry v a = TermEntry
  { termEntryReferent :: Referent,
    termEntryName :: HQ'.HQSegment,
    termEntryType :: Maybe (Type v a),
    termEntryTag :: Maybe TermTag
  }
  deriving (Eq, Ord, Show, Generic)

data TypeEntry = TypeEntry
  { typeEntryReference :: Reference,
    typeEntryName :: HQ'.HQSegment,
    typeEntryTag :: TypeTag
  }
  deriving (Eq, Ord, Show, Generic)

data FoundRef = FoundTermRef Referent
              | FoundTypeRef Reference
  deriving (Eq, Ord, Show, Generic)

-- After finding a search results with fuzzy find we do some post processing to
-- refine the result:
--  * Sort:
--      we sort both on the FZF score and the number of segments in the FQN
--      preferring shorter FQNs over longer. This helps with things like forks
--      of base.
--  * Dedupe:
--      we dedupe on the found refs to avoid having several rows of a
--      definition with different names in the result set.
fuzzyFind
  :: Monad m
  => Path
  -> Branch m
  -> String
  -> [(FZF.Alignment, UnisonName, [FoundRef])]
fuzzyFind path branch query =
  let
    printNames =
      basicPrettyPrintNames0 branch path

    fzfNames =
      Names.fuzzyFind (words query) printNames

    toFoundRef =
      fmap (fmap (either FoundTermRef FoundTypeRef) . toList)

    -- Remove dupes based on refs
    dedupe =
      nubOrdOn (\(_, _, refs) -> refs)

    -- Prefer shorter FQNs
    rank (alignment, name, _) =
      (Name.countSegments (Name.unsafeFromText name)
      , negate (FZF.score alignment)
      )

    refine =
      dedupe . sortOn rank
  in
  refine $ toFoundRef . over _2 Name.toText <$> fzfNames

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

findShallowReadmeInBranchAndRender ::
  Var v =>
  Width ->
  Rt.Runtime v ->
  Codebase IO v Ann ->
  Branch IO ->
  Backend IO (Maybe Doc.Doc)
findShallowReadmeInBranchAndRender width runtime codebase branch =
  let ppe hqLen = PPE.fromNamesDecl hqLen printNames

      printNames = getCurrentPrettyNames (Path.fromList []) branch

      renderReadme ppe r = do
        res <- renderDoc ppe width runtime codebase (Referent.toReference r)
        pure $ case res of
          (_, _, doc) : _ -> Just doc
          _ -> Nothing

      -- allow any of these capitalizations
      toCheck = NameSegment <$> ["README", "Readme", "ReadMe", "readme" ]
      readmes :: Set Referent
      readmes = foldMap lookup toCheck
        where lookup seg = R.lookupRan seg rel
              rel = Star3.d1 (Branch._terms (Branch.head branch))
   in do
        hqLen <- liftIO $ Codebase.hashLength codebase
        join <$> traverse (renderReadme (ppe hqLen)) (Set.lookupMin readmes)


termListEntry
  :: Monad m
  => Var v
  => Codebase m v Ann
  -> Branch0 m
  -> Referent
  -> HQ'.HQSegment
  -> Backend m (TermEntry v Ann)
termListEntry codebase b0 r n = do
  ot <- lift $ loadReferentType codebase r
  -- A term is a doc if its type conforms to the `Doc` type.
  let isDoc = case ot of
        Just t  -> Typechecker.isSubtype t (Type.ref mempty Decls.docRef) ||
                   Typechecker.isSubtype t (Type.ref mempty DD.doc2Ref)
        Nothing -> False
      -- A term is a test if it has a link of type `IsTest`.
      isTest =
        Metadata.hasMetadataWithType' r (Decls.isTestRef) $ Branch.deepTermMetadata b0
      tag = if isDoc then Just Doc else if isTest then Just Test else Nothing
  pure $ TermEntry r n ot tag

typeListEntry
  :: Monad m
  => Var v
  => Codebase m v Ann
  -> Reference
  -> HQ'.HQSegment
  -> Backend m TypeEntry
typeListEntry codebase r n = do
  -- The tag indicates whether the type is a data declaration or an ability.
  tag <- case Reference.toId r of
    Just r -> do
      decl <- lift $ Codebase.getTypeDeclaration codebase r
      pure $ case decl of
        Just (Left _) -> Ability
        _             -> Data
    _ -> pure Data
  pure $ TypeEntry r n tag

typeDeclHeader
  :: forall v m
   . Monad m
  => Var v
  => Codebase m v Ann
  -> PPE.PrettyPrintEnv
  -> Reference
  -> Backend m (DisplayObject Syntax.SyntaxText Syntax.SyntaxText)
typeDeclHeader code ppe r = case Reference.toId r of
  Just rid ->
    (lift $ Codebase.getTypeDeclaration code rid) <&> \case
      Nothing -> DisplayObject.MissingObject (Reference.toShortHash r)
      Just decl ->
        DisplayObject.UserObject $
          Syntax.convertElement <$>
            Pretty.render defaultWidth (DeclPrinter.prettyDeclHeader name decl)
  Nothing ->
    pure (DisplayObject.BuiltinObject (formatTypeName ppe r))
  where
    name = PPE.typeName ppe r

formatTypeName :: PPE.PrettyPrintEnv -> Reference -> Syntax.SyntaxText
formatTypeName ppe =
  fmap Syntax.convertElement . formatTypeName' ppe

formatTypeName' :: PPE.PrettyPrintEnv -> Reference -> SyntaxText
formatTypeName' ppe r =
  Pretty.renderUnbroken .
  NP.styleHashQualified id $
  PPE.typeName ppe r

termEntryToNamedTerm
  :: Var v => PPE.PrettyPrintEnv -> Maybe Width -> TermEntry v a -> NamedTerm
termEntryToNamedTerm ppe typeWidth (TermEntry r name mayType tag) = NamedTerm
  { termName = HQ'.toText name
  , termHash = Referent.toText r
  , termType = formatType ppe (mayDefaultWidth typeWidth) <$> mayType
  , termTag  = tag
  }

typeEntryToNamedType :: TypeEntry -> NamedType
typeEntryToNamedType (TypeEntry r name tag) = NamedType
  { typeName = HQ'.toText name
  , typeHash = Reference.toText r
  , typeTag  = tag
  }

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
  termEntries <- for (R.toList . Star3.d1 $ Branch._terms b0) $ \(r, ns) ->
    ShallowTermEntry <$> termListEntry codebase b0 r (hqTerm b0 ns r)
  typeEntries <- for (R.toList . Star3.d1 $ Branch._types b0)
    $ \(r, ns) -> ShallowTypeEntry <$> typeListEntry codebase r (hqType b0 ns r)
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
      let refs = toList $ Names3.lookupRelativeHQType query names
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
      let refs = toList $ Names3.lookupRelativeHQTerm query names
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

hqNameQuery
  :: Monad m
  => Maybe Path
  -> Branch m
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> m QueryResult
hqNameQuery relativeTo root codebase hqs = do
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
      parseNames = parseNames0
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

-- TODO: Move this to its own module
data DefinitionResults v =
  DefinitionResults
    { termResults :: Map Reference (DisplayObject (Type v Ann) (Term v Ann))
    , typeResults :: Map Reference (DisplayObject () (DD.Decl v Ann))
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

formatType' :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> SyntaxText
formatType' ppe w =
  Pretty.render w . TypePrinter.pretty0 ppe mempty (-1)

formatType :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> Syntax.SyntaxText
formatType ppe w = mungeSyntaxText . formatType' ppe w

formatSuffixedType
  :: Var v
  => PPE.PrettyPrintEnvDecl
  -> Width
  -> Type v Ann
  -> Syntax.SyntaxText
formatSuffixedType ppe = formatType (PPE.suffixifiedPPE ppe)

mungeSyntaxText
  :: Functor g => g (UST.Element Reference) -> g Syntax.Element
mungeSyntaxText = fmap Syntax.convertElement

prettyDefinitionsBySuffixes
  :: forall v
   . Var v
  => Maybe Path
  -> Maybe Branch.Hash
  -> Maybe Width
  -> Suffixify
  -> Rt.Runtime v
  -> Codebase IO v Ann
  -> [HQ.HashQualified Name]
  -> Backend IO DefinitionDisplayResults
prettyDefinitionsBySuffixes relativeTo root renderWidth suffixifyBindings rt codebase query
  = do
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
      printNames =
        getCurrentPrettyNames (fromMaybe Path.empty relativeTo) branch
      parseNames =
        getCurrentParseNames (fromMaybe Path.empty relativeTo) branch
      ppe   = PPE.fromNamesDecl hqLength printNames
      width = mayDefaultWidth renderWidth
      isAbsolute (Name.toText -> n) = "." `Text.isPrefixOf` n && n /= "."
      termFqns :: Map Reference (Set Text)
      termFqns = Map.mapWithKey f terms
       where
        rel = Names.terms $ currentNames parseNames
        f k _ = Set.fromList . fmap Name.toText . filter isAbsolute . toList
              $ R.lookupRan (Referent.Ref k) rel
      typeFqns :: Map Reference (Set Text)
      typeFqns = Map.mapWithKey f types
       where
        rel = Names.types $ currentNames parseNames
        f k _ = Set.fromList . fmap Name.toText . filter isAbsolute . toList
              $ R.lookupRan k rel
      flatten = Set.toList . fromMaybe Set.empty

      docNames :: Set (HQ'.HashQualified Name) -> [Name]
      docNames hqs = fmap docify . nubOrd . join . map toList . Set.toList $ hqs
        where docify n = Name.joinDot n "doc"

      selectDocs :: [Referent] -> Backend IO [Reference]
      selectDocs rs = do
        rts <- fmap join . for rs $ \case
          Referent.Ref r ->
            maybe [] (pure . (r,)) <$> lift (Codebase.getTypeOfTerm codebase r)
          _ -> pure []
        pure [ r | (r, t) <- rts, Typechecker.isSubtype t (Type.ref mempty DD.doc2Ref) ]

      -- rs0 can be empty or the term fetched, so when viewing a doc term
      -- you get both its source and its rendered form
      docResults :: [Reference] -> [Name] -> Backend IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults rs0 docs = do
        let refsFor n = Names3.lookupHQTerm (HQ.NameOnly n) parseNames
        let rs = Set.unions (refsFor <$> docs) <> Set.fromList (Referent.Ref <$> rs0)
        -- lookup the type of each, make sure it's a doc
        docs <- selectDocs (toList rs)
        -- render all the docs
        join <$> traverse (renderDoc ppe width rt codebase) docs

      mkTermDefinition r tm = do
        ts <- lift (Codebase.getTypeOfTerm codebase r)
        let bn = bestNameForTerm @v (PPE.suffixifiedPPE ppe) width (Referent.Ref r)
        tag <- termEntryTag <$> termListEntry codebase
                                              (Branch.head branch)
                                              (Referent.Ref r)
                                              (HQ'.NameOnly (NameSegment bn))
        docs <- docResults [r] $ docNames (Names3.termName hqLength (Referent.Ref r) printNames)
        mk docs ts bn tag
       where
        mk _ Nothing _ _ = throwError $ MissingSignatureForTerm r
        mk docs (Just typeSig) bn tag =
          pure $
            TermDefinition (flatten $ Map.lookup r termFqns)
                             bn
                             tag
                             (bimap mungeSyntaxText mungeSyntaxText tm)
                             (formatSuffixedType ppe width typeSig)
                             docs
      mkTypeDefinition r tp = do
        let bn = bestNameForType @v (PPE.suffixifiedPPE ppe) width r
        tag <- Just . typeEntryTag <$> typeListEntry
          codebase
          r
          (HQ'.NameOnly (NameSegment bn))
        docs <- docResults [] $ docNames (Names3.typeName hqLength r printNames)
        pure $ TypeDefinition (flatten $ Map.lookup r typeFqns)
                              bn
                              tag
                              (bimap mungeSyntaxText mungeSyntaxText tp)
                              docs
    typeDefinitions <- Map.traverseWithKey mkTypeDefinition
      $ typesToSyntax suffixifyBindings width ppe types
    termDefinitions <- Map.traverseWithKey mkTermDefinition
      $ termsToSyntax suffixifyBindings width ppe terms
    let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
        renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
        renderedMisses       = fmap HQ.toText misses
    pure $ DefinitionDisplayResults renderedDisplayTerms
                                    renderedDisplayTypes
                                    renderedMisses

renderDoc ::
  forall v.
  Var v =>
  PPE.PrettyPrintEnvDecl ->
  Width ->
  Rt.Runtime v ->
  Codebase IO v Ann ->
  Reference ->
  Backend IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
renderDoc ppe width rt codebase r = do
  let name = bestNameForTerm @v (PPE.suffixifiedPPE ppe) width (Referent.Ref r)
  let hash = Reference.toText r
  map (name,hash,) . pure
    <$> let tm = Term.ref () r
         in Doc.renderDoc @v ppe terms typeOf eval decls tm
  where
    terms r@(Reference.Builtin _) = pure (Just (Term.ref () r))
    terms (Reference.DerivedId r) =
      fmap Term.unannotate <$> lift (Codebase.getTerm codebase r)

    typeOf r = fmap void <$> lift (Codebase.getTypeOfReferent codebase r)
    eval (Term.amap (const mempty) -> tm) = do
      let ppes = PPE.suffixifiedPPE ppe
      let codeLookup = Codebase.toCodeLookup codebase
      let cache r = fmap Term.unannotate <$> Codebase.lookupWatchCache codebase r
      r <- fmap hush . liftIO $ Rt.evaluateTerm' codeLookup cache ppes rt tm
      lift $ case r of
        Just tmr ->
          Codebase.putWatch
            codebase
            WK.RegularWatch
            (Term.hashClosedTerm tm)
            (Term.amap (const mempty) tmr)
        Nothing -> pure ()
      pure $ r <&> Term.amap (const mempty)

    decls (Reference.DerivedId r) = fmap (DD.amap (const ())) <$> lift (Codebase.getTypeDeclaration codebase r)
    decls _ = pure Nothing


bestNameForTerm
  :: forall v . Var v => PPE.PrettyPrintEnv -> Width -> Referent -> Text
bestNameForTerm ppe width =
  Text.pack
    . Pretty.render width
    . fmap UST.toPlain
    . TermPrinter.pretty0 @v ppe TermPrinter.emptyAc
    . Term.fromReferent mempty

bestNameForType
  :: forall v . Var v => PPE.PrettyPrintEnv -> Width -> Reference -> Text
bestNameForType ppe width =
  Text.pack
    . Pretty.render width
    . fmap UST.toPlain
    . TypePrinter.pretty0 @v ppe mempty (-1)
    . Type.ref ()

resolveBranchHash ::
  Monad m => Maybe Branch.Hash -> Codebase m v Ann -> Backend m (Branch m)
resolveBranchHash h codebase = case h of
  Nothing -> getRootBranch codebase
  Just bhash -> do
    mayBranch <- lift $ Codebase.getBranchForHash codebase bhash
    mayBranch ?? NoBranchForHash bhash


resolveRootBranchHash ::
  Monad m => Maybe ShortBranchHash -> Codebase m v Ann -> Backend m (Branch m)
resolveRootBranchHash mayRoot codebase = case mayRoot of
  Nothing ->
    getRootBranch codebase
  Just sbh -> do
    h <- expandShortBranchHash codebase sbh
    resolveBranchHash (Just h) codebase


definitionsBySuffixes
  :: forall m v
   . (MonadIO m)
  => Var v
  => Maybe Path
  -> Branch m
  -> Codebase m v Ann
  -> [HQ.HashQualified Name]
  -> Backend m (DefinitionResults v)
definitionsBySuffixes relativeTo branch codebase query = do
  -- First find the hashes by name and note any query misses.
  QueryResult misses results <- lift
    $ hqNameQuery relativeTo branch codebase query
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
    r@(Reference.Builtin _) -> pure $ (r,) $ case Map.lookup r B.termRefTypes of
      Nothing -> MissingObject $ Reference.toShortHash r
      Just typ -> BuiltinObject (mempty <$ typ)
  let loadedDisplayTypes = Map.fromList . (`fmap` toList collatedTypes) $ \case
        r@(Reference.DerivedId i) ->
          (r, )
            . maybe (MissingObject $ Reference.idToShortHash i) UserObject
            $ Map.lookup i loadedDerivedTypes
        r@(Reference.Builtin _) -> (r, BuiltinObject ())
  pure $ DefinitionResults loadedDisplayTerms loadedDisplayTypes misses

termsToSyntax
  :: Var v
  => Ord a
  => Suffixify
  -> Width
  -> PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayObject (Type v a) (Term v a))
  -> Map Reference.Reference (DisplayObject SyntaxText SyntaxText)
termsToSyntax suff width ppe0 terms =
  Map.fromList . map go . Map.toList $ Map.mapKeys
    (first (PPE.termName ppeDecl . Referent.Ref) . dupe)
    terms
 where
  ppeBody r = if suffixified suff
    then PPE.suffixifiedPPE ppe0
    else PPE.declarationPPE ppe0 r
  ppeDecl =
    (if suffixified suff then PPE.suffixifiedPPE else PPE.unsuffixifiedPPE) ppe0
  go ((n, r), dt) = (r,) $ case dt of
    DisplayObject.BuiltinObject typ -> DisplayObject.BuiltinObject $
      formatType' (ppeBody r) width typ
    DisplayObject.MissingObject sh -> DisplayObject.MissingObject sh
    DisplayObject.UserObject tm -> DisplayObject.UserObject .
      Pretty.render width . TermPrinter.prettyBinding (ppeBody r) n $ tm

typesToSyntax
  :: Var v
  => Ord a
  => Suffixify
  -> Width
  -> PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayObject () (DD.Decl v a))
  -> Map Reference.Reference (DisplayObject SyntaxText SyntaxText)
typesToSyntax suff width ppe0 types =
  Map.fromList $ map go . Map.toList $ Map.mapKeys
    (first (PPE.typeName ppeDecl) . dupe)
    types
 where
  ppeDecl = if suffixified suff
    then PPE.suffixifiedPPE ppe0
    else PPE.unsuffixifiedPPE ppe0
  go ((n, r), dt) = (r,) $ case dt of
    BuiltinObject _ -> BuiltinObject (formatTypeName' ppeDecl r)
    MissingObject sh -> MissingObject sh
    UserObject d -> UserObject . Pretty.render width $
      DeclPrinter.prettyDecl (PPE.declarationPPEDecl ppe0 r) r n d

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
  -> m (DisplayObject () (DD.Decl v Ann))
loadTypeDisplayObject c = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> Codebase.getTypeDeclaration c id


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Backend where

import Control.Error.Util (hush)
import Control.Lens hiding ((??))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List as List
import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import Data.Text.Lazy (toStrict)
import Data.Tuple.Extra (dupe)
import qualified Data.Yaml as Yaml
import qualified Lucid
import System.Directory
import System.FilePath
import qualified Text.FuzzyFind as FZF
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import qualified U.Codebase.HashTags as V2.Hash
import qualified U.Codebase.Referent as V2
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as B
import qualified Unison.Builtin.Decls as Decls
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.Causal.Type as Causal
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Editor.DisplayObject as DisplayObject
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Rt
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash,
  )
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.DataDeclaration as DD
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Name (Name)
import Unison.Name as Name
  ( unsafeFromText,
  )
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import Unison.Names (Names (Names))
import qualified Unison.Names as Names
import qualified Unison.Names.Scoped as ScopedNames
import Unison.NamesWithHistory (NamesWithHistory (..))
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Util as PPE
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPED
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as DD
import qualified Unison.Server.Doc as Doc
import qualified Unison.Server.Doc.AsHtml as DocHtml
import Unison.Server.QueryResult
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.Server.Syntax as Syntax
import Unison.Server.Types
import Unison.ShortHash
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.NamePrinter as NP
import qualified Unison.Syntax.TermPrinter as TermPrinter
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import Unison.Util.AnnotatedText (AnnotatedText)
import Unison.Util.List (uniqueBy)
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Monoid as Monoid
import Unison.Util.Pretty (Width)
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Set as Set
import qualified Unison.Util.Star3 as Star3
import qualified Unison.Util.SyntaxText as UST
import Unison.Var (Var)
import qualified Unison.WatchKind as WK

type SyntaxText = UST.SyntaxText' Reference

data ShallowListEntry v a
  = ShallowTermEntry (TermEntry v a)
  | ShallowTypeEntry TypeEntry
  | -- The integer here represents the number of children.
    -- it may be omitted depending on the context the query is run in.
    ShallowBranchEntry NameSegment Branch.CausalHash (Maybe Int)
  | ShallowPatchEntry NameSegment
  deriving (Eq, Ord, Show, Generic)

listEntryName :: ShallowListEntry v a -> Text
listEntryName = \case
  ShallowTermEntry (TermEntry _ s _ _) -> HQ'.toText s
  ShallowTypeEntry (TypeEntry _ s _) -> HQ'.toText s
  ShallowBranchEntry n _ _ -> NameSegment.toText n
  ShallowPatchEntry n -> NameSegment.toText n

data BackendError
  = NoSuchNamespace Path.Absolute
  | -- Failed to parse path
    BadNamespace
      String
      -- ^ error message
      String
      -- ^ namespace
  | CouldntExpandBranchHash ShortBranchHash
  | AmbiguousBranchHash ShortBranchHash (Set ShortBranchHash)
  | NoBranchForHash Branch.CausalHash
  | CouldntLoadBranch Branch.CausalHash
  | MissingSignatureForTerm Reference

data BackendEnv = BackendEnv
  { -- | Whether to use the sqlite name-lookup table to generate Names objects rather than building Names from the root branch.
    useNamesIndex :: Bool
  }

newtype Backend m a = Backend {runBackend :: ReaderT BackendEnv (ExceptT BackendError m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader BackendEnv, MonadError BackendError)

instance MonadTrans Backend where
  lift m = Backend (lift . lift $ m)

suffixifyNames :: Int -> Names -> PPE.PrettyPrintEnv
suffixifyNames hashLength names =
  PPED.suffixifiedPPE . PPED.fromNamesDecl hashLength $ NamesWithHistory.fromCurrentNames names

-- implementation detail of parseNamesForBranch and prettyNamesForBranch
-- Returns (parseNames, prettyNames, localNames)
namesForBranch :: Branch m -> NameScoping -> (Names, Names, Names)
namesForBranch root scope =
  (parseNames0, prettyPrintNames0, currentPathNames)
  where
    path :: Path
    includeAllNames :: Bool
    (path, includeAllNames) = case scope of
      AllNames path -> (path, True)
      Within path -> (path, False)
    root0 = Branch.head root
    currentBranch = fromMaybe Branch.empty $ Branch.getAt path root
    absoluteRootNames = Names.makeAbsolute (Branch.toNames root0)
    currentBranch0 = Branch.head currentBranch
    currentPathNames = Branch.toNames currentBranch0
    -- all names, but with local names in their relative form only, rather
    -- than absolute; external names appear as absolute
    currentAndExternalNames =
      currentPathNames
        `Names.unionLeft` Names.mapNames Name.makeAbsolute externalNames
      where
        externalNames = rootNames `Names.difference` pathPrefixed currentPathNames
        rootNames = Branch.toNames root0
        pathPrefixed = case Path.toName path of
          Nothing -> const mempty
          Just pathName -> Names.prefix0 pathName
    -- parsing should respond to local and absolute names
    parseNames0 = currentPathNames <> Monoid.whenM includeAllNames absoluteRootNames
    -- pretty-printing should use local names where available
    prettyPrintNames0 =
      if includeAllNames
        then currentAndExternalNames
        else currentPathNames

basicSuffixifiedNames :: Int -> Branch m -> NameScoping -> PPE.PrettyPrintEnv
basicSuffixifiedNames hashLength root nameScope =
  let names0 = prettyNamesForBranch root nameScope
   in suffixifyNames hashLength names0

parseNamesForBranch :: Branch m -> NameScoping -> Names
parseNamesForBranch root = namesForBranch root <&> \(n, _, _) -> n

prettyNamesForBranch :: Branch m -> NameScoping -> Names
prettyNamesForBranch root = namesForBranch root <&> \(_, n, _) -> n

shallowPPE :: Monad m => Codebase m v a -> V2Branch.Branch m -> m PPE.PrettyPrintEnv
shallowPPE codebase b = do
  hashLength <- Codebase.hashLength codebase
  names <- shallowNames codebase b
  pure $ PPED.suffixifiedPPE . PPED.fromNamesDecl hashLength $ NamesWithHistory names mempty

-- | A 'Names' which only includes mappings for things _directly_ accessible from the branch.
--
-- I.e. names in nested children are omitted.
-- This should probably live elsewhere, but the package dependency graph makes it hard to find
-- a good place.
shallowNames :: forall m v a. Monad m => Codebase m v a -> V2Branch.Branch m -> m Names
shallowNames codebase b = do
  newTerms <-
    V2Branch.terms b
      & Map.mapKeys (Name.fromSegment . Cv.namesegment2to1)
      & fmap Map.keysSet
      & traverse . Set.traverse %%~ Cv.referent2to1 (Codebase.getDeclType codebase)

  let newTypes =
        V2Branch.types b
          & Map.mapKeys (Name.fromSegment . Cv.namesegment2to1)
          & fmap Map.keysSet
          & traverse . Set.traverse %~ Cv.reference2to1
  pure (Names (R.fromMultimap newTerms) (R.fromMultimap newTypes))

loadReferentType ::
  Applicative m =>
  Codebase m Symbol Ann ->
  Referent ->
  m (Maybe (Type Symbol Ann))
loadReferentType codebase = \case
  Referent.Ref r -> Codebase.getTypeOfTerm codebase r
  Referent.Con r _ -> getTypeOfConstructor r
  where
    -- Mitchell wonders: why was this definition copied from Unison.Codebase?
    getTypeOfConstructor (ConstructorReference (Reference.DerivedId r) cid) = do
      maybeDecl <- Codebase.getTypeDeclaration codebase r
      pure $ case maybeDecl of
        Nothing -> Nothing
        Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
    getTypeOfConstructor r =
      error $
        "Don't know how to getTypeOfConstructor "
          ++ show r

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

data FoundRef
  = FoundTermRef Referent
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
fuzzyFind ::
  Names ->
  String ->
  [(FZF.Alignment, UnisonName, [FoundRef])]
fuzzyFind printNames query =
  let fzfNames =
        Names.fuzzyFind (words query) printNames

      toFoundRef =
        fmap (fmap (either FoundTermRef FoundTypeRef) . toList)

      -- Remove dupes based on refs
      dedupe =
        nubOrdOn (\(_, _, refs) -> refs)

      -- Prefer shorter FQNs
      rank (alignment, name, _) =
        ( Name.countSegments (Name.unsafeFromText name),
          negate (FZF.score alignment)
        )

      refine =
        dedupe . sortOn rank
   in refine $ toFoundRef . over _2 Name.toText <$> fzfNames

-- List the immediate children of a namespace
findShallow ::
  Monad m =>
  Codebase m Symbol Ann ->
  Path.Absolute ->
  m [ShallowListEntry Symbol Ann]
findShallow codebase path' = do
  let path = Path.unabsolute path'
  root <- Codebase.getRootBranch codebase
  let mayb = Branch.getAt path root
  case mayb of
    Nothing -> pure []
    Just b -> lsBranch codebase b

findShallowReadmeInBranchAndRender ::
  Width ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  PPED.PrettyPrintEnvDecl ->
  V2Branch.Branch m ->
  Backend IO (Maybe Doc.Doc)
findShallowReadmeInBranchAndRender width runtime codebase ppe namespaceBranch =
  let renderReadme :: PPED.PrettyPrintEnvDecl -> Reference -> IO Doc.Doc
      renderReadme ppe docReference = do
        (_, _, doc) <- renderDoc ppe width runtime codebase docReference
        pure doc

      -- choose the first term (among conflicted terms) matching any of these names, in this order.
      -- we might later want to return all of them to let the front end decide
      toCheck = V2Branch.NameSegment <$> ["README", "Readme", "ReadMe", "readme"]
      readme :: Maybe Reference
      readme = listToMaybe $ do
        name <- toCheck
        term <- toList $ Map.lookup name termsMap
        k <- Map.keys term
        case k of
          -- This shouldn't ever happen unless someone puts a non-doc as their readme.
          V2.Con {} -> empty
          V2.Ref ref -> pure $ Cv.reference2to1 ref
        where
          termsMap = V2Branch.terms namespaceBranch
   in liftIO $ do
        traverse (renderReadme ppe) readme

isDoc :: Monad m => Codebase m Symbol Ann -> Referent -> m Bool
isDoc codebase ref = do
  ot <- loadReferentType codebase ref
  pure $ isDoc' ot

isDoc' :: (Var v, Monoid loc) => Maybe (Type v loc) -> Bool
isDoc' typeOfTerm = do
  -- A term is a dococ if its type conforms to the `Doc` type.
  case typeOfTerm of
    Just t ->
      Typechecker.isSubtype t doc1Type
        || Typechecker.isSubtype t doc2Type
    Nothing -> False

doc1Type :: (Ord v, Monoid a) => Type v a
doc1Type = Type.ref mempty Decls.docRef

doc2Type :: (Ord v, Monoid a) => Type v a
doc2Type = Type.ref mempty DD.doc2Ref

isTestResultList :: forall v a. (Var v, Monoid a) => Maybe (Type v a) -> Bool
isTestResultList typ = case typ of
  Nothing -> False
  Just t -> Typechecker.isSubtype t resultListType

resultListType :: (Ord v, Monoid a) => Type v a
resultListType = Type.app mempty (Type.list mempty) (Type.ref mempty Decls.testResultRef)

termListEntry ::
  Monad m =>
  Codebase m Symbol Ann ->
  Referent ->
  HQ'.HQSegment ->
  m (TermEntry Symbol Ann)
termListEntry codebase r n = do
  ot <- loadReferentType codebase r
  let tag =
        if
            | isDoc' ot -> Just Doc
            | isTestResultList ot -> Just Test
            | otherwise -> Nothing

  pure $ TermEntry r n ot tag

typeListEntry ::
  Monad m =>
  Var v =>
  Codebase m v Ann ->
  Reference ->
  HQ'.HQSegment ->
  m TypeEntry
typeListEntry codebase r n = do
  -- The tag indicates whether the type is a data declaration or an ability.
  tag <- case Reference.toId r of
    Just r -> do
      decl <- Codebase.getTypeDeclaration codebase r
      pure $ case decl of
        Just (Left _) -> Ability
        _ -> Data
    _ -> pure (if Set.member r Type.builtinAbilities then Ability else Data)
  pure $ TypeEntry r n tag

typeDeclHeader ::
  forall v m.
  Monad m =>
  Var v =>
  Codebase m v Ann ->
  PPE.PrettyPrintEnv ->
  Reference ->
  m (DisplayObject Syntax.SyntaxText Syntax.SyntaxText)
typeDeclHeader code ppe r = case Reference.toId r of
  Just rid ->
    Codebase.getTypeDeclaration code rid <&> \case
      Nothing -> DisplayObject.MissingObject (Reference.toShortHash r)
      Just decl ->
        DisplayObject.UserObject $
          Syntax.convertElement
            <$> Pretty.render defaultWidth (DeclPrinter.prettyDeclHeader name decl)
  Nothing ->
    pure (DisplayObject.BuiltinObject (formatTypeName ppe r))
  where
    name = PPE.typeName ppe r

formatTypeName :: PPE.PrettyPrintEnv -> Reference -> Syntax.SyntaxText
formatTypeName ppe =
  fmap Syntax.convertElement . formatTypeName' ppe

formatTypeName' :: PPE.PrettyPrintEnv -> Reference -> SyntaxText
formatTypeName' ppe r =
  Pretty.renderUnbroken
    . NP.styleHashQualified id
    $ PPE.typeName ppe r

termEntryToNamedTerm ::
  Var v => PPE.PrettyPrintEnv -> Maybe Width -> TermEntry v a -> NamedTerm
termEntryToNamedTerm ppe typeWidth (TermEntry r name mayType tag) =
  NamedTerm
    { termName = HQ'.toText name,
      termHash = Referent.toText r,
      termType = formatType ppe (mayDefaultWidth typeWidth) <$> mayType,
      termTag = tag
    }

typeEntryToNamedType :: TypeEntry -> NamedType
typeEntryToNamedType (TypeEntry r name tag) =
  NamedType
    { typeName = HQ'.toText name,
      typeHash = Reference.toText r,
      typeTag = tag
    }

-- | Find all definitions and children reachable from the given branch.
-- Note: this differs from 'lsShallowBranch' in that it takes a fully loaded 'Branch' object,
-- and thus can include definition counts for child namespaces.
lsBranch ::
  Monad m =>
  Codebase m Symbol Ann ->
  Branch m ->
  m [ShallowListEntry Symbol Ann]
lsBranch codebase b = do
  hashLength <- Codebase.hashLength codebase
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
      b0 = Branch.head b
  termEntries <- for (R.toList . Star3.d1 $ Branch._terms b0) $ \(r, ns) ->
    ShallowTermEntry <$> termListEntry codebase r (hqTerm b0 ns r)
  typeEntries <- for (R.toList . Star3.d1 $ Branch._types b0) $
    \(r, ns) -> ShallowTypeEntry <$> typeListEntry codebase r (hqType b0 ns r)
  let branchEntries =
        [ ShallowBranchEntry
            ns
            (Branch.headHash b)
            (Just $ defnCount b)
          | (ns, b) <- Map.toList $ Branch.nonEmptyChildren b0
        ]
      patchEntries =
        [ ShallowPatchEntry ns
          | (ns, (_h, _mp)) <- Map.toList $ Branch._edits b0
        ]
  pure
    . List.sortOn listEntryName
    $ termEntries
      ++ typeEntries
      ++ branchEntries
      ++ patchEntries

-- | Find all definitions and children reachable from the given 'V2Branch.Branch',
-- Note: this differs from 'lsBranch' in that it takes a shallow v2 branch,
-- As a result, it omits definition counts from child-namespaces in its results,
-- but doesn't require loading the entire sub-tree to do so.
lsShallowBranch ::
  Monad m =>
  Codebase m Symbol Ann ->
  V2Branch.Branch m ->
  m [ShallowListEntry Symbol Ann]
lsShallowBranch codebase b0 = do
  hashLength <- Codebase.hashLength codebase
  let hqTerm ::
        ( V2Branch.Branch m ->
          V2Branch.NameSegment ->
          Referent ->
          HQ'.HashQualified NameSegment
        )
      hqTerm b ns r =
        let refs = Map.lookup ns . V2Branch.terms $ b
         in case length refs of
              1 -> HQ'.fromName (Cv.namesegment2to1 ns)
              _ -> HQ'.take hashLength $ HQ'.fromNamedReferent (Cv.namesegment2to1 ns) r
      hqType ::
        ( V2Branch.Branch m ->
          V2Branch.NameSegment ->
          Reference ->
          (HQ'.HashQualified NameSegment)
        )
      hqType b ns r =
        let refs = Map.lookup ns . V2Branch.types $ b
         in case length refs of
              1 -> HQ'.fromName (Cv.namesegment2to1 ns)
              _ -> HQ'.take hashLength $ HQ'.fromNamedReference (Cv.namesegment2to1 ns) r
  let flattenRefs :: Map V2Branch.NameSegment (Map ref v) -> [(ref, V2Branch.NameSegment)]
      flattenRefs m = do
        (ns, refs) <- Map.toList m
        r <- Map.keys refs
        pure (r, ns)
  termEntries <- for (flattenRefs $ V2Branch.terms b0) $ \(r, ns) -> do
    v1Ref <- Cv.referent2to1 (Codebase.getDeclType codebase) r
    ShallowTermEntry <$> termListEntry codebase v1Ref (hqTerm b0 ns v1Ref)
  typeEntries <- for (flattenRefs $ V2Branch.types b0) \(r, ns) -> do
    let v1Ref = Cv.reference2to1 r
    ShallowTypeEntry <$> typeListEntry codebase v1Ref (hqType b0 ns v1Ref)
  let branchEntries =
        [ ShallowBranchEntry (Cv.namesegment2to1 ns) (Cv.causalHash2to1 . V2Causal.causalHash $ h) Nothing
          | (ns, h) <- Map.toList $ V2Branch.children b0
        ]
      patchEntries =
        [ ShallowPatchEntry (Cv.namesegment2to1 ns)
          | (ns, _h) <- Map.toList $ V2Branch.patches b0
        ]
  pure
    . List.sortOn listEntryName
    $ termEntries
      ++ typeEntries
      ++ branchEntries
      ++ patchEntries

termReferencesByShortHash ::
  Monad m => Codebase m v a -> ShortHash -> m (Set Reference)

typeReferencesByShortHash codebase sh = do
  fromCodebase <- Codebase.typeReferencesByPrefix codebase sh
  let fromBuiltins =
        Set.filter
          (\r -> sh == Reference.toShortHash r)
          B.intrinsicTypeReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

-- | Look up types in the codebase by short hash, and include builtins.
typeReferencesByShortHash :: Monad m => Codebase m v a -> ShortHash -> m (Set Reference)

termReferencesByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferencesByPrefix codebase sh
  let fromBuiltins =
        Set.filter
          (\r -> sh == Reference.toShortHash r)
          B.intrinsicTermReferences
  pure (fromBuiltins <> Set.mapMonotonic Reference.DerivedId fromCodebase)

-- | Look up terms in the codebase by short hash, and include builtins.
termReferentsByShortHash :: Monad m => Codebase m v a -> ShortHash -> m (Set Referent)
termReferentsByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferentsByPrefix codebase sh
  let fromBuiltins =
        Set.map Referent.Ref $
          Set.filter
            (\r -> sh == Reference.toShortHash r)
            B.intrinsicTermReferences
  pure (fromBuiltins <> Set.mapMonotonic (over Referent.reference_ Reference.DerivedId) fromCodebase)

-- currentPathNames :: Path -> Names
-- currentPathNames = Branch.toNames . Branch.head . Branch.getAt

-- | Configure how names will be constructed and filtered.
--   this is typically used when fetching names for printing source code or when finding
--   definitions by name.
data NameScoping
  = -- | Find all names, making any names which are children of this path,
    -- otherwise leave them absolute.
    AllNames Path
  | -- | Filter returned names to only include names within this path.
    Within Path

toAllNames :: NameScoping -> NameScoping
toAllNames (AllNames p) = AllNames p
toAllNames (Within p) = AllNames p

getCurrentPrettyNames :: Int -> NameScoping -> Branch m -> PPED.PrettyPrintEnvDecl
getCurrentPrettyNames hashLen scope root =
  let primary = PPED.fromNamesDecl hashLen $ NamesWithHistory (parseNamesForBranch root scope) mempty
      backup = PPED.fromNamesDecl hashLen $ NamesWithHistory (parseNamesForBranch root (AllNames mempty)) mempty
   in PPED.PrettyPrintEnvDecl
        (PPED.unsuffixifiedPPE primary `PPE.addFallback` PPED.unsuffixifiedPPE backup)
        (PPED.suffixifiedPPE primary `PPE.addFallback` PPED.suffixifiedPPE backup)

getCurrentParseNames :: NameScoping -> Branch m -> NamesWithHistory
getCurrentParseNames scope root =
  NamesWithHistory (parseNamesForBranch root scope) mempty

-- Any absolute names in the input which have `root` as a prefix
-- are converted to names relative to current path. All other names are
-- converted to absolute names. For example:
--
-- e.g. if currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names -> Names
fixupNamesRelative root names =
  case (Path.toName $ Path.unabsolute root) of
    Nothing -> names
    Just prefix -> Names.map (fixName prefix) names
  where
    fixName prefix n =
      if root == Path.absoluteEmpty
        then n
        else fromMaybe (Name.makeAbsolute n) (Name.stripNamePrefix prefix n)

-- | A @Search r@ is a small bag of functions that is used to power a search for @r@s.
--
-- Construct a 'Search' with 'makeTypeSearch' or 'makeTermSearch', and eliminate it with 'applySearch'.
data Search r = Search
  { lookupNames :: r -> Set (HQ'.HashQualified Name),
    lookupRelativeHQRefs' :: HQ'.HashQualified Name -> (Set r),
    makeResult :: HQ.HashQualified Name -> r -> Set (HQ'.HashQualified Name) -> SR.SearchResult,
    matchesNamedRef :: Name -> r -> HQ'.HashQualified Name -> Bool
  }

data NameSearch = NameSearch
  { typeSearch :: Search Reference,
    termSearch :: Search Referent
  }

-- | Make a type search, given a short hash length and names to search in.
makeTypeSearch :: Int -> NamesWithHistory -> Search Reference
makeTypeSearch len names =
  Search
    { lookupNames = \ref -> NamesWithHistory.typeName len ref names,
      lookupRelativeHQRefs' = \name -> NamesWithHistory.lookupRelativeHQType' name names,
      matchesNamedRef = HQ'.matchesNamedReference,
      makeResult = SR.typeResult
    }

-- | Make a term search, given a short hash length and names to search in.
makeTermSearch :: Int -> NamesWithHistory -> Search Referent
makeTermSearch len names =
  Search
    { lookupNames = \ref -> NamesWithHistory.termName len ref names,
      lookupRelativeHQRefs' = \name -> NamesWithHistory.lookupRelativeHQTerm' name names,
      matchesNamedRef = HQ'.matchesNamedReferent,
      makeResult = SR.termResult
    }

makeNameSearch :: Int -> NamesWithHistory -> NameSearch
makeNameSearch hashLength names =
  NameSearch
    { typeSearch = makeTypeSearch hashLength names,
      termSearch = makeTermSearch hashLength names
    }

-- | Interpret a 'Search' as a function from name to search results.
applySearch :: (Show r) => Search r -> HQ'.HashQualified Name -> [SR.SearchResult]
applySearch Search {lookupNames, lookupRelativeHQRefs', makeResult, matchesNamedRef} query = do
  -- a bunch of references will match a HQ ref.
  toList (lookupRelativeHQRefs' query) <&> \ref ->
    let -- Precondition: the input set is non-empty
        prioritize :: Set (HQ'.HashQualified Name) -> (HQ'.HashQualified Name, Set (HQ'.HashQualified Name))
        prioritize =
          Set.toList
            >>> sortOn (\n -> matchesNamedRef (HQ'.toName n) ref query)
            >>> List.uncons
            >>> fromMaybe (error (reportBug "E839404" ("query = " ++ show query ++ ", ref = " ++ show ref)))
            >>> over _2 Set.fromList
        names = lookupNames ref
        (primaryName, aliases) =
          -- The precondition of `prioritize` should hold here because we are passing in the set of names that are
          -- related to this ref, which is itself one of the refs that the query name was related to! (Hence it should
          -- be non-empty).
          prioritize names
     in makeResult (HQ'.toHQ primaryName) ref aliases

hqNameQuery ::
  Monad m =>
  Codebase m v Ann ->
  NameSearch ->
  [HQ.HashQualified Name] ->
  m QueryResult
hqNameQuery codebase (NameSearch {typeSearch, termSearch}) hqs = do
  -- Split the query into hash-only and hash-qualified-name queries.
  let (hashes, hqnames) = partitionEithers (map HQ'.fromHQ2 hqs)
  -- Find the terms with those hashes.
  termRefs <-
    filter (not . Set.null . snd) . zip hashes
      <$> traverse
        (termReferentsByShortHash codebase)
        hashes
  -- Find types with those hashes.
  typeRefs <-
    filter (not . Set.null . snd) . zip hashes
      <$> traverse
        (typeReferencesByShortHash codebase)
        hashes
  -- Now do the name queries.
  let mkTermResult sh r = SR.termResult (HQ.HashOnly sh) r Set.empty
      mkTypeResult sh r = SR.typeResult (HQ.HashOnly sh) r Set.empty
      -- Transform the hash results a bit
      termResults =
        (\(sh, tms) -> mkTermResult sh <$> toList tms) <$> termRefs
      typeResults =
        (\(sh, tps) -> mkTypeResult sh <$> toList tps) <$> typeRefs
      -- Now do the actual name query
      resultss = map (\name -> applySearch typeSearch name <> applySearch termSearch name) hqnames
      (misses, hits) =
        zip hqnames resultss
          & map (\(hqname, results) -> if null results then Left hqname else Right results)
          & partitionEithers
      -- Handle query misses correctly
      missingRefs =
        [ HQ.HashOnly x
          | x <- hashes,
            isNothing (lookup x termRefs) && isNothing (lookup x typeRefs)
        ]
      -- Gather the results
      results =
        List.sort
          . uniqueBy SR.toReferent
          . concat
          $ (hits ++ termResults ++ typeResults)
  pure
    QueryResult
      { misses = missingRefs ++ map HQ'.toHQ misses,
        hits = results
      }

-- TODO: Move this to its own module
data DefinitionResults v = DefinitionResults
  { termResults :: Map Reference (DisplayObject (Type v Ann) (Term v Ann)),
    typeResults :: Map Reference (DisplayObject () (DD.Decl v Ann)),
    noResults :: [HQ.HashQualified Name]
  }

expandShortBranchHash ::
  Monad m => Codebase m v a -> ShortBranchHash -> Backend m (Branch.CausalHash)
expandShortBranchHash codebase hash = do
  hashSet <- lift $ Codebase.branchHashesByPrefix codebase hash
  len <- lift $ Codebase.branchHashLength codebase
  case Set.toList hashSet of
    [] -> throwError $ CouldntExpandBranchHash hash
    [h] -> pure h
    _ ->
      throwError . AmbiguousBranchHash hash $ Set.map (SBH.fromHash len) hashSet

-- | Efficiently resolve a root hash and path to a shallow branch's causal.
getShallowCausalAtPathFromRootHash :: Monad m => Codebase m v a -> Maybe (Branch.CausalHash) -> Path -> Backend m (V2Branch.CausalBranch m)
getShallowCausalAtPathFromRootHash codebase mayRootHash path = do
  shallowRoot <- case mayRootHash of
    Nothing -> lift (Codebase.getShallowRootBranch codebase)
    Just h -> do
      lift $ Codebase.getShallowBranchForHash codebase (Cv.causalHash1to2 h)
  causal <-
    (lift $ Codebase.shallowBranchAtPath path shallowRoot) >>= \case
      Nothing -> pure $ Cv.causalbranch1to2 (Branch.empty)
      Just lc -> pure lc
  pure causal

formatType' :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> SyntaxText
formatType' ppe w =
  Pretty.render w . TypePrinter.pretty0 ppe mempty (-1)

formatType :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> Syntax.SyntaxText
formatType ppe w = mungeSyntaxText . formatType' ppe w

formatSuffixedType ::
  Var v =>
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Type v Ann ->
  Syntax.SyntaxText
formatSuffixedType ppe = formatType (PPED.suffixifiedPPE ppe)

mungeSyntaxText ::
  Functor g => g (UST.Element Reference) -> g Syntax.Element
mungeSyntaxText = fmap Syntax.convertElement

prettyDefinitionsBySuffixes ::
  Path ->
  Maybe (Branch.CausalHash) ->
  Maybe Width ->
  Suffixify ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  [HQ.HashQualified Name] ->
  Backend IO DefinitionDisplayResults
prettyDefinitionsBySuffixes path root renderWidth suffixifyBindings rt codebase query = do
  hqLength <- lift $ Codebase.hashLength codebase
  -- We might like to make sure that the user search terms get used as
  -- the names in the pretty-printer, but the current implementation
  -- doesn't.
  (parseNames, localNamesOnly, unbiasedPPE) <- scopedNamesForBranchHash codebase root path
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our perspective but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  let biases = mapMaybe HQ.toName query
  let ppe = PPED.biasTo biases unbiasedPPE
  let nameSearch :: NameSearch
      nameSearch = makeNameSearch hqLength (NamesWithHistory.fromCurrentNames localNamesOnly)
  DefinitionResults terms types misses <- lift (definitionsBySuffixes codebase nameSearch DontIncludeCycles query)
  let width =
        mayDefaultWidth renderWidth

      namesWithFallback = localNamesOnly `Names.unionLeftRef` parseNames

      termFqns :: Map Reference (Set Text)
      termFqns = Map.mapWithKey f terms
        where
          rel = Names.terms namesWithFallback
          f k _ =
            Set.fromList . fmap Name.toText . toList $
              R.lookupRan (Referent.Ref k) rel

      typeFqns :: Map Reference (Set Text)
      typeFqns = Map.mapWithKey f types
        where
          rel = Names.types namesWithFallback
          f k _ =
            Set.fromList . fmap Name.toText . toList $
              R.lookupRan k rel

      flatten = Set.toList . fromMaybe Set.empty

      docNames :: Set (HQ'.HashQualified Name) -> [Name]
      docNames hqs = fmap docify . nubOrd . join . map toList . Set.toList $ hqs
        where
          docify n = Name.joinDot n "doc"

      selectDocs :: [Referent] -> IO [Reference]
      selectDocs rs = do
        rts <- fmap join . for rs $ \case
          Referent.Ref r ->
            maybe [] (pure . (r,)) <$> Codebase.getTypeOfTerm codebase r
          _ -> pure []
        pure [r | (r, t) <- rts, Typechecker.isSubtype t (Type.ref mempty DD.doc2Ref)]

      -- rs0 can be empty or the term fetched, so when viewing a doc term
      -- you get both its source and its rendered form
      docResults :: [Reference] -> [Name] -> IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults rs0 docs = do
        let refsFor n = NamesWithHistory.lookupHQTerm (HQ.NameOnly n) (NamesWithHistory.fromCurrentNames localNamesOnly)
        let rs = Set.unions (refsFor <$> docs) <> Set.fromList (Referent.Ref <$> rs0)
        -- lookup the type of each, make sure it's a doc
        docs <- selectDocs (toList rs)
        -- render all the docs
        traverse (renderDoc ppe width rt codebase) docs

      mkTermDefinition ::
        ( Reference ->
          DisplayObject
            (AnnotatedText (UST.Element Reference))
            (AnnotatedText (UST.Element Reference)) ->
          Backend IO TermDefinition
        )
      mkTermDefinition r tm = do
        let referent = Referent.Ref r
        ts <- lift (Codebase.getTypeOfTerm codebase r)
        let bn = bestNameForTerm @Symbol (PPED.suffixifiedPPE ppe) width (Referent.Ref r)
        tag <-
          lift
            ( termEntryTag
                <$> termListEntry
                  codebase
                  referent
                  (HQ'.NameOnly (NameSegment bn))
            )
        docs <- lift (docResults [r] $ docNames (NamesWithHistory.termName hqLength (Referent.Ref r) (NamesWithHistory.fromCurrentNames localNamesOnly)))
        mk docs ts bn tag
        where
          mk _ Nothing _ _ = throwError $ MissingSignatureForTerm r
          mk docs (Just typeSig) bn tag =
            pure $
              TermDefinition
                (flatten $ Map.lookup r termFqns)
                bn
                tag
                (bimap mungeSyntaxText mungeSyntaxText tm)
                (formatSuffixedType ppe width typeSig)
                docs
      mkTypeDefinition r tp = do
        let bn = bestNameForType @Symbol (PPED.suffixifiedPPE ppe) width r
        tag <-
          Just . typeEntryTag
            <$> typeListEntry
              codebase
              r
              (HQ'.NameOnly (NameSegment bn))
        docs <- docResults [] $ docNames (NamesWithHistory.typeName hqLength r (NamesWithHistory.fromCurrentNames localNamesOnly))
        pure $
          TypeDefinition
            (flatten $ Map.lookup r typeFqns)
            bn
            tag
            (bimap mungeSyntaxText mungeSyntaxText tp)
            docs
  typeDefinitions <-
    lift do
      Map.traverseWithKey mkTypeDefinition $
        typesToSyntax suffixifyBindings width ppe types
  termDefinitions <-
    Map.traverseWithKey mkTermDefinition $
      termsToSyntax suffixifyBindings width ppe terms
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $
    DefinitionDisplayResults
      renderedDisplayTerms
      renderedDisplayTypes
      renderedMisses

renderDoc ::
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Reference ->
  IO (HashQualifiedName, UnisonHash, Doc.Doc)
renderDoc ppe width rt codebase r = do
  let name = bestNameForTerm @Symbol (PPED.suffixifiedPPE ppe) width (Referent.Ref r)
  let hash = Reference.toText r
  (name,hash,)
    <$> let tm = Term.ref () r
         in Doc.renderDoc ppe terms typeOf eval decls tm
  where
    terms r@(Reference.Builtin _) = pure (Just (Term.ref () r))
    terms (Reference.DerivedId r) =
      fmap Term.unannotate <$> Codebase.getTerm codebase r

    typeOf r = fmap void <$> Codebase.getTypeOfReferent codebase r
    eval (Term.amap (const mempty) -> tm) = do
      let ppes = PPED.suffixifiedPPE ppe
      let codeLookup = Codebase.toCodeLookup codebase
      let cache r = fmap Term.unannotate <$> Codebase.lookupWatchCache codebase r
      r <- fmap hush . liftIO $ Rt.evaluateTerm' codeLookup cache ppes rt tm
      case r of
        Just tmr ->
          Codebase.putWatch
            codebase
            WK.RegularWatch
            (Hashing.hashClosedTerm tm)
            (Term.amap (const mempty) tmr)
        Nothing -> pure ()
      pure $ r <&> Term.amap (const mempty)

    decls (Reference.DerivedId r) = fmap (DD.amap (const ())) <$> Codebase.getTypeDeclaration codebase r
    decls _ = pure Nothing

docsInBranchToHtmlFiles ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Branch IO ->
  Path ->
  FilePath ->
  IO ()
docsInBranchToHtmlFiles runtime codebase root currentPath directory = do
  let currentBranch = Branch.getAt' currentPath root
  let allTerms = (R.toList . Branch.deepTerms . Branch.head) currentBranch
  -- ignores docs inside lib namespace, recursively
  let notLib (_, name) = all (/= "lib") (Name.segments name)
  docTermsWithNames <- filterM (isDoc codebase . fst) (filter notLib allTerms)
  let docNamesByRef = Map.fromList docTermsWithNames
  hqLength <- Codebase.hashLength codebase
  let printNames = prettyNamesForBranch root (AllNames currentPath)
  let printNamesWithHistory = NamesWithHistory {currentNames = printNames, oldNames = mempty}
  let ppe = PPED.fromNamesDecl hqLength printNamesWithHistory
  docs <- for docTermsWithNames (renderDoc' ppe runtime codebase)
  liftIO $ traverse_ (renderDocToHtmlFile docNamesByRef directory) docs
  where
    renderDoc' ppe runtime codebase (ref, name) = do
      (_, hash, doc) <- renderDoc ppe defaultWidth runtime codebase (Referent.toReference ref)
      pure (name, hash, doc)

    cleanPath :: FilePath -> FilePath
    cleanPath filePath =
      filePath <&> \case
        '#' -> '@'
        c -> c

    docFilePath :: FilePath -> Name -> FilePath
    docFilePath destination docFQN =
      let (dir, fileName) =
            case unsnoc . map NameSegment.toString . toList . Name.segments $ docFQN of
              Just (path, leafName) ->
                (directoryPath path, docFileName leafName)
              Nothing ->
                error "Could not parse doc name"

          directoryPath p =
            destination </> joinPath p

          docFileName n =
            cleanPath $ n <> ".html"
       in dir </> fileName

    renderDocToHtmlFile :: Map Referent Name -> FilePath -> (Name, UnisonHash, Doc.Doc) -> IO ()
    renderDocToHtmlFile docNamesByRef destination (docName, _, doc) = do
      let fullPath =
            docFilePath destination docName

          directoryPath =
            takeDirectory fullPath

          (DocHtml.FrontMatterData frontmatter, html) =
            DocHtml.toHtml docNamesByRef doc

          go [v] = Yaml.String v
          go vs = Yaml.array $ map Yaml.String vs

          frontMatterToYaml fm =
            fmap go fm

          frontmatterTxt =
            if Map.null frontmatter
              then ""
              else "---\n" <> TextE.decodeUtf8 (Yaml.encode $ frontMatterToYaml frontmatter) <> "---\n"

          htmlAsText =
            Lucid.renderText html

          fileContents =
            frontmatterTxt <> toStrict htmlAsText
       in do
            -- Ensure all directories exists
            _ <- createDirectoryIfMissing True directoryPath
            writeFile fullPath (Text.unpack fileContents)

bestNameForTerm ::
  forall v. Var v => PPE.PrettyPrintEnv -> Width -> Referent -> Text
bestNameForTerm ppe width =
  Text.pack
    . Pretty.render width
    . fmap UST.toPlain
    . TermPrinter.pretty0 @v ppe TermPrinter.emptyAc
    . Term.fromReferent mempty

bestNameForType ::
  forall v. Var v => PPE.PrettyPrintEnv -> Width -> Reference -> Text
bestNameForType ppe width =
  Text.pack
    . Pretty.render width
    . fmap UST.toPlain
    . TypePrinter.pretty0 @v ppe mempty (-1)
    . Type.ref ()

-- | Returns (parse, pretty, local, ppe) where:
--
-- - 'parse' includes ALL fully qualified names from the root, and ALSO all names from within the provided path, relative to that path.
-- - 'pretty' includes names within the provided path, relative to that path, and also all globally scoped names _outside_ of the path
-- - 'local' includes ONLY the names within the provided path
-- - 'ppe' is a ppe which searches for a name within the path first, but falls back to a global name search.
--     The 'suffixified' component of this ppe will search for the shortest unambiguous suffix within the scope in which the name is found (local, falling back to global)
scopedNamesForBranchHash :: forall m v a. Monad m => Codebase m v a -> Maybe (Branch.CausalHash) -> Path -> Backend m (Names, Names, PPED.PrettyPrintEnvDecl)
scopedNamesForBranchHash codebase mbh path = do
  shouldUseNamesIndex <- asks useNamesIndex
  hashLen <- lift $ Codebase.hashLength codebase
  (parseNames, localNames) <- case mbh of
    Nothing
      | shouldUseNamesIndex -> indexNames
      | otherwise -> do
          rootBranch <- lift $ Codebase.getRootBranch codebase
          let (parseNames, _prettyNames, localNames) = namesForBranch rootBranch (AllNames path)
          pure (parseNames, localNames)
    Just bh -> do
      rootHash <- lift $ Codebase.getRootBranchHash codebase
      if (Causal.unCausalHash bh == V2.Hash.unCausalHash rootHash) && shouldUseNamesIndex
        then indexNames
        else do
          (parseNames, _pretty, localNames) <- flip namesForBranch (AllNames path) <$> resolveCausalHash (Just bh) codebase
          pure (parseNames, localNames)

  let localPPE = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames localNames)
  let globalPPE = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames parseNames)
  pure (parseNames, localNames, mkPPE localPPE globalPPE)
  where
    mkPPE :: PPED.PrettyPrintEnvDecl -> PPED.PrettyPrintEnvDecl -> PPED.PrettyPrintEnvDecl
    mkPPE primary addFallback =
      PPED.PrettyPrintEnvDecl
        (PPED.unsuffixifiedPPE primary `PPE.addFallback` PPED.unsuffixifiedPPE addFallback)
        (PPED.suffixifiedPPE primary `PPE.addFallback` PPED.suffixifiedPPE addFallback)
    indexNames :: Backend m (Names, Names)
    indexNames = do
      scopedNames <- lift $ Codebase.namesAtPath codebase path
      pure (ScopedNames.parseNames scopedNames, ScopedNames.namesAtPath scopedNames)

resolveCausalHash ::
  Monad m => Maybe (Branch.CausalHash) -> Codebase m v a -> Backend m (Branch m)
resolveCausalHash h codebase = case h of
  Nothing -> lift (Codebase.getRootBranch codebase)
  Just bhash -> do
    mayBranch <- lift $ Codebase.getBranchForHash codebase bhash
    whenNothing mayBranch (throwError $ NoBranchForHash bhash)

resolveRootBranchHash ::
  Monad m => Maybe ShortBranchHash -> Codebase m v a -> Backend m (Branch m)
resolveRootBranchHash mayRoot codebase = case mayRoot of
  Nothing ->
    lift (Codebase.getRootBranch codebase)
  Just sbh -> do
    h <- expandShortBranchHash codebase sbh
    resolveCausalHash (Just h) codebase

-- | Determines whether we include full cycles in the results, (e.g. if I search for `isEven`, will I find `isOdd` too?)
data IncludeCycles
  = IncludeCycles
  | DontIncludeCycles

definitionsBySuffixes ::
  forall m.
  MonadIO m =>
  Codebase m Symbol Ann ->
  NameSearch ->
  IncludeCycles ->
  [HQ.HashQualified Name] ->
  m (DefinitionResults Symbol)
definitionsBySuffixes codebase nameSearch includeCycles query = do
  QueryResult misses results <- hqNameQuery codebase nameSearch query
  -- todo: remember to replace this with getting components directly,
  -- and maybe even remove getComponentLength from Codebase interface altogether
  terms <- do
    let termRefsWithoutCycles = searchResultsToTermRefs results
    termRefs <- case includeCycles of
      IncludeCycles ->
        Monoid.foldMapM
          (Codebase.componentReferencesForReference codebase)
          termRefsWithoutCycles
      DontIncludeCycles -> pure termRefsWithoutCycles
    Map.foldMapM (\ref -> (ref,) <$> displayTerm ref) termRefs
  types <- do
    let typeRefsWithoutCycles = searchResultsToTypeRefs results
    typeRefs <- case includeCycles of
      IncludeCycles ->
        Monoid.foldMapM
          (Codebase.componentReferencesForReference codebase)
          typeRefsWithoutCycles
      DontIncludeCycles -> pure typeRefsWithoutCycles
    Map.foldMapM (\ref -> (ref,) <$> displayType ref) typeRefs
  pure (DefinitionResults terms types misses)
  where
    searchResultsToTermRefs :: [SR.SearchResult] -> Set Reference
    searchResultsToTermRefs results =
      Set.fromList [r | SR.Tm' _ (Referent.Ref r) _ <- results]
    searchResultsToTypeRefs :: [SR.SearchResult] -> Set Reference
    searchResultsToTypeRefs results =
      Set.fromList (mapMaybe f results)
      where
        f :: SR.SearchResult -> Maybe Reference
        f = \case
          SR.Tm' _ (Referent.Con r _) _ -> Just (r ^. ConstructorReference.reference_)
          SR.Tp' _ r _ -> Just r
          _ -> Nothing
    displayTerm :: Reference -> m (DisplayObject (Type Symbol Ann) (Term Symbol Ann))
    displayTerm = \case
      ref@(Reference.Builtin _) -> do
        pure case Map.lookup ref B.termRefTypes of
          -- This would be better as a `MissingBuiltin` constructor; `MissingObject` is kind of being
          -- misused here. Is `MissingObject` even possible anymore?
          Nothing -> MissingObject $ Reference.toShortHash ref
          Just typ -> BuiltinObject (mempty <$ typ)
      Reference.DerivedId rid -> do
        (term, ty) <- Codebase.unsafeGetTermWithType codebase rid
        pure case term of
          Term.Ann' _ _ -> UserObject term
          -- manually annotate if necessary
          _ -> UserObject (Term.ann (ABT.annotation term) term ty)
    displayType :: Reference -> m (DisplayObject () (DD.Decl Symbol Ann))
    displayType = \case
      Reference.Builtin _ -> pure (BuiltinObject ())
      Reference.DerivedId rid -> do
        decl <- Codebase.unsafeGetTypeDeclaration codebase rid
        pure (UserObject decl)

termsToSyntax ::
  Var v =>
  Ord a =>
  Suffixify ->
  Width ->
  PPED.PrettyPrintEnvDecl ->
  Map Reference.Reference (DisplayObject (Type v a) (Term v a)) ->
  Map Reference.Reference (DisplayObject SyntaxText SyntaxText)
termsToSyntax suff width ppe0 terms =
  Map.fromList . map go . Map.toList $
    Map.mapKeys
      (first (PPE.termName ppeDecl . Referent.Ref) . dupe)
      terms
  where
    ppeBody r =
      if suffixified suff
        then PPED.suffixifiedPPE ppe0
        else PPE.declarationPPE ppe0 r
    ppeDecl =
      (if suffixified suff then PPED.suffixifiedPPE else PPED.unsuffixifiedPPE) ppe0
    go ((n, r), dt) = (r,) $ case dt of
      DisplayObject.BuiltinObject typ ->
        DisplayObject.BuiltinObject $
          formatType' (ppeBody r) width typ
      DisplayObject.MissingObject sh -> DisplayObject.MissingObject sh
      DisplayObject.UserObject tm ->
        DisplayObject.UserObject
          . Pretty.render width
          . TermPrinter.prettyBinding (ppeBody r) n
          $ tm

typesToSyntax ::
  Var v =>
  Ord a =>
  Suffixify ->
  Width ->
  PPED.PrettyPrintEnvDecl ->
  Map Reference.Reference (DisplayObject () (DD.Decl v a)) ->
  Map Reference.Reference (DisplayObject SyntaxText SyntaxText)
typesToSyntax suff width ppe0 types =
  Map.fromList $
    map go . Map.toList $
      Map.mapKeys
        (first (PPE.typeName ppeDecl) . dupe)
        types
  where
    ppeDecl =
      if suffixified suff
        then PPED.suffixifiedPPE ppe0
        else PPED.unsuffixifiedPPE ppe0
    go ((n, r), dt) = (r,) $ case dt of
      BuiltinObject _ -> BuiltinObject (formatTypeName' ppeDecl r)
      MissingObject sh -> MissingObject sh
      UserObject d ->
        UserObject . Pretty.render width $
          DeclPrinter.prettyDecl (PPE.declarationPPEDecl ppe0 r) r n d

loadSearchResults ::
  Applicative m =>
  Codebase m Symbol Ann ->
  [SR.SearchResult] ->
  m [SR'.SearchResult' Symbol Ann]
loadSearchResults c = traverse loadSearchResult
  where
    loadSearchResult = \case
      SR.Tm (SR.TermResult name r aliases) -> do
        typ <- loadReferentType c r
        pure $ SR'.Tm name typ r aliases
      SR.Tp (SR.TypeResult name r aliases) -> do
        dt <- loadTypeDisplayObject c r
        pure $ SR'.Tp name dt r aliases

loadTypeDisplayObject ::
  Applicative m =>
  Codebase m v Ann ->
  Reference ->
  m (DisplayObject () (DD.Decl v Ann))
loadTypeDisplayObject c = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> Codebase.getTypeDeclaration c id

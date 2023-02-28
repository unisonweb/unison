{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Backend
  ( -- * Types
    BackendError (..),
    Backend (..),
    ShallowListEntry (..),
    BackendEnv (..),
    TypeEntry (..),
    FoundRef (..),
    NameScoping (..),
    IncludeCycles (..),
    DefinitionResults (..),

    -- * Endpoints
    prettyDefinitionsForHQName,
    fuzzyFind,

    -- * Utilities
    basicSuffixifiedNames,
    bestNameForTerm,
    bestNameForType,
    definitionsBySuffixes,
    displayType,
    docsInBranchToHtmlFiles,
    expandShortCausalHash,
    findShallowReadmeInBranchAndRender,
    formatSuffixedType,
    getCurrentParseNames,
    getCurrentPrettyNames,
    getShallowCausalAtPathFromRootHash,
    getTermTag,
    getTypeTag,
    hoistBackend,
    hqNameQuery,
    loadReferentType,
    loadSearchResults,
    lsAtPath,
    lsBranch,
    mungeSyntaxText,
    namesForBranch,
    parseNamesForBranch,
    prettyNamesForBranch,
    resolveCausalHashV2,
    resolveRootBranchHashV2,
    scopedNamesForBranchHash,
    termEntryDisplayName,
    termEntryHQName,
    termEntryToNamedTerm,
    termEntryType,
    termListEntry,
    termReferentsByShortHash,
    typeDeclHeader,
    typeEntryDisplayName,
    typeEntryHQName,
    typeEntryToNamedType,
    typeListEntry,
    typeReferencesByShortHash,
    typeToSyntaxHeader,
  )
where

import Control.Error.Util (hush)
import Control.Lens hiding ((??))
import qualified Control.Lens.Cons as Cons
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (..), first)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List as List
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
import U.Codebase.Branch (NamespaceStats (..))
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import U.Codebase.HashTags (BranchHash, CausalHash (..))
import U.Codebase.Projects as Projects
import qualified U.Codebase.Referent as V2Referent
import qualified U.Codebase.Sqlite.Operations as Operations
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as B
import qualified Unison.Builtin.Decls as Decls
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Editor.DisplayObject as DisplayObject
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Rt
import Unison.Codebase.ShortCausalHash
  ( ShortCausalHash,
  )
import qualified Unison.Codebase.ShortCausalHash as SCH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.ConstructorType as CT
import qualified Unison.DataDeclaration as DD
import qualified Unison.Debug as Debug
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
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
import qualified Unison.PrettyPrintEnvDecl.Sqlite as PPESqlite
import Unison.Reference (Reference, TermReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as DD
import qualified Unison.Server.Doc as Doc
import qualified Unison.Server.Doc.AsHtml as DocHtml
import Unison.Server.NameSearch (NameSearch (..), Search (..), applySearch, makeNameSearch)
import Unison.Server.NameSearch.Sqlite (termReferentsByShortHash, typeReferencesByShortHash)
import qualified Unison.Server.NameSearch.Sqlite as SqliteNameSearch
import Unison.Server.QueryResult
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.Server.Syntax as Syntax
import Unison.Server.Types
import Unison.ShortHash
import qualified Unison.ShortHash as SH
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.HashQualified as HQ (toText)
import qualified Unison.Syntax.HashQualified' as HQ' (toText)
import Unison.Syntax.Name as Name (toString, toText, unsafeFromText)
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
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Monoid as Monoid
import Unison.Util.Pretty (Width)
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Set as Set
import qualified Unison.Util.SyntaxText as UST
import Unison.Var (Var)
import qualified Unison.WatchKind as WK

type SyntaxText = UST.SyntaxText' Reference

data ShallowListEntry v a
  = ShallowTermEntry (TermEntry v a)
  | ShallowTypeEntry TypeEntry
  | ShallowBranchEntry NameSegment CausalHash NamespaceStats
  | ShallowPatchEntry NameSegment
  deriving (Eq, Ord, Show, Generic)

listEntryName :: ShallowListEntry v a -> Text
listEntryName = \case
  ShallowTermEntry te -> termEntryDisplayName te
  ShallowTypeEntry te -> typeEntryDisplayName te
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
  | CouldntExpandBranchHash ShortCausalHash
  | AmbiguousBranchHash ShortCausalHash (Set ShortCausalHash)
  | AmbiguousHashForDefinition ShortHash
  | NoBranchForHash CausalHash
  | CouldntLoadBranch CausalHash
  | MissingSignatureForTerm Reference
  | NoSuchDefinition (HQ.HashQualified Name)
  deriving stock (Show)

newtype BackendEnv = BackendEnv
  { -- | Whether to use the sqlite name-lookup table to generate Names objects rather than building Names from the root branch.
    useNamesIndex :: Bool
  }

newtype Backend m a = Backend {runBackend :: ReaderT BackendEnv (ExceptT BackendError m) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader BackendEnv, MonadError BackendError)

instance MonadTrans Backend where
  lift m = Backend (lift . lift $ m)

hoistBackend :: (forall x. m x -> n x) -> Backend m a -> Backend n a
hoistBackend f (Backend m) =
  Backend (mapReaderT (mapExceptT f) m)

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

shallowPPE :: (MonadIO m) => Codebase m v a -> V2Branch.Branch m -> m PPE.PrettyPrintEnv
shallowPPE codebase b = do
  hashLength <- Codebase.runTransaction codebase Codebase.hashLength
  names <- shallowNames codebase b
  pure $ PPED.suffixifiedPPE . PPED.fromNamesDecl hashLength $ NamesWithHistory names mempty

-- | A 'Names' which only includes mappings for things _directly_ accessible from the branch.
--
-- I.e. names in nested children are omitted.
-- This should probably live elsewhere, but the package dependency graph makes it hard to find
-- a good place.
shallowNames :: forall m v a. (Monad m) => Codebase m v a -> V2Branch.Branch m -> m Names
shallowNames codebase b = do
  newTerms <-
    V2Branch.terms b
      & Map.mapKeys Name.fromSegment
      & fmap Map.keysSet
      & traverse . Set.traverse %%~ Cv.referent2to1 (Codebase.getDeclType codebase)

  let newTypes =
        V2Branch.types b
          & Map.mapKeys Name.fromSegment
          & fmap Map.keysSet
          & traverse . Set.traverse %~ Cv.reference2to1
  pure (Names (R.fromMultimap newTerms) (R.fromMultimap newTypes))

loadReferentType ::
  Codebase m Symbol Ann ->
  Referent ->
  Sqlite.Transaction (Maybe (Type Symbol Ann))
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
  { termEntryReferent :: V2Referent.Referent,
    termEntryHash :: ShortHash,
    termEntryName :: NameSegment,
    termEntryConflicted :: Bool,
    termEntryType :: Maybe (Type v a),
    termEntryTag :: TermTag
  }
  deriving (Eq, Ord, Show, Generic)

termEntryDisplayName :: TermEntry v a -> Text
termEntryDisplayName = HQ'.toTextWith NameSegment.toText . termEntryHQName

termEntryHQName :: TermEntry v a -> HQ'.HashQualified NameSegment
termEntryHQName TermEntry {termEntryName, termEntryConflicted, termEntryHash} =
  if termEntryConflicted
    then HQ'.HashQualified termEntryName termEntryHash
    else HQ'.NameOnly termEntryName

data TypeEntry = TypeEntry
  { typeEntryReference :: Reference,
    typeEntryHash :: ShortHash,
    typeEntryName :: NameSegment,
    typeEntryConflicted :: Bool,
    typeEntryTag :: TypeTag
  }
  deriving (Eq, Ord, Show, Generic)

typeEntryDisplayName :: TypeEntry -> Text
typeEntryDisplayName = HQ'.toTextWith NameSegment.toText . typeEntryHQName

typeEntryHQName :: TypeEntry -> HQ'.HashQualified NameSegment
typeEntryHQName TypeEntry {typeEntryName, typeEntryConflicted, typeEntryReference} =
  if typeEntryConflicted
    then HQ'.HashQualified typeEntryName (Reference.toShortHash typeEntryReference)
    else HQ'.NameOnly typeEntryName

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
        Names.fuzzyFind Name.toText (words query) printNames

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
lsAtPath ::
  (MonadIO m) =>
  Codebase m Symbol Ann ->
  -- The root to follow the path from.
  Maybe (V2Branch.Branch Sqlite.Transaction) ->
  -- Path from the root to the branch to 'ls'
  Path.Absolute ->
  m [ShallowListEntry Symbol Ann]
lsAtPath codebase mayRootBranch absPath = do
  b <- Codebase.runTransaction codebase (Codebase.getShallowBranchAtPath (Path.unabsolute absPath) mayRootBranch)
  lsBranch codebase b

findShallowReadmeInBranchAndRender ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  PPED.PrettyPrintEnvDecl ->
  V2Branch.Branch m ->
  Backend IO (Maybe Doc.Doc)
findShallowReadmeInBranchAndRender runtime codebase ppe namespaceBranch =
  let renderReadme :: PPED.PrettyPrintEnvDecl -> Reference -> IO Doc.Doc
      renderReadme ppe docReference = do
        doc <- evalDocRef runtime codebase docReference <&> Doc.renderDoc ppe
        pure doc

      -- choose the first term (among conflicted terms) matching any of these names, in this order.
      -- we might later want to return all of them to let the front end decide
      toCheck = NameSegment <$> ["README", "Readme", "ReadMe", "readme"]
      readme :: Maybe Reference
      readme = listToMaybe $ do
        name <- toCheck
        term <- toList $ Map.lookup name termsMap
        k <- Map.keys term
        case k of
          -- This shouldn't ever happen unless someone puts a non-doc as their readme.
          V2Referent.Con {} -> empty
          V2Referent.Ref ref -> pure $ Cv.reference2to1 ref
        where
          termsMap = V2Branch.terms namespaceBranch
   in liftIO $ do
        traverse (renderReadme ppe) readme

isDoc :: Codebase m Symbol Ann -> Referent.Referent -> Sqlite.Transaction Bool
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
  (MonadIO m) =>
  Codebase m Symbol Ann ->
  V2Branch.Branch n ->
  ExactName NameSegment V2Referent.Referent ->
  m (TermEntry Symbol Ann)
termListEntry codebase branch (ExactName nameSegment ref) = do
  v1Referent <- Cv.referent2to1 (Codebase.getDeclType codebase) ref
  ot <- Codebase.runTransaction codebase (loadReferentType codebase v1Referent)
  tag <- getTermTag codebase ref ot
  pure $
    TermEntry
      { termEntryReferent = ref,
        termEntryName = nameSegment,
        termEntryType = ot,
        termEntryTag = tag,
        termEntryConflicted = isConflicted,
        termEntryHash = Cv.referent2toshorthash1 Nothing ref
      }
  where
    isConflicted =
      branch
        & V2Branch.terms
        & Map.lookup nameSegment
        & maybe 0 Map.size
        & (> 1)

getTermTag ::
  (Monad m, Var v) =>
  Codebase m v a ->
  V2Referent.Referent ->
  Maybe (Type v Ann) ->
  m TermTag
getTermTag codebase r sig = do
  -- A term is a doc if its type conforms to the `Doc` type.
  let isDoc = case sig of
        Just t ->
          Typechecker.isSubtype t (Type.ref mempty Decls.docRef)
            || Typechecker.isSubtype t (Type.ref mempty DD.doc2Ref)
        Nothing -> False
  -- A term is a test if it has the type [test.Result]
  let isTest = case sig of
        Just t ->
          Typechecker.isSubtype t (Decls.testResultType mempty)
        Nothing -> False
  constructorType <- case r of
    V2Referent.Ref {} -> pure Nothing
    V2Referent.Con ref _ -> Just <$> Codebase.getDeclType codebase ref
  pure $
    if
        | isDoc -> Doc
        | isTest -> Test
        | Just CT.Effect <- constructorType -> Constructor Ability
        | Just CT.Data <- constructorType -> Constructor Data
        | otherwise -> Plain

getTypeTag ::
  (Var v) =>
  Codebase m v Ann ->
  Reference ->
  Sqlite.Transaction TypeTag
getTypeTag codebase r = do
  case Reference.toId r of
    Just r -> do
      decl <- Codebase.getTypeDeclaration codebase r
      pure $ case decl of
        Just (Left _) -> Ability
        _ -> Data
    _ -> pure (if Set.member r Type.builtinAbilities then Ability else Data)

typeListEntry ::
  (Var v) =>
  Codebase m v Ann ->
  V2Branch.Branch n ->
  ExactName NameSegment Reference ->
  Sqlite.Transaction TypeEntry
typeListEntry codebase b (ExactName nameSegment ref) = do
  hashLength <- Codebase.hashLength
  tag <- getTypeTag codebase ref
  pure $
    TypeEntry
      { typeEntryReference = ref,
        typeEntryName = nameSegment,
        typeEntryConflicted = isConflicted,
        typeEntryTag = tag,
        typeEntryHash = SH.take hashLength $ Reference.toShortHash ref
      }
  where
    isConflicted =
      b
        & V2Branch.types
        & Map.lookup nameSegment
        & maybe 0 Map.size
        & (> 1)

typeDeclHeader ::
  forall v m.
  (Var v) =>
  Codebase m v Ann ->
  PPE.PrettyPrintEnv ->
  Reference ->
  Sqlite.Transaction (DisplayObject Syntax.SyntaxText Syntax.SyntaxText)
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
  (Var v) => PPE.PrettyPrintEnv -> Maybe Width -> TermEntry v a -> NamedTerm
termEntryToNamedTerm ppe typeWidth te@TermEntry {termEntryType = mayType, termEntryTag = tag, termEntryHash} =
  NamedTerm
    { termName = termEntryHQName te,
      termHash = termEntryHash,
      termType = formatType ppe (mayDefaultWidth typeWidth) <$> mayType,
      termTag = tag
    }

typeEntryToNamedType :: TypeEntry -> NamedType
typeEntryToNamedType te@TypeEntry {typeEntryTag, typeEntryHash} =
  NamedType
    { typeName = typeEntryHQName $ te,
      typeHash = typeEntryHash,
      typeTag = typeEntryTag
    }

-- | Find all definitions and children reachable from the given 'V2Branch.Branch',
lsBranch ::
  (MonadIO m) =>
  Codebase m Symbol Ann ->
  V2Branch.Branch n ->
  m [ShallowListEntry Symbol Ann]
lsBranch codebase b0 = do
  let flattenRefs :: Map NameSegment (Map ref v) -> [(ref, NameSegment)]
      flattenRefs m = do
        (ns, refs) <- Map.toList m
        r <- Map.keys refs
        pure (r, ns)
  termEntries <- for (flattenRefs $ V2Branch.terms b0) $ \(r, ns) -> do
    ShallowTermEntry <$> termListEntry codebase b0 (ExactName ns r)
  typeEntries <-
    Codebase.runTransaction codebase do
      for (flattenRefs $ V2Branch.types b0) \(r, ns) -> do
        let v1Ref = Cv.reference2to1 r
        ShallowTypeEntry <$> typeListEntry codebase b0 (ExactName ns v1Ref)
  childrenWithStats <- Codebase.runTransaction codebase (V2Branch.childStats b0)
  let branchEntries :: [ShallowListEntry Symbol Ann] = do
        (ns, (h, stats)) <- Map.toList $ childrenWithStats
        guard $ V2Branch.hasDefinitions stats
        pure $ ShallowBranchEntry ns (V2Causal.causalHash h) stats
      patchEntries :: [ShallowListEntry Symbol Ann] = do
        (ns, _h) <- Map.toList $ V2Branch.patches b0
        pure $ ShallowPatchEntry ns
  pure . List.sortOn listEntryName $
    termEntries
      ++ typeEntries
      ++ branchEntries
      ++ patchEntries

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
  case Path.toName $ Path.unabsolute root of
    Nothing -> names
    Just prefix -> Names.map (fixName prefix) names
  where
    fixName prefix n =
      if root == Path.absoluteEmpty
        then n
        else fromMaybe (Name.makeAbsolute n) (Name.stripNamePrefix prefix n)

hqNameQuery ::
  Codebase m v Ann ->
  NameSearch Sqlite.Transaction ->
  [HQ.HashQualified Name] ->
  Sqlite.Transaction QueryResult
hqNameQuery codebase NameSearch {typeSearch, termSearch} hqs = do
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
        typeReferencesByShortHash
        hashes
  -- Now do the name queries.
  let mkTermResult sh r = SR.termResult (HQ.HashOnly sh) r Set.empty
      mkTypeResult sh r = SR.typeResult (HQ.HashOnly sh) r Set.empty
      -- Transform the hash results a bit
      termResults =
        (\(sh, tms) -> mkTermResult sh <$> toList tms) <$> termRefs
      typeResults =
        (\(sh, tps) -> mkTypeResult sh <$> toList tps) <$> typeRefs

  Debug.debugLogM Debug.Server "hqNameQuery: Applying Search"
  -- Now do the actual name query
  resultss <- for hqnames (\name -> liftA2 (<>) (applySearch typeSearch name) (applySearch termSearch name))
  Debug.debugLogM Debug.Server "hqNameQuery: Done Search"
  let (misses, hits) =
        zipWith
          ( \hqname results ->
              (if null results then Left hqname else Right results)
          )
          hqnames
          resultss
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
data DefinitionResults = DefinitionResults
  { termResults :: Map Reference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)),
    typeResults :: Map Reference (DisplayObject () (DD.Decl Symbol Ann)),
    noResults :: [HQ.HashQualified Name]
  }

definitionResultsDependencies :: DefinitionResults -> Set LD.LabeledDependency
definitionResultsDependencies (DefinitionResults {termResults, typeResults}) =
  let topLevelTerms = Set.fromList . fmap LD.TermReference $ Map.keys termResults
      topLevelTypes = Set.fromList . fmap LD.TypeReference $ Map.keys typeResults
      termDeps =
        termResults
          & foldOf
            ( folded
                . beside
                  (to Type.labeledDependencies)
                  (to Term.labeledDependencies)
            )
      typeDeps =
        typeResults
          & ifoldMap \typeRef ddObj ->
            foldMap (DD.labeledDeclDependenciesIncludingSelf typeRef) ddObj
   in termDeps <> typeDeps <> topLevelTerms <> topLevelTypes

expandShortCausalHash :: ShortCausalHash -> Backend Sqlite.Transaction CausalHash
expandShortCausalHash hash = do
  hashSet <- lift $ Codebase.causalHashesByPrefix hash
  len <- lift $ Codebase.branchHashLength
  case Set.toList hashSet of
    [] -> throwError $ CouldntExpandBranchHash hash
    [h] -> pure h
    _ ->
      throwError . AmbiguousBranchHash hash $ Set.map (SCH.fromHash len) hashSet

-- | Efficiently resolve a root hash and path to a shallow branch's causal.
getShallowCausalAtPathFromRootHash ::
  Maybe CausalHash ->
  Path ->
  Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
getShallowCausalAtPathFromRootHash mayRootHash path = do
  shallowRoot <- case mayRootHash of
    Nothing -> Codebase.getShallowRootCausal
    Just h -> Codebase.expectCausalBranchByCausalHash h
  Codebase.getShallowCausalAtPath path (Just shallowRoot)

formatType' :: (Var v) => PPE.PrettyPrintEnv -> Width -> Type v a -> SyntaxText
formatType' ppe w =
  Pretty.render w . TypePrinter.prettySyntax ppe

formatType :: (Var v) => PPE.PrettyPrintEnv -> Width -> Type v a -> Syntax.SyntaxText
formatType ppe w = mungeSyntaxText . formatType' ppe w

formatSuffixedType ::
  (Var v) =>
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Type v Ann ->
  Syntax.SyntaxText
formatSuffixedType ppe = formatType (PPED.suffixifiedPPE ppe)

mungeSyntaxText ::
  (Functor g) => g (UST.Element Reference) -> g Syntax.Element
mungeSyntaxText = fmap Syntax.convertElement

-- | Renders a definition for the given name or hash alongside its documentation.
prettyDefinitionsForHQName ::
  -- | The path representing the user's current perspective.
  -- Searches will be limited to definitions within this path, and names will be relative to
  -- this path.
  Path ->
  -- | The root branch to use
  Maybe CausalHash ->
  Maybe Width ->
  -- | Whether to suffixify bindings in the rendered syntax
  Suffixify ->
  -- | Runtime used to evaluate docs. This should be sandboxed if run on the server.
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  -- | The name, hash, or both, of the definition to display.
  HQ.HashQualified Name ->
  Backend IO DefinitionDisplayResults
prettyDefinitionsForHQName path mayRoot renderWidth suffixifyBindings rt codebase query = do
  (shallowRoot) <- (lift . Codebase.runTransaction codebase) do resolveCausalHashV2 mayRoot
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our perspective but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  -- ppe which returns names fully qualified to the current perspective,  not to the codebase root.
  let biases = maybeToList $ HQ.toName query

  Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: building names search"
  (nameSearch, backendPPE) <- mkNamesStuff biases shallowRoot path codebase
  (dr@(DefinitionResults terms types misses), branchAtPath) <- liftIO $ Codebase.runTransaction codebase do
    Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: starting search"
    dr <- definitionsBySuffixes codebase nameSearch DontIncludeCycles [query]
    Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: finished search"
    causalAtPath <- Codebase.getShallowCausalAtPath path (Just shallowRoot)
    branchAtPath <- V2Causal.value causalAtPath
    pure (dr, branchAtPath)

  let width = mayDefaultWidth renderWidth
  let docResults :: Name -> IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults name = do
        docRefs <- docsForTermName codebase nameSearch name
        renderDocRefs backendPPE width codebase rt docRefs
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
        renderedDocs <- lift (maybe (pure []) docResults (HQ.toName hqTermName))
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
      mkTypeDefinition pped r tp = lift do
        let hqTypeName = PPE.typeNameOrHashOnly fqnPPE r
        let bn = bestNameForType @Symbol (PPED.suffixifiedPPE pped) width r
        Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: getting type tag"
        tag <-
          Codebase.runTransaction codebase do
            typeEntryTag <$> typeListEntry codebase branchAtPath (ExactName (NameSegment bn) r)
        Debug.debugLogM Debug.Server "prettyDefinitionsForHQName: getting type tag"
        docs <- liftIO (maybe (pure []) docResults (HQ.toName hqTypeName))
        pure $
          TypeDefinition
            (HQ'.toText <$> PPE.allTypeNames fqnPPE r)
            bn
            tag
            (bimap mungeSyntaxText mungeSyntaxText tp)
            docs
        where
          fqnPPE = PPED.unsuffixifiedPPE pped

  let deps = definitionResultsDependencies dr
  Debug.debugLogM Debug.Server $ "prettyDefinitionsForHQName: building term and type pped. Num deps: " <> show (Set.size deps)
  termAndTypePPED <- PPED.biasTo biases <$> getPPED deps backendPPE
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

mkNamesStuff :: [Name] -> V2Branch.CausalBranch n -> Path -> Codebase IO Symbol Ann -> Backend IO (NameSearch Sqlite.Transaction, BackendPPE)
mkNamesStuff biases shallowRoot path codebase = do
  asks useNamesIndex >>= \case
    True -> do
      Debug.debugLogM Debug.Server "using sqlite index"
      pure (sqliteNameSearch, UseSQLiteIndex codebase rootBranchHash path sqliteNameSearch)
    False -> do
      Debug.debugLogM Debug.Server "using names object"
      hqLength <- liftIO $ Codebase.runTransaction codebase $ Codebase.hashLength
      (localNamesOnly, unbiasedPPED) <- scopedNamesForBranchHash codebase (Just shallowRoot) path
      pure $ (makeNameSearch hqLength (NamesWithHistory.fromCurrentNames localNamesOnly), UsePPED localNamesOnly (PPED.biasTo biases unbiasedPPED))
  where
    rootBranchHash :: BranchHash
    rootBranchHash = V2Causal.valueHash shallowRoot
    sqliteNameSearch :: NameSearch Sqlite.Transaction
    sqliteNameSearch = SqliteNameSearch.scopedNameSearch rootBranchHash path

evalDocRef ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  TermReference ->
  IO (Doc.EvaluatedDoc Symbol)
evalDocRef rt codebase r = do
  let tm = Term.ref () r
  Doc.evalDoc terms typeOf eval decls tm
  where
    terms r@(Reference.Builtin _) = pure (Just (Term.ref () r))
    terms (Reference.DerivedId r) =
      fmap Term.unannotate <$> Codebase.runTransaction codebase (Codebase.getTerm codebase r)

    typeOf r = fmap void <$> Codebase.runTransaction codebase (Codebase.getTypeOfReferent codebase r)
    eval (Term.amap (const mempty) -> tm) = do
      -- We use an empty ppe for evalutation, it's only used for adding additional context to errors.
      let evalPPE = PPE.empty
      let codeLookup = Codebase.toCodeLookup codebase
      let cache r = fmap Term.unannotate <$> Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase r)
      r <- fmap hush . liftIO $ Rt.evaluateTerm' codeLookup cache evalPPE rt tm
      case r of
        Just tmr ->
          Codebase.runTransaction codebase do
            Codebase.putWatch
              WK.RegularWatch
              (Hashing.hashClosedTerm tm)
              (Term.amap (const mempty) tmr)
        Nothing -> pure ()
      pure $ r <&> Term.amap (const mempty)

    decls (Reference.DerivedId r) =
      fmap (DD.amap (const ())) <$> Codebase.runTransaction codebase (Codebase.getTypeDeclaration codebase r)
    decls _ = pure Nothing

-- | Fetch the docs associated with the given name.
-- Returns all references with a Doc type which are at the name provided, or at '<name>.doc'.
docsForTermName ::
  Codebase IO Symbol Ann ->
  NameSearch Sqlite.Transaction ->
  Name ->
  IO [TermReference]
docsForTermName codebase (NameSearch {termSearch}) name = do
  Debug.debugLogM Debug.Server $ "docsForTermName: Docs for: " <> Name.toString name
  let potentialDocNames = [name, name Cons.:> "doc"]
  Codebase.runTransaction codebase do
    refs <-
      potentialDocNames & foldMapM \name ->
        -- TODO: Should replace this with an exact name lookup.
        lookupRelativeHQRefs' termSearch (HQ'.NameOnly name)
    filterForDocs (toList refs)
  where
    filterForDocs :: [Referent] -> Sqlite.Transaction [TermReference]
    filterForDocs rs = do
      rts <- fmap join . for rs $ \case
        Referent.Ref r ->
          maybe [] (pure . (r,)) <$> Codebase.getTypeOfTerm codebase r
        _ -> pure []
      pure [r | (r, t) <- rts, Typechecker.isSubtype t (Type.ref mempty DD.doc2Ref)]

renderDocRefs ::
  BackendPPE ->
  Width ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  [TermReference] ->
  IO [(HashQualifiedName, UnisonHash, Doc.Doc)]
renderDocRefs backendPPE width codebase rt docRefs = do
  Debug.debugLogM Debug.Server $ "Evaluating docs"
  eDocs <- for docRefs \ref -> (ref,) <$> (evalDocRef rt codebase ref)
  let docDeps = foldMap (Doc.dependencies . snd) eDocs <> Set.fromList (LD.TermReference <$> docRefs)
  Debug.debugLogM Debug.Server $ "Building pped for doc refs: " <> show (Set.size docDeps)
  docsPPED <- getPPED docDeps backendPPE
  Debug.debugLogM Debug.Server $ "Done building pped"
  for eDocs \(ref, eDoc) -> do
    let name = bestNameForTerm @Symbol (PPED.suffixifiedPPE docsPPED) width (Referent.Ref ref)
    let hash = Reference.toText ref
    let renderedDoc = Doc.renderDoc docsPPED eDoc
    pure (name, hash, renderedDoc)

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
  let notLib (_, name) = "lib" `notElem` Name.segments name
  (docTermsWithNames, hqLength) <-
    Codebase.runTransaction codebase do
      docTermsWithNames <- filterM (isDoc codebase . fst) (filter notLib allTerms)
      hqLength <- Codebase.hashLength
      pure (docTermsWithNames, hqLength)
  let docNamesByRef = Map.fromList docTermsWithNames
  let printNames = prettyNamesForBranch root (AllNames currentPath)
  let printNamesWithHistory = NamesWithHistory {currentNames = printNames, oldNames = mempty}
  let ppe = PPED.fromNamesDecl hqLength printNamesWithHistory
  docs <- for docTermsWithNames (renderDoc' ppe runtime codebase)
  liftIO $ traverse_ (renderDocToHtmlFile docNamesByRef directory) docs
  where
    renderDoc' ppe runtime codebase (docReferent, name) = do
      let docReference = Referent.toReference docReferent
      doc <- evalDocRef runtime codebase docReference <&> Doc.renderDoc ppe
      let hash = Reference.toText docReference
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
  forall v. (Var v) => PPE.PrettyPrintEnv -> Width -> Referent -> Text
bestNameForTerm ppe width =
  Text.pack
    . Pretty.render width
    . fmap UST.toPlain
    . TermPrinter.runPretty ppe
    . TermPrinter.pretty0 @v TermPrinter.emptyAc
    . Term.fromReferent mempty

bestNameForType ::
  forall v. (Var v) => PPE.PrettyPrintEnv -> Width -> Reference -> Text
bestNameForType ppe width =
  Text.pack
    . Pretty.render width
    . fmap UST.toPlain
    . TypePrinter.prettySyntax @v ppe
    . Type.ref ()

-- | Returns (parse, pretty, local, ppe) where:
--
-- - 'parse' includes ALL fully qualified names from the root, and ALSO all names from within the provided path, relative to that path.
-- - 'pretty' includes names within the provided path, relative to that path, and also all globally scoped names _outside_ of the path
-- - 'local' includes ONLY the names within the provided path
-- - 'ppe' is a ppe which searches for a name within the path first, but falls back to a global name search.
--     The 'suffixified' component of this ppe will search for the shortest unambiguous suffix within the scope in which the name is found (local, falling back to global)
scopedNamesForBranchHash ::
  forall m n v a.
  (MonadIO m) =>
  Codebase m v a ->
  Maybe (V2Branch.CausalBranch n) ->
  Path ->
  Backend m (Names, PPED.PrettyPrintEnvDecl)
scopedNamesForBranchHash codebase mbh path = do
  shouldUseNamesIndex <- asks useNamesIndex
  hashLen <- lift $ Codebase.runTransaction codebase Codebase.hashLength
  (parseNames, localNames) <- case mbh of
    Nothing
      | shouldUseNamesIndex -> do
        lift $ Codebase.runTransaction codebase indexNames
      | otherwise -> do
        rootBranch <- lift $ Codebase.getRootBranch codebase
        let (parseNames, _prettyNames, localNames) = namesForBranch rootBranch (AllNames path)
        pure (parseNames, localNames)
    Just rootCausal -> do
      let ch = V2Causal.causalHash rootCausal
      rootHash <- lift $ Codebase.runTransaction codebase Operations.expectRootCausalHash
      if (ch == rootHash) && shouldUseNamesIndex
        then lift $ Codebase.runTransaction codebase indexNames
        else do
          (parseNames, _pretty, localNames) <- flip namesForBranch (AllNames path) <$> resolveCausalHash (Just ch) codebase
          pure (parseNames, localNames)

  let localPPE = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames localNames)
  let globalPPE = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames parseNames)
  pure (localNames, mkPPE localPPE globalPPE)
  where
    mkPPE :: PPED.PrettyPrintEnvDecl -> PPED.PrettyPrintEnvDecl -> PPED.PrettyPrintEnvDecl
    mkPPE primary addFallback =
      PPED.PrettyPrintEnvDecl
        (PPED.unsuffixifiedPPE primary `PPE.addFallback` PPED.unsuffixifiedPPE addFallback)
        (PPED.suffixifiedPPE primary `PPE.addFallback` PPED.suffixifiedPPE addFallback)
    indexNames :: Sqlite.Transaction (Names, Names)
    indexNames = do
      branch <- Codebase.getShallowRootBranch
      mayProjectRoot <- Projects.inferNamesRoot path branch
      let namesRoot = fromMaybe path mayProjectRoot
      scopedNames <- Codebase.namesAtPath namesRoot path
      pure (ScopedNames.parseNames scopedNames, ScopedNames.namesAtPath scopedNames)

resolveCausalHash ::
  (Monad m) => Maybe CausalHash -> Codebase m v a -> Backend m (Branch m)
resolveCausalHash h codebase = case h of
  Nothing -> lift (Codebase.getRootBranch codebase)
  Just bhash -> do
    mayBranch <- lift $ Codebase.getBranchForHash codebase bhash
    whenNothing mayBranch (throwError $ NoBranchForHash bhash)

resolveCausalHashV2 :: Maybe CausalHash -> Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
resolveCausalHashV2 h = case h of
  Nothing -> Codebase.getShallowRootCausal
  Just ch -> Codebase.expectCausalBranchByCausalHash ch

resolveRootBranchHash ::
  (MonadIO m) => Maybe ShortCausalHash -> Codebase m v a -> Backend m (Branch m)
resolveRootBranchHash mayRoot codebase = case mayRoot of
  Nothing ->
    lift (Codebase.getRootBranch codebase)
  Just sch -> do
    h <- hoistBackend (Codebase.runTransaction codebase) (expandShortCausalHash sch)
    resolveCausalHash (Just h) codebase

resolveRootBranchHashV2 ::
  Maybe ShortCausalHash -> Backend Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
resolveRootBranchHashV2 mayRoot = case mayRoot of
  Nothing -> lift Codebase.getShallowRootCausal
  Just sch -> do
    h <- expandShortCausalHash sch
    lift (resolveCausalHashV2 (Just h))

-- | Determines whether we include full cycles in the results, (e.g. if I search for `isEven`, will I find `isOdd` too?)
--
-- This was once used for both term and decl components, but now is only used for decl components, because 'update' does
-- The Right Thing for terms (i.e. propagates changes to all dependents, including component-mates, which are de facto
-- dependents).
--
-- Ticket of interest: https://github.com/unisonweb/unison/issues/3445
data IncludeCycles
  = IncludeCycles
  | DontIncludeCycles

definitionsBySuffixes ::
  Codebase m Symbol Ann ->
  NameSearch Sqlite.Transaction ->
  IncludeCycles ->
  [HQ.HashQualified Name] ->
  Sqlite.Transaction DefinitionResults
definitionsBySuffixes codebase nameSearch includeCycles query = do
  Debug.debugLogM Debug.Server "definitionsBySuffixes: before query"
  QueryResult misses results <- hqNameQuery codebase nameSearch query
  Debug.debugLogM Debug.Server "definitionsBySuffixes: after query"
  -- todo: remember to replace this with getting components directly,
  -- and maybe even remove getComponentLength from Codebase interface altogether
  terms <- Map.foldMapM (\ref -> (ref,) <$> displayTerm codebase ref) (searchResultsToTermRefs results)
  types <- do
    let typeRefsWithoutCycles = searchResultsToTypeRefs results
    typeRefs <- case includeCycles of
      IncludeCycles ->
        Monoid.foldMapM
          Codebase.componentReferencesForReference
          typeRefsWithoutCycles
      DontIncludeCycles -> pure typeRefsWithoutCycles
    Map.foldMapM (\ref -> (ref,) <$> displayType codebase ref) typeRefs
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

displayTerm :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (DisplayObject (Type Symbol Ann) (Term Symbol Ann))
displayTerm codebase = \case
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

displayType :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (DisplayObject () (DD.Decl Symbol Ann))
displayType codebase = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId rid -> do
    decl <- Codebase.unsafeGetTypeDeclaration codebase rid
    pure (UserObject decl)

termsToSyntax ::
  (Var v) =>
  (Ord a) =>
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
          $ TermPrinter.prettyBinding (ppeBody r) n tm

typesToSyntax ::
  (Var v) =>
  (Ord a) =>
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

-- | Renders a type to its decl header, e.g.
--
-- Effect:
--
-- unique ability Stream s
--
-- Data:
--
-- structural type Maybe a
typeToSyntaxHeader ::
  Width ->
  HQ.HashQualified Name ->
  DisplayObject () (DD.Decl Symbol Ann) ->
  DisplayObject SyntaxText SyntaxText
typeToSyntaxHeader width hqName obj =
  case obj of
    BuiltinObject _ ->
      let syntaxName = Pretty.renderUnbroken . NP.styleHashQualified id $ hqName
       in BuiltinObject syntaxName
    MissingObject sh -> MissingObject sh
    UserObject d ->
      UserObject . Pretty.render width $
        DeclPrinter.prettyDeclHeader hqName d

loadSearchResults ::
  Codebase m Symbol Ann ->
  [SR.SearchResult] ->
  Sqlite.Transaction [SR'.SearchResult' Symbol Ann]
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
  Codebase m v Ann ->
  Reference ->
  Sqlite.Transaction (DisplayObject () (DD.Decl v Ann))
loadTypeDisplayObject c = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> Codebase.getTypeDeclaration c id

data BackendPPE
  = UseSQLiteIndex (Codebase IO Symbol Ann) BranchHash Path (NameSearch Sqlite.Transaction)
  | UsePPED Names PPED.PrettyPrintEnvDecl

-- selectBackendPPE :: Codebase IO Symbol Ann -> Path -> Backend IO BackendPPE
-- selectBackendPPE codebase path = do
--   useNamesIndex <- asks useNamesIndex
--   if useNamesIndex
--     then pure $ UseSQLiteIndex codebase path (error "name search")
--     else do
--       liftIO $ Codebase.runTransaction codebase $ Codebase.hashLength
--       (localNamesOnly, unbiasedPPED) <- scopedNamesForBranchHash codebase Nothing path
--       pure $ UsePPED localNamesOnly unbiasedPPED

-- runPPE :: BackendPPE -> PrettyPrintGrouper (Backend IO) a -> Backend IO a
-- runPPE backendPPE action = do
--   case backendPPE of
--     UseSQLiteIndex cb perspective _nameSearch -> do
--       hashLen <- liftIO $ Codebase.runTransaction cb $ Codebase.hashLength
--       PPESqlite.prettyPrintUsingNamesIndex cb hashLen perspective action
--     UsePPED _names pped -> PPG.runWithPPE pped action

getPPED :: (MonadIO m) => Set LabeledDependency -> BackendPPE -> m PPED.PrettyPrintEnvDecl
getPPED deps = \case
  UseSQLiteIndex codebase rootHash perspective _nameSearch -> do
    liftIO $ Codebase.runTransaction codebase $ PPESqlite.ppedForReferences rootHash perspective deps
  UsePPED _na pped -> pure pped

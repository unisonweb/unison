module U.Codebase.Sqlite.Operations
  ( -- * branches
    saveRootBranch,
    loadRootCausalHash,
    expectRootCausalHash,
    expectRootCausal,
    expectRootBranchHash,
    loadCausalHashAtPath,
    expectCausalHashAtPath,
    saveBranch,
    loadCausalBranchByCausalHash,
    expectCausalBranchByCausalHash,
    expectBranchByBranchHash,
    expectNamespaceStatsByHash,
    expectNamespaceStatsByHashId,

    -- * terms
    Q.saveTermComponent,
    loadTermComponent,
    loadTermByReference,
    loadTypeOfTermByTermReference,

    -- * decls
    Q.saveDeclComponent,
    loadDeclComponent,
    loadDeclByReference,
    expectDeclTypeById,

    -- * terms/decls
    getCycleLen,

    -- * patches
    savePatch,
    expectPatch,

    -- * test for stuff in codebase
    objectExistsForHash,

    -- * watch expression cache
    saveWatch,
    loadWatch,
    listWatches,
    Q.clearWatches,

    -- * indexes

    -- ** nearest common ancestor
    before,
    lca,

    -- ** prefix index
    componentReferencesByPrefix,
    termReferentsByPrefix,
    declReferentsByPrefix,
    causalHashesByPrefix,

    -- ** dependents index
    dependents,
    dependentsOfComponent,

    -- ** type index
    Q.addTypeToIndexForTerm,
    termsHavingType,

    -- ** type mentions index
    Q.addTypeMentionsToIndexForTerm,
    termsMentioningType,

    -- ** name lookup index
    rootNamesByPath,
    NamesByPath (..),
    termNamesForRefWithinNamespace,
    typeNamesForRefWithinNamespace,
    termNamesBySuffix,
    typeNamesBySuffix,
    checkBranchHashNameLookupExists,
    buildNameLookupForBranchHash,

    -- * reflog
    getReflog,
    appendReflog,

    -- * low-level stuff
    expectDbBranch,
    saveDbBranch,
    saveDbBranchUnderHashId,
    expectDbPatch,
    saveDbPatch,
    expectDbBranchByCausalHashId,
    namespaceStatsForDbBranch,

    -- * somewhat unexpectedly unused definitions
    c2sReferenceId,
    c2sReferentId,
    diffPatch,
    decodeTermElementWithType,
    loadTermWithTypeByReference,
    Q.s2cTermWithType,
    Q.s2cDecl,
    declReferencesByPrefix,
    namespaceHashesByPrefix,
    derivedDependencies,
  )
where

import Control.Lens hiding (children)
import qualified Control.Monad.Extra as Monad
import Data.Bitraversable (Bitraversable (bitraverse))
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (uncurry3, (***))
import U.Codebase.Branch.Type (NamespaceStats (..))
import qualified U.Codebase.Branch.Type as C.Branch
import qualified U.Codebase.Causal as C
import U.Codebase.Decl (ConstructorId)
import qualified U.Codebase.Decl as C
import qualified U.Codebase.Decl as C.Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import qualified U.Codebase.Reference as C
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C
import qualified U.Codebase.Referent as C.Referent
import qualified U.Codebase.Reflog as Reflog
import U.Codebase.ShortHash (ShortCausalHash (..), ShortNamespaceHash (..))
import qualified U.Codebase.Sqlite.Branch.Diff as S.Branch
import qualified U.Codebase.Sqlite.Branch.Diff as S.Branch.Diff
import qualified U.Codebase.Sqlite.Branch.Diff as S.BranchDiff
import qualified U.Codebase.Sqlite.Branch.Format as S
import qualified U.Codebase.Sqlite.Branch.Format as S.BranchFormat
import qualified U.Codebase.Sqlite.Branch.Full as S
import qualified U.Codebase.Sqlite.Branch.Full as S.Branch.Full
import qualified U.Codebase.Sqlite.Branch.Full as S.MetadataSet
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import U.Codebase.Sqlite.Decode
import U.Codebase.Sqlite.HashHandle (HashHandle (..))
import U.Codebase.Sqlite.LocalIds
  ( LocalIds,
    WatchLocalIds,
  )
import qualified U.Codebase.Sqlite.LocalizeObject as LocalizeObject
import qualified U.Codebase.Sqlite.NamedRef as S
import qualified U.Codebase.Sqlite.ObjectType as ObjectType
import qualified U.Codebase.Sqlite.Patch.Diff as S
import qualified U.Codebase.Sqlite.Patch.Format as S
import qualified U.Codebase.Sqlite.Patch.Format as S.Patch.Format
import qualified U.Codebase.Sqlite.Patch.Full as S (LocalPatch, Patch, Patch' (..))
import qualified U.Codebase.Sqlite.Patch.TermEdit as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as S.TermEdit
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S.TypeEdit
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Reference as S.Reference
import qualified U.Codebase.Sqlite.Referent as S
import qualified U.Codebase.Sqlite.Referent as S.Referent
import qualified U.Codebase.Sqlite.Serialization as S
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Sqlite.Term.Format as S.Term
import qualified U.Codebase.Term as C
import qualified U.Codebase.Term as C.Term
import qualified U.Codebase.TermEdit as C
import qualified U.Codebase.TermEdit as C.TermEdit
import qualified U.Codebase.TypeEdit as C
import qualified U.Codebase.TypeEdit as C.TypeEdit
import U.Codebase.WatchKind (WatchKind)
import qualified U.Util.Base32Hex as Base32Hex
import qualified U.Util.Serialization as S
import qualified Unison.Hash as H
import qualified Unison.Hash32 as Hash32
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import Unison.Sqlite
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

-- * Error handling

debug :: Bool
debug = False

newtype NeedTypeForBuiltinMetadata
  = NeedTypeForBuiltinMetadata Text
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

-- * Database lookups

objectExistsForHash :: H.Hash -> Transaction Bool
objectExistsForHash h =
  isJust <$> runMaybeT do
    id <- MaybeT . Q.loadHashId . Hash32.fromHash $ h
    MaybeT $ Q.loadObjectIdForAnyHashId id

expectValueHashByCausalHashId :: Db.CausalHashId -> Transaction BranchHash
expectValueHashByCausalHashId = loadValueHashById <=< Q.expectCausalValueHashId
  where
    loadValueHashById :: Db.BranchHashId -> Transaction BranchHash
    loadValueHashById = fmap BranchHash . Q.expectHash . Db.unBranchHashId

expectRootCausalHash :: Transaction CausalHash
expectRootCausalHash = Q.expectCausalHash =<< Q.expectNamespaceRoot

expectRootBranchHash :: Transaction BranchHash
expectRootBranchHash = do
  rootCausalHashId <- Q.expectNamespaceRoot
  expectValueHashByCausalHashId rootCausalHashId

loadRootCausalHash :: Transaction (Maybe CausalHash)
loadRootCausalHash =
  runMaybeT $
    lift . Q.expectCausalHash =<< MaybeT Q.loadNamespaceRoot

-- | Load the causal hash at the given path from the root.
loadCausalHashAtPath :: Q.TextPathSegments -> Transaction (Maybe CausalHash)
loadCausalHashAtPath =
  let go :: Db.CausalHashId -> [Text] -> MaybeT Transaction CausalHash
      go hashId = \case
        [] -> lift (Q.expectCausalHash hashId)
        t : ts -> do
          tid <- MaybeT (Q.loadTextId t)
          S.Branch {children} <- MaybeT (loadDbBranchByCausalHashId hashId)
          (_, hashId') <- MaybeT (pure (Map.lookup tid children))
          go hashId' ts
   in \path -> do
        hashId <- Q.expectNamespaceRoot
        runMaybeT (go hashId path)

-- | Expect the causal hash at the given path from the root.
expectCausalHashAtPath :: Q.TextPathSegments -> Transaction CausalHash
expectCausalHashAtPath =
  let go :: Db.CausalHashId -> [Text] -> Transaction CausalHash
      go hashId = \case
        [] -> Q.expectCausalHash hashId
        t : ts -> do
          tid <- Q.expectTextId t
          S.Branch {children} <- expectDbBranchByCausalHashId hashId
          let (_, hashId') = children Map.! tid
          go hashId' ts
   in \path -> do
        hashId <- Q.expectNamespaceRoot
        go hashId path

-- * Reference transformations

-- ** read existing references

-- | Assumes that a derived reference would already exist in the database
--  (by virtue of dependencies being stored before dependents), but does
--  not assume a builtin reference would.
c2sReference :: C.Reference -> Transaction S.Reference
c2sReference = bitraverse Q.saveText Q.expectObjectIdForPrimaryHash

c2sTextReference :: C.Reference -> S.TextReference
c2sTextReference = bimap id H.toBase32Hex

s2cReference :: S.Reference -> Transaction C.Reference
s2cReference = bitraverse Q.expectText Q.expectPrimaryHashByObjectId

s2cTextReference :: S.TextReference -> C.Reference
s2cTextReference = bimap id H.fromBase32Hex

c2sReferenceId :: C.Reference.Id -> Transaction S.Reference.Id
c2sReferenceId = C.Reference.idH Q.expectObjectIdForPrimaryHash

s2cReferenceId :: S.Reference.Id -> Transaction C.Reference.Id
s2cReferenceId = C.Reference.idH Q.expectPrimaryHashByObjectId

h2cReferenceId :: S.Reference.IdH -> Transaction C.Reference.Id
h2cReferenceId = C.Reference.idH Q.expectHash

h2cReference :: S.ReferenceH -> Transaction C.Reference
h2cReference = bitraverse Q.expectText Q.expectHash

c2hReference :: C.Reference -> MaybeT Transaction S.ReferenceH
c2hReference = bitraverse (MaybeT . Q.loadTextId) (MaybeT . Q.loadHashIdByHash)

s2cReferent :: S.Referent -> Transaction C.Referent
s2cReferent = bitraverse s2cReference s2cReference

s2cTextReferent :: S.TextReferent -> C.Referent
s2cTextReferent = bimap s2cTextReference s2cTextReference

s2cConstructorType :: S.ConstructorType -> C.ConstructorType
s2cConstructorType = \case
  S.DataConstructor -> C.DataConstructor
  S.EffectConstructor -> C.EffectConstructor

c2sConstructorType :: C.ConstructorType -> S.ConstructorType
c2sConstructorType = \case
  C.DataConstructor -> S.DataConstructor
  C.EffectConstructor -> S.EffectConstructor

s2cReferentId :: S.Referent.Id -> Transaction C.Referent.Id
s2cReferentId = bitraverse Q.expectPrimaryHashByObjectId Q.expectPrimaryHashByObjectId

c2sReferent :: C.Referent -> Transaction S.Referent
c2sReferent = bitraverse c2sReference c2sReference

c2sTextReferent :: C.Referent -> S.TextReferent
c2sTextReferent = bimap c2sTextReference c2sTextReference

c2sReferentId :: C.Referent.Id -> Transaction S.Referent.Id
c2sReferentId = bitraverse Q.expectObjectIdForPrimaryHash Q.expectObjectIdForPrimaryHash

h2cReferent :: S.ReferentH -> Transaction C.Referent
h2cReferent = bitraverse h2cReference h2cReference

-- ** convert and save references

saveReferentH :: C.Referent -> Transaction S.ReferentH
saveReferentH = bitraverse Q.saveReferenceH Q.saveReferenceH

-- ** Edits transformations

s2cTermEdit :: S.TermEdit -> Transaction C.TermEdit
s2cTermEdit = \case
  S.TermEdit.Replace r t -> C.TermEdit.Replace <$> s2cReferent r <*> pure (s2cTyping t)
  S.TermEdit.Deprecate -> pure C.TermEdit.Deprecate

s2cTyping :: S.TermEdit.Typing -> C.TermEdit.Typing
s2cTyping = \case
  S.TermEdit.Same -> C.TermEdit.Same
  S.TermEdit.Subtype -> C.TermEdit.Subtype
  S.TermEdit.Different -> C.TermEdit.Different

c2sTyping :: C.TermEdit.Typing -> S.TermEdit.Typing
c2sTyping = \case
  C.TermEdit.Same -> S.TermEdit.Same
  C.TermEdit.Subtype -> S.TermEdit.Subtype
  C.TermEdit.Different -> S.TermEdit.Different

s2cTypeEdit :: S.TypeEdit -> Transaction C.TypeEdit
s2cTypeEdit = \case
  S.TypeEdit.Replace r -> C.TypeEdit.Replace <$> s2cReference r
  S.TypeEdit.Deprecate -> pure C.TypeEdit.Deprecate

-- | assumes that all relevant defns are already in the DB
c2sPatch :: C.Branch.Patch -> Transaction S.Patch
c2sPatch (C.Branch.Patch termEdits typeEdits) =
  S.Patch
    <$> Map.bitraverse saveReferentH (Set.traverse c2sTermEdit) termEdits
    <*> Map.bitraverse Q.saveReferenceH (Set.traverse c2sTypeEdit) typeEdits
  where
    c2sTermEdit = \case
      C.TermEdit.Replace r t -> S.TermEdit.Replace <$> c2sReferent r <*> pure (c2sTyping t)
      C.TermEdit.Deprecate -> pure S.TermEdit.Deprecate

    c2sTypeEdit = \case
      C.TypeEdit.Replace r -> S.TypeEdit.Replace <$> c2sReference r
      C.TypeEdit.Deprecate -> pure S.TypeEdit.Deprecate

-- | produces a diff
-- diff = full - ref; full = diff + ref
diffPatch :: S.LocalPatch -> S.LocalPatch -> S.LocalPatchDiff
diffPatch (S.Patch fullTerms fullTypes) (S.Patch refTerms refTypes) =
  (S.PatchDiff addTermEdits addTypeEdits removeTermEdits removeTypeEdits)
  where
    -- add: present in full. but absent in ref.
    addTermEdits = Map.merge Map.preserveMissing Map.dropMissing addDiffSet fullTerms refTerms
    addTypeEdits = Map.merge Map.preserveMissing Map.dropMissing addDiffSet fullTypes refTypes
    -- remove: present in ref. but absent in full.
    removeTermEdits = Map.merge Map.dropMissing Map.preserveMissing removeDiffSet fullTerms refTerms
    removeTypeEdits = Map.merge Map.dropMissing Map.preserveMissing removeDiffSet fullTypes refTypes
    -- things that are present in full but absent in ref
    addDiffSet,
      removeDiffSet ::
        (Ord k, Ord a) => Map.WhenMatched Identity k (Set a) (Set a) (Set a)
    addDiffSet = Map.zipWithMatched (const Set.difference)
    removeDiffSet = Map.zipWithMatched (const (flip Set.difference))

getCycleLen :: H.Hash -> Transaction (Maybe Word64)
getCycleLen h = do
  when debug $ traceM $ "\ngetCycleLen " ++ (Text.unpack . Base32Hex.toText $ H.toBase32Hex h)
  runMaybeT do
    -- actually want Nothing in case of non term/decl component hash
    oid <- MaybeT (Q.loadObjectIdForAnyHash h)
    -- todo: decodeComponentLengthOnly is unintentionally a hack that relies on
    -- the fact the two things that references can refer to (term and decl
    -- components) have the same basic serialized structure: first a format
    -- byte that is always 0 for now, followed by a framed array representing
    -- the strongly-connected component. :grimace:
    lift (Q.expectObject oid decodeComponentLengthOnly)

-- | Get the 'C.DeclType.DeclType' of a 'C.Reference.Id'.
expectDeclTypeById :: C.Reference.Id -> Transaction C.Decl.DeclType
expectDeclTypeById =
  fmap C.Decl.declType . expectDeclByReference

componentByObjectId :: Db.ObjectId -> Transaction [S.Reference.Id]
componentByObjectId id = do
  when debug . traceM $ "Operations.componentByObjectId " ++ show id
  len <- Q.expectObject id decodeComponentLengthOnly
  pure [C.Reference.Id id i | i <- [0 .. len - 1]]

-- * Codebase operations

-- ** Saving & loading terms

loadTermComponent :: H.Hash -> MaybeT Transaction [(C.Term Symbol, C.Term.Type Symbol)]
loadTermComponent h = do
  oid <- MaybeT (Q.loadObjectIdForAnyHash h)
  S.Term.Term (S.Term.LocallyIndexedComponent elements) <- MaybeT (Q.loadTermObject oid decodeTermFormat)
  lift . traverse (uncurry3 Q.s2cTermWithType) $ Foldable.toList elements

loadTermWithTypeByReference :: C.Reference.Id -> MaybeT Transaction (C.Term Symbol, C.Term.Type Symbol)
loadTermWithTypeByReference (C.Reference.Id h i) = do
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, term, typ) <- MaybeT (Q.loadTermObject oid (decodeTermElementWithType i))
  lift (Q.s2cTermWithType localIds term typ)

loadTermByReference :: C.Reference.Id -> MaybeT Transaction (C.Term Symbol)
loadTermByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTermByReference " ++ show r
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, term) <- MaybeT (Q.loadTermObject oid (decodeTermElementDiscardingType i))
  lift (s2cTerm localIds term)

loadTypeOfTermByTermReference :: C.Reference.Id -> MaybeT Transaction (C.Term.Type Symbol)
loadTypeOfTermByTermReference id@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTypeOfTermByTermReference " ++ show id
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, typ) <- MaybeT (Q.loadTermObject oid (decodeTermElementDiscardingTerm i))
  lift (s2cTypeOfTerm localIds typ)

s2cTerm :: LocalIds -> S.Term.Term -> Transaction (C.Term Symbol)
s2cTerm ids tm = do
  (substText, substHash) <- Q.localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure $ Q.x2cTerm substText substHash tm

s2cTypeOfTerm :: LocalIds -> S.Term.Type -> Transaction (C.Term.Type Symbol)
s2cTypeOfTerm ids tp = do
  (substText, substHash) <- Q.localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure $ Q.x2cTType substText substHash tp

-- *** Watch expressions

listWatches :: WatchKind -> Transaction [C.Reference.Id]
listWatches k = Q.loadWatchesByWatchKind k >>= traverse h2cReferenceId

-- | returns Nothing if the expression isn't cached.
loadWatch :: WatchKind -> C.Reference.Id -> MaybeT Transaction (C.Term Symbol)
loadWatch k r = do
  r' <- C.Reference.idH (lift . Q.saveHashHash) r
  S.Term.WatchResult wlids t <- MaybeT (Q.loadWatch k r' decodeWatchResultFormat)
  lift (w2cTerm wlids t)

saveWatch :: WatchKind -> C.Reference.Id -> C.Term Symbol -> Transaction ()
saveWatch w r t = do
  rs <- C.Reference.idH Q.saveHashHash r
  wterm <- c2wTerm t
  let bytes = S.putBytes S.putWatchResultFormat (uncurry S.Term.WatchResult wterm)
  Q.saveWatch w rs bytes

c2wTerm :: C.Term Symbol -> Transaction (WatchLocalIds, S.Term.Term)
c2wTerm tm = Q.c2xTerm Q.saveText Q.saveHashHash tm Nothing <&> \(w, tm, _) -> (w, tm)

w2cTerm :: WatchLocalIds -> S.Term.Term -> Transaction (C.Term Symbol)
w2cTerm ids tm = do
  (substText, substHash) <- Q.localIdsToLookups Q.expectText Q.expectHash ids
  pure $ Q.x2cTerm substText substHash tm

-- ** Saving & loading type decls

loadDeclComponent :: H.Hash -> MaybeT Transaction [C.Decl Symbol]
loadDeclComponent h = do
  oid <- MaybeT (Q.loadObjectIdForAnyHash h)
  S.Decl.Decl (S.Decl.LocallyIndexedComponent elements) <- MaybeT (Q.loadDeclObject oid decodeDeclFormat)
  lift . traverse (uncurry Q.s2cDecl) $ Foldable.toList elements

loadDeclByReference :: C.Reference.Id -> MaybeT Transaction (C.Decl Symbol)
loadDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadDeclByReference " ++ show r
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  (localIds, decl) <- MaybeT (Q.loadDeclObject oid (decodeDeclElement i))
  lift (Q.s2cDecl localIds decl)

expectDeclByReference :: C.Reference.Id -> Transaction (C.Decl Symbol)
expectDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "expectDeclByReference " ++ show r
  -- retrieve the blob
  Q.expectObjectIdForPrimaryHash h
    >>= (\oid -> Q.expectDeclObject oid (decodeDeclElement i))
    >>= uncurry Q.s2cDecl

-- * Branch transformation

s2cBranch :: S.DbBranch -> Transaction (C.Branch.Branch Transaction)
s2cBranch (S.Branch.Full.Branch tms tps patches children) =
  C.Branch.Branch
    <$> doTerms tms
    <*> doTypes tps
    <*> doPatches patches
    <*> doChildren children
  where
    loadMetadataType :: S.Reference -> Transaction C.Reference
    loadMetadataType = \case
      C.ReferenceBuiltin tId ->
        Q.expectTextCheck tId (Left . NeedTypeForBuiltinMetadata)
      C.ReferenceDerived id ->
        typeReferenceForTerm id >>= h2cReference

    loadTypesForMetadata :: Set S.Reference -> Transaction (Map C.Reference C.Reference)
    loadTypesForMetadata rs =
      Map.fromList
        <$> traverse
          (\r -> (,) <$> s2cReference r <*> loadMetadataType r)
          (Foldable.toList rs)

    doTerms ::
      Map Db.TextId (Map S.Referent S.DbMetadataSet) ->
      Transaction (Map NameSegment (Map C.Referent (Transaction C.Branch.MdValues)))
    doTerms =
      Map.bitraverse
        (fmap NameSegment . Q.expectText)
        ( Map.bitraverse s2cReferent \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doTypes ::
      Map Db.TextId (Map S.Reference S.DbMetadataSet) ->
      Transaction (Map NameSegment (Map C.Reference (Transaction C.Branch.MdValues)))
    doTypes =
      Map.bitraverse
        (fmap NameSegment . Q.expectText)
        ( Map.bitraverse s2cReference \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doPatches ::
      Map Db.TextId Db.PatchObjectId ->
      Transaction (Map NameSegment (PatchHash, Transaction C.Branch.Patch))
    doPatches = Map.bitraverse (fmap NameSegment . Q.expectText) \patchId -> do
      h <- PatchHash <$> (Q.expectPrimaryHashByObjectId . Db.unPatchObjectId) patchId
      pure (h, expectPatch patchId)

    doChildren ::
      Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) ->
      Transaction (Map NameSegment (C.Causal Transaction CausalHash BranchHash (C.Branch.Branch Transaction)))
    doChildren = Map.bitraverse (fmap NameSegment . Q.expectText) \(boId, chId) ->
      C.Causal
        <$> Q.expectCausalHash chId
        <*> expectValueHashByCausalHashId chId
        <*> headParents chId
        <*> pure (expectBranch boId)
      where
        headParents ::
          Db.CausalHashId ->
          Transaction
            ( Map
                CausalHash
                (Transaction (C.Causal Transaction CausalHash BranchHash (C.Branch.Branch Transaction)))
            )
        headParents chId = do
          parentsChIds <- Q.loadCausalParents chId
          fmap Map.fromList $ traverse pairParent parentsChIds
        pairParent ::
          Db.CausalHashId ->
          Transaction
            ( CausalHash,
              Transaction (C.Causal Transaction CausalHash BranchHash (C.Branch.Branch Transaction))
            )
        pairParent chId = do
          h <- Q.expectCausalHash chId
          pure (h, loadCausal chId)
        loadCausal ::
          Db.CausalHashId ->
          Transaction (C.Causal Transaction CausalHash BranchHash (C.Branch.Branch Transaction))
        loadCausal chId = do
          C.Causal
            <$> Q.expectCausalHash chId
            <*> expectValueHashByCausalHashId chId
            <*> headParents chId
            <*> pure (loadValue chId)
        loadValue :: Db.CausalHashId -> Transaction (C.Branch.Branch Transaction)
        loadValue chId = do
          boId <- Q.expectBranchObjectIdByCausalHashId chId
          expectBranch boId

saveRootBranch ::
  HashHandle ->
  C.Branch.CausalBranch Transaction ->
  Transaction (Db.BranchObjectId, Db.CausalHashId)
saveRootBranch hh c = do
  when debug $ traceM $ "Operations.saveRootBranch " ++ show (C.causalHash c)
  (boId, chId) <- saveBranch hh c
  Q.setNamespaceRoot chId
  pure (boId, chId)

-- saveBranch is kind of a "deep save causal"

-- we want a "shallow save causal" that could take a
--   forall m e. Causal m CausalHash BranchHash e
--
-- data Identity a = Identity
-- e == ()
--
-- data C.Branch m = Branch
--   { terms    :: Map NameSegment (Map Referent (m MdValues)),
--     types    :: Map NameSegment (Map Reference (m MdValues)),
--     patches  :: Map NameSegment (PatchHash, m Patch),
--     children :: Map NameSegment (Causal m)
--   }
--
-- U.Codebase.Sqlite.Branch.Full.Branch'
-- type ShallowBranch = Branch' NameSegment Hash PatchHash CausalHash
-- data ShallowBranch causalHash patchHash = ShallowBranch
--   { terms    :: Map NameSegment (Map Referent MdValues),
--     types    :: Map NameSegment (Map Reference MdValues),
--     patches  :: Map NameSegment patchHash,
--     children :: Map NameSegment causalHash
--   }
--
-- data Causal m hc he e = Causal
--  { causalHash :: hc,
--    valueHash :: he,
--    parents :: Map hc (m (Causal m hc he e)),
--    value :: m e
--  }
-- data ShallowCausal causalHash branchHash = ShallowCausal
--  { causalHash :: causalHash,
--    valueHash :: branchHash,
--    parents :: Set causalHash,
--  }
--
-- References, but also values
-- Shallow - Hash? representation of the database relationships

saveBranch ::
  HashHandle ->
  C.Branch.CausalBranch Transaction ->
  Transaction (Db.BranchObjectId, Db.CausalHashId)
saveBranch hh (C.Causal hc he parents me) = do
  when debug $ traceM $ "\nOperations.saveBranch \n  hc = " ++ show hc ++ ",\n  he = " ++ show he ++ ",\n  parents = " ++ show (Map.keys parents)

  -- Save the causal
  (chId, bhId) <- whenNothingM (Q.loadCausalByCausalHash hc) do
    -- if not exist, create these
    chId <- Q.saveCausalHash hc
    bhId <- Q.saveBranchHash he

    parentCausalHashIds <-
      -- so try to save each parent (recursively) before continuing to save hc
      for (Map.toList parents) $ \(parentHash, mcausal) ->
        -- check if we can short circuit the parent before loading it,
        -- by checking if there are causal parents associated with hc
        (flip Monad.fromMaybeM)
          (Q.loadCausalHashIdByCausalHash parentHash)
          (mcausal >>= fmap snd . saveBranch hh)

    -- Save these CausalHashIds to the causal_parents table,
    Q.saveCausal hh chId bhId parentCausalHashIds
    pure (chId, bhId)

  -- Save the namespace
  boId <- flip Monad.fromMaybeM (Q.loadBranchObjectIdByBranchHashId bhId) do
    branch <- me
    dbBranch <- c2sBranch branch
    stats <- namespaceStatsForDbBranch dbBranch
    boId <- saveDbBranchUnderHashId hh bhId stats dbBranch
    pure boId
  pure (boId, chId)
  where
    c2sBranch :: C.Branch.Branch Transaction -> Transaction S.DbBranch
    c2sBranch (C.Branch.Branch terms types patches children) =
      S.Branch
        <$> Map.bitraverse saveNameSegment (Map.bitraverse c2sReferent c2sMetadata) terms
        <*> Map.bitraverse saveNameSegment (Map.bitraverse c2sReference c2sMetadata) types
        <*> Map.bitraverse saveNameSegment savePatchObjectId patches
        <*> Map.bitraverse saveNameSegment (saveBranch hh) children

    saveNameSegment :: NameSegment -> Transaction Db.TextId
    saveNameSegment = Q.saveText . NameSegment.toText

    c2sMetadata :: Transaction C.Branch.MdValues -> Transaction S.Branch.Full.DbMetadataSet
    c2sMetadata mm = do
      C.Branch.MdValues m <- mm
      S.Branch.Full.Inline <$> Set.traverse c2sReference (Map.keysSet m)

    savePatchObjectId :: (PatchHash, Transaction C.Branch.Patch) -> Transaction Db.PatchObjectId
    savePatchObjectId (h, mp) = do
      Q.loadPatchObjectIdForPrimaryHash h >>= \case
        Just patchOID -> pure patchOID
        Nothing -> do
          patch <- mp
          savePatch hh h patch

expectRootCausal :: Transaction (C.Branch.CausalBranch Transaction)
expectRootCausal = Q.expectNamespaceRoot >>= expectCausalBranchByCausalHashId

loadCausalBranchByCausalHash :: CausalHash -> Transaction (Maybe (C.Branch.CausalBranch Transaction))
loadCausalBranchByCausalHash hc = do
  Q.loadCausalHashIdByCausalHash hc >>= \case
    Just chId -> Just <$> expectCausalBranchByCausalHashId chId
    Nothing -> pure Nothing

expectCausalBranchByCausalHashId :: Db.CausalHashId -> Transaction (C.Branch.CausalBranch Transaction)
expectCausalBranchByCausalHashId id = do
  hc <- Q.expectCausalHash id
  hb <- expectValueHashByCausalHashId id
  parentHashIds <- Q.loadCausalParents id
  loadParents <- for parentHashIds \hId -> do
    h <- Q.expectCausalHash hId
    pure (h, expectCausalBranchByCausalHashId hId)
  pure $ C.Causal hc hb (Map.fromList loadParents) (expectBranchByCausalHashId id)

expectCausalBranchByCausalHash :: CausalHash -> Transaction (C.Branch.CausalBranch Transaction)
expectCausalBranchByCausalHash hash = do
  chId <- Q.expectCausalHashIdByCausalHash hash
  expectCausalBranchByCausalHashId chId

expectBranchByCausalHashId :: Db.CausalHashId -> Transaction (C.Branch.Branch Transaction)
expectBranchByCausalHashId id = do
  boId <- Q.expectBranchObjectIdByCausalHashId id
  expectBranch boId

-- | Load a branch value given its causal hash id.
loadDbBranchByCausalHashId :: Db.CausalHashId -> Transaction (Maybe S.DbBranch)
loadDbBranchByCausalHashId causalHashId =
  Q.loadBranchObjectIdByCausalHashId causalHashId >>= \case
    Nothing -> pure Nothing
    Just branchObjectId -> Just <$> expectDbBranch branchObjectId

expectBranchByBranchHashId :: Db.BranchHashId -> Transaction (C.Branch.Branch Transaction)
expectBranchByBranchHashId bhId = do
  boId <- Q.expectBranchObjectIdByBranchHashId bhId
  expectBranch boId

expectBranchByBranchHash :: BranchHash -> Transaction (C.Branch.Branch Transaction)
expectBranchByBranchHash bh = do
  bhId <- Q.expectBranchHashId bh
  expectBranchByBranchHashId bhId

-- | Expect a branch value given its causal hash id.
expectDbBranchByCausalHashId :: Db.CausalHashId -> Transaction S.DbBranch
expectDbBranchByCausalHashId causalHashId = do
  branchObjectId <- Q.expectBranchObjectIdByCausalHashId causalHashId
  expectDbBranch branchObjectId

expectDbBranch :: Db.BranchObjectId -> Transaction S.DbBranch
expectDbBranch id =
  deserializeBranchObject id >>= \case
    S.BranchFormat.Full li f -> pure (S.BranchFormat.localToDbBranch li f)
    S.BranchFormat.Diff r li d -> doDiff r [S.BranchFormat.localToDbDiff li d]
  where
    deserializeBranchObject :: Db.BranchObjectId -> Transaction S.BranchFormat
    deserializeBranchObject id = do
      when debug $ traceM $ "deserializeBranchObject " ++ show id
      Q.expectNamespaceObject (Db.unBranchObjectId id) decodeBranchFormat

    doDiff :: Db.BranchObjectId -> [S.Branch.Diff] -> Transaction S.DbBranch
    doDiff ref ds =
      deserializeBranchObject ref >>= \case
        S.BranchFormat.Full li f -> pure (joinFull (S.BranchFormat.localToDbBranch li f) ds)
        S.BranchFormat.Diff ref' li' d' -> doDiff ref' (S.BranchFormat.localToDbDiff li' d' : ds)
      where
        joinFull :: S.DbBranch -> [S.Branch.Diff] -> S.DbBranch
        joinFull f [] = f
        joinFull
          (S.Branch.Full.Branch tms tps patches children)
          (S.Branch.Diff tms' tps' patches' children' : ds) = joinFull f' ds
            where
              f' =
                S.Branch.Full.Branch
                  (mergeDefns tms tms')
                  (mergeDefns tps tps')
                  (mergePatches patches patches')
                  (mergeChildren children children')
        mergeChildren ::
          (Ord ns) =>
          Map ns (Db.BranchObjectId, Db.CausalHashId) ->
          Map ns S.BranchDiff.ChildOp ->
          Map ns (Db.BranchObjectId, Db.CausalHashId)
        mergeChildren =
          Map.merge
            Map.preserveMissing
            (Map.mapMissing fromChildOp)
            (Map.zipWithMaybeMatched mergeChildOp)
        mergeChildOp ::
          ns ->
          (Db.BranchObjectId, Db.CausalHashId) ->
          S.BranchDiff.ChildOp ->
          Maybe (Db.BranchObjectId, Db.CausalHashId)
        mergeChildOp =
          const . const \case
            S.BranchDiff.ChildAddReplace id -> Just id
            S.BranchDiff.ChildRemove -> Nothing
        fromChildOp :: ns -> S.BranchDiff.ChildOp -> (Db.BranchObjectId, Db.CausalHashId)
        fromChildOp = const \case
          S.BranchDiff.ChildAddReplace id -> id
          S.BranchDiff.ChildRemove -> error "diff tries to remove a nonexistent child"
        mergePatches ::
          (Ord ns) =>
          Map ns Db.PatchObjectId ->
          Map ns S.BranchDiff.PatchOp ->
          Map ns Db.PatchObjectId
        mergePatches =
          Map.merge Map.preserveMissing (Map.mapMissing fromPatchOp) (Map.zipWithMaybeMatched mergePatchOp)
        fromPatchOp :: ns -> S.BranchDiff.PatchOp -> Db.PatchObjectId
        fromPatchOp = const \case
          S.BranchDiff.PatchAddReplace id -> id
          S.BranchDiff.PatchRemove -> error "diff tries to remove a nonexistent child"
        mergePatchOp :: ns -> Db.PatchObjectId -> S.BranchDiff.PatchOp -> Maybe Db.PatchObjectId
        mergePatchOp =
          const . const \case
            S.BranchDiff.PatchAddReplace id -> Just id
            S.BranchDiff.PatchRemove -> Nothing

        mergeDefns ::
          (Ord ns, Ord r) =>
          Map ns (Map r S.MetadataSet.DbMetadataSet) ->
          Map ns (Map r S.BranchDiff.DefinitionOp) ->
          Map ns (Map r S.MetadataSet.DbMetadataSet)
        mergeDefns =
          Map.merge
            Map.preserveMissing
            (Map.mapMissing (const (fmap fromDefnOp)))
            (Map.zipWithMatched (const mergeDefnOp))
        fromDefnOp :: S.BranchDiff.DefinitionOp -> S.MetadataSet.DbMetadataSet
        fromDefnOp = \case
          S.Branch.Diff.AddDefWithMetadata md -> S.MetadataSet.Inline md
          S.Branch.Diff.RemoveDef -> error "diff tries to remove a nonexistent definition"
          S.Branch.Diff.AlterDefMetadata _md -> error "diff tries to change metadata for a nonexistent definition"
        mergeDefnOp ::
          (Ord r) =>
          Map r S.MetadataSet.DbMetadataSet ->
          Map r S.BranchDiff.DefinitionOp ->
          Map r S.MetadataSet.DbMetadataSet
        mergeDefnOp =
          Map.merge
            Map.preserveMissing
            (Map.mapMissing (const fromDefnOp))
            (Map.zipWithMaybeMatched (const mergeDefnOp'))
        mergeDefnOp' ::
          S.MetadataSet.DbMetadataSet ->
          S.BranchDiff.DefinitionOp ->
          Maybe S.MetadataSet.DbMetadataSet
        mergeDefnOp' (S.MetadataSet.Inline md) = \case
          S.Branch.Diff.AddDefWithMetadata _md ->
            error "diff tries to create a child that already exists"
          S.Branch.Diff.RemoveDef -> Nothing
          S.Branch.Diff.AlterDefMetadata md' ->
            let (Set.fromList -> adds, Set.fromList -> removes) = S.BranchDiff.addsRemoves md'
             in Just . S.MetadataSet.Inline $ (Set.union adds $ Set.difference md removes)

-- | Save a 'S.DbBranch', given its hash (which the caller is expected to produce from the branch).
--
-- Note: long-standing question: should this package depend on the hashing package? (If so, we would only need to take
-- the DbBranch, and hash internally).
saveDbBranch ::
  HashHandle ->
  BranchHash ->
  C.Branch.NamespaceStats ->
  S.DbBranch ->
  Transaction Db.BranchObjectId
saveDbBranch hh hash stats branch = do
  hashId <- Q.saveBranchHash hash
  saveDbBranchUnderHashId hh hashId stats branch

-- | Variant of 'saveDbBranch' that might be preferred by callers that already have a hash id, not a hash.
saveDbBranchUnderHashId ::
  HashHandle ->
  Db.BranchHashId ->
  C.Branch.NamespaceStats ->
  S.DbBranch ->
  Transaction Db.BranchObjectId
saveDbBranchUnderHashId hh bhId@(Db.unBranchHashId -> hashId) stats branch = do
  let (localBranchIds, localBranch) = LocalizeObject.localizeBranch branch
  when debug $
    traceM $
      "saveBranchObject\n\tid = "
        ++ show bhId
        ++ "\n\tli = "
        ++ show localBranchIds
        ++ "\n\tlBranch = "
        ++ show localBranch
  let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full localBranchIds localBranch
  oId <- Q.saveObject hh hashId ObjectType.Namespace bytes
  Q.saveNamespaceStats bhId stats
  pure $ Db.BranchObjectId oId

expectBranch :: Db.BranchObjectId -> Transaction (C.Branch.Branch Transaction)
expectBranch id =
  expectDbBranch id >>= s2cBranch

-- * Patch transformation

expectPatch :: Db.PatchObjectId -> Transaction C.Branch.Patch
expectPatch patchId =
  expectDbPatch patchId >>= s2cPatch

expectDbPatch :: Db.PatchObjectId -> Transaction S.Patch
expectDbPatch patchId =
  deserializePatchObject patchId >>= \case
    S.Patch.Format.Full li p -> pure (S.Patch.Format.localPatchToPatch li p)
    S.Patch.Format.Diff ref li d -> doDiff ref [S.Patch.Format.localPatchDiffToPatchDiff li d]
  where
    doDiff :: Db.PatchObjectId -> [S.PatchDiff] -> Transaction S.Patch
    doDiff ref ds =
      deserializePatchObject ref >>= \case
        S.Patch.Format.Full li f -> pure (S.Patch.Format.applyPatchDiffs (S.Patch.Format.localPatchToPatch li f) ds)
        S.Patch.Format.Diff ref' li' d' -> doDiff ref' (S.Patch.Format.localPatchDiffToPatchDiff li' d' : ds)

savePatch ::
  HashHandle ->
  PatchHash ->
  C.Branch.Patch ->
  Transaction Db.PatchObjectId
savePatch hh h c = do
  (li, lPatch) <- LocalizeObject.localizePatch <$> c2sPatch c
  saveDbPatch hh h (S.Patch.Format.Full li lPatch)

saveDbPatch ::
  HashHandle ->
  PatchHash ->
  S.PatchFormat ->
  Transaction Db.PatchObjectId
saveDbPatch hh hash patch = do
  hashId <- Q.saveHashHash (unPatchHash hash)
  let bytes = S.putBytes S.putPatchFormat patch
  Db.PatchObjectId <$> Q.saveObject hh hashId ObjectType.Patch bytes

s2cPatch :: S.Patch -> Transaction C.Branch.Patch
s2cPatch (S.Patch termEdits typeEdits) =
  C.Branch.Patch
    <$> Map.bitraverse h2cReferent (Set.traverse s2cTermEdit) termEdits
    <*> Map.bitraverse h2cReference (Set.traverse s2cTypeEdit) typeEdits

deserializePatchObject :: Db.PatchObjectId -> Transaction S.PatchFormat
deserializePatchObject id = do
  when debug $ traceM $ "Operations.deserializePatchObject " ++ show id
  Q.expectPatchObject (Db.unPatchObjectId id) decodePatchFormat

lca :: CausalHash -> CausalHash -> Transaction (Maybe CausalHash)
lca h1 h2 = runMaybeT do
  chId1 <- MaybeT $ Q.loadCausalHashIdByCausalHash h1
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  chId3 <- MaybeT $ Q.lca chId1 chId2
  lift (Q.expectCausalHash chId3)

before :: CausalHash -> CausalHash -> Transaction (Maybe Bool)
before h1 h2 = runMaybeT do
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  lift (Q.loadCausalHashIdByCausalHash h1) >>= \case
    Just chId1 -> lift (Q.before chId1 chId2)
    Nothing -> pure False

-- * Searches

termsHavingType :: C.Reference -> Transaction (Set C.Referent.Id)
termsHavingType cTypeRef =
  runMaybeT (c2hReference cTypeRef) >>= \case
    Nothing -> pure Set.empty
    Just sTypeRef -> do
      sIds <- Q.getReferentsByType sTypeRef
      set <- traverse s2cReferentId sIds
      pure (Set.fromList set)

typeReferenceForTerm :: S.Reference.Id -> Transaction S.ReferenceH
typeReferenceForTerm = Q.getTypeReferenceForReferent . C.Referent.RefId

termsMentioningType :: C.Reference -> Transaction (Set C.Referent.Id)
termsMentioningType cTypeRef =
  runMaybeT (c2hReference cTypeRef) >>= \case
    Nothing -> pure Set.empty
    Just sTypeRef -> do
      sIds <- Q.getReferentsByTypeMention sTypeRef
      set <- traverse s2cReferentId sIds
      pure (Set.fromList set)

-- something kind of funny here.  first, we don't need to enumerate all the reference pos if we're just picking one
-- second, it would be nice if we could leave these as S.References a little longer
-- so that we remember how to blow up if they're missing
componentReferencesByPrefix :: ObjectType.ObjectType -> Text -> Maybe C.Reference.Pos -> Transaction [S.Reference.Id]
componentReferencesByPrefix ot b32prefix pos = do
  oIds :: [Db.ObjectId] <- Q.objectIdByBase32Prefix ot b32prefix
  let test = maybe (const True) (==) pos
  let filterComponent l = [x | x@(C.Reference.Id _ pos) <- l, test pos]
  join <$> traverse (fmap filterComponent . componentByObjectId) oIds

-- | Get the set of user-defined terms whose hash matches the given prefix.
termReferencesByPrefix :: Text -> Maybe Word64 -> Transaction [C.Reference.Id]
termReferencesByPrefix t w =
  componentReferencesByPrefix ObjectType.TermComponent t w
    >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)

declReferencesByPrefix :: Text -> Maybe Word64 -> Transaction [C.Reference.Id]
declReferencesByPrefix t w =
  componentReferencesByPrefix ObjectType.DeclComponent t w
    >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)

termReferentsByPrefix :: Text -> Maybe Word64 -> Transaction [C.Referent.Id]
termReferentsByPrefix b32prefix pos =
  fmap C.Referent.RefId <$> termReferencesByPrefix b32prefix pos

-- todo: simplify this if we stop caring about constructor type
-- todo: remove the cycle length once we drop it from Unison.Reference
declReferentsByPrefix ::
  Text ->
  Maybe C.Reference.Pos ->
  Maybe ConstructorId ->
  Transaction [(H.Hash, C.Reference.Pos, C.DeclType, [C.Decl.ConstructorId])]
declReferentsByPrefix b32prefix pos cid = do
  componentReferencesByPrefix ObjectType.DeclComponent b32prefix pos
    >>= traverse (loadConstructors cid)
  where
    loadConstructors ::
      Maybe Word64 ->
      S.Reference.Id ->
      Transaction (H.Hash, C.Reference.Pos, C.DeclType, [ConstructorId])
    loadConstructors cid rid@(C.Reference.Id oId pos) = do
      (dt, ctorCount) <- getDeclCtorCount rid
      h <- Q.expectPrimaryHashByObjectId oId
      let cids =
            case cid of
              Nothing -> take ctorCount [0 :: ConstructorId ..]
              Just cid -> if fromIntegral cid < ctorCount then [cid] else []
      pure (h, pos, dt, cids)
    getDeclCtorCount :: S.Reference.Id -> Transaction (C.Decl.DeclType, Int)
    getDeclCtorCount id@(C.Reference.Id r i) = do
      when debug $ traceM $ "getDeclCtorCount " ++ show id
      (_localIds, decl) <- Q.expectDeclObject r (decodeDeclElement i)
      pure (C.Decl.declType decl, length (C.Decl.constructorTypes decl))

namespaceHashesByPrefix :: ShortNamespaceHash -> Transaction (Set BranchHash)
namespaceHashesByPrefix (ShortNamespaceHash b32prefix) = do
  hashIds <- Q.namespaceHashIdByBase32Prefix b32prefix
  hashes <- traverse (Q.expectHash . Db.unBranchHashId) hashIds
  pure $ Set.fromList . map BranchHash $ hashes

causalHashesByPrefix :: ShortCausalHash -> Transaction (Set CausalHash)
causalHashesByPrefix (ShortCausalHash b32prefix) = do
  hashIds <- Q.causalHashIdByBase32Prefix b32prefix
  hashes <- traverse (Q.expectHash . Db.unCausalHashId) hashIds
  pure $ Set.fromList . map CausalHash $ hashes

-- | returns a list of known definitions referencing `r`
dependents :: Q.DependentsSelector -> C.Reference -> Transaction (Set C.Reference.Id)
dependents selector r = do
  mr <- case r of
    C.ReferenceBuiltin {} -> pure (Just r)
    C.ReferenceDerived id_ ->
      objectExistsForHash (view C.idH id_) <&> \case
        True -> Just r
        False -> Nothing
  case mr of
    Nothing -> pure mempty
    Just r -> do
      r' <- c2sReference r
      sIds <- Q.getDependentsForDependency selector r'
      Set.traverse s2cReferenceId sIds

-- | returns a list of known definitions referencing `h`
dependentsOfComponent :: H.Hash -> Transaction (Set C.Reference.Id)
dependentsOfComponent h = do
  oId <- Q.expectObjectIdForPrimaryHash h
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependencyComponent oId
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- | returns empty set for unknown inputs; doesn't distinguish between term and decl
derivedDependencies :: C.Reference.Id -> Transaction (Set C.Reference.Id)
derivedDependencies cid = do
  sid <- c2sReferenceId cid
  sids <- Q.getDependencyIdsForDependent sid
  cids <- traverse s2cReferenceId sids
  pure $ Set.fromList cids

-- | Apply a set of name updates to an existing index.
buildNameLookupForBranchHash ::
  -- The existing name lookup index to copy before applying the diff.
  -- If Nothing, run the diff against an empty index.
  -- If Just, the name lookup must exist or an error will be thrown.
  Maybe BranchHash ->
  BranchHash ->
  -- |  (add terms, remove terms)
  ([S.NamedRef (C.Referent, Maybe C.ConstructorType)], [S.NamedRef C.Referent]) ->
  -- |  (add types, remove types)
  ([S.NamedRef C.Reference], [S.NamedRef C.Reference]) ->
  Transaction ()
buildNameLookupForBranchHash mayExistingBranchIndex newBranchHash (newTermNames, removedTermNames) (newTypeNames, removedTypeNames) = do
  newBranchHashId <- Q.expectBranchHashId newBranchHash
  Q.trackNewBranchHashNameLookup newBranchHashId
  case mayExistingBranchIndex of
    Nothing -> pure ()
    Just existingBranchIndex -> do
      unlessM (checkBranchHashNameLookupExists existingBranchIndex) $ error "buildNameLookupForBranchHash: existingBranchIndex was provided, but no index was found for that branch hash."
      existingBranchHashId <- Q.expectBranchHashId existingBranchIndex
      Q.copyScopedNameLookup existingBranchHashId newBranchHashId
  Q.removeScopedTermNames newBranchHashId ((fmap c2sTextReferent <$> removedTermNames))
  Q.removeScopedTypeNames newBranchHashId ((fmap c2sTextReference <$> removedTypeNames))
  Q.insertScopedTermNames newBranchHashId (fmap (c2sTextReferent *** fmap c2sConstructorType) <$> newTermNames)
  Q.insertScopedTypeNames newBranchHashId (fmap c2sTextReference <$> newTypeNames)

-- | Check whether we've already got an index for a given causal hash.
checkBranchHashNameLookupExists :: BranchHash -> Transaction Bool
checkBranchHashNameLookupExists bh = do
  bhId <- Q.expectBranchHashId bh
  Q.checkBranchHashNameLookupExists bhId

data NamesByPath = NamesByPath
  { termNamesInPath :: [S.NamedRef (C.Referent, Maybe C.ConstructorType)],
    typeNamesInPath :: [S.NamedRef C.Reference]
  }

-- | Get all the term and type names for the root namespace from the lookup table.
-- Requires that an index for this branch hash already exists, which is currently
-- only true on Share.
rootNamesByPath ::
  -- | A relative namespace string, e.g. Just "base.List"
  Maybe Text ->
  Transaction NamesByPath
rootNamesByPath path = do
  bhId <- Q.expectNamespaceRootBranchHashId
  termNamesInPath <- Q.termNamesWithinNamespace bhId path
  typeNamesInPath <- Q.typeNamesWithinNamespace bhId path
  pure $
    NamesByPath
      { termNamesInPath = convertTerms <$> termNamesInPath,
        typeNamesInPath = convertTypes <$> typeNamesInPath
      }
  where
    convertTerms = fmap (bimap s2cTextReferent (fmap s2cConstructorType))
    convertTypes = fmap s2cTextReference

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a names for a given Referent.
termNamesForRefWithinNamespace :: BranchHash -> Q.NamespaceText -> C.Referent -> Maybe S.ReversedSegments -> Transaction [S.ReversedSegments]
termNamesForRefWithinNamespace bh namespace ref maySuffix = do
  bhId <- Q.expectBranchHashId bh
  Q.termNamesForRefWithinNamespace bhId namespace (c2sTextReferent ref) maySuffix

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a names for a given Reference, with an optional required suffix.
typeNamesForRefWithinNamespace :: BranchHash -> Q.NamespaceText -> C.Reference -> Maybe S.ReversedSegments -> Transaction [S.ReversedSegments]
typeNamesForRefWithinNamespace bh namespace ref maySuffix = do
  bhId <- Q.expectBranchHashId bh
  Q.typeNamesForRefWithinNamespace bhId namespace (c2sTextReference ref) maySuffix

termNamesBySuffix :: BranchHash -> Q.NamespaceText -> S.ReversedSegments -> Transaction [S.NamedRef (C.Referent, Maybe C.ConstructorType)]
termNamesBySuffix bh namespace suffix = do
  bhId <- Q.expectBranchHashId bh
  Q.termNamesBySuffix bhId namespace suffix <&> fmap (fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

typeNamesBySuffix :: BranchHash -> Q.NamespaceText -> S.ReversedSegments -> Transaction [S.NamedRef C.Reference]
typeNamesBySuffix bh namespace suffix = do
  bhId <- Q.expectBranchHashId bh
  Q.typeNamesBySuffix bhId namespace suffix <&> fmap (fmap s2cTextReference)

-- | Looks up statistics for a given branch, if none exist, we compute them and save them
-- then return them.
expectNamespaceStatsByHash :: BranchHash -> Transaction C.Branch.NamespaceStats
expectNamespaceStatsByHash bh = do
  bhId <- Q.expectBranchHashId bh
  expectNamespaceStatsByHashId bhId

-- | Looks up statistics for a given branch, if none exist, we compute them and save them
-- then return them.
expectNamespaceStatsByHashId :: Db.BranchHashId -> Transaction C.Branch.NamespaceStats
expectNamespaceStatsByHashId bhId = do
  Q.loadNamespaceStatsByHashId bhId `whenNothingM` do
    boId <- Db.BranchObjectId <$> Q.expectObjectIdForPrimaryHashId (Db.unBranchHashId bhId)
    dbBranch <- expectDbBranch boId
    stats <- namespaceStatsForDbBranch dbBranch
    Q.saveNamespaceStats bhId stats
    pure stats

namespaceStatsForDbBranch :: S.DbBranch -> Transaction NamespaceStats
namespaceStatsForDbBranch S.Branch {terms, types, patches, children} = do
  childStats <- for children \(_boId, chId) -> do
    bhId <- Q.expectCausalValueHashId chId
    expectNamespaceStatsByHashId bhId
  pure $
    NamespaceStats
      { numContainedTerms =
          let childTermCount = sumOf (folded . to numContainedTerms) childStats
              termCount = lengthOf (folded . folded) terms
           in childTermCount + termCount,
        numContainedTypes =
          let childTypeCount = sumOf (folded . to numContainedTypes) childStats
              typeCount = lengthOf (folded . folded) types
           in childTypeCount + typeCount,
        numContainedPatches =
          let childPatchCount = sumOf (folded . to numContainedPatches) childStats
              patchCount = Map.size patches
           in childPatchCount + patchCount
      }

-- | Gets the specified number of reflog entries in chronological order, most recent first.
getReflog :: Int -> Transaction [Reflog.Entry CausalHash Text]
getReflog numEntries = do
  entries <- Q.getReflog numEntries
  traverse (bitraverse Q.expectCausalHash pure) entries

appendReflog :: Reflog.Entry CausalHash Text -> Transaction ()
appendReflog entry = do
  dbEntry <- (bitraverse Q.saveCausalHash pure) entry
  Q.appendReflog dbEntry

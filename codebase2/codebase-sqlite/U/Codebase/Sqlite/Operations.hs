module U.Codebase.Sqlite.Operations
  ( -- * branches
    saveRootBranch,
    loadRootCausalHash,
    expectRootCausalHash,
    expectRootCausal,
    saveBranch,
    loadCausalBranchByCausalHash,

    -- * terms
    saveTermComponent,
    loadTermComponent,
    loadTermByReference,
    loadTypeOfTermByTermReference,

    -- * decls
    saveDeclComponent,
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
    clearWatches,

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
    addTypeToIndexForTerm,
    termsHavingType,

    -- ** type mentions index
    addTypeMentionsToIndexForTerm,
    termsMentioningType,

    -- * low-level stuff
    expectDbBranch,
    expectDbPatch,
    saveBranchObject,
    saveDbPatch,

    -- * somewhat unexpectedly unused definitions
    c2sReferenceId,
    c2sReferentId,
    diffPatch,
    decodeTermElementWithType,
    loadTermWithTypeByReference,
    s2cTermWithType,
    declReferencesByPrefix,
    branchHashesByPrefix,
    derivedDependencies,
  )
where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import qualified Control.Monad.Extra as Monad
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Writer (MonadWriter, runWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Bytes.Get (runGetS)
import qualified Data.Bytes.Get as Get
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (uncurry3)
import qualified Data.Vector as Vector
import qualified U.Codebase.Branch as C.Branch
import qualified U.Codebase.Causal as C
import U.Codebase.Decl (ConstructorId)
import qualified U.Codebase.Decl as C
import qualified U.Codebase.Decl as C.Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import qualified U.Codebase.Reference as C
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.ShortHash (ShortBranchHash (ShortBranchHash))
import qualified U.Codebase.Sqlite.Branch.Diff as S.Branch
import qualified U.Codebase.Sqlite.Branch.Diff as S.Branch.Diff
import qualified U.Codebase.Sqlite.Branch.Diff as S.BranchDiff
import U.Codebase.Sqlite.Branch.Format (BranchLocalIds)
import qualified U.Codebase.Sqlite.Branch.Format as S
import qualified U.Codebase.Sqlite.Branch.Format as S.BranchFormat
import qualified U.Codebase.Sqlite.Branch.Full as S
import qualified U.Codebase.Sqlite.Branch.Full as S.Branch.Full
import qualified U.Codebase.Sqlite.Branch.Full as S.MetadataSet
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import U.Codebase.Sqlite.LocalIds
  ( LocalDefnId (..),
    LocalIds,
    LocalIds' (..),
    LocalTextId (..),
    WatchLocalIds,
  )
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import qualified U.Codebase.Sqlite.LocalizeObject as LocalizeObject
import qualified U.Codebase.Sqlite.ObjectType as OT
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
import qualified U.Codebase.Type as C.Type
import qualified U.Codebase.TypeEdit as C
import qualified U.Codebase.TypeEdit as C.TypeEdit
import U.Codebase.WatchKind (WatchKind)
import qualified U.Core.ABT as ABT
import qualified U.Util.Base32Hex as Base32Hex
import qualified U.Util.Hash as H
import qualified U.Util.Lens as Lens
import U.Util.Serialization (Get)
import qualified U.Util.Serialization as S
import qualified U.Util.Term as TermUtil
import Unison.Prelude
import Unison.Sqlite
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

-- * Error handling

debug :: Bool
debug = False

data DecodeError = DecodeError
  { decoder :: Text, -- the name of the decoder
    err :: String -- the error message
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

newtype NeedTypeForBuiltinMetadata
  = NeedTypeForBuiltinMetadata Text
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

getFromBytesOr :: Text -> Get a -> ByteString -> Either DecodeError a
getFromBytesOr decoder get bs = case runGetS get bs of
  Left err -> Left (DecodeError decoder err)
  Right a -> Right a

-- * Database lookups

objectExistsForHash :: H.Hash -> Transaction Bool
objectExistsForHash h =
  isJust <$> runMaybeT do
    id <- MaybeT . Q.loadHashId . H.toBase32Hex $ h
    MaybeT $ Q.loadObjectIdForAnyHashId id

expectValueHashByCausalHashId :: Db.CausalHashId -> Transaction BranchHash
expectValueHashByCausalHashId = loadValueHashById <=< Q.expectCausalValueHashId
  where
    loadValueHashById :: Db.BranchHashId -> Transaction BranchHash
    loadValueHashById = fmap BranchHash . Q.expectHash . Db.unBranchHashId

expectRootCausalHash :: Transaction CausalHash
expectRootCausalHash = Q.expectCausalHash =<< Q.expectNamespaceRoot

loadRootCausalHash :: Transaction (Maybe CausalHash)
loadRootCausalHash =
  runMaybeT $
    lift . Q.expectCausalHash =<< MaybeT Q.loadNamespaceRoot

-- * Reference transformations

-- ** read existing references

-- | Assumes that a derived reference would already exist in the database
--  (by virtue of dependencies being stored before dependents), but does
--  not assume a builtin reference would.
c2sReference :: C.Reference -> Transaction S.Reference
c2sReference = bitraverse Q.saveText Q.expectObjectIdForPrimaryHash

s2cReference :: S.Reference -> Transaction C.Reference
s2cReference = bitraverse Q.expectText Q.expectPrimaryHashByObjectId

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

s2cReferentId :: S.Referent.Id -> Transaction C.Referent.Id
s2cReferentId = bitraverse Q.expectPrimaryHashByObjectId Q.expectPrimaryHashByObjectId

c2sReferent :: C.Referent -> Transaction S.Referent
c2sReferent = bitraverse c2sReference c2sReference

c2sReferentId :: C.Referent.Id -> Transaction S.Referent.Id
c2sReferentId = bitraverse Q.expectObjectIdForPrimaryHash Q.expectObjectIdForPrimaryHash

h2cReferent :: S.ReferentH -> Transaction C.Referent
h2cReferent = bitraverse h2cReference h2cReference

-- ** convert and save references

-- | Save the text and hash parts of a Reference to the database and substitute their ids.
saveReferenceH :: C.Reference -> Transaction S.ReferenceH
saveReferenceH = bitraverse Q.saveText Q.saveHashHash

saveReferentH :: C.Referent -> Transaction S.ReferentH
saveReferentH = bitraverse saveReferenceH saveReferenceH

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
    <*> Map.bitraverse saveReferenceH (Set.traverse c2sTypeEdit) typeEdits
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

-- * Deserialization helpers

decodeBranchFormat :: ByteString -> Either DecodeError S.BranchFormat.BranchFormat
decodeBranchFormat = getFromBytesOr "getBranchFormat" S.getBranchFormat

decodePatchFormat :: ByteString -> Either DecodeError S.Patch.Format.PatchFormat
decodePatchFormat = getFromBytesOr "getPatchFormat" S.getPatchFormat

decodeTermFormat :: ByteString -> Either DecodeError S.Term.TermFormat
decodeTermFormat = getFromBytesOr "getTermFormat" S.getTermFormat

decodeComponentLengthOnly :: ByteString -> Either DecodeError Word64
decodeComponentLengthOnly = getFromBytesOr "lengthFramedArray" (Get.skip 1 >> S.lengthFramedArray)

decodeTermElementWithType :: C.Reference.Pos -> ByteString -> Either DecodeError (LocalIds, S.Term.Term, S.Term.Type)
decodeTermElementWithType i = getFromBytesOr ("lookupTermElement" <> tShow i) (S.lookupTermElement i)

decodeTermElementDiscardingTerm :: C.Reference.Pos -> ByteString -> Either DecodeError (LocalIds, S.Term.Type)
decodeTermElementDiscardingTerm i =
  getFromBytesOr ("lookupTermElementDiscardingTerm " <> tShow i) (S.lookupTermElementDiscardingTerm i)

decodeTermElementDiscardingType :: C.Reference.Pos -> ByteString -> Either DecodeError (LocalIds, S.Term.Term)
decodeTermElementDiscardingType i =
  getFromBytesOr ("lookupTermElementDiscardingType " <> tShow i) (S.lookupTermElementDiscardingType i)

decodeDeclFormat :: ByteString -> Either DecodeError S.Decl.DeclFormat
decodeDeclFormat = getFromBytesOr "getDeclFormat" S.getDeclFormat

decodeDeclElement :: Word64 -> ByteString -> Either DecodeError (LocalIds, S.Decl.Decl Symbol)
decodeDeclElement i = getFromBytesOr ("lookupDeclElement " <> tShow i) (S.lookupDeclElement i)

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
  lift . traverse (uncurry3 s2cTermWithType) $ Foldable.toList elements

saveTermComponent :: H.Hash -> [(C.Term Symbol, C.Term.Type Symbol)] -> Transaction Db.ObjectId
saveTermComponent h terms = do
  when debug . traceM $ "Operations.saveTermComponent " ++ show h
  sTermElements <- traverse (uncurry c2sTerm) terms
  hashId <- Q.saveHashHash h
  let li = S.Term.LocallyIndexedComponent $ Vector.fromList sTermElements
      bytes = S.putBytes S.putTermFormat $ S.Term.Term li
  oId <- Q.saveObject hashId OT.TermComponent bytes
  -- populate dependents index
  let dependencies :: Set (S.Reference.Reference, S.Reference.Id) = foldMap unlocalizeRefs (sTermElements `zip` [0 ..])
      unlocalizeRefs :: ((LocalIds, S.Term.Term, S.Term.Type), C.Reference.Pos) -> Set (S.Reference.Reference, S.Reference.Id)
      unlocalizeRefs ((LocalIds tIds oIds, tm, tp), i) =
        let self = C.Reference.Id oId i
            dependencies :: Set S.Reference =
              let (tmRefs, tpRefs, tmLinks, tpLinks) = TermUtil.dependencies tm
                  tpRefs' = Foldable.toList $ C.Type.dependencies tp
                  getTermSRef :: S.Term.TermRef -> S.Reference
                  getTermSRef = \case
                    C.ReferenceBuiltin t -> C.ReferenceBuiltin (tIds Vector.! fromIntegral t)
                    C.Reference.Derived Nothing i -> C.Reference.Derived oId i -- index self-references
                    C.Reference.Derived (Just h) i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
                  getTypeSRef :: S.Term.TypeRef -> S.Reference
                  getTypeSRef = \case
                    C.ReferenceBuiltin t -> C.ReferenceBuiltin (tIds Vector.! fromIntegral t)
                    C.Reference.Derived h i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
                  getSTypeLink = getTypeSRef
                  getSTermLink :: S.Term.TermLink -> S.Reference
                  getSTermLink = \case
                    C.Referent.Con ref _conId -> getTypeSRef ref
                    C.Referent.Ref ref -> getTermSRef ref
               in Set.fromList $
                    map getTermSRef tmRefs
                      ++ map getSTermLink tmLinks
                      ++ map getTypeSRef (tpRefs ++ tpRefs')
                      ++ map getSTypeLink tpLinks
         in Set.map (,self) dependencies
  traverse_ (uncurry Q.addToDependentsIndex) dependencies

  pure oId

-- | implementation detail of c2{s,w}Term
--  The Type is optional, because we don't store them for watch expression results.
c2xTerm :: forall m t d. Monad m => (Text -> m t) -> (H.Hash -> m d) -> C.Term Symbol -> Maybe (C.Term.Type Symbol) -> m (LocalIds' t d, S.Term.Term, Maybe (S.Term.Type))
c2xTerm saveText saveDefn tm tp =
  done =<< (runWriterT . flip evalStateT mempty) do
    sterm <- ABT.transformM go tm
    stype <- traverse (ABT.transformM goType) tp
    pure (sterm, stype)
  where
    go :: forall m a. (MonadWriter (Seq Text, Seq H.Hash) m, MonadState (Map Text LocalTextId, Map H.Hash LocalDefnId) m) => C.Term.F Symbol a -> m (S.Term.F a)
    go = \case
      C.Term.Int n -> pure $ C.Term.Int n
      C.Term.Nat n -> pure $ C.Term.Nat n
      C.Term.Float n -> pure $ C.Term.Float n
      C.Term.Boolean b -> pure $ C.Term.Boolean b
      C.Term.Text t -> C.Term.Text <$> lookupText t
      C.Term.Char ch -> pure $ C.Term.Char ch
      C.Term.Ref r ->
        C.Term.Ref <$> bitraverse lookupText (traverse lookupDefn) r
      C.Term.Constructor typeRef cid ->
        C.Term.Constructor
          <$> bitraverse lookupText lookupDefn typeRef
          <*> pure cid
      C.Term.Request typeRef cid ->
        C.Term.Request <$> bitraverse lookupText lookupDefn typeRef <*> pure cid
      C.Term.Handle a a2 -> pure $ C.Term.Handle a a2
      C.Term.App a a2 -> pure $ C.Term.App a a2
      C.Term.Ann a typ -> C.Term.Ann a <$> ABT.transformM goType typ
      C.Term.List as -> pure $ C.Term.List as
      C.Term.If c t f -> pure $ C.Term.If c t f
      C.Term.And a a2 -> pure $ C.Term.And a a2
      C.Term.Or a a2 -> pure $ C.Term.Or a a2
      C.Term.Lam a -> pure $ C.Term.Lam a
      C.Term.LetRec bs a -> pure $ C.Term.LetRec bs a
      C.Term.Let a a2 -> pure $ C.Term.Let a a2
      C.Term.Match a cs -> C.Term.Match a <$> traverse goCase cs
      C.Term.TermLink r ->
        C.Term.TermLink
          <$> bitraverse
            (bitraverse lookupText (traverse lookupDefn))
            (bitraverse lookupText lookupDefn)
            r
      C.Term.TypeLink r ->
        C.Term.TypeLink <$> bitraverse lookupText lookupDefn r
    goType ::
      forall m a.
      (MonadWriter (Seq Text, Seq H.Hash) m, MonadState (Map Text LocalTextId, Map H.Hash LocalDefnId) m) =>
      C.Type.FT a ->
      m (S.Term.FT a)
    goType = \case
      C.Type.Ref r -> C.Type.Ref <$> bitraverse lookupText lookupDefn r
      C.Type.Arrow i o -> pure $ C.Type.Arrow i o
      C.Type.Ann a k -> pure $ C.Type.Ann a k
      C.Type.App f a -> pure $ C.Type.App f a
      C.Type.Effect e a -> pure $ C.Type.Effect e a
      C.Type.Effects es -> pure $ C.Type.Effects es
      C.Type.Forall a -> pure $ C.Type.Forall a
      C.Type.IntroOuter a -> pure $ C.Type.IntroOuter a
    goCase ::
      forall m w s a.
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field1' s (Map Text LocalTextId),
        Lens.Field1' w (Seq Text),
        Lens.Field2' s (Map H.Hash LocalDefnId),
        Lens.Field2' w (Seq H.Hash)
      ) =>
      C.Term.MatchCase Text C.Term.TypeRef a ->
      m (C.Term.MatchCase LocalTextId S.Term.TypeRef a)
    goCase = \case
      C.Term.MatchCase pat guard body ->
        C.Term.MatchCase <$> goPat pat <*> pure guard <*> pure body
    goPat ::
      forall m s w.
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field1' s (Map Text LocalTextId),
        Lens.Field1' w (Seq Text),
        Lens.Field2' s (Map H.Hash LocalDefnId),
        Lens.Field2' w (Seq H.Hash)
      ) =>
      C.Term.Pattern Text C.Term.TypeRef ->
      m (C.Term.Pattern LocalTextId S.Term.TypeRef)
    goPat = \case
      C.Term.PUnbound -> pure $ C.Term.PUnbound
      C.Term.PVar -> pure $ C.Term.PVar
      C.Term.PBoolean b -> pure $ C.Term.PBoolean b
      C.Term.PInt i -> pure $ C.Term.PInt i
      C.Term.PNat n -> pure $ C.Term.PNat n
      C.Term.PFloat d -> pure $ C.Term.PFloat d
      C.Term.PText t -> C.Term.PText <$> lookupText t
      C.Term.PChar c -> pure $ C.Term.PChar c
      C.Term.PConstructor r i ps -> C.Term.PConstructor <$> bitraverse lookupText lookupDefn r <*> pure i <*> traverse goPat ps
      C.Term.PAs p -> C.Term.PAs <$> goPat p
      C.Term.PEffectPure p -> C.Term.PEffectPure <$> goPat p
      C.Term.PEffectBind r i bindings k -> C.Term.PEffectBind <$> bitraverse lookupText lookupDefn r <*> pure i <*> traverse goPat bindings <*> goPat k
      C.Term.PSequenceLiteral ps -> C.Term.PSequenceLiteral <$> traverse goPat ps
      C.Term.PSequenceOp l op r -> C.Term.PSequenceOp <$> goPat l <*> pure op <*> goPat r

    done :: ((S.Term.Term, Maybe S.Term.Type), (Seq Text, Seq H.Hash)) -> m (LocalIds' t d, S.Term.Term, Maybe S.Term.Type)
    done ((tm, tp), (localTextValues, localDefnValues)) = do
      textIds <- traverse saveText localTextValues
      defnIds <- traverse saveDefn localDefnValues
      let ids =
            LocalIds
              (Vector.fromList (Foldable.toList textIds))
              (Vector.fromList (Foldable.toList defnIds))
      pure (ids, void tm, void <$> tp)

loadTermWithTypeByReference :: C.Reference.Id -> MaybeT Transaction (C.Term Symbol, C.Term.Type Symbol)
loadTermWithTypeByReference (C.Reference.Id h i) = do
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, term, typ) <- MaybeT (Q.loadTermObject oid (decodeTermElementWithType i))
  lift (s2cTermWithType localIds term typ)

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

s2cTermWithType :: LocalIds -> S.Term.Term -> S.Term.Type -> Transaction (C.Term Symbol, C.Term.Type Symbol)
s2cTermWithType ids tm tp = do
  (substText, substHash) <- localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure (x2cTerm substText substHash tm, x2cTType substText substHash tp)

s2cTerm :: LocalIds -> S.Term.Term -> Transaction (C.Term Symbol)
s2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure $ x2cTerm substText substHash tm

s2cTypeOfTerm :: LocalIds -> S.Term.Type -> Transaction (C.Term.Type Symbol)
s2cTypeOfTerm ids tp = do
  (substText, substHash) <- localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure $ x2cTType substText substHash tp

-- | implementation detail of {s,w}2c*Term* & s2cDecl
localIdsToLookups :: Monad m => (t -> m Text) -> (d -> m H.Hash) -> LocalIds' t d -> m (LocalTextId -> Text, LocalDefnId -> H.Hash)
localIdsToLookups loadText loadHash localIds = do
  texts <- traverse loadText $ LocalIds.textLookup localIds
  hashes <- traverse loadHash $ LocalIds.defnLookup localIds
  let substText (LocalTextId w) = texts Vector.! fromIntegral w
      substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
  pure (substText, substHash)

localIdsToTypeRefLookup :: LocalIds -> Transaction (S.Decl.TypeRef -> C.Decl.TypeRef)
localIdsToTypeRefLookup localIds = do
  (substText, substHash) <- localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId localIds
  pure $ bimap substText (fmap substHash)

-- | implementation detail of {s,w}2c*Term*
x2cTerm :: (LocalTextId -> Text) -> (LocalDefnId -> H.Hash) -> S.Term.Term -> C.Term Symbol
x2cTerm substText substHash =
  -- substitute the text and hashes back into the term
  C.Term.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id
  where
    substTermRef = bimap substText (fmap substHash)
    substTypeRef = bimap substText substHash
    substTermLink = bimap substTermRef substTypeRef
    substTypeLink = substTypeRef

-- | implementation detail of {s,w}2c*Term*
x2cTType :: (LocalTextId -> Text) -> (LocalDefnId -> H.Hash) -> S.Term.Type -> C.Term.Type Symbol
x2cTType substText substHash = C.Type.rmap (bimap substText substHash)

lookupText ::
  forall m s w t.
  ( MonadState s m,
    MonadWriter w m,
    Lens.Field1' s (Map t LocalTextId),
    Lens.Field1' w (Seq t),
    Ord t
  ) =>
  t ->
  m LocalTextId
lookupText = lookup_ Lens._1 Lens._1 LocalTextId

lookupDefn ::
  forall m s w d.
  ( MonadState s m,
    MonadWriter w m,
    Lens.Field2' s (Map d LocalDefnId),
    Lens.Field2' w (Seq d),
    Ord d
  ) =>
  d ->
  m LocalDefnId
lookupDefn = lookup_ Lens._2 Lens._2 LocalDefnId

-- | shared implementation of lookupTextHelper and lookupDefnHelper
--  Look up a value in the LUT, or append it.
lookup_ ::
  (MonadState s m, MonadWriter w m, Ord t) =>
  Lens' s (Map t t') ->
  Lens' w (Seq t) ->
  (Word64 -> t') ->
  t ->
  m t'
lookup_ stateLens writerLens mk t = do
  map <- Lens.use stateLens
  case Map.lookup t map of
    Nothing -> do
      let id = mk . fromIntegral $ Map.size map
      stateLens Lens.%= Map.insert t id
      Writer.tell $ Lens.set writerLens (Seq.singleton t) mempty
      pure id
    Just t' -> pure t'

c2sTerm :: C.Term Symbol -> C.Term.Type Symbol -> Transaction (LocalIds, S.Term.Term, S.Term.Type)
c2sTerm tm tp = c2xTerm Q.saveText Q.expectObjectIdForPrimaryHash tm (Just tp) <&> \(w, tm, Just tp) -> (w, tm, tp)

-- *** Watch expressions

listWatches :: WatchKind -> Transaction [C.Reference.Id]
listWatches k = Q.loadWatchesByWatchKind k >>= traverse h2cReferenceId

-- | returns Nothing if the expression isn't cached.
loadWatch :: WatchKind -> C.Reference.Id -> MaybeT Transaction (C.Term Symbol)
loadWatch k r = do
  r' <- C.Reference.idH (lift . Q.saveHashHash) r
  S.Term.WatchResult wlids t <- MaybeT (Q.loadWatch k r' (getFromBytesOr "getWatchResultFormat" S.getWatchResultFormat))
  lift (w2cTerm wlids t)

saveWatch :: WatchKind -> C.Reference.Id -> C.Term Symbol -> Transaction ()
saveWatch w r t = do
  rs <- C.Reference.idH Q.saveHashHash r
  wterm <- c2wTerm t
  let bytes = S.putBytes S.putWatchResultFormat (uncurry S.Term.WatchResult wterm)
  Q.saveWatch w rs bytes

clearWatches :: Transaction ()
clearWatches = Q.clearWatches

c2wTerm :: C.Term Symbol -> Transaction (WatchLocalIds, S.Term.Term)
c2wTerm tm = c2xTerm Q.saveText Q.saveHashHash tm Nothing <&> \(w, tm, _) -> (w, tm)

w2cTerm :: WatchLocalIds -> S.Term.Term -> Transaction (C.Term Symbol)
w2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups Q.expectText Q.expectHash ids
  pure $ x2cTerm substText substHash tm

-- ** Saving & loading type decls

loadDeclComponent :: H.Hash -> MaybeT Transaction [C.Decl Symbol]
loadDeclComponent h = do
  oid <- MaybeT (Q.loadObjectIdForAnyHash h)
  S.Decl.Decl (S.Decl.LocallyIndexedComponent elements) <- MaybeT (Q.loadDeclObject oid decodeDeclFormat)
  lift . traverse (uncurry s2cDecl) $ Foldable.toList elements

saveDeclComponent :: H.Hash -> [C.Decl Symbol] -> Transaction Db.ObjectId
saveDeclComponent h decls = do
  when debug . traceM $ "Operations.saveDeclComponent " ++ show h
  sDeclElements <- traverse (c2sDecl Q.saveText Q.expectObjectIdForPrimaryHash) decls
  hashId <- Q.saveHashHash h
  let li = S.Decl.LocallyIndexedComponent $ Vector.fromList sDeclElements
      bytes = S.putBytes S.putDeclFormat $ S.Decl.Decl li
  oId <- Q.saveObject hashId OT.DeclComponent bytes
  -- populate dependents index
  let dependencies :: Set (S.Reference.Reference, S.Reference.Id) = foldMap unlocalizeRefs (sDeclElements `zip` [0 ..])
      unlocalizeRefs :: ((LocalIds, S.Decl.Decl Symbol), C.Reference.Pos) -> Set (S.Reference.Reference, S.Reference.Id)
      unlocalizeRefs ((LocalIds tIds oIds, decl), i) =
        let self = C.Reference.Id oId i
            dependencies :: Set S.Decl.TypeRef = C.Decl.dependencies decl
            getSRef :: C.Reference.Reference' LocalTextId (Maybe LocalDefnId) -> S.Reference.Reference
            getSRef = \case
              C.ReferenceBuiltin t -> C.ReferenceBuiltin (tIds Vector.! fromIntegral t)
              C.Reference.Derived Nothing i -> C.Reference.Derived oId i -- index self-references
              C.Reference.Derived (Just h) i -> C.Reference.Derived (oIds Vector.! fromIntegral h) i
         in Set.map ((,self) . getSRef) dependencies
  traverse_ (uncurry Q.addToDependentsIndex) dependencies

  pure oId

c2sDecl ::
  forall m t d.
  Monad m =>
  (Text -> m t) ->
  (H.Hash -> m d) ->
  C.Decl Symbol ->
  m (LocalIds' t d, S.Decl.Decl Symbol)
c2sDecl saveText saveDefn (C.Decl.DataDeclaration dt m b cts) = do
  done =<< (runWriterT . flip evalStateT mempty) do
    cts' <- traverse (ABT.transformM goType) cts
    pure (C.Decl.DataDeclaration dt m b cts')
  where
    goType ::
      forall m a.
      (MonadWriter (Seq Text, Seq H.Hash) m, MonadState (Map Text LocalTextId, Map H.Hash LocalDefnId) m) =>
      C.Type.FD a ->
      m (S.Decl.F a)
    goType = \case
      C.Type.Ref r -> C.Type.Ref <$> bitraverse lookupText (traverse lookupDefn) r
      C.Type.Arrow i o -> pure $ C.Type.Arrow i o
      C.Type.Ann a k -> pure $ C.Type.Ann a k
      C.Type.App f a -> pure $ C.Type.App f a
      C.Type.Effect e a -> pure $ C.Type.Effect e a
      C.Type.Effects es -> pure $ C.Type.Effects es
      C.Type.Forall a -> pure $ C.Type.Forall a
      C.Type.IntroOuter a -> pure $ C.Type.IntroOuter a
    done :: (S.Decl.Decl Symbol, (Seq Text, Seq H.Hash)) -> m (LocalIds' t d, S.Decl.Decl Symbol)
    done (decl, (localTextValues, localDefnValues)) = do
      textIds <- traverse saveText localTextValues
      defnIds <- traverse saveDefn localDefnValues
      let ids =
            LocalIds
              (Vector.fromList (Foldable.toList textIds))
              (Vector.fromList (Foldable.toList defnIds))
      pure (ids, decl)

-- | Unlocalize a decl.
s2cDecl :: LocalIds -> S.Decl.Decl Symbol -> Transaction (C.Decl Symbol)
s2cDecl ids (C.Decl.DataDeclaration dt m b ct) = do
  substTypeRef <- localIdsToTypeRefLookup ids
  pure (C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct))

loadDeclByReference :: C.Reference.Id -> MaybeT Transaction (C.Decl Symbol)
loadDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadDeclByReference " ++ show r
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  (localIds, decl) <- MaybeT (Q.loadDeclObject oid (decodeDeclElement i))
  lift (s2cDecl localIds decl)

expectDeclByReference :: C.Reference.Id -> Transaction (C.Decl Symbol)
expectDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "expectDeclByReference " ++ show r
  -- retrieve the blob
  Q.expectObjectIdForPrimaryHash h
    >>= (\oid -> Q.expectDeclObject oid (decodeDeclElement i))
    >>= uncurry s2cDecl

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
      Transaction (Map C.Branch.NameSegment (Map C.Referent (Transaction C.Branch.MdValues)))
    doTerms =
      Map.bitraverse
        (fmap C.Branch.NameSegment . Q.expectText)
        ( Map.bitraverse s2cReferent \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doTypes ::
      Map Db.TextId (Map S.Reference S.DbMetadataSet) ->
      Transaction (Map C.Branch.NameSegment (Map C.Reference (Transaction C.Branch.MdValues)))
    doTypes =
      Map.bitraverse
        (fmap C.Branch.NameSegment . Q.expectText)
        ( Map.bitraverse s2cReference \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doPatches ::
      Map Db.TextId Db.PatchObjectId ->
      Transaction (Map C.Branch.NameSegment (PatchHash, Transaction C.Branch.Patch))
    doPatches = Map.bitraverse (fmap C.Branch.NameSegment . Q.expectText) \patchId -> do
      h <- PatchHash <$> (Q.expectPrimaryHashByObjectId . Db.unPatchObjectId) patchId
      pure (h, expectPatch patchId)

    doChildren ::
      Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) ->
      Transaction (Map C.Branch.NameSegment (C.Causal Transaction CausalHash BranchHash (C.Branch.Branch Transaction)))
    doChildren = Map.bitraverse (fmap C.Branch.NameSegment . Q.expectText) \(boId, chId) ->
      C.Causal <$> Q.expectCausalHash chId
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
          C.Causal <$> Q.expectCausalHash chId
            <*> expectValueHashByCausalHashId chId
            <*> headParents chId
            <*> pure (loadValue chId)
        loadValue :: Db.CausalHashId -> Transaction (C.Branch.Branch Transaction)
        loadValue chId = do
          boId <- Q.expectBranchObjectIdByCausalHashId chId
          expectBranch boId

saveRootBranch :: C.Branch.Causal Transaction -> Transaction (Db.BranchObjectId, Db.CausalHashId)
saveRootBranch c = do
  when debug $ traceM $ "Operations.saveRootBranch " ++ show (C.causalHash c)
  (boId, chId) <- saveBranch c
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

saveBranch :: C.Branch.Causal Transaction -> Transaction (Db.BranchObjectId, Db.CausalHashId)
saveBranch (C.Causal hc he parents me) = do
  when debug $ traceM $ "\nOperations.saveBranch \n  hc = " ++ show hc ++ ",\n  he = " ++ show he ++ ",\n  parents = " ++ show (Map.keys parents)

  (chId, bhId) <- flip Monad.fromMaybeM (Q.loadCausalByCausalHash hc) do
    -- if not exist, create these
    chId <- Q.saveCausalHash hc
    bhId <- Q.saveBranchHash he
    Q.saveCausal chId bhId
    -- save the link between child and parents
    parentCausalHashIds <-
      -- so try to save each parent (recursively) before continuing to save hc
      for (Map.toList parents) $ \(parentHash, mcausal) ->
        -- check if we can short circuit the parent before loading it,
        -- by checking if there are causal parents associated with hc
        (flip Monad.fromMaybeM)
          (Q.loadCausalHashIdByCausalHash parentHash)
          (mcausal >>= fmap snd . saveBranch)
    unless (null parentCausalHashIds) $
      Q.saveCausalParents chId parentCausalHashIds
    pure (chId, bhId)
  boId <- flip Monad.fromMaybeM (Q.loadBranchObjectIdByCausalHashId chId) do
    branch <- c2sBranch =<< me
    let (li, lBranch) = LocalizeObject.localizeBranch branch
    saveBranchObject bhId li lBranch
  pure (boId, chId)
  where
    c2sBranch :: C.Branch.Branch Transaction -> Transaction S.DbBranch
    c2sBranch (C.Branch.Branch terms types patches children) =
      S.Branch
        <$> Map.bitraverse saveNameSegment (Map.bitraverse c2sReferent c2sMetadata) terms
        <*> Map.bitraverse saveNameSegment (Map.bitraverse c2sReference c2sMetadata) types
        <*> Map.bitraverse saveNameSegment savePatchObjectId patches
        <*> Map.bitraverse saveNameSegment saveBranch children

    saveNameSegment :: C.Branch.NameSegment -> Transaction Db.TextId
    saveNameSegment = Q.saveText . C.Branch.unNameSegment

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
          savePatch h patch

saveBranchObject :: Db.BranchHashId -> BranchLocalIds -> S.Branch.Full.LocalBranch -> Transaction Db.BranchObjectId
saveBranchObject id@(Db.unBranchHashId -> hashId) li lBranch = do
  when debug $ traceM $ "saveBranchObject\n\tid = " ++ show id ++ "\n\tli = " ++ show li ++ "\n\tlBranch = " ++ show lBranch
  let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full li lBranch
  oId <- Q.saveObject hashId OT.Namespace bytes
  pure $ Db.BranchObjectId oId

expectRootCausal :: Transaction (C.Branch.Causal Transaction)
expectRootCausal = Q.expectNamespaceRoot >>= expectCausalByCausalHashId

loadCausalBranchByCausalHash :: CausalHash -> Transaction (Maybe (C.Branch.Causal Transaction))
loadCausalBranchByCausalHash hc = do
  Q.loadCausalHashIdByCausalHash hc >>= \case
    Just chId -> Just <$> expectCausalByCausalHashId chId
    Nothing -> pure Nothing

expectCausalByCausalHashId :: Db.CausalHashId -> Transaction (C.Branch.Causal Transaction)
expectCausalByCausalHashId id = do
  hc <- Q.expectCausalHash id
  hb <- expectValueHashByCausalHashId id
  parentHashIds <- Q.loadCausalParents id
  loadParents <- for parentHashIds \hId -> do
    h <- Q.expectCausalHash hId
    pure (h, expectCausalByCausalHashId hId)
  pure $ C.Causal hc hb (Map.fromList loadParents) (expectBranchByCausalHashId id)

expectBranchByCausalHashId :: Db.CausalHashId -> Transaction (C.Branch.Branch Transaction)
expectBranchByCausalHashId id = do
  boId <- Q.expectBranchObjectIdByCausalHashId id
  expectBranch boId

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
          Ord ns =>
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
          Ord ns =>
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
          Ord r =>
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

savePatch :: PatchHash -> C.Branch.Patch -> Transaction Db.PatchObjectId
savePatch h c = do
  (li, lPatch) <- LocalizeObject.localizePatch <$> c2sPatch c
  saveDbPatch h (S.Patch.Format.Full li lPatch)

saveDbPatch :: PatchHash -> S.PatchFormat -> Transaction Db.PatchObjectId
saveDbPatch hash patch = do
  hashId <- Q.saveHashHash (unPatchHash hash)
  let bytes = S.putBytes S.putPatchFormat patch
  Db.PatchObjectId <$> Q.saveObject hashId OT.Patch bytes

s2cPatch :: S.Patch -> Transaction C.Branch.Patch
s2cPatch (S.Patch termEdits typeEdits) =
  C.Branch.Patch
    <$> Map.bitraverse h2cReferent (Set.traverse s2cTermEdit) termEdits
    <*> Map.bitraverse h2cReference (Set.traverse s2cTypeEdit) typeEdits

deserializePatchObject :: Db.PatchObjectId -> Transaction S.PatchFormat
deserializePatchObject id = do
  when debug $ traceM $ "Operations.deserializePatchObject " ++ show id
  Q.expectPatchObject (Db.unPatchObjectId id) decodePatchFormat

lca :: CausalHash -> CausalHash -> Connection -> Connection -> Transaction (Maybe CausalHash)
lca h1 h2 c1 c2 = runMaybeT do
  chId1 <- MaybeT $ Q.loadCausalHashIdByCausalHash h1
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  chId3 <- MaybeT . idempotentIO $ Q.lca chId1 chId2 c1 c2
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

addTypeToIndexForTerm :: S.Referent.Id -> C.Reference -> Transaction ()
addTypeToIndexForTerm sTermId cTypeRef = do
  sTypeRef <- saveReferenceH cTypeRef
  Q.addToTypeIndex sTypeRef sTermId

addTypeMentionsToIndexForTerm :: S.Referent.Id -> Set C.Reference -> Transaction ()
addTypeMentionsToIndexForTerm sTermId cTypeMentionRefs = do
  traverse_ (flip Q.addToTypeMentionsIndex sTermId <=< saveReferenceH) cTypeMentionRefs

-- something kind of funny here.  first, we don't need to enumerate all the reference pos if we're just picking one
-- second, it would be nice if we could leave these as S.References a little longer
-- so that we remember how to blow up if they're missing
componentReferencesByPrefix :: OT.ObjectType -> Text -> Maybe C.Reference.Pos -> Transaction [S.Reference.Id]
componentReferencesByPrefix ot b32prefix pos = do
  oIds :: [Db.ObjectId] <- Q.objectIdByBase32Prefix ot b32prefix
  let test = maybe (const True) (==) pos
  let filterComponent l = [x | x@(C.Reference.Id _ pos) <- l, test pos]
  join <$> traverse (fmap filterComponent . componentByObjectId) oIds

termReferencesByPrefix :: Text -> Maybe Word64 -> Transaction [C.Reference.Id]
termReferencesByPrefix t w =
  componentReferencesByPrefix OT.TermComponent t w
    >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)

declReferencesByPrefix :: Text -> Maybe Word64 -> Transaction [C.Reference.Id]
declReferencesByPrefix t w =
  componentReferencesByPrefix OT.DeclComponent t w
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
  componentReferencesByPrefix OT.DeclComponent b32prefix pos
    >>= traverse (loadConstructors cid)
  where
    loadConstructors ::
      Maybe Word64 ->
      S.Reference.Id ->
      Transaction (H.Hash, C.Reference.Pos, C.DeclType, [ConstructorId])
    loadConstructors cid rid@(C.Reference.Id oId pos) = do
      (dt, ctorCount) <- getDeclCtorCount rid
      h <- Q.expectPrimaryHashByObjectId oId
      let test :: ConstructorId -> Bool
          test = maybe (const True) (==) cid
          cids = [cid | cid <- [0 :: ConstructorId .. ctorCount - 1], test cid]
      pure (h, pos, dt, cids)
    getDeclCtorCount :: S.Reference.Id -> Transaction (C.Decl.DeclType, ConstructorId)
    getDeclCtorCount id@(C.Reference.Id r i) = do
      when debug $ traceM $ "getDeclCtorCount " ++ show id
      (_localIds, decl) <- Q.expectDeclObject r (decodeDeclElement i)
      pure (C.Decl.declType decl, fromIntegral $ length (C.Decl.constructorTypes decl))

branchHashesByPrefix :: ShortBranchHash -> Transaction (Set BranchHash)
branchHashesByPrefix (ShortBranchHash b32prefix) = do
  hashIds <- Q.namespaceHashIdByBase32Prefix b32prefix
  hashes <- traverse (Q.expectHash . Db.unBranchHashId) hashIds
  pure $ Set.fromList . map BranchHash $ hashes

causalHashesByPrefix :: ShortBranchHash -> Transaction (Set CausalHash)
causalHashesByPrefix (ShortBranchHash b32prefix) = do
  hashIds <- Q.causalHashIdByBase32Prefix b32prefix
  hashes <- traverse (Q.expectHash . Db.unCausalHashId) hashIds
  pure $ Set.fromList . map CausalHash $ hashes

-- | returns a list of known definitions referencing `r`
dependents :: C.Reference -> Transaction (Set C.Reference.Id)
dependents r = do
  r' <- c2sReference r
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependency r'
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

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

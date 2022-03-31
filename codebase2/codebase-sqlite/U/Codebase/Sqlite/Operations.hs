{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Operations
  ( -- * branches
    saveRootBranch,
    loadMaybeRootCausalHash,
    loadRootCausalHash,
    loadRootCausal,
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
    getDeclTypeById,

    -- * terms/decls
    getCycleLen,

    -- * patches
    savePatch,
    loadPatchById,

    -- * test for stuff in codebase
    objectExistsForHash,

    -- * dubiously exported stuff involving database ids
    loadHashByObjectId,
    primaryHashToMaybeObjectId,
    primaryHashToMaybePatchObjectId,

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
    loadDbBranchByObjectId,
    loadDbPatchById,
    saveBranchObject,
    saveDbPatch,

    -- * Error types
    Error (..),
    DecodeError (..),

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
import Control.Monad (join, unless, when, (<=<))
import Control.Monad.Except (MonadIO (liftIO))
import qualified Control.Monad.Extra as Monad
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Writer (MonadWriter, runWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import qualified Data.Bytes.Get as Get
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Functor (void, (<&>))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import Debug.Trace
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
import qualified U.Util.Monoid as Monoid
import U.Util.Serialization (Get)
import qualified U.Util.Serialization as S
import qualified U.Util.Term as TermUtil
import Unison.Sqlite
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

-- * Error handling

debug :: Bool
debug = False

type ErrString = String

data DecodeError
  = ErrTermFormat
  | ErrDeclFormat
  | ErrTermElement Word64
  | ErrDeclElement Word64
  | ErrFramedArrayLen
  | ErrTypeOfTerm C.Reference.Id
  | ErrWatch WatchKind C.Reference.Id
  | ErrBranch Db.BranchObjectId
  | ErrPatch Db.PatchObjectId
  | ErrObjectDependencies OT.ObjectType Db.ObjectId
  deriving (Show)

-- TODO rename
data Error
  = DecodeError DecodeError ByteString ErrString
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

newtype NeedTypeForBuiltinMetadata
  = NeedTypeForBuiltinMetadata Text
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

getFromBytesOr :: DecodeError -> Get a -> ByteString -> Either Error a
getFromBytesOr e get bs = case runGetS get bs of
  Left err -> Left (DecodeError e bs err)
  Right a -> Right a

-- * Database lookups

loadTextById :: DB m => Db.TextId -> m Text
loadTextById = Q.loadTextById

-- | look up an existing object by its primary hash
primaryHashToExistingObjectId :: DB m => H.Hash -> m Db.ObjectId
primaryHashToExistingObjectId h = do
  hashId <- Q.expectHashIdByHash h
  Q.expectObjectIdForPrimaryHashId hashId

primaryHashToMaybeObjectId :: DB m => H.Hash -> m (Maybe Db.ObjectId)
primaryHashToMaybeObjectId h = do
  Q.loadHashIdByHash h >>= \case
    Just hashId -> Q.maybeObjectIdForPrimaryHashId hashId
    Nothing -> pure Nothing

anyHashToMaybeObjectId :: DB m => H.Hash -> m (Maybe Db.ObjectId)
anyHashToMaybeObjectId h = do
  (Q.loadHashId . H.toBase32Hex) h >>= \case
    Just hashId -> Q.maybeObjectIdForAnyHashId hashId
    Nothing -> pure Nothing

primaryHashToMaybePatchObjectId :: DB m => PatchHash -> m (Maybe Db.PatchObjectId)
primaryHashToMaybePatchObjectId =
  (fmap . fmap) Db.PatchObjectId . primaryHashToMaybeObjectId . unPatchHash

objectExistsForHash :: DB m => H.Hash -> m Bool
objectExistsForHash h =
  isJust <$> runMaybeT do
    id <- MaybeT . Q.loadHashId . H.toBase32Hex $ h
    MaybeT $ Q.maybeObjectIdForAnyHashId id

loadHashByObjectId :: DB m => Db.ObjectId -> m H.Hash
loadHashByObjectId = fmap H.fromBase32Hex . Q.loadPrimaryHashByObjectId

loadHashByHashId :: DB m => Db.HashId -> m H.Hash
loadHashByHashId = fmap H.fromBase32Hex . Q.loadHashById

loadCausalHashById :: DB m => Db.CausalHashId -> m CausalHash
loadCausalHashById = fmap (CausalHash . H.fromBase32Hex) . Q.loadHashById . Db.unCausalHashId

loadValueHashByCausalHashId :: DB m => Db.CausalHashId -> m BranchHash
loadValueHashByCausalHashId = loadValueHashById <=< Q.loadCausalValueHashId
  where
    loadValueHashById :: DB m => Db.BranchHashId -> m BranchHash
    loadValueHashById = fmap (BranchHash . H.fromBase32Hex) . Q.loadHashById . Db.unBranchHashId

loadRootCausalHash :: DB m => m CausalHash
loadRootCausalHash = loadCausalHashById =<< Q.loadNamespaceRoot

loadMaybeRootCausalHash :: DB m => m (Maybe CausalHash)
loadMaybeRootCausalHash =
  runMaybeT $
    loadCausalHashById =<< MaybeT Q.loadMaybeNamespaceRoot

-- * Reference transformations

-- ** read existing references

-- | Assumes that a derived reference would already exist in the database
--  (by virtue of dependencies being stored before dependents), but does
--  not assume a builtin reference would.
c2sReference :: DB m => C.Reference -> m S.Reference
c2sReference = bitraverse Q.saveText primaryHashToExistingObjectId

s2cReference :: DB m => S.Reference -> m C.Reference
s2cReference = bitraverse loadTextById loadHashByObjectId

c2sReferenceId :: DB m => C.Reference.Id -> m S.Reference.Id
c2sReferenceId = C.Reference.idH primaryHashToExistingObjectId

s2cReferenceId :: DB m => S.Reference.Id -> m C.Reference.Id
s2cReferenceId = C.Reference.idH loadHashByObjectId

h2cReferenceId :: DB m => S.Reference.IdH -> m C.Reference.Id
h2cReferenceId = C.Reference.idH loadHashByHashId

h2cReference :: DB m => S.ReferenceH -> m C.Reference
h2cReference = bitraverse loadTextById loadHashByHashId

c2hReference :: DB m => C.Reference -> MaybeT m S.ReferenceH
c2hReference = bitraverse (MaybeT . Q.loadText) (MaybeT . Q.loadHashIdByHash)

s2cReferent :: DB m => S.Referent -> m C.Referent
s2cReferent = bitraverse s2cReference s2cReference

s2cReferentId :: DB m => S.Referent.Id -> m C.Referent.Id
s2cReferentId = bitraverse loadHashByObjectId loadHashByObjectId

c2sReferent :: DB m => C.Referent -> m S.Referent
c2sReferent = bitraverse c2sReference c2sReference

c2sReferentId :: DB m => C.Referent.Id -> m S.Referent.Id
c2sReferentId = bitraverse primaryHashToExistingObjectId primaryHashToExistingObjectId

h2cReferent :: DB m => S.ReferentH -> m C.Referent
h2cReferent = bitraverse h2cReference h2cReference

-- ** convert and save references

-- | Save the text and hash parts of a Reference to the database and substitute their ids.
saveReferenceH :: DB m => C.Reference -> m S.ReferenceH
saveReferenceH = bitraverse Q.saveText Q.saveHashHash

saveReferentH :: DB m => C.Referent -> m S.ReferentH
saveReferentH = bitraverse saveReferenceH saveReferenceH

-- ** Edits transformations

s2cTermEdit :: DB m => S.TermEdit -> m C.TermEdit
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

s2cTypeEdit :: DB m => S.TypeEdit -> m C.TypeEdit
s2cTypeEdit = \case
  S.TypeEdit.Replace r -> C.TypeEdit.Replace <$> s2cReference r
  S.TypeEdit.Deprecate -> pure C.TypeEdit.Deprecate

-- | assumes that all relevant defns are already in the DB
c2sPatch :: DB m => C.Branch.Patch -> m S.Patch
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

decodeTermFormat :: ByteString -> Either Error S.Term.TermFormat
decodeTermFormat = getFromBytesOr ErrTermFormat S.getTermFormat

decodeComponentLengthOnly :: ByteString -> Either Error Word64
decodeComponentLengthOnly = getFromBytesOr ErrFramedArrayLen (Get.skip 1 >> S.lengthFramedArray)

decodeTermElementWithType :: C.Reference.Pos -> ByteString -> Either Error (LocalIds, S.Term.Term, S.Term.Type)
decodeTermElementWithType i = getFromBytesOr (ErrTermElement i) (S.lookupTermElement i)

decodeTermElementDiscardingTerm :: C.Reference.Pos -> ByteString -> Either Error (LocalIds, S.Term.Type)
decodeTermElementDiscardingTerm i = getFromBytesOr (ErrTermElement i) (S.lookupTermElementDiscardingTerm i)

decodeTermElementDiscardingType :: C.Reference.Pos -> ByteString -> Either Error (LocalIds, S.Term.Term)
decodeTermElementDiscardingType i = getFromBytesOr (ErrTermElement i) (S.lookupTermElementDiscardingType i)

decodeDeclFormat :: ByteString -> Either Error S.Decl.DeclFormat
decodeDeclFormat = getFromBytesOr ErrDeclFormat S.getDeclFormat

decodeDeclElement :: Word64 -> ByteString -> Either Error (LocalIds, S.Decl.Decl Symbol)
decodeDeclElement i = getFromBytesOr (ErrDeclElement i) (S.lookupDeclElement i)

getCycleLen :: DB m => H.Hash -> m (Maybe Word64)
getCycleLen h = do
  when debug $ traceM $ "\ngetCycleLen " ++ (Text.unpack . Base32Hex.toText $ H.toBase32Hex h)
  runMaybeT do
    -- actually want Nothing in case of non term/decl component hash
    oid <- MaybeT (anyHashToMaybeObjectId h)
    -- todo: decodeComponentLengthOnly is unintentionally a hack that relies on
    -- the fact the two things that references can refer to (term and decl
    -- components) have the same basic serialized structure: first a format
    -- byte that is always 0 for now, followed by a framed array representing
    -- the strongly-connected component. :grimace:
    Q.loadObjectById oid decodeComponentLengthOnly

-- | Get the 'C.DeclType.DeclType' of a 'C.Reference.Id'.
-- TODO rename to expectDeclTypeById
getDeclTypeById :: DB m => C.Reference.Id -> m C.Decl.DeclType
getDeclTypeById =
  fmap C.Decl.declType . expectDeclByReference

componentByObjectId :: DB m => Db.ObjectId -> m [S.Reference.Id]
componentByObjectId id = do
  when debug . traceM $ "Operations.componentByObjectId " ++ show id
  len <- Q.loadObjectById id decodeComponentLengthOnly
  pure [C.Reference.Id id i | i <- [0 .. len - 1]]

-- * Codebase operations

-- ** Saving & loading terms

loadTermComponent :: DB m => H.Hash -> MaybeT m [(C.Term Symbol, C.Term.Type Symbol)]
loadTermComponent h = do
  oid <- MaybeT (anyHashToMaybeObjectId h)
  S.Term.Term (S.Term.LocallyIndexedComponent elements) <- MaybeT (Q.loadTermObjectById oid decodeTermFormat)
  lift . traverse (uncurry3 s2cTermWithType) $ Foldable.toList elements

saveTermComponent :: DB m => H.Hash -> [(C.Term Symbol, C.Term.Type Symbol)] -> m Db.ObjectId
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

loadTermWithTypeByReference :: DB m => C.Reference.Id -> MaybeT m (C.Term Symbol, C.Term.Type Symbol)
loadTermWithTypeByReference (C.Reference.Id h i) = do
  oid <- MaybeT (primaryHashToMaybeObjectId h)
  -- retrieve and deserialize the blob
  (localIds, term, typ) <- MaybeT (Q.loadTermObjectById oid (decodeTermElementWithType i))
  s2cTermWithType localIds term typ

loadTermByReference :: DB m => C.Reference.Id -> MaybeT m (C.Term Symbol)
loadTermByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTermByReference " ++ show r
  oid <- MaybeT (primaryHashToMaybeObjectId h)
  -- retrieve and deserialize the blob
  (localIds, term) <- MaybeT (Q.loadTermObjectById oid (decodeTermElementDiscardingType i))
  s2cTerm localIds term

loadTypeOfTermByTermReference :: DB m => C.Reference.Id -> MaybeT m (C.Term.Type Symbol)
loadTypeOfTermByTermReference id@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTypeOfTermByTermReference " ++ show id
  oid <- MaybeT (primaryHashToMaybeObjectId h)
  -- retrieve and deserialize the blob
  (localIds, typ) <- MaybeT (Q.loadTermObjectById oid (decodeTermElementDiscardingTerm i))
  s2cTypeOfTerm localIds typ

s2cTermWithType :: DB m => LocalIds -> S.Term.Term -> S.Term.Type -> m (C.Term Symbol, C.Term.Type Symbol)
s2cTermWithType ids tm tp = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure (x2cTerm substText substHash tm, x2cTType substText substHash tp)

s2cTerm :: DB m => LocalIds -> S.Term.Term -> m (C.Term Symbol)
s2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure $ x2cTerm substText substHash tm

s2cTypeOfTerm :: DB m => LocalIds -> S.Term.Type -> m (C.Term.Type Symbol)
s2cTypeOfTerm ids tp = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure $ x2cTType substText substHash tp

-- | implementation detail of {s,w}2c*Term* & s2cDecl
localIdsToLookups :: Monad m => (t -> m Text) -> (d -> m H.Hash) -> LocalIds' t d -> m (LocalTextId -> Text, LocalDefnId -> H.Hash)
localIdsToLookups loadText loadHash localIds = do
  texts <- traverse loadText $ LocalIds.textLookup localIds
  hashes <- traverse loadHash $ LocalIds.defnLookup localIds
  let substText (LocalTextId w) = texts Vector.! fromIntegral w
      substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
  pure (substText, substHash)

localIdsToTypeRefLookup :: DB m => LocalIds -> m (S.Decl.TypeRef -> C.Decl.TypeRef)
localIdsToTypeRefLookup localIds = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId localIds
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

c2sTerm :: DB m => C.Term Symbol -> C.Term.Type Symbol -> m (LocalIds, S.Term.Term, S.Term.Type)
c2sTerm tm tp = c2xTerm Q.saveText primaryHashToExistingObjectId tm (Just tp) <&> \(w, tm, Just tp) -> (w, tm, tp)

-- *** Watch expressions

listWatches :: DB m => WatchKind -> m [C.Reference.Id]
listWatches k = Q.loadWatchesByWatchKind k >>= traverse h2cReferenceId

-- | returns Nothing if the expression isn't cached.
loadWatch :: DB m => WatchKind -> C.Reference.Id -> MaybeT m (C.Term Symbol)
loadWatch k r = do
  r' <- C.Reference.idH Q.saveHashHash r
  S.Term.WatchResult wlids t <- MaybeT (Q.loadWatch k r' (getFromBytesOr (ErrWatch k r) S.getWatchResultFormat))
  w2cTerm wlids t

saveWatch :: DB m => WatchKind -> C.Reference.Id -> C.Term Symbol -> m ()
saveWatch w r t = do
  rs <- C.Reference.idH Q.saveHashHash r
  wterm <- c2wTerm t
  let bytes = S.putBytes S.putWatchResultFormat (uncurry S.Term.WatchResult wterm)
  Q.saveWatch w rs bytes

clearWatches :: DB m => m ()
clearWatches = Q.clearWatches

c2wTerm :: DB m => C.Term Symbol -> m (WatchLocalIds, S.Term.Term)
c2wTerm tm = c2xTerm Q.saveText Q.saveHashHash tm Nothing <&> \(w, tm, _) -> (w, tm)

w2cTerm :: DB m => WatchLocalIds -> S.Term.Term -> m (C.Term Symbol)
w2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByHashId ids
  pure $ x2cTerm substText substHash tm

-- ** Saving & loading type decls

loadDeclComponent :: DB m => H.Hash -> MaybeT m [C.Decl Symbol]
loadDeclComponent h = do
  oid <- MaybeT (anyHashToMaybeObjectId h)
  S.Decl.Decl (S.Decl.LocallyIndexedComponent elements) <- MaybeT (Q.loadDeclObjectById oid decodeDeclFormat)
  lift . traverse (uncurry s2cDecl) $ Foldable.toList elements

saveDeclComponent :: DB m => H.Hash -> [C.Decl Symbol] -> m Db.ObjectId
saveDeclComponent h decls = do
  when debug . traceM $ "Operations.saveDeclComponent " ++ show h
  sDeclElements <- traverse (c2sDecl Q.saveText primaryHashToExistingObjectId) decls
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

c2sDecl :: forall m t d. DB m => (Text -> m t) -> (H.Hash -> m d) -> C.Decl Symbol -> m (LocalIds' t d, S.Decl.Decl Symbol)
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
s2cDecl :: DB m => LocalIds -> S.Decl.Decl Symbol -> m (C.Decl Symbol)
s2cDecl ids (C.Decl.DataDeclaration dt m b ct) = do
  substTypeRef <- localIdsToTypeRefLookup ids
  pure (C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct))

loadDeclByReference :: DB m => C.Reference.Id -> MaybeT m (C.Decl Symbol)
loadDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadDeclByReference " ++ show r
  oid <- MaybeT (primaryHashToMaybeObjectId h)
  (localIds, decl) <- MaybeT (Q.loadDeclObjectById oid (decodeDeclElement i))
  s2cDecl localIds decl

-- TODO rename expectDeclById
expectDeclByReference :: DB m => C.Reference.Id -> m (C.Decl Symbol)
expectDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "expectDeclByReference " ++ show r
  -- retrieve the blob
  primaryHashToExistingObjectId h
    >>= (\oid -> Q.expectDeclObjectById oid (decodeDeclElement i))
    >>= uncurry s2cDecl

-- * Branch transformation

s2cBranch :: DB m => S.DbBranch -> m (C.Branch.Branch m)
s2cBranch (S.Branch.Full.Branch tms tps patches children) =
  C.Branch.Branch
    <$> doTerms tms
    <*> doTypes tps
    <*> doPatches patches
    <*> doChildren children
  where
    loadMetadataType :: DB m => S.Reference -> m C.Reference
    loadMetadataType = \case
      C.ReferenceBuiltin tId ->
        Q.loadTextByIdCheck (Left . NeedTypeForBuiltinMetadata) tId
      C.ReferenceDerived id ->
        typeReferenceForTerm id >>= h2cReference

    loadTypesForMetadata rs = Map.fromList <$> traverse (\r -> (,) <$> s2cReference r <*> loadMetadataType r) (Foldable.toList rs)
    doTerms :: DB m => Map Db.TextId (Map S.Referent S.DbMetadataSet) -> m (Map C.Branch.NameSegment (Map C.Referent (m C.Branch.MdValues)))
    doTerms =
      Map.bitraverse
        (fmap C.Branch.NameSegment . loadTextById)
        ( Map.bitraverse s2cReferent \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doTypes :: DB m => Map Db.TextId (Map S.Reference S.DbMetadataSet) -> m (Map C.Branch.NameSegment (Map C.Reference (m C.Branch.MdValues)))
    doTypes =
      Map.bitraverse
        (fmap C.Branch.NameSegment . loadTextById)
        ( Map.bitraverse s2cReference \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doPatches :: DB m => Map Db.TextId Db.PatchObjectId -> m (Map C.Branch.NameSegment (PatchHash, m C.Branch.Patch))
    doPatches = Map.bitraverse (fmap C.Branch.NameSegment . loadTextById) \patchId -> do
      h <- PatchHash <$> (loadHashByObjectId . Db.unPatchObjectId) patchId
      pure (h, loadPatchById patchId)

    doChildren :: DB m => Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> m (Map C.Branch.NameSegment (C.Causal m CausalHash BranchHash (C.Branch.Branch m)))
    doChildren = Map.bitraverse (fmap C.Branch.NameSegment . loadTextById) \(boId, chId) ->
      C.Causal <$> loadCausalHashById chId
        <*> loadValueHashByCausalHashId chId
        <*> headParents chId
        <*> pure (loadBranchByObjectId boId)
      where
        headParents :: DB m => Db.CausalHashId -> m (Map CausalHash (m (C.Causal m CausalHash BranchHash (C.Branch.Branch m))))
        headParents chId = do
          parentsChIds <- Q.loadCausalParents chId
          fmap Map.fromList $ traverse pairParent parentsChIds
        pairParent :: DB m => Db.CausalHashId -> m (CausalHash, m (C.Causal m CausalHash BranchHash (C.Branch.Branch m)))
        pairParent chId = do
          h <- loadCausalHashById chId
          pure (h, loadCausal chId)
        loadCausal :: DB m => Db.CausalHashId -> m (C.Causal m CausalHash BranchHash (C.Branch.Branch m))
        loadCausal chId = do
          C.Causal <$> loadCausalHashById chId
            <*> loadValueHashByCausalHashId chId
            <*> headParents chId
            <*> pure (loadValue chId)
        loadValue :: DB m => Db.CausalHashId -> m (C.Branch.Branch m)
        loadValue chId = do
          boId <- Q.expectBranchObjectIdByCausalHashId chId
          loadBranchByObjectId boId

saveRootBranch :: DB m => C.Branch.Causal m -> m (Db.BranchObjectId, Db.CausalHashId)
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

saveBranch :: DB m => C.Branch.Causal m -> m (Db.BranchObjectId, Db.CausalHashId)
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
    c2sBranch :: DB m => C.Branch.Branch m -> m S.DbBranch
    c2sBranch (C.Branch.Branch terms types patches children) =
      S.Branch
        <$> Map.bitraverse saveNameSegment (Map.bitraverse c2sReferent c2sMetadata) terms
        <*> Map.bitraverse saveNameSegment (Map.bitraverse c2sReference c2sMetadata) types
        <*> Map.bitraverse saveNameSegment savePatchObjectId patches
        <*> Map.bitraverse saveNameSegment saveBranch children

    saveNameSegment :: DB m => C.Branch.NameSegment -> m Db.TextId
    saveNameSegment = Q.saveText . C.Branch.unNameSegment

    c2sMetadata :: DB m => m C.Branch.MdValues -> m S.Branch.Full.DbMetadataSet
    c2sMetadata mm = do
      C.Branch.MdValues m <- mm
      S.Branch.Full.Inline <$> Set.traverse c2sReference (Map.keysSet m)

    savePatchObjectId :: DB m => (PatchHash, m C.Branch.Patch) -> m Db.PatchObjectId
    savePatchObjectId (h, mp) = do
      primaryHashToMaybePatchObjectId h >>= \case
        Just patchOID -> pure patchOID
        Nothing -> do
          patch <- mp
          savePatch h patch

saveBranchObject :: DB m => Db.BranchHashId -> BranchLocalIds -> S.Branch.Full.LocalBranch -> m Db.BranchObjectId
saveBranchObject id@(Db.unBranchHashId -> hashId) li lBranch = do
  when debug $ traceM $ "saveBranchObject\n\tid = " ++ show id ++ "\n\tli = " ++ show li ++ "\n\tlBranch = " ++ show lBranch
  let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full li lBranch
  oId <- Q.saveObject hashId OT.Namespace bytes
  pure $ Db.BranchObjectId oId

loadRootCausal :: DB m => m (C.Branch.Causal m)
loadRootCausal = Q.loadNamespaceRoot >>= loadCausalByCausalHashId

loadCausalBranchByCausalHash :: DB m => CausalHash -> m (Maybe (C.Branch.Causal m))
loadCausalBranchByCausalHash hc = do
  Q.loadCausalHashIdByCausalHash hc >>= \case
    Just chId -> Just <$> loadCausalByCausalHashId chId
    Nothing -> pure Nothing

loadCausalByCausalHashId :: DB m => Db.CausalHashId -> m (C.Branch.Causal m)
loadCausalByCausalHashId id = do
  hc <- loadCausalHashById id
  hb <- loadValueHashByCausalHashId id
  parentHashIds <- Q.loadCausalParents id
  loadParents <- for parentHashIds \hId -> do
    h <- loadCausalHashById hId
    pure (h, loadCausalByCausalHashId hId)
  pure $ C.Causal hc hb (Map.fromList loadParents) (loadBranchByCausalHashId id)

-- TODO rename expect
loadBranchByCausalHashId :: DB m => Db.CausalHashId -> m (C.Branch.Branch m)
loadBranchByCausalHashId id = do
  boId <- Q.expectBranchObjectIdByCausalHashId id
  loadBranchByObjectId boId

loadDbBranchByObjectId :: DB m => Db.BranchObjectId -> m S.DbBranch
loadDbBranchByObjectId id =
  deserializeBranchObject id >>= \case
    S.BranchFormat.Full li f -> pure (S.BranchFormat.localToDbBranch li f)
    S.BranchFormat.Diff r li d -> doDiff r [S.BranchFormat.localToDbDiff li d]
  where
    deserializeBranchObject :: DB m => Db.BranchObjectId -> m S.BranchFormat
    deserializeBranchObject id = do
      when debug $ traceM $ "deserializeBranchObject " ++ show id
      Q.expectNamespaceObjectById (Db.unBranchObjectId id) (getFromBytesOr (ErrBranch id) S.getBranchFormat)

    doDiff :: DB m => Db.BranchObjectId -> [S.Branch.Diff] -> m S.DbBranch
    doDiff ref ds =
      deserializeBranchObject ref >>= \case
        S.BranchFormat.Full li f -> joinFull (S.BranchFormat.localToDbBranch li f) ds
        S.BranchFormat.Diff ref' li' d' -> doDiff ref' (S.BranchFormat.localToDbDiff li' d' : ds)
      where
        joinFull :: DB m => S.DbBranch -> [S.Branch.Diff] -> m S.DbBranch
        joinFull f [] = pure f
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

loadBranchByObjectId :: DB m => Db.BranchObjectId -> m (C.Branch.Branch m)
loadBranchByObjectId id =
  loadDbBranchByObjectId id >>= s2cBranch

-- * Patch transformation

loadPatchById :: DB m => Db.PatchObjectId -> m C.Branch.Patch
loadPatchById patchId =
  loadDbPatchById patchId >>= s2cPatch

loadDbPatchById :: DB m => Db.PatchObjectId -> m S.Patch
loadDbPatchById patchId =
  deserializePatchObject patchId >>= \case
    S.Patch.Format.Full li p -> pure (S.Patch.Format.localPatchToPatch li p)
    S.Patch.Format.Diff ref li d -> doDiff ref [S.Patch.Format.localPatchDiffToPatchDiff li d]
  where
    doDiff :: DB m => Db.PatchObjectId -> [S.PatchDiff] -> m S.Patch
    doDiff ref ds =
      deserializePatchObject ref >>= \case
        S.Patch.Format.Full li f -> pure (S.Patch.Format.applyPatchDiffs (S.Patch.Format.localPatchToPatch li f) ds)
        S.Patch.Format.Diff ref' li' d' -> doDiff ref' (S.Patch.Format.localPatchDiffToPatchDiff li' d' : ds)

savePatch :: DB m => PatchHash -> C.Branch.Patch -> m Db.PatchObjectId
savePatch h c = do
  (li, lPatch) <- LocalizeObject.localizePatch <$> c2sPatch c
  saveDbPatch h (S.Patch.Format.Full li lPatch)

saveDbPatch :: DB m => PatchHash -> S.PatchFormat -> m Db.PatchObjectId
saveDbPatch hash patch = do
  hashId <- Q.saveHashHash (unPatchHash hash)
  let bytes = S.putBytes S.putPatchFormat patch
  Db.PatchObjectId <$> Q.saveObject hashId OT.Patch bytes

s2cPatch :: DB m => S.Patch -> m C.Branch.Patch
s2cPatch (S.Patch termEdits typeEdits) =
  C.Branch.Patch
    <$> Map.bitraverse h2cReferent (Set.traverse s2cTermEdit) termEdits
    <*> Map.bitraverse h2cReference (Set.traverse s2cTypeEdit) typeEdits

deserializePatchObject :: DB m => Db.PatchObjectId -> m S.PatchFormat
deserializePatchObject id = do
  when debug $ traceM $ "Operations.deserializePatchObject " ++ show id
  Q.expectPatchObjectById (Db.unPatchObjectId id) (getFromBytesOr (ErrPatch id) S.getPatchFormat)

lca :: DB m => CausalHash -> CausalHash -> Connection -> Connection -> m (Maybe CausalHash)
lca h1 h2 c1 c2 = runMaybeT do
  chId1 <- MaybeT $ Q.loadCausalHashIdByCausalHash h1
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  chId3 <- MaybeT . liftIO $ Q.lca chId1 chId2 c1 c2
  Q.loadCausalHash chId3

before :: DB m => CausalHash -> CausalHash -> m (Maybe Bool)
before h1 h2 = runMaybeT do
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  lift (Q.loadCausalHashIdByCausalHash h1) >>= \case
    Just chId1 -> lift (Q.before chId1 chId2)
    Nothing -> pure False

-- * Searches

termsHavingType :: DB m => C.Reference -> m (Set C.Referent.Id)
termsHavingType cTypeRef = do
  maySet <- runMaybeT $ do
    sTypeRef <- c2hReference cTypeRef
    sIds <- Q.getReferentsByType sTypeRef
    traverse s2cReferentId sIds
  pure case maySet of
    Nothing -> mempty
    Just set -> Set.fromList set

typeReferenceForTerm :: DB m => S.Reference.Id -> m S.ReferenceH
typeReferenceForTerm = Q.getTypeReferenceForReferent . C.Referent.RefId

termsMentioningType :: DB m => C.Reference -> m (Set C.Referent.Id)
termsMentioningType cTypeRef = do
  maySet <- runMaybeT $ do
    sTypeRef <- c2hReference cTypeRef
    sIds <- Q.getReferentsByTypeMention sTypeRef
    traverse s2cReferentId sIds
  pure case maySet of
    Nothing -> mempty
    Just set -> Set.fromList set

addTypeToIndexForTerm :: DB m => S.Referent.Id -> C.Reference -> m ()
addTypeToIndexForTerm sTermId cTypeRef = do
  sTypeRef <- saveReferenceH cTypeRef
  Q.addToTypeIndex sTypeRef sTermId

addTypeMentionsToIndexForTerm :: DB m => S.Referent.Id -> Set C.Reference -> m ()
addTypeMentionsToIndexForTerm sTermId cTypeMentionRefs = do
  traverse_ (flip Q.addToTypeMentionsIndex sTermId <=< saveReferenceH) cTypeMentionRefs

-- something kind of funny here.  first, we don't need to enumerate all the reference pos if we're just picking one
-- second, it would be nice if we could leave these as S.References a little longer
-- so that we remember how to blow up if they're missing
componentReferencesByPrefix :: DB m => OT.ObjectType -> Text -> Maybe C.Reference.Pos -> m [S.Reference.Id]
componentReferencesByPrefix ot b32prefix pos = do
  oIds :: [Db.ObjectId] <- Q.objectIdByBase32Prefix ot b32prefix
  let test = maybe (const True) (==) pos
  let filterComponent l = [x | x@(C.Reference.Id _ pos) <- l, test pos]
  fmap Monoid.fromMaybe . runMaybeT $
    join <$> traverse (fmap filterComponent . componentByObjectId) oIds

termReferencesByPrefix :: DB m => Text -> Maybe Word64 -> m [C.Reference.Id]
termReferencesByPrefix t w =
  componentReferencesByPrefix OT.TermComponent t w
    >>= traverse (C.Reference.idH loadHashByObjectId)

declReferencesByPrefix :: DB m => Text -> Maybe Word64 -> m [C.Reference.Id]
declReferencesByPrefix t w =
  componentReferencesByPrefix OT.DeclComponent t w
    >>= traverse (C.Reference.idH loadHashByObjectId)

termReferentsByPrefix :: DB m => Text -> Maybe Word64 -> m [C.Referent.Id]
termReferentsByPrefix b32prefix pos =
  fmap C.Referent.RefId <$> termReferencesByPrefix b32prefix pos

-- todo: simplify this if we stop caring about constructor type
-- todo: remove the cycle length once we drop it from Unison.Reference
declReferentsByPrefix ::
  DB m =>
  Text ->
  Maybe C.Reference.Pos ->
  Maybe ConstructorId ->
  m [(H.Hash, C.Reference.Pos, C.DeclType, [C.Decl.ConstructorId])]
declReferentsByPrefix b32prefix pos cid = do
  componentReferencesByPrefix OT.DeclComponent b32prefix pos
    >>= traverse (loadConstructors cid)
  where
    loadConstructors :: DB m => Maybe Word64 -> S.Reference.Id -> m (H.Hash, C.Reference.Pos, C.DeclType, [ConstructorId])
    loadConstructors cid rid@(C.Reference.Id oId pos) = do
      (dt, ctorCount) <- getDeclCtorCount rid
      h <- loadHashByObjectId oId
      let test :: ConstructorId -> Bool
          test = maybe (const True) (==) cid
          cids = [cid | cid <- [0 :: ConstructorId .. ctorCount - 1], test cid]
      pure (h, pos, dt, cids)
    getDeclCtorCount :: DB m => S.Reference.Id -> m (C.Decl.DeclType, ConstructorId)
    getDeclCtorCount id@(C.Reference.Id r i) = do
      when debug $ traceM $ "getDeclCtorCount " ++ show id
      (_localIds, decl) <- Q.expectDeclObjectById r (decodeDeclElement i)
      pure (C.Decl.declType decl, fromIntegral $ length (C.Decl.constructorTypes decl))

branchHashesByPrefix :: DB m => ShortBranchHash -> m (Set BranchHash)
branchHashesByPrefix (ShortBranchHash b32prefix) = do
  hashIds <- Q.namespaceHashIdByBase32Prefix b32prefix
  b32s <- traverse (Q.loadHashById . Db.unBranchHashId) hashIds
  pure $ Set.fromList . fmap BranchHash . fmap H.fromBase32Hex $ b32s

causalHashesByPrefix :: DB m => ShortBranchHash -> m (Set CausalHash)
causalHashesByPrefix (ShortBranchHash b32prefix) = do
  hashIds <- Q.causalHashIdByBase32Prefix b32prefix
  b32s <- traverse (Q.loadHashById . Db.unCausalHashId) hashIds
  pure $ Set.fromList . fmap CausalHash . fmap H.fromBase32Hex $ b32s

-- | returns a list of known definitions referencing `r`
dependents :: DB m => C.Reference -> m (Set C.Reference.Id)
dependents r = do
  r' <- c2sReference r
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependency r'
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- | returns a list of known definitions referencing `h`
dependentsOfComponent :: DB m => H.Hash -> m (Set C.Reference.Id)
dependentsOfComponent h = do
  oId <- primaryHashToExistingObjectId h
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependencyComponent oId
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- | returns empty set for unknown inputs; doesn't distinguish between term and decl
derivedDependencies :: DB m => C.Reference.Id -> m (Set C.Reference.Id)
derivedDependencies cid = do
  sid <- c2sReferenceId cid
  sids <- Q.getDependencyIdsForDependent sid
  cids <- traverse s2cReferenceId sids
  pure $ Set.fromList cids

-- lca              :: (forall he e. [Causal m CausalHash he e] -> m (Maybe BranchHash)),

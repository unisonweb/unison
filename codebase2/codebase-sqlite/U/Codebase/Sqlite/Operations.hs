{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
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

module U.Codebase.Sqlite.Operations (
  -- * data version
  dataVersion,

  -- * branches
  saveRootBranch,
  loadMaybeRootCausalHash,
  loadRootCausalHash,
  loadRootCausal,
  saveBranch,
  loadCausalBranchByCausalHash,

  -- * terms
  saveTermComponent,
  loadTermByReference,
  loadTypeOfTermByTermReference,

  -- * decls
  saveDeclComponent,
  loadDeclByReference,
  getDeclTypeByReference,

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
  -- ** type index
  addTypeToIndexForTerm,
  termsHavingType,
  -- ** type mentions index
  addTypeMentionsToIndexForTerm,
  termsMentioningType,

  -- * delete me
  getCycleLen,

  -- * Error types
  Error(..),
  DecodeError(..),

  -- ** Constraint kinds
  EDB,

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
) where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import Control.Monad (MonadPlus (mzero), join, unless, when, (<=<))
import Control.Monad.Except (ExceptT, MonadError, MonadIO (liftIO), runExceptT)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Extra as Monad
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT)
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
import GHC.Stack (HasCallStack)
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
import U.Codebase.Sqlite.Connection (Connection)
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalHashId (..),
    LocalIds,
    LocalIds' (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
    WatchLocalIds,
  )
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Patch.Diff as S
import qualified U.Codebase.Sqlite.Patch.Diff as S.PatchDiff
import qualified U.Codebase.Sqlite.Patch.Format as S
import qualified U.Codebase.Sqlite.Patch.Format as S.PatchFormat
import qualified U.Codebase.Sqlite.Patch.Full as S
import qualified U.Codebase.Sqlite.Patch.Full as S.Patch.Full
import qualified U.Codebase.Sqlite.Patch.TermEdit as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as S.TermEdit
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S.TypeEdit
import U.Codebase.Sqlite.Queries (DB)
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
import qualified Unison.Util.Map as Map
import qualified U.Util.Monoid as Monoid
import U.Util.Serialization (Get)
import qualified U.Util.Serialization as S
import qualified Unison.Util.Set as Set
import qualified U.Util.Term as TermUtil
import UnliftIO (Exception)

-- * Error handling

throwError :: Err m => Error -> m a
throwError = if crashOnError then error . show else Except.throwError

debug, crashOnError :: Bool
debug = False

-- | crashOnError can be helpful for debugging.
-- If it is False, the errors will be delivered to the user elsewhere.
crashOnError = False

type Err m = (MonadError Error m, HasCallStack)

type EDB m = (Err m, DB m)

type ErrString = String

data DecodeError
  = ErrTermElement Word64
  | ErrDeclElement Word64
  | ErrFramedArrayLen
  | ErrTypeOfTerm C.Reference.Id
  | ErrWatch WatchKind C.Reference.Id
  | ErrBranch Db.BranchObjectId
  | ErrPatch Db.PatchObjectId
  | ErrObjectDependencies OT.ObjectType Db.ObjectId
  deriving (Show)

data Error
  = DecodeError DecodeError ByteString ErrString
  | DatabaseIntegrityError Q.Integrity
  | UnknownDependency H.Hash
  | UnknownText Text
  | ExpectedBranch CausalHash BranchHash
  | ExpectedBranch' Db.CausalHashId
  | LegacyUnknownCycleLen H.Hash
  | LegacyUnknownConstructorType H.Hash C.Reference.Pos
  | NeedTypeForBuiltinMetadata Text
  deriving (Show)

instance Exception Error

getFromBytesOr :: Err m => DecodeError -> Get a -> ByteString -> m a
getFromBytesOr e get bs = case runGetS get bs of
  Left err -> throwError (DecodeError e bs err)
  Right a -> pure a

liftQ :: Err m => ExceptT Q.Integrity m a -> m a
liftQ a =
  runExceptT a >>= \case
    Left e -> throwError (DatabaseIntegrityError e)
    Right a -> pure a

-- * Database lookups

lookupTextId :: EDB m => Text -> m Db.TextId
lookupTextId t =
  Q.loadText t >>= \case
    Just textId -> pure textId
    Nothing -> throwError $ UnknownText t

loadTextById :: EDB m => Db.TextId -> m Text
loadTextById = liftQ . Q.loadTextById

-- | look up an existing object by its primary hash
primaryHashToExistingObjectId :: EDB m => H.Hash -> m Db.ObjectId
primaryHashToExistingObjectId h = do
  (Q.loadHashId . H.toBase32Hex) h >>= \case
    Just hashId -> liftQ $ Q.expectObjectIdForPrimaryHashId hashId
    Nothing -> throwError $ UnknownDependency h

primaryHashToMaybeObjectId :: DB m => H.Hash -> m (Maybe Db.ObjectId)
primaryHashToMaybeObjectId h = do
  (Q.loadHashId . H.toBase32Hex) h >>= \case
    Just hashId -> Q.maybeObjectIdForPrimaryHashId hashId
    Nothing -> pure Nothing

primaryHashToMaybePatchObjectId :: EDB m => PatchHash -> m (Maybe Db.PatchObjectId)
primaryHashToMaybePatchObjectId =
  (fmap . fmap) Db.PatchObjectId . primaryHashToMaybeObjectId . unPatchHash

objectExistsForHash :: DB m => H.Hash -> m Bool
objectExistsForHash h =
  isJust <$> runMaybeT do
    id <- MaybeT . Q.loadHashId . H.toBase32Hex $ h
    MaybeT $ Q.maybeObjectIdForAnyHashId id

loadHashByObjectId :: EDB m => Db.ObjectId -> m H.Hash
loadHashByObjectId = fmap H.fromBase32Hex . liftQ . Q.loadPrimaryHashByObjectId

loadHashByHashId :: EDB m => Db.HashId -> m H.Hash
loadHashByHashId = fmap H.fromBase32Hex . liftQ . Q.loadHashById

loadCausalHashById :: EDB m => Db.CausalHashId -> m CausalHash
loadCausalHashById = fmap (CausalHash . H.fromBase32Hex) . liftQ . Q.loadHashById . Db.unCausalHashId

loadValueHashByCausalHashId :: EDB m => Db.CausalHashId -> m BranchHash
loadValueHashByCausalHashId = loadValueHashById <=< liftQ . Q.loadCausalValueHashId
  where
    loadValueHashById :: EDB m => Db.BranchHashId -> m BranchHash
    loadValueHashById = fmap (BranchHash . H.fromBase32Hex) . liftQ . Q.loadHashById . Db.unBranchHashId

loadRootCausalHash :: EDB m => m CausalHash
loadRootCausalHash = loadCausalHashById =<< liftQ Q.loadNamespaceRoot

loadMaybeRootCausalHash :: EDB m => m (Maybe CausalHash)
loadMaybeRootCausalHash = runMaybeT $
  loadCausalHashById =<< MaybeT (liftQ Q.loadMaybeNamespaceRoot)

-- * Reference transformations

-- ** read existing references

c2sReference :: EDB m => C.Reference -> m S.Reference
c2sReference = bitraverse lookupTextId primaryHashToExistingObjectId

s2cReference :: EDB m => S.Reference -> m C.Reference
s2cReference = bitraverse loadTextById loadHashByObjectId

c2sReferenceId :: EDB m => C.Reference.Id -> m S.Reference.Id
c2sReferenceId = C.Reference.idH primaryHashToExistingObjectId

s2cReferenceId :: EDB m => S.Reference.Id -> m C.Reference.Id
s2cReferenceId = C.Reference.idH loadHashByObjectId

h2cReferenceId :: EDB m => S.Reference.IdH -> m C.Reference.Id
h2cReferenceId = C.Reference.idH loadHashByHashId

h2cReference :: EDB m => S.ReferenceH -> m C.Reference
h2cReference = bitraverse loadTextById loadHashByHashId

c2hReference :: DB m => C.Reference -> MaybeT m S.ReferenceH
c2hReference = bitraverse (MaybeT . Q.loadText) (MaybeT . Q.loadHashIdByHash)

s2cReferent :: EDB m => S.Referent -> m C.Referent
s2cReferent = bitraverse s2cReference s2cReference

s2cReferentId :: EDB m => S.Referent.Id -> m C.Referent.Id
s2cReferentId = bitraverse loadHashByObjectId loadHashByObjectId

c2sReferentId :: EDB m => C.Referent.Id -> m S.Referent.Id
c2sReferentId = bitraverse primaryHashToExistingObjectId primaryHashToExistingObjectId

h2cReferent :: EDB m => S.ReferentH -> m C.Referent
h2cReferent = bitraverse h2cReference h2cReference

-- ** Edits transformations

s2cTermEdit :: EDB m => S.TermEdit -> m C.TermEdit
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

s2cTypeEdit :: EDB m => S.TypeEdit -> m C.TypeEdit
s2cTypeEdit = \case
  S.TypeEdit.Replace r -> C.TypeEdit.Replace <$> s2cReference r
  S.TypeEdit.Deprecate -> pure C.TypeEdit.Deprecate

-- | assumes that all relevant values are already in the DB
c2lPatch :: EDB m => C.Branch.Patch -> m (S.PatchLocalIds, S.LocalPatch)
c2lPatch (C.Branch.Patch termEdits typeEdits) =
  done =<< (runWriterT . flip evalStateT startState) do
    S.Patch
      <$> Map.bitraverse saveReferentH (Set.traverse saveTermEdit) termEdits
      <*> Map.bitraverse saveReferenceH (Set.traverse saveTypeEdit) typeEdits
  where
    startState = mempty @(Map Text LocalTextId, Map H.Hash LocalHashId, Map H.Hash LocalDefnId)
    done ::
      EDB m =>
      (a, (Seq Text, Seq H.Hash, Seq H.Hash)) ->
      m (S.PatchFormat.PatchLocalIds, a)
    done (lPatch, (textValues, hashValues, defnValues)) = do
      textIds <- liftQ $ traverse Q.saveText textValues
      hashIds <- liftQ $ traverse Q.saveHashHash hashValues
      objectIds <- traverse primaryHashToExistingObjectId defnValues
      let ids =
            S.PatchFormat.LocalIds
              (Vector.fromList (Foldable.toList textIds))
              (Vector.fromList (Foldable.toList hashIds))
              (Vector.fromList (Foldable.toList objectIds))
      pure (ids, lPatch)

    lookupText ::
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field1' s (Map t LocalTextId),
        Lens.Field1' w (Seq t),
        Ord t
      ) =>
      t ->
      m LocalTextId
    lookupText = lookup_ Lens._1 Lens._1 LocalTextId

    lookupHash ::
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field2' s (Map d LocalHashId),
        Lens.Field2' w (Seq d),
        Ord d
      ) =>
      d ->
      m LocalHashId
    lookupHash = lookup_ Lens._2 Lens._2 LocalHashId

    lookupDefn ::
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field3' s (Map d LocalDefnId),
        Lens.Field3' w (Seq d),
        Ord d
      ) =>
      d ->
      m LocalDefnId
    lookupDefn = lookup_ Lens._3 Lens._3 LocalDefnId

    saveTermEdit = \case
      C.TermEdit.Replace r t -> S.TermEdit.Replace <$> saveReferent r <*> pure (c2sTyping t)
      C.TermEdit.Deprecate -> pure S.TermEdit.Deprecate

    saveTypeEdit = \case
      C.TypeEdit.Replace r -> S.TypeEdit.Replace <$> saveReference r
      C.TypeEdit.Deprecate -> pure S.TypeEdit.Deprecate

    saveReference = bitraverse lookupText lookupDefn
    saveReferenceH = bitraverse lookupText lookupHash
    saveReferent = bitraverse saveReference saveReference
    saveReferentH = bitraverse saveReferenceH saveReferenceH

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

decodeComponentLengthOnly :: Err m => ByteString -> m Word64
decodeComponentLengthOnly = getFromBytesOr ErrFramedArrayLen (Get.skip 1 >> S.lengthFramedArray)

decodeTermElementWithType :: Err m => C.Reference.Pos -> ByteString -> m (LocalIds, S.Term.Term, S.Term.Type)
decodeTermElementWithType i = getFromBytesOr (ErrTermElement i) (S.lookupTermElement i)

decodeTermElementDiscardingTerm :: Err m => C.Reference.Pos -> ByteString -> m (LocalIds, S.Term.Type)
decodeTermElementDiscardingTerm i = getFromBytesOr (ErrTermElement i) (S.lookupTermElementDiscardingTerm i)

decodeTermElementDiscardingType :: Err m => C.Reference.Pos -> ByteString -> m (LocalIds, S.Term.Term)
decodeTermElementDiscardingType i = getFromBytesOr (ErrTermElement i) (S.lookupTermElementDiscardingType i)

decodeDeclElement :: Err m => Word64 -> ByteString -> m (LocalIds, S.Decl.Decl Symbol)
decodeDeclElement i = getFromBytesOr (ErrDeclElement i) (S.lookupDeclElement i)

-- * legacy conversion helpers

getCycleLen :: EDB m => H.Hash -> m Word64
getCycleLen h = do
  when debug $ traceM $ "\ngetCycleLen " ++ (Text.unpack . Base32Hex.toText $ H.toBase32Hex h)
  runMaybeT (primaryHashToExistingObjectId h)
    >>= maybe (throwError $ LegacyUnknownCycleLen h) pure
    >>= liftQ . Q.loadObjectById
    -- todo: decodeComponentLengthOnly is unintentionally a hack that relies on
    -- the fact the two things that references can refer to (term and decl
    -- components) have the same basic serialized structure: first a format
    -- byte that is always 0 for now, followed by a framed array representing
    -- the strongly-connected component. :grimace:
    >>= decodeComponentLengthOnly
    >>= pure . fromIntegral

getDeclTypeByReference :: EDB m => C.Reference.Id -> m C.Decl.DeclType
getDeclTypeByReference r@(C.Reference.Id h pos) =
  runMaybeT (loadDeclByReference r)
    >>= maybe (throwError $ LegacyUnknownConstructorType h pos) pure
    >>= pure . C.Decl.declType

componentByObjectId :: EDB m => Db.ObjectId -> m [S.Reference.Id]
componentByObjectId id = do
  when debug . traceM $ "Operations.componentByObjectId " ++ show id
  len <- (liftQ . Q.loadObjectById) id >>= decodeComponentLengthOnly
  pure [C.Reference.Id id i | i <- [0 .. len - 1]]

-- * Codebase operations

-- ** Saving & loading terms
saveTermComponent :: EDB m => H.Hash -> [(C.Term Symbol, C.Term.Type Symbol)] -> m Db.ObjectId
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

-- | Save the text and hash parts of a Reference to the database and substitute their ids.
saveReferenceH :: DB m => C.Reference' Text H.Hash -> m (C.Reference' Db.TextId Db.HashId)
saveReferenceH = bitraverse Q.saveText Q.saveHashHash

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

loadTermWithTypeByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term Symbol, C.Term.Type Symbol)
loadTermWithTypeByReference (C.Reference.Id h i) =
  MaybeT (primaryHashToMaybeObjectId h)
    >>= liftQ . Q.loadObjectById
    -- retrieve and deserialize the blob
    >>= decodeTermElementWithType i
    >>= uncurry3 s2cTermWithType

loadTermByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term Symbol)
loadTermByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTermByReference " ++ show r
  MaybeT (primaryHashToMaybeObjectId h)
    >>= liftQ . Q.loadObjectWithTypeById
    >>= \case (OT.TermComponent, blob) -> pure blob; _ -> mzero
    -- retrieve and deserialize the blob
    >>= decodeTermElementDiscardingType i
    >>= uncurry s2cTerm

loadTypeOfTermByTermReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term.Type Symbol)
loadTypeOfTermByTermReference id@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTypeOfTermByTermReference " ++ show id
  MaybeT (primaryHashToMaybeObjectId h)
    >>= liftQ . Q.loadObjectWithTypeById
    >>= \case (OT.TermComponent, blob) -> pure blob; _ -> mzero
    -- retrieve and deserialize the blob
    >>= decodeTermElementDiscardingTerm i
    >>= uncurry s2cTypeOfTerm

s2cTermWithType :: EDB m => LocalIds -> S.Term.Term -> S.Term.Type -> m (C.Term Symbol, C.Term.Type Symbol)
s2cTermWithType ids tm tp = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure (x2cTerm substText substHash tm, x2cTType substText substHash tp)

s2cTerm :: EDB m => LocalIds -> S.Term.Term -> m (C.Term Symbol)
s2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure $ x2cTerm substText substHash tm

s2cTypeOfTerm :: EDB m => LocalIds -> S.Term.Type -> m (C.Term.Type Symbol)
s2cTypeOfTerm ids tp = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure $ x2cTType substText substHash tp

-- | implementation detail of {s,w}2c*Term*
localIdsToLookups :: Monad m => (t -> m Text) -> (d -> m H.Hash) -> LocalIds' t d -> m (LocalTextId -> Text, LocalDefnId -> H.Hash)
localIdsToLookups loadText loadHash localIds = do
  texts <- traverse loadText $ LocalIds.textLookup localIds
  hashes <- traverse loadHash $ LocalIds.defnLookup localIds
  let substText (LocalTextId w) = texts Vector.! fromIntegral w
      substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
  pure (substText, substHash)

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

c2sTerm :: EDB m => C.Term Symbol -> C.Term.Type Symbol -> m (LocalIds, S.Term.Term, S.Term.Type)
c2sTerm tm tp = c2xTerm Q.saveText primaryHashToExistingObjectId tm (Just tp) <&> \(w, tm, Just tp) -> (w, tm, tp)

-- *** Watch expressions

listWatches :: EDB m => WatchKind -> m [C.Reference.Id]
listWatches k = Q.loadWatchesByWatchKind k >>= traverse h2cReferenceId

-- | returns Nothing if the expression isn't cached.
loadWatch :: EDB m => WatchKind -> C.Reference.Id -> MaybeT m (C.Term Symbol)
loadWatch k r =
  C.Reference.idH Q.saveHashHash r
    >>= MaybeT . Q.loadWatch k
    >>= getFromBytesOr (ErrWatch k r) S.getWatchResultFormat
    >>= \case
      S.Term.WatchResult wlids t -> w2cTerm wlids t

saveWatch :: EDB m => WatchKind -> C.Reference.Id -> C.Term Symbol -> m ()
saveWatch w r t = do
  rs <- C.Reference.idH Q.saveHashHash r
  wterm <- c2wTerm t
  let bytes = S.putBytes S.putWatchResultFormat (uncurry S.Term.WatchResult wterm)
  Q.saveWatch w rs bytes

clearWatches :: DB m => m ()
clearWatches = Q.clearWatches

c2wTerm :: EDB m => C.Term Symbol -> m (WatchLocalIds, S.Term.Term)
c2wTerm tm = c2xTerm Q.saveText Q.saveHashHash tm Nothing <&> \(w, tm, _) -> (w, tm)

w2cTerm :: EDB m => WatchLocalIds -> S.Term.Term -> m (C.Term Symbol)
w2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByHashId ids
  pure $ x2cTerm substText substHash tm

-- ** Saving & loading type decls

saveDeclComponent :: EDB m => H.Hash -> [C.Decl Symbol] -> m Db.ObjectId
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

c2sDecl :: forall m t d. EDB m => (Text -> m t) -> (H.Hash -> m d) -> C.Decl Symbol -> m (LocalIds' t d, S.Decl.Decl Symbol)
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

loadDeclByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Decl Symbol)
loadDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadDeclByReference " ++ show r
  -- retrieve the blob
  (localIds, C.Decl.DataDeclaration dt m b ct) <-
    MaybeT (primaryHashToMaybeObjectId h)
      >>= liftQ . Q.loadObjectWithTypeById
      >>= \case (OT.DeclComponent, blob) -> pure blob; _ -> mzero
      >>= decodeDeclElement i

  -- look up the text and hashes that are used by the term
  texts <- traverse loadTextById $ LocalIds.textLookup localIds
  hashes <- traverse loadHashByObjectId $ LocalIds.defnLookup localIds

  -- substitute the text and hashes back into the term
  let substText tIdx = texts Vector.! fromIntegral tIdx
      substHash hIdx = hashes Vector.! fromIntegral hIdx
      substTypeRef :: S.Decl.TypeRef -> C.Decl.TypeRef
      substTypeRef = bimap substText (fmap substHash)
  pure (C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct)) -- lens might be nice here

-- * Branch transformation

s2cBranch :: EDB m => S.DbBranch -> m (C.Branch.Branch m)
s2cBranch (S.Branch.Full.Branch tms tps patches children) =
  C.Branch.Branch
    <$> doTerms tms
    <*> doTypes tps
    <*> doPatches patches
    <*> doChildren children
  where
    loadMetadataType :: EDB m => S.Reference -> m C.Reference
    loadMetadataType = \case
      C.ReferenceBuiltin tId ->
        loadTextById tId >>= throwError . NeedTypeForBuiltinMetadata
      C.ReferenceDerived id ->
        typeReferenceForTerm id >>= h2cReference

    loadTypesForMetadata rs = Map.fromList <$> traverse (\r -> (,) <$> s2cReference r <*> loadMetadataType r) (Foldable.toList rs)
    doTerms :: EDB m => Map Db.TextId (Map S.Referent S.DbMetadataSet) -> m (Map C.Branch.NameSegment (Map C.Referent (m C.Branch.MdValues)))
    doTerms =
      Map.bitraverse
        (fmap C.Branch.NameSegment . loadTextById)
        ( Map.bitraverse s2cReferent \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doTypes :: EDB m => Map Db.TextId (Map S.Reference S.DbMetadataSet) -> m (Map C.Branch.NameSegment (Map C.Reference (m C.Branch.MdValues)))
    doTypes =
      Map.bitraverse
        (fmap C.Branch.NameSegment . loadTextById)
        ( Map.bitraverse s2cReference \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> loadTypesForMetadata rs
        )
    doPatches :: EDB m => Map Db.TextId Db.PatchObjectId -> m (Map C.Branch.NameSegment (PatchHash, m C.Branch.Patch))
    doPatches = Map.bitraverse (fmap C.Branch.NameSegment . loadTextById) \patchId -> do
      h <- PatchHash <$> (loadHashByObjectId . Db.unPatchObjectId) patchId
      pure (h, loadPatchById patchId)

    doChildren :: EDB m => Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> m (Map C.Branch.NameSegment (C.Causal m CausalHash BranchHash (C.Branch.Branch m)))
    doChildren = Map.bitraverse (fmap C.Branch.NameSegment . loadTextById) \(boId, chId) ->
      C.Causal <$> loadCausalHashById chId
        <*> loadValueHashByCausalHashId chId
        <*> headParents chId
        <*> pure (loadBranchByObjectId boId)
      where
        headParents :: EDB m => Db.CausalHashId -> m (Map CausalHash (m (C.Causal m CausalHash BranchHash (C.Branch.Branch m))))
        headParents chId = do
          parentsChIds <- Q.loadCausalParents chId
          fmap Map.fromList $ traverse pairParent parentsChIds
        pairParent :: EDB m => Db.CausalHashId -> m (CausalHash, m (C.Causal m CausalHash BranchHash (C.Branch.Branch m)))
        pairParent chId = do
          h <- loadCausalHashById chId
          pure (h, loadCausal chId)
        loadCausal :: EDB m => Db.CausalHashId -> m (C.Causal m CausalHash BranchHash (C.Branch.Branch m))
        loadCausal chId = do
          C.Causal <$> loadCausalHashById chId
            <*> loadValueHashByCausalHashId chId
            <*> headParents chId
            <*> pure (loadValue chId)
        loadValue :: EDB m => Db.CausalHashId -> m (C.Branch.Branch m)
        loadValue chId =
          liftQ (Q.loadBranchObjectIdByCausalHashId chId) >>= \case
            Nothing -> throwError (ExpectedBranch' chId)
            Just boId -> loadBranchByObjectId boId

-- this maps from the key used by C.Branch to a local id
type BranchSavingState = (Map Text LocalTextId, Map H.Hash LocalDefnId, Map Db.PatchObjectId LocalPatchObjectId, Map (Db.BranchObjectId, Db.CausalHashId) LocalBranchChildId)

type BranchSavingWriter = (Seq Text, Seq H.Hash, Seq Db.PatchObjectId, Seq (Db.BranchObjectId, Db.CausalHashId))

type BranchSavingConstraint m = (MonadState BranchSavingState m, MonadWriter BranchSavingWriter m)

type BranchSavingMonad m = StateT BranchSavingState (WriterT BranchSavingWriter m)

saveRootBranch :: EDB m => C.Branch.Causal m -> m (Db.BranchObjectId, Db.CausalHashId)
saveRootBranch c = do
  when debug $ traceM $ "Operations.saveRootBranch " ++ show (C.causalHash c)
  (boId, chId) <- saveBranch c
  Q.setNamespaceRoot chId
  pure (boId, chId)

saveBranch :: EDB m => C.Branch.Causal m -> m (Db.BranchObjectId, Db.CausalHashId)
saveBranch (C.Causal hc he parents me) = do
  when debug $ traceM $ "\nOperations.saveBranch \n  hc = " ++ show hc ++ ",\n  he = " ++ show he ++ ",\n  parents = " ++ show (Map.keys parents)

  (chId, bhId) <- flip Monad.fromMaybeM (liftQ $ Q.loadCausalByCausalHash hc) do
    -- if not exist, create these
    chId <- liftQ (Q.saveCausalHash hc)
    bhId <- liftQ (Q.saveBranchHash he)
    liftQ (Q.saveCausal chId bhId)
    -- save the link between child and parents
    parentCausalHashIds <-
      -- so try to save each parent (recursively) before continuing to save hc
      for (Map.toList parents) $ \(parentHash, mcausal) ->
        -- check if we can short circuit the parent before loading it,
        -- by checking if there are causal parents associated with hc
        (flip Monad.fromMaybeM)
          (liftQ $ Q.loadCausalHashIdByCausalHash parentHash)
          (mcausal >>= fmap snd . saveBranch)
    unless (null parentCausalHashIds) $
      liftQ (Q.saveCausalParents chId parentCausalHashIds)
    pure (chId, bhId)
  boId <- flip Monad.fromMaybeM (liftQ $ Q.loadBranchObjectIdByCausalHashId chId) do
    (li, lBranch) <- c2lBranch =<< me
    saveBranchObject bhId li lBranch
  pure (boId, chId)
  where
    c2lBranch :: EDB m => C.Branch.Branch m -> m (BranchLocalIds, S.Branch.Full.LocalBranch)
    c2lBranch (C.Branch.Branch terms types patches children) =
      done =<< (runWriterT . flip evalStateT startState) do
        S.Branch
          <$> Map.bitraverse saveNameSegment (Map.bitraverse saveReferent saveMetadata) terms
          <*> Map.bitraverse saveNameSegment (Map.bitraverse saveReference saveMetadata) types
          <*> Map.bitraverse saveNameSegment savePatch' patches
          <*> Map.bitraverse saveNameSegment saveChild children
    saveNameSegment (C.Branch.NameSegment t) = lookupText t
    saveReference :: BranchSavingConstraint m => C.Reference.Reference -> m S.Reference.LocalReference
    saveReference = bitraverse lookupText lookupDefn
    saveReferent :: BranchSavingConstraint m => C.Referent.Referent -> m S.Referent.LocalReferent
    saveReferent = bitraverse saveReference saveReference
    saveMetadata :: Monad m => m C.Branch.MdValues -> BranchSavingMonad m S.Branch.Full.LocalMetadataSet
    saveMetadata mm = do
      C.Branch.MdValues m <- (lift . lift) mm
      S.Branch.Full.Inline <$> Set.traverse saveReference (Map.keysSet m)
    savePatch' :: EDB m => (PatchHash, m C.Branch.Patch) -> BranchSavingMonad m LocalPatchObjectId
    savePatch' (h, mp) = do
      patchOID <-
        primaryHashToMaybePatchObjectId h >>= \case
          Just patchOID -> pure patchOID
          Nothing -> savePatch h =<< (lift . lift) mp
      lookupPatch patchOID
    saveChild :: EDB m => C.Branch.Causal m -> BranchSavingMonad m LocalBranchChildId
    saveChild c = (lift . lift) (saveBranch c) >>= lookupChild
    lookupText ::
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
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field2' s (Map d LocalDefnId),
        Lens.Field2' w (Seq d),
        Ord d
      ) =>
      d ->
      m LocalDefnId
    lookupDefn = lookup_ Lens._2 Lens._2 LocalDefnId
    lookupPatch ::
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field3' s (Map p LocalPatchObjectId),
        Lens.Field3' w (Seq p),
        Ord p
      ) =>
      p ->
      m LocalPatchObjectId
    lookupPatch = lookup_ Lens._3 Lens._3 LocalPatchObjectId
    lookupChild ::
      ( MonadState s m,
        MonadWriter w m,
        Lens.Field4' s (Map c LocalBranchChildId),
        Lens.Field4' w (Seq c),
        Ord c
      ) =>
      c ->
      m LocalBranchChildId
    lookupChild = lookup_ Lens._4 Lens._4 LocalBranchChildId
    startState = mempty @BranchSavingState
    saveBranchObject :: DB m => Db.BranchHashId -> BranchLocalIds -> S.Branch.Full.LocalBranch -> m Db.BranchObjectId
    saveBranchObject id@(Db.unBranchHashId -> hashId) li lBranch = do
      when debug $ traceM $ "saveBranchObject\n\tid = " ++ show id ++ "\n\tli = " ++ show li ++ "\n\tlBranch = " ++ show lBranch
      let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full li lBranch
      oId <- Q.saveObject hashId OT.Namespace bytes
      pure $ Db.BranchObjectId oId
    done :: (EDB m, Show a) => (a, BranchSavingWriter) -> m (BranchLocalIds, a)
    done (lBranch, written@(textValues, defnHashes, patchObjectIds, branchCausalIds)) = do
      when debug $ traceM $ "saveBranch.done\n\tlBranch = " ++ show lBranch ++ "\n\twritten = " ++ show written
      textIds <- liftQ $ traverse Q.saveText textValues
      defnObjectIds <- traverse primaryHashToExistingObjectId defnHashes
      let ids =
            S.BranchFormat.LocalIds
              (Vector.fromList (Foldable.toList textIds))
              (Vector.fromList (Foldable.toList defnObjectIds))
              (Vector.fromList (Foldable.toList patchObjectIds))
              (Vector.fromList (Foldable.toList branchCausalIds))
      pure (ids, lBranch)

loadRootCausal :: EDB m => m (C.Branch.Causal m)
loadRootCausal = liftQ Q.loadNamespaceRoot >>= loadCausalByCausalHashId

dataVersion :: DB m => m Q.DataVersion
dataVersion = Q.dataVersion

loadCausalBranchByCausalHash :: EDB m => CausalHash -> m (Maybe (C.Branch.Causal m))
loadCausalBranchByCausalHash hc = do
  Q.loadCausalHashIdByCausalHash hc >>= \case
    Just chId -> Just <$> loadCausalByCausalHashId chId
    Nothing -> pure Nothing

loadCausalByCausalHashId :: EDB m => Db.CausalHashId -> m (C.Branch.Causal m)
loadCausalByCausalHashId id = do
  hc <- loadCausalHashById id
  hb <- loadValueHashByCausalHashId id
  let loadNamespace =
        loadBranchByCausalHashId id >>= \case
          Nothing -> throwError (ExpectedBranch' id)
          Just b -> pure b
  parentHashIds <- Q.loadCausalParents id
  loadParents <- for parentHashIds \hId -> do
    h <- loadCausalHashById hId
    pure (h, loadCausalByCausalHashId hId)
  pure $ C.Causal hc hb (Map.fromList loadParents) loadNamespace

-- | is this even a thing?  loading a branch by causal hash?  yes I guess so.
loadBranchByCausalHashId :: EDB m => Db.CausalHashId -> m (Maybe (C.Branch.Branch m))
loadBranchByCausalHashId id = do
  (liftQ . Q.loadBranchObjectIdByCausalHashId) id
    >>= traverse loadBranchByObjectId

loadBranchByObjectId :: EDB m => Db.BranchObjectId -> m (C.Branch.Branch m)
loadBranchByObjectId id = do
  deserializeBranchObject id >>= \case
    S.BranchFormat.Full li f -> s2cBranch (l2sFull li f)
    S.BranchFormat.Diff r li d -> doDiff r [l2sDiff li d]
  where
    deserializeBranchObject :: EDB m => Db.BranchObjectId -> m S.BranchFormat
    deserializeBranchObject id = do
      when debug $ traceM $ "deserializeBranchObject " ++ show id
      (liftQ . Q.loadObjectById) (Db.unBranchObjectId id)
        >>= getFromBytesOr (ErrBranch id) S.getBranchFormat

    l2sFull :: S.BranchFormat.BranchLocalIds -> S.LocalBranch -> S.DbBranch
    l2sFull li =
      S.Branch.Full.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

    l2sDiff :: S.BranchFormat.BranchLocalIds -> S.Branch.LocalDiff -> S.Branch.Diff
    l2sDiff li = S.BranchDiff.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

    doDiff :: EDB m => Db.BranchObjectId -> [S.Branch.Diff] -> m (C.Branch.Branch m)
    doDiff ref ds =
      deserializeBranchObject ref >>= \case
        S.BranchFormat.Full li f -> joinFull (l2sFull li f) ds
        S.BranchFormat.Diff ref' li' d' -> doDiff ref' (l2sDiff li' d' : ds)
      where
        joinFull :: EDB m => S.DbBranch -> [S.Branch.Diff] -> m (C.Branch.Branch m)
        joinFull f [] = s2cBranch f
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

    lookupBranchLocalText :: S.BranchLocalIds -> LocalTextId -> Db.TextId
    lookupBranchLocalText li (LocalTextId w) = S.BranchFormat.branchTextLookup li Vector.! fromIntegral w

    lookupBranchLocalDefn :: S.BranchLocalIds -> LocalDefnId -> Db.ObjectId
    lookupBranchLocalDefn li (LocalDefnId w) = S.BranchFormat.branchDefnLookup li Vector.! fromIntegral w

    lookupBranchLocalPatch :: BranchLocalIds -> LocalPatchObjectId -> Db.PatchObjectId
    lookupBranchLocalPatch li (LocalPatchObjectId w) = S.BranchFormat.branchPatchLookup li Vector.! fromIntegral w

    lookupBranchLocalChild :: BranchLocalIds -> LocalBranchChildId -> (Db.BranchObjectId, Db.CausalHashId)
    lookupBranchLocalChild li (LocalBranchChildId w) = S.BranchFormat.branchChildLookup li Vector.! fromIntegral w

-- * Patch transformation

loadPatchById :: EDB m => Db.PatchObjectId -> m C.Branch.Patch
loadPatchById patchId =
  deserializePatchObject patchId >>= \case
    S.PatchFormat.Full li p -> s2cPatch (l2sPatchFull li p)
    S.PatchFormat.Diff ref li d -> doDiff ref [l2sPatchDiff li d]
  where
    doDiff :: EDB m => Db.PatchObjectId -> [S.PatchDiff] -> m C.Branch.Patch
    doDiff ref ds =
      deserializePatchObject ref >>= \case
        S.PatchFormat.Full li f -> joinFull (l2sPatchFull li f) ds
        S.PatchFormat.Diff ref' li' d' -> doDiff ref' (l2sPatchDiff li' d' : ds)
    joinFull :: EDB m => S.Patch -> [S.PatchDiff] -> m C.Branch.Patch
    joinFull f [] = s2cPatch f
    joinFull (S.Patch termEdits typeEdits) (S.PatchDiff addedTermEdits addedTypeEdits removedTermEdits removedTypeEdits : ds) = joinFull f' ds
      where
        f' = S.Patch (addRemove addedTermEdits removedTermEdits termEdits) (addRemove addedTypeEdits removedTypeEdits typeEdits)
        addRemove :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b) -> Map a (Set b)
        addRemove add del src =
          (Map.unionWith (<>) add (Map.differenceWith remove src del))
        remove :: Ord b => Set b -> Set b -> Maybe (Set b)
        remove src del =
          let diff = Set.difference src del
           in if diff == mempty then Nothing else Just diff

    -- implementation detail of loadPatchById?
    lookupPatchLocalText :: S.PatchLocalIds -> LocalTextId -> Db.TextId
    lookupPatchLocalText li (LocalTextId w) = S.PatchFormat.patchTextLookup li Vector.! fromIntegral w

    lookupPatchLocalHash :: S.PatchLocalIds -> LocalHashId -> Db.HashId
    lookupPatchLocalHash li (LocalHashId w) = S.PatchFormat.patchHashLookup li Vector.! fromIntegral w

    lookupPatchLocalDefn :: S.PatchLocalIds -> LocalDefnId -> Db.ObjectId
    lookupPatchLocalDefn li (LocalDefnId w) = S.PatchFormat.patchDefnLookup li Vector.! fromIntegral w

    l2sPatchFull :: S.PatchFormat.PatchLocalIds -> S.LocalPatch -> S.Patch
    l2sPatchFull li =
      S.Patch.Full.trimap
        (lookupPatchLocalText li)
        (lookupPatchLocalHash li)
        (lookupPatchLocalDefn li)

    l2sPatchDiff :: S.PatchFormat.PatchLocalIds -> S.LocalPatchDiff -> S.PatchDiff
    l2sPatchDiff li =
      S.PatchDiff.trimap
        (lookupPatchLocalText li)
        (lookupPatchLocalHash li)
        (lookupPatchLocalDefn li)

savePatch :: EDB m => PatchHash -> C.Branch.Patch -> m Db.PatchObjectId
savePatch h c = do
  (li, lPatch) <- c2lPatch c
  hashId <- Q.saveHashHash (unPatchHash h)
  let bytes = S.putBytes S.putPatchFormat $ S.PatchFormat.Full li lPatch
  Db.PatchObjectId <$> Q.saveObject hashId OT.Patch bytes

s2cPatch :: EDB m => S.Patch -> m C.Branch.Patch
s2cPatch (S.Patch termEdits typeEdits) =
  C.Branch.Patch
    <$> Map.bitraverse h2cReferent (Set.traverse s2cTermEdit) termEdits
    <*> Map.bitraverse h2cReference (Set.traverse s2cTypeEdit) typeEdits

deserializePatchObject :: EDB m => Db.PatchObjectId -> m S.PatchFormat
deserializePatchObject id = do
  when debug $ traceM $ "Operations.deserializePatchObject " ++ show id
  (liftQ . Q.loadObjectById) (Db.unPatchObjectId id)
    >>= getFromBytesOr (ErrPatch id) S.getPatchFormat

lca :: EDB m => CausalHash -> CausalHash -> Connection -> Connection -> m (Maybe CausalHash)
lca h1 h2 c1 c2 = runMaybeT do
  chId1 <- MaybeT $ Q.loadCausalHashIdByCausalHash h1
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  chId3 <- MaybeT . liftIO $ Q.lca chId1 chId2 c1 c2
  liftQ $ Q.loadCausalHash chId3

before :: DB m => CausalHash -> CausalHash -> m (Maybe Bool)
before h1 h2 = runMaybeT do
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  lift (Q.loadCausalHashIdByCausalHash h1) >>= \case
    Just chId1 -> lift (Q.before chId1 chId2)
    Nothing -> pure False

-- * Searches

termsHavingType :: EDB m => C.Reference -> m (Set C.Referent.Id)
termsHavingType cTypeRef = do
  maySet <- runMaybeT $ do
    sTypeRef <- c2hReference cTypeRef
    sIds <- Q.getReferentsByType sTypeRef
    traverse s2cReferentId sIds
  pure case maySet of
    Nothing -> mempty
    Just set -> Set.fromList set

typeReferenceForTerm :: EDB m => S.Reference.Id -> m S.ReferenceH
typeReferenceForTerm = liftQ . Q.getTypeReferenceForReferent . C.Referent.RefId

termsMentioningType :: EDB m => C.Reference -> m (Set C.Referent.Id)
termsMentioningType cTypeRef = do
  maySet <- runMaybeT $ do
    sTypeRef <- c2hReference cTypeRef
    sIds <- Q.getReferentsByTypeMention sTypeRef
    traverse s2cReferentId sIds
  pure case maySet of
    Nothing -> mempty
    Just set -> Set.fromList set

addTypeToIndexForTerm :: EDB m => S.Referent.Id -> C.Reference -> m ()
addTypeToIndexForTerm sTermId cTypeRef = do
  sTypeRef <- saveReferenceH cTypeRef
  Q.addToTypeIndex sTypeRef sTermId

addTypeMentionsToIndexForTerm :: EDB m => S.Referent.Id -> Set C.Reference -> m ()
addTypeMentionsToIndexForTerm sTermId cTypeMentionRefs = do
  traverse_ (flip Q.addToTypeMentionsIndex sTermId <=< saveReferenceH) cTypeMentionRefs

-- something kind of funny here.  first, we don't need to enumerate all the reference pos if we're just picking one
-- second, it would be nice if we could leave these as S.References a little longer
-- so that we remember how to blow up if they're missing
componentReferencesByPrefix :: EDB m => OT.ObjectType -> Text -> Maybe C.Reference.Pos -> m [S.Reference.Id]
componentReferencesByPrefix ot b32prefix pos = do
  oIds :: [Db.ObjectId] <- Q.objectIdByBase32Prefix ot b32prefix
  let test = maybe (const True) (==) pos
  let filterComponent l = [x | x@(C.Reference.Id _ pos) <- l, test pos]
  fmap Monoid.fromMaybe . runMaybeT $
    join <$> traverse (fmap filterComponent . componentByObjectId) oIds

termReferencesByPrefix :: EDB m => Text -> Maybe Word64 -> m [C.Reference.Id]
termReferencesByPrefix t w =
  componentReferencesByPrefix OT.TermComponent t w
    >>= traverse (C.Reference.idH loadHashByObjectId)

declReferencesByPrefix :: EDB m => Text -> Maybe Word64 -> m [C.Reference.Id]
declReferencesByPrefix t w =
  componentReferencesByPrefix OT.DeclComponent t w
    >>= traverse (C.Reference.idH loadHashByObjectId)

termReferentsByPrefix :: EDB m => Text -> Maybe Word64 -> m [C.Referent.Id]
termReferentsByPrefix b32prefix pos =
  fmap C.Referent.RefId <$> termReferencesByPrefix b32prefix pos

-- todo: simplify this if we stop caring about constructor type
-- todo: remove the cycle length once we drop it from Unison.Reference
declReferentsByPrefix ::
  EDB m =>
  Text ->
  Maybe C.Reference.Pos ->
  Maybe ConstructorId ->
  m [(H.Hash, C.Reference.Pos, Word64, C.DeclType, [C.Decl.ConstructorId])]
declReferentsByPrefix b32prefix pos cid = do
  componentReferencesByPrefix OT.DeclComponent b32prefix pos
    >>= traverse (loadConstructors cid)
  where
    loadConstructors :: EDB m => Maybe Word64 -> S.Reference.Id -> m (H.Hash, C.Reference.Pos, Word64, C.DeclType, [ConstructorId])
    loadConstructors cid rid@(C.Reference.Id oId pos) = do
      (dt, len, ctorCount) <- getDeclCtorCount rid
      h <- loadHashByObjectId oId
      let test :: ConstructorId -> Bool
          test = maybe (const True) (==) cid
          cids = [cid | cid <- [0 :: ConstructorId .. ctorCount - 1], test cid]
      pure (h, pos, len, dt, cids)
    getDeclCtorCount :: EDB m => S.Reference.Id -> m (C.Decl.DeclType, Word64, ConstructorId)
    getDeclCtorCount id@(C.Reference.Id r i) = do
      when debug $ traceM $ "getDeclCtorCount " ++ show id
      bs <- liftQ (Q.loadObjectById r)
      len <- decodeComponentLengthOnly bs
      (_localIds, decl) <- decodeDeclElement i bs
      pure (C.Decl.declType decl, len, fromIntegral $ length (C.Decl.constructorTypes decl))

branchHashesByPrefix :: EDB m => ShortBranchHash -> m (Set BranchHash)
branchHashesByPrefix (ShortBranchHash b32prefix) = do
  hashIds <- Q.namespaceHashIdByBase32Prefix b32prefix
  b32s <- traverse (liftQ . Q.loadHashById . Db.unBranchHashId) hashIds
  pure $ Set.fromList . fmap BranchHash . fmap H.fromBase32Hex $ b32s

causalHashesByPrefix :: EDB m => ShortBranchHash -> m (Set CausalHash)
causalHashesByPrefix (ShortBranchHash b32prefix) = do
  hashIds <- Q.causalHashIdByBase32Prefix b32prefix
  b32s <- traverse (liftQ . Q.loadHashById . Db.unCausalHashId) hashIds
  pure $ Set.fromList . fmap CausalHash . fmap H.fromBase32Hex $ b32s

-- | returns a list of known definitions referencing `r`
dependents :: EDB m => C.Reference -> m (Set C.Reference.Id)
dependents r = do
  r' <- c2sReference r
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependency r'
  -- how will you convert this back to Unison.Reference if you
  -- don't know the cycle size?
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- | returns empty set for unknown inputs; doesn't distinguish between term and decl
derivedDependencies :: EDB m => C.Reference.Id -> m (Set C.Reference.Id)
derivedDependencies cid = do
  sid <- c2sReferenceId cid
  sids <- Q.getDependencyIdsForDependent sid
  cids <- traverse s2cReferenceId sids
  pure $ Set.fromList cids

-- lca              :: (forall he e. [Causal m CausalHash he e] -> m (Maybe BranchHash)),

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Operations where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import Control.Monad (join, (<=<))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Writer (MonadWriter, runWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import qualified Data.Foldable as Foldable
import Data.Functor (void, (<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Tuple.Extra (uncurry3)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import qualified U.Codebase.Branch as C
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
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalIds,
    LocalIds' (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
    WatchLocalIds,
  )
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Patch.Diff as S
import qualified U.Codebase.Sqlite.Patch.Format as S
import qualified U.Codebase.Sqlite.Patch.Format as S.PatchFormat
import qualified U.Codebase.Sqlite.Patch.Full as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as S.TermEdit
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S.TypeEdit
import U.Codebase.Sqlite.Queries (DB)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Reference as S.Reference
import qualified U.Codebase.Sqlite.Referent as S
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
import qualified U.Util.Hash as H
import qualified U.Util.Lens as Lens
import qualified U.Util.Map as Map
import qualified U.Util.Monoid as Monoid
import U.Util.Serialization (Get)
import qualified U.Util.Serialization as S
import qualified U.Util.Set as Set

type Err m = MonadError Error m

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
  deriving (Show)

data Error
  = DecodeError DecodeError ByteString ErrString
  | DatabaseIntegrityError Q.Integrity
  | UnknownDependency H.Hash
  | UnknownText Text
  | LegacyUnknownCycleLen H.Hash
  | LegacyUnknownConstructorType H.Hash C.Reference.Pos
  deriving (Show)

getFromBytesOr :: Err m => DecodeError -> Get a -> ByteString -> m a
getFromBytesOr e get bs = case runGetS get bs of
  Left err -> throwError (DecodeError e bs err)
  Right a -> pure a

liftQ :: Err m => ExceptT Q.Integrity m a -> m a
liftQ a =
  runExceptT a >>= \case
    Left e -> throwError (DatabaseIntegrityError e)
    Right a -> pure a

-- * helpers

c2sReference :: EDB m => C.Reference -> MaybeT m S.Reference
c2sReference = bitraverse lookupTextId hashToObjectId

s2cReference :: EDB m => S.Reference -> m C.Reference
s2cReference = bitraverse loadTextById loadHashByObjectId

c2sReferenceId :: EDB m => C.Reference.Id -> MaybeT m S.Reference.Id
c2sReferenceId = C.Reference.idH hashToObjectId

s2cReferenceId :: EDB m => S.Reference.Id -> m C.Reference.Id
s2cReferenceId = C.Reference.idH loadHashByObjectId

s2cReferent :: EDB m => S.Referent -> m C.Referent
s2cReferent = bitraverse s2cReference s2cReference

s2cTermEdit :: EDB m => S.TermEdit -> m C.TermEdit
s2cTermEdit = \case
  S.TermEdit.Replace r t -> C.TermEdit.Replace <$> s2cReference r <*> pure (s2cTyping t)
  S.TermEdit.Deprecate -> pure C.TermEdit.Deprecate

s2cTyping :: S.TermEdit.Typing -> C.TermEdit.Typing
s2cTyping = \case
  S.TermEdit.Same -> C.TermEdit.Same
  S.TermEdit.Subtype -> C.TermEdit.Subtype
  S.TermEdit.Different -> C.TermEdit.Different

s2cTypeEdit :: EDB m => S.TypeEdit -> m C.TypeEdit
s2cTypeEdit = \case
  S.TypeEdit.Replace r -> C.TypeEdit.Replace <$> s2cReference r
  S.TypeEdit.Deprecate -> pure C.TypeEdit.Deprecate

s2cBranch :: EDB m => S.DbBranch -> m (C.Branch m)
s2cBranch (S.Branch.Full.Branch tms tps patches children) =
  C.Branch
    <$> doTerms tms
    <*> doTypes tps
    <*> doPatches patches
    <*> doChildren children
  where
    doTerms :: EDB m => Map Db.TextId (Map S.Referent S.DbMetadataSet) -> m (Map C.NameSegment (Map C.Referent (m C.MdValues)))
    doTerms =
      Map.bitraverse
        (fmap C.NameSegment . loadTextById)
        ( Map.bitraverse s2cReferent \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> Set.traverse s2cReference rs
        )
    doTypes :: EDB m => Map Db.TextId (Map S.Reference S.DbMetadataSet) -> m (Map C.NameSegment (Map C.Reference (m C.MdValues)))
    doTypes =
      Map.bitraverse
        (fmap C.NameSegment . loadTextById)
        ( Map.bitraverse s2cReference \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> Set.traverse s2cReference rs
        )
    doPatches :: EDB m => Map Db.TextId Db.PatchObjectId -> m (Map C.NameSegment (PatchHash, m C.Patch))
    doPatches = Map.bitraverse (fmap C.NameSegment . loadTextById) \patchId -> do
      h <- PatchHash <$> (loadHashByObjectId . Db.unPatchObjectId) patchId
      let patch :: EDB m => m C.Patch
          patch = do
            deserializePatchObject patchId >>= \case
              S.PatchFormat.Full p -> s2cPatch p
              S.PatchFormat.Diff ref d -> doDiff ref [d]
          doDiff ref ds =
            deserializePatchObject ref >>= \case
              S.PatchFormat.Full f -> s2cPatch (joinFull f ds)
              S.PatchFormat.Diff ref' d' -> doDiff ref' (d' : ds)
          joinFull f [] = f
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
      pure (h, patch)

    doChildren :: EDB m => Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> m (Map C.Branch.NameSegment (C.CausalHead m CausalHash BranchHash (C.Branch m)))
    doChildren = Map.bitraverse (fmap C.NameSegment . loadTextById) \(boId, chId) ->
      C.CausalHead <$> loadCausalHashById chId
        <*> loadValueHashByCausalHashId chId
        <*> headParents chId
        <*> pure (loadBranchByObjectId boId)
      where
        headParents :: EDB m => Db.CausalHashId -> m (Map CausalHash (m (C.Causal m CausalHash BranchHash (C.Branch m))))
        headParents chId = do
          parentsChIds <- Q.loadCausalParents chId
          fmap Map.fromList $ traverse pairParent parentsChIds
        pairParent :: EDB m => Db.CausalHashId -> m (CausalHash, m (C.Causal m CausalHash BranchHash (C.Branch m)))
        pairParent chId = do
          h <- loadCausalHashById chId
          pure (h, loadCausal chId)
        loadCausal :: EDB m => Db.CausalHashId -> m (C.Causal m CausalHash BranchHash (C.Branch m))
        loadCausal chId = do
          C.Causal <$> loadCausalHashById chId
            <*> loadValueHashByCausalHashId chId
            <*> headParents chId
            <*> pure (loadValue chId)
        loadValue :: EDB m => Db.CausalHashId -> m (Maybe (C.Branch m))
        loadValue chId = do
          boId <- liftQ $ Q.loadBranchObjectIdByCausalHashId chId
          traverse loadBranchByObjectId boId

s2cPatch :: EDB m => S.Patch -> m C.Patch
s2cPatch (S.Patch termEdits typeEdits) =
  C.Patch
    <$> Map.bitraverse s2cReferent (Set.traverse s2cTermEdit) termEdits
    <*> Map.bitraverse s2cReference (Set.traverse s2cTypeEdit) typeEdits

lookupTextId :: EDB m => Text -> m Db.TextId
lookupTextId t =
  Q.loadText t >>= \case
    Just textId -> pure textId
    Nothing -> throwError $ UnknownText t

loadTextById :: EDB m => Db.TextId -> m Text
loadTextById = liftQ . Q.loadTextById

hashToObjectId :: EDB m => H.Hash -> m Db.ObjectId
hashToObjectId h = do
  (Q.loadHashId . H.toBase32Hex) h >>= \case
    Just hashId -> liftQ $ Q.expectObjectIdForPrimaryHashId hashId
    Nothing -> throwError $ UnknownDependency h

objectExistsForHash :: EDB m => H.Hash -> m Bool
objectExistsForHash h = isJust <$> runMaybeT (hashToObjectId h)

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

decodeComponentLengthOnly :: Err m => ByteString -> m Word64
decodeComponentLengthOnly = getFromBytesOr ErrFramedArrayLen S.lengthFramedArray

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
  runMaybeT (hashToObjectId h)
    >>= maybe (throwError $ LegacyUnknownCycleLen h) pure
    >>= liftQ . Q.loadObjectById
    >>= decodeComponentLengthOnly
    >>= pure . fromIntegral

getDeclTypeByReference :: EDB m => C.Reference.Id -> m C.Decl.DeclType
getDeclTypeByReference r@(C.Reference.Id h pos) =
  runMaybeT (loadDeclByReference r)
    >>= maybe (throwError $ LegacyUnknownConstructorType h pos) pure
    >>= pure . C.Decl.declType

-- * meat and veggies

loadTermWithTypeByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term Symbol, C.Term.Type Symbol)
loadTermWithTypeByReference (C.Reference.Id h i) =
  hashToObjectId h
    >>= liftQ . Q.loadObjectById
    -- retrieve and deserialize the blob
    >>= decodeTermElementWithType i
    >>= uncurry3 s2cTermWithType

loadTermByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term Symbol)
loadTermByReference (C.Reference.Id h i) =
  hashToObjectId h
    >>= liftQ . Q.loadObjectById
    -- retrieve and deserialize the blob
    >>= decodeTermElementDiscardingType i
    >>= uncurry s2cTerm

s2cTermWithType :: EDB m => LocalIds -> S.Term.Term -> S.Term.Type -> MaybeT m (C.Term Symbol, C.Term.Type Symbol)
s2cTermWithType ids tm tp = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure (x2cTerm substText substHash tm, x2cTType substText substHash tp)

s2cTerm :: EDB m => LocalIds -> S.Term.Term -> MaybeT m (C.Term Symbol)
s2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure $ x2cTerm substText substHash tm

s2cTypeOfTerm :: EDB m => LocalIds -> S.Term.Type -> MaybeT m (C.Term.Type Symbol)
s2cTypeOfTerm ids tp = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByObjectId ids
  pure $ x2cTType substText substHash tp

w2cTerm :: EDB m => WatchLocalIds -> S.Term.Term -> MaybeT m (C.Term Symbol)
w2cTerm ids tm = do
  (substText, substHash) <- localIdsToLookups loadTextById loadHashByHashId ids
  pure $ x2cTerm substText substHash tm

localIdsToLookups :: Monad m => (t -> m Text) -> (d -> m H.Hash) -> LocalIds' t d -> m (LocalTextId -> Text, LocalDefnId -> H.Hash)
localIdsToLookups loadText loadHash localIds = do
  texts <- traverse loadText $ LocalIds.textLookup localIds
  hashes <- traverse loadHash $ LocalIds.defnLookup localIds
  let substText (LocalTextId w) = texts Vector.! fromIntegral w
      substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
  pure (substText, substHash)

x2cTerm :: (LocalTextId -> Text) -> (LocalDefnId -> H.Hash) -> S.Term.Term -> C.Term Symbol
x2cTerm substText substHash =
  -- substitute the text and hashes back into the term
  C.Term.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id
  where
    substTermRef = bimap substText (fmap substHash)
    substTypeRef = bimap substText substHash
    substTermLink = bimap substTermRef substTypeRef
    substTypeLink = substTypeRef

x2cTType :: (LocalTextId -> Text) -> (LocalDefnId -> H.Hash) -> S.Term.Type -> C.Term.Type Symbol
x2cTType substText substHash = C.Type.rmap (bimap substText substHash)

-- | Shared implementation for preparing term definition+type or watch expression result for database.
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
      C.Term.Text t -> C.Term.Text <$> lookupText_ t
      C.Term.Char ch -> pure $ C.Term.Char ch
      C.Term.Ref r ->
        C.Term.Ref <$> bitraverse lookupText_ (traverse lookupDefn_) r
      C.Term.Constructor typeRef cid ->
        C.Term.Constructor
          <$> bitraverse lookupText_ lookupDefn_ typeRef
          <*> pure cid
      C.Term.Request typeRef cid ->
        C.Term.Request <$> bitraverse lookupText_ lookupDefn_ typeRef <*> pure cid
      C.Term.Handle a a2 -> pure $ C.Term.Handle a a2
      C.Term.App a a2 -> pure $ C.Term.App a a2
      C.Term.Ann a typ -> C.Term.Ann a <$> ABT.transformM goType typ
      C.Term.Sequence as -> pure $ C.Term.Sequence as
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
            (bitraverse lookupText_ (traverse lookupDefn_))
            (bitraverse lookupText_ lookupDefn_)
            r
      C.Term.TypeLink r ->
        C.Term.TypeLink <$> bitraverse lookupText_ lookupDefn_ r
    goType ::
      forall m a.
      (MonadWriter (Seq Text, Seq H.Hash) m, MonadState (Map Text LocalTextId, Map H.Hash LocalDefnId) m) =>
      C.Type.FT a ->
      m (S.Term.FT a)
    goType = \case
      C.Type.Ref r -> C.Type.Ref <$> bitraverse lookupText_ lookupDefn_ r
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
      C.Term.PText t -> C.Term.PText <$> lookupText_ t
      C.Term.PChar c -> pure $ C.Term.PChar c
      C.Term.PConstructor r i ps -> C.Term.PConstructor <$> bitraverse lookupText_ lookupDefn_ r <*> pure i <*> traverse goPat ps
      C.Term.PAs p -> C.Term.PAs <$> goPat p
      C.Term.PEffectPure p -> C.Term.PEffectPure <$> goPat p
      C.Term.PEffectBind r i bindings k -> C.Term.PEffectBind <$> bitraverse lookupText_ lookupDefn_ r <*> pure i <*> traverse goPat bindings <*> goPat k
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

lookupText_ ::
  ( MonadState s m,
    MonadWriter w m,
    Lens.Field1' s (Map t LocalTextId),
    Lens.Field1' w (Seq t),
    Ord t
  ) =>
  t ->
  m LocalTextId
lookupText_ = lookup_ Lens._1 Lens._1 LocalTextId

lookupDefn_ ::
  ( MonadState s m,
    MonadWriter w m,
    Lens.Field2' s (Map d LocalDefnId),
    Lens.Field2' w (Seq d),
    Ord d
  ) =>
  d ->
  m LocalDefnId
lookupDefn_ = lookup_ Lens._2 Lens._2 LocalDefnId

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

c2wTerm :: DB m => C.Term Symbol -> m (WatchLocalIds, S.Term.Term)
c2wTerm tm = c2xTerm Q.saveText Q.saveHashHash tm Nothing <&> \(w, tm, _) -> (w, tm)

c2sTerm :: EDB m => C.Term Symbol -> C.Term.Type Symbol -> m (LocalIds, S.Term.Term, S.Term.Type)
c2sTerm tm tp = c2xTerm Q.saveText hashToObjectId tm (Just tp) <&> \(w, tm, Just tp) -> (w, tm, tp)

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
      C.Type.Ref r -> C.Type.Ref <$> bitraverse lookupText_ (traverse lookupDefn_) r
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

loadTypeOfTermByTermReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term.Type Symbol)
loadTypeOfTermByTermReference (C.Reference.Id h i) =
  hashToObjectId h
    >>= liftQ . Q.loadObjectById
    -- retrieve and deserialize the blob
    >>= decodeTermElementDiscardingTerm i
    >>= uncurry s2cTypeOfTerm

loadDeclByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Decl Symbol)
loadDeclByReference (C.Reference.Id h i) = do
  -- retrieve the blob
  (localIds, C.Decl.DataDeclaration dt m b ct) <-
    hashToObjectId h >>= liftQ . Q.loadObjectById >>= decodeDeclElement i

  -- look up the text and hashes that are used by the term
  texts <- traverse loadTextById $ LocalIds.textLookup localIds
  hashes <- traverse loadHashByObjectId $ LocalIds.defnLookup localIds

  -- substitute the text and hashes back into the term
  let substText (LocalTextId w) = texts Vector.! fromIntegral w
      substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
      substTypeRef :: S.Decl.TypeRef -> C.Decl.TypeRef
      substTypeRef = bimap substText (fmap substHash)
  pure (C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct)) -- lens might be nice here

saveTermComponent :: EDB m => H.Hash -> [(C.Term Symbol, C.Term.Type Symbol)] -> m Db.ObjectId
saveTermComponent h terms = do
  sTermElements <- traverse (uncurry c2sTerm) terms
  hashId <- Q.saveHashHash h
  let bytes = S.putBytes S.putTermComponent (S.Term.LocallyIndexedComponent $ Vector.fromList sTermElements)
  Q.saveObject hashId OT.TermComponent bytes

saveDeclComponent :: EDB m => H.Hash -> [C.Decl Symbol] -> m Db.ObjectId
saveDeclComponent h decls = do
  sDeclElements <- traverse (c2sDecl Q.saveText hashToObjectId) decls
  hashId <- Q.saveHashHash h
  let bytes =
        S.putBytes
          S.putDeclFormat
          ( S.Decl.Decl . S.Decl.LocallyIndexedComponent $
              Vector.fromList sDeclElements
          )
  Q.saveObject hashId OT.DeclComponent bytes

listWatches :: EDB m => WatchKind -> m [C.Reference.Id]
listWatches k = Q.loadWatchesByWatchKind k >>= traverse s2cReferenceId

loadWatch :: EDB m => WatchKind -> C.Reference.Id -> MaybeT m (C.Term Symbol)
loadWatch k r =
  C.Reference.idH Q.saveHashHash r
    >>= MaybeT . Q.loadWatch k
    >>= getFromBytesOr (ErrWatch k r) (S.getPair S.getLocalIds S.getTerm)
    >>= uncurry s2cTerm

saveWatch :: EDB m => WatchKind -> C.Reference.Id -> C.Term Symbol -> m ()
saveWatch w r t = do
  rs <- C.Reference.idH Q.saveHashHash r
  wterm <- c2wTerm t
  let bytes = S.putBytes (S.putPair S.putLocalIds S.putTerm) wterm
  Q.saveWatch w rs bytes

termsHavingType :: DB m => C.Reference -> m (Set C.Referent.Id)
termsHavingType = error "todo"

termsMentioningType :: DB m => C.Reference -> m (Set C.Referent.Id)
termsMentioningType = error "todo"

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
    getDeclCtorCount (C.Reference.Id r i) = do
      bs <- liftQ (Q.loadObjectById r)
      len <- decodeComponentLengthOnly bs
      (_localIds, decl) <- decodeDeclElement i bs
      pure (C.Decl.declType decl, len, fromIntegral $ length (C.Decl.constructorTypes decl))

-- (localIds, C.Decl.DataDeclaration dt m b ct) <-
--   hashToObjectId h >>= liftQ . Q.loadObjectById >>= decodeDeclElement i

componentByObjectId :: EDB m => Db.ObjectId -> m [S.Reference.Id]
componentByObjectId id = do
  len <- (liftQ . Q.loadObjectById) id >>= decodeComponentLengthOnly
  pure [C.Reference.Id id i | i <- [0 .. len - 1]]

lookupLocalText :: S.BranchLocalIds -> LocalTextId -> Db.TextId
lookupLocalText li (LocalTextId w) = S.BranchFormat.branchTextLookup li Vector.! fromIntegral w

lookupLocalDefn :: S.BranchLocalIds -> LocalDefnId -> Db.ObjectId
lookupLocalDefn li (LocalDefnId w) = S.BranchFormat.branchDefnLookup li Vector.! fromIntegral w

lookupLocalPatch :: BranchLocalIds -> LocalPatchObjectId -> Db.PatchObjectId
lookupLocalPatch li (LocalPatchObjectId w) = S.BranchFormat.branchPatchLookup li Vector.! fromIntegral w

lookupLocalChild :: BranchLocalIds -> LocalBranchChildId -> (Db.BranchObjectId, Db.CausalHashId)
lookupLocalChild li (LocalBranchChildId w) = S.BranchFormat.branchChildLookup li Vector.! fromIntegral w

loadBranchByCausalHashId :: EDB m => Db.CausalHashId -> m (Maybe (C.Branch m))
loadBranchByCausalHashId id = do
  (liftQ . Q.loadBranchObjectIdByCausalHashId) id
    >>= traverse loadBranchByObjectId

deserializePatchObject :: EDB m => Db.PatchObjectId -> m S.PatchFormat
deserializePatchObject id =
  (liftQ . Q.loadObjectById) (Db.unPatchObjectId id)
    >>= getFromBytesOr (ErrPatch id) S.getPatchFormat

loadBranchByObjectId :: EDB m => Db.BranchObjectId -> m (C.Branch m)
loadBranchByObjectId id = do
  deserializeBranchObject id >>= \case
    S.BranchFormat.Full li f -> s2cBranch (l2sFull li f)
    S.BranchFormat.Diff r li d -> doDiff r [l2sDiff li d]
  where
    deserializeBranchObject :: EDB m => Db.BranchObjectId -> m S.BranchFormat
    deserializeBranchObject id =
      (liftQ . Q.loadObjectById) (Db.unBranchObjectId id)
        >>= getFromBytesOr (ErrBranch id) S.getBranchFormat

    l2sFull :: S.BranchFormat.BranchLocalIds -> S.LocalBranch -> S.DbBranch
    l2sFull li =
      S.Branch.Full.quadmap (lookupLocalText li) (lookupLocalDefn li) (lookupLocalPatch li) (lookupLocalChild li)

    l2sDiff :: S.BranchFormat.BranchLocalIds -> S.Branch.LocalDiff -> S.Branch.Diff
    l2sDiff li = S.BranchDiff.quadmap (lookupLocalText li) (lookupLocalDefn li) (lookupLocalPatch li) (lookupLocalChild li)

    doDiff :: EDB m => Db.BranchObjectId -> [S.Branch.Diff] -> m (C.Branch m)
    doDiff ref lds =
      deserializeBranchObject ref >>= \case
        S.BranchFormat.Full li f -> joinFull (l2sFull li f) lds
        S.BranchFormat.Diff ref' li' d' -> doDiff ref' (l2sDiff li' d' : lds)
      where
        joinFull :: EDB m => S.DbBranch -> [S.Branch.Diff] -> m (C.Branch m)
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
dependents :: EDB m => C.Reference -> MaybeT m (Set C.Reference.Id)
dependents r = do
  r' <- c2sReference r
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependency r'
  -- how will you convert this back to Unison.Reference if you
  -- don't know the cycle size?
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- * Sync-related dependency queries

termDependencies :: DB m => C.Reference.Id -> m (Maybe (Set C.Reference.Id))
termDependencies = error "todo"

declDependencies :: DB m => C.Reference.Id -> m (Maybe (Set C.Reference.Id))
declDependencies = error "todo"

-- branchDependencies ::
--   Branch.Hash -> m (Maybe (CausalHash, BD.Dependencies)),
-- -- |the "new" terms and types mentioned in a patch
-- patchDependencies :: EditHash -> m (Set Reference, Set Reference)

-- getBranchByAnyHash ::
-- getBranchByBranchHash :: DB m => BranchHash -> m (Maybe (Branch m))
-- getBranchByCausalHash :: DB m => CausalHash -> m (Maybe (Branch m))

-- lca              :: (forall he e. [Causal m CausalHash he e] -> m (Maybe BranchHash)),

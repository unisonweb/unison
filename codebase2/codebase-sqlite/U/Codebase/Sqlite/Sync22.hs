{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Sync22 where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Control.Monad.Except as Except
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Validate (ValidateT, runValidateT)
import qualified Control.Monad.Validate as Validate
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.List.Extra (nubOrd)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified U.Codebase.Reference as Reference
import qualified U.Codebase.Sqlite.Branch.Format as BL
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import qualified U.Codebase.Sqlite.LocalIds as L
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Patch.Format as PL
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as Sqlite
import qualified U.Codebase.Sqlite.Reference as Sqlite.Reference
import qualified U.Codebase.Sqlite.Referent as Sqlite.Referent
import qualified U.Codebase.Sqlite.Serialization as S
import qualified U.Codebase.Sqlite.Term.Format as TL
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Codebase.Sync (Sync (Sync), TrySyncResult)
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import U.Util.Cache (Cache)
import qualified U.Util.Cache as Cache
import Unison.Prelude

data Entity
  = O ObjectId
  | C CausalHashId
  | W WK.WatchKind Sqlite.Reference.IdH
  deriving (Eq, Ord, Show)

data DbTag = SrcDb | DestDb

data DecodeError
  = ErrTermComponent
  | ErrDeclComponent
  | ErrBranchFormat
  | ErrPatchFormat
  | ErrWatchResult
  deriving (Show)

type ErrString = String

data Error
  = DbIntegrity Q.Integrity
  | DecodeError DecodeError ByteString ErrString
  | -- | hashes corresponding to a single object in source codebase
    --  correspond to multiple objects in destination codebase
    HashObjectCorrespondence ObjectId [HashId] [HashId] [ObjectId]
  | SourceDbNotExist
  deriving (Show)

data Env = Env
  { srcDB :: Connection,
    destDB :: Connection,
    -- | there are three caches of this size
    idCacheSize :: Word
  }

debug :: Bool
debug = False

-- data Mappings
sync22 ::
  ( MonadIO m,
    MonadError Error m,
    MonadReader Env m
  ) =>
  m (Sync m Entity)
sync22 = do
  size <- Reader.reader idCacheSize
  tCache <- Cache.semispaceCache size
  hCache <- Cache.semispaceCache size
  oCache <- Cache.semispaceCache size
  cCache <- Cache.semispaceCache size
  pure $ Sync (trySync tCache hCache oCache cCache)

trySync ::
  forall m.
  (MonadIO m, MonadError Error m, MonadReader Env m) =>
  Cache TextId TextId ->
  Cache HashId HashId ->
  Cache ObjectId ObjectId ->
  Cache CausalHashId CausalHashId ->
  Entity ->
  m (TrySyncResult Entity)
trySync tCache hCache oCache cCache = \case
  -- for causals, we need to get the value_hash_id of the thingo
  -- - maybe enqueue their parents
  -- - enqueue the self_ and value_ hashes
  -- - enqueue the namespace object, if present
  C chId ->
    isSyncedCausal chId >>= \case
      Just {} -> pure Sync.PreviouslyDone
      Nothing -> do
        result <- runValidateT @(Set Entity) @m @() do
          bhId <- runSrc $ Q.loadCausalValueHashId chId
          mayBoId <- runSrc . Q.maybeObjectIdForAnyHashId $ unBranchHashId bhId
          traverse_ syncLocalObjectId mayBoId

          parents' :: [CausalHashId] <- findParents' chId
          bhId' <- lift $ syncBranchHashId bhId
          chId' <- lift $ syncCausalHashId chId
          runDest do
            Q.saveCausal chId' bhId' parents'

        case result of
          Left deps -> pure . Sync.Missing $ toList deps
          Right () -> pure Sync.Done

  -- objects are the hairiest. obviously, if they
  -- exist, we're done; otherwise we do some fancy stuff
  O oId ->
    isSyncedObject oId >>= \case
      Just {} -> pure Sync.PreviouslyDone
      Nothing -> do
        (hId, objType, bytes) <- runSrc $ Q.loadObjectWithHashIdAndTypeById oId
        hId' <- syncHashLiteral hId
        result <- runValidateT @(Set Entity) @m @ObjectId case objType of
          OT.TermComponent -> do
            -- split up the localIds (parsed), term, and type blobs
            case flip runGetS bytes S.decomposeTermFormat of
              Left s -> throwError $ DecodeError ErrTermComponent bytes s
              Right
                ( TermFormat.SyncTerm
                    ( TermFormat.SyncLocallyIndexedComponent
                        (Vector.unzip -> (localIds, bytes))
                      )
                  ) -> do
                  -- iterate through the local ids looking for missing deps;
                  -- then either enqueue the missing deps, or proceed to move the object
                  when debug $ traceM $ "LocalIds for Source " ++ show oId ++ ": " ++ show localIds
                  localIds' <- traverse syncLocalIds localIds
                  when debug $ traceM $ "LocalIds for Dest: " ++ show localIds'
                  -- reassemble and save the reindexed term
                  let bytes' =
                        runPutS
                          . S.recomposeTermFormat
                          . TermFormat.SyncTerm
                          . TermFormat.SyncLocallyIndexedComponent
                          $ Vector.zip localIds' bytes
                  oId' <- runDest $ Q.saveObject hId' objType bytes'
                  lift do
                    -- copy reference-specific stuff
                    for_ [0 .. length localIds - 1] \(fromIntegral -> idx) -> do
                      let ref = Reference.Id oId idx
                          refH = Reference.Id hId idx
                          ref' = Reference.Id oId' idx
                      -- sync watch results
                      for_ [WK.TestWatch] \wk ->
                        syncWatch wk refH
                      syncDependenciesIndex ref ref'
                    syncTypeIndex oId oId'
                    syncTypeMentionsIndex oId oId'
                  pure oId'
          OT.DeclComponent -> do
            -- split up the localIds (parsed), decl blobs
            case flip runGetS bytes S.decomposeDeclFormat of
              Left s -> throwError $ DecodeError ErrDeclComponent bytes s
              Right
                ( DeclFormat.SyncDecl
                    ( DeclFormat.SyncLocallyIndexedComponent
                        (Vector.unzip -> (localIds, declBytes))
                      )
                  ) -> do
                  -- iterate through the local ids looking for missing deps;
                  -- then either enqueue the missing deps, or proceed to move the object
                  localIds' <- traverse syncLocalIds localIds
                  -- reassemble and save the reindexed term
                  let bytes' =
                        runPutS . S.recomposeDeclFormat
                          . DeclFormat.SyncDecl
                          . DeclFormat.SyncLocallyIndexedComponent
                          $ Vector.zip localIds' declBytes
                  oId' <- runDest $ Q.saveObject hId' objType bytes'
                  lift do
                    -- copy per-element-of-the-component stuff
                    for_ [0 .. length localIds - 1] \(fromIntegral -> idx) -> do
                      let ref = Reference.Id oId idx
                          ref' = Reference.Id oId' idx
                      syncDependenciesIndex ref ref'
                    syncTypeIndex oId oId'
                    syncTypeMentionsIndex oId oId'
                  pure oId'
          OT.Namespace -> case flip runGetS bytes S.decomposeBranchFormat of
            Right (BL.SyncFull ids body) -> do
              ids' <- syncBranchLocalIds ids
              let bytes' = runPutS $ S.recomposeBranchFormat (BL.SyncFull ids' body)
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            Right (BL.SyncDiff boId ids body) -> do
              boId' <- syncBranchObjectId boId
              ids' <- syncBranchLocalIds ids
              let bytes' = runPutS $ S.recomposeBranchFormat (BL.SyncDiff boId' ids' body)
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            Left s -> throwError $ DecodeError ErrBranchFormat bytes s
          OT.Patch -> case flip runGetS bytes S.decomposePatchFormat of
            Right (PL.SyncFull ids body) -> do
              ids' <- syncPatchLocalIds ids
              let bytes' = runPutS $ S.recomposePatchFormat (PL.SyncFull ids' body)
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            Right (PL.SyncDiff poId ids body) -> do
              poId' <- syncPatchObjectId poId
              ids' <- syncPatchLocalIds ids
              let bytes' = runPutS $ S.recomposePatchFormat (PL.SyncDiff poId' ids' body)
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            Left s -> throwError $ DecodeError ErrPatchFormat bytes s
        case result of
          Left deps -> pure . Sync.Missing $ toList deps
          Right oId' -> do
            syncSecondaryHashes oId oId'
            when debug $ traceM $ "Source " ++ show (hId, oId) ++ " becomes Dest " ++ show (hId', oId')
            Cache.insert oCache oId oId'
            pure Sync.Done
  W k r -> syncWatch k r
  where
    syncLocalObjectId :: ObjectId -> ValidateT (Set Entity) m ObjectId
    syncLocalObjectId oId =
      lift (isSyncedObject oId) >>= \case
        Just oId' -> pure oId'
        Nothing -> Validate.refute . Set.singleton $ O oId

    syncPatchObjectId :: PatchObjectId -> ValidateT (Set Entity) m PatchObjectId
    syncPatchObjectId = fmap PatchObjectId . syncLocalObjectId . unPatchObjectId

    syncBranchObjectId :: BranchObjectId -> ValidateT (Set Entity) m BranchObjectId
    syncBranchObjectId = fmap BranchObjectId . syncLocalObjectId . unBranchObjectId

    syncCausal :: CausalHashId -> ValidateT (Set Entity) m CausalHashId
    syncCausal chId =
      lift (isSyncedCausal chId) >>= \case
        Just chId' -> pure chId'
        Nothing -> Validate.refute . Set.singleton $ C chId

    syncDependenciesIndex :: Sqlite.Reference.Id -> Sqlite.Reference.Id -> m ()
    syncDependenciesIndex ref ref' = do
      deps <- runSrc (Q.getDependenciesForDependent ref)
      for_ deps \dep -> do
        dep' <- expectSyncedObjectReference dep
        runDest (Q.addToDependentsIndex dep' ref')

    syncLocalIds :: L.LocalIds -> ValidateT (Set Entity) m L.LocalIds
    syncLocalIds (L.LocalIds tIds oIds) = do
      oIds' <- traverse syncLocalObjectId oIds
      tIds' <- lift $ traverse syncTextLiteral tIds
      pure $ L.LocalIds tIds' oIds'

    syncPatchLocalIds :: PL.PatchLocalIds -> ValidateT (Set Entity) m PL.PatchLocalIds
    syncPatchLocalIds (PL.LocalIds tIds hIds oIds) = do
      oIds' <- traverse syncLocalObjectId oIds
      tIds' <- lift $ traverse syncTextLiteral tIds
      hIds' <- lift $ traverse syncHashLiteral hIds

      -- workaround for requiring components to compute component lengths for references.
      -- this line requires objects in the destination for any hashes referenced in the source,
      -- (making those objects dependencies of this patch).  See Sync21.filter{Term,Type}Edit
      traverse_ syncLocalObjectId =<< traverse (runSrc . Q.expectObjectIdForAnyHashId) hIds

      pure $ PL.LocalIds tIds' hIds' oIds'

    syncBranchLocalIds :: BL.BranchLocalIds -> ValidateT (Set Entity) m BL.BranchLocalIds
    syncBranchLocalIds (BL.LocalIds tIds oIds poIds chboIds) = do
      oIds' <- traverse syncLocalObjectId oIds
      poIds' <- traverse (fmap PatchObjectId . syncLocalObjectId . unPatchObjectId) poIds
      chboIds' <- traverse (bitraverse syncBranchObjectId syncCausal) chboIds
      tIds' <- lift $ traverse syncTextLiteral tIds
      pure $ BL.LocalIds tIds' oIds' poIds' chboIds'

    syncTypeIndex :: ObjectId -> ObjectId -> m ()
    syncTypeIndex oId oId' = do
      rows <- runSrc (Q.getTypeReferencesForComponent oId)
      -- defensively nubOrd to guard against syncing from codebases with duplicate rows in their type (mentions) indexes
      -- alternatively, we could put a unique constraint on the whole 6-tuple of the index tables, and optimistically
      -- insert with an `on conflict do nothing`.
      for_ (nubOrd rows) \row -> do
        row' <- syncTypeIndexRow oId' row
        runDest (uncurry Q.addToTypeIndex row')

    syncTypeMentionsIndex :: ObjectId -> ObjectId -> m ()
    syncTypeMentionsIndex oId oId' = do
      rows <- runSrc (Q.getTypeMentionsReferencesForComponent oId)
      -- see "defensively nubOrd..." comment above in `syncTypeIndex`
      for_ (nubOrd rows) \row -> do
        row' <- syncTypeIndexRow oId' row
        runDest (uncurry Q.addToTypeMentionsIndex row')

    syncTypeIndexRow ::
      ObjectId ->
      (Sqlite.Reference.ReferenceH, Sqlite.Referent.Id) ->
      m (Sqlite.Reference.ReferenceH, Sqlite.Referent.Id)
    syncTypeIndexRow oId' = bitraverse syncHashReference (pure . rewriteTypeIndexReferent oId')

    rewriteTypeIndexReferent :: ObjectId -> Sqlite.Referent.Id -> Sqlite.Referent.Id
    rewriteTypeIndexReferent oId' = bimap (const oId') (const oId')

    syncTextLiteral :: TextId -> m TextId
    syncTextLiteral = Cache.apply tCache \tId -> do
      t <- runSrc $ Q.loadTextById tId
      tId' <- runDest $ Q.saveText t
      when debug $ traceM $ "Source " ++ show tId ++ " is Dest " ++ show tId' ++ " (" ++ show t ++ ")"
      pure tId'

    syncHashLiteral :: HashId -> m HashId
    syncHashLiteral = Cache.apply hCache \hId -> do
      b32hex <- runSrc $ Q.loadHashById hId
      hId' <- runDest $ Q.saveHash b32hex
      when debug $ traceM $ "Source " ++ show hId ++ " is Dest " ++ show hId' ++ " (" ++ show b32hex ++ ")"
      pure hId'

    isSyncedObjectReference :: Sqlite.Reference -> m (Maybe Sqlite.Reference)
    isSyncedObjectReference = \case
      Reference.ReferenceBuiltin t ->
        Just . Reference.ReferenceBuiltin <$> syncTextLiteral t
      Reference.ReferenceDerived id ->
        fmap Reference.ReferenceDerived <$> isSyncedObjectReferenceId id

    isSyncedObjectReferenceId :: Sqlite.Reference.Id -> m (Maybe Sqlite.Reference.Id)
    isSyncedObjectReferenceId (Reference.Id oId idx) =
      isSyncedObject oId <&> fmap (\oId' -> Reference.Id oId' idx)

    -- Assert that a reference's component is already synced, and return the corresponding reference.
    expectSyncedObjectReference :: Sqlite.Reference -> m Sqlite.Reference
    expectSyncedObjectReference ref =
      isSyncedObjectReference ref <&> \case
        Nothing -> error (reportBug "E452280" ("unsynced object reference " ++ show ref))
        Just ref' -> ref'

    syncHashReference :: Sqlite.ReferenceH -> m Sqlite.ReferenceH
    syncHashReference = bitraverse syncTextLiteral syncHashLiteral

    syncCausalHashId :: CausalHashId -> m CausalHashId
    syncCausalHashId = fmap CausalHashId . syncHashLiteral . unCausalHashId

    syncBranchHashId :: BranchHashId -> m BranchHashId
    syncBranchHashId = fmap BranchHashId . syncHashLiteral . unBranchHashId

    findParents' :: CausalHashId -> ValidateT (Set Entity) m [CausalHashId]
    findParents' chId = do
      srcParents <- runSrc $ Q.loadCausalParents chId
      traverse syncCausal srcParents

    -- Sync any watches of the given kinds to the dest if and only if watches of those kinds
    -- exist in the src.
    syncWatch :: WK.WatchKind -> Sqlite.Reference.IdH -> m (TrySyncResult Entity)
    syncWatch wk r | debug && trace ("Sync22.syncWatch " ++ show wk ++ " " ++ show r) False = undefined
    syncWatch wk r = do
      runSrc (Q.loadWatch wk r) >>= \case
        Nothing -> pure Sync.Done
        Just blob -> do
          r' <- traverse syncHashLiteral r
          doneKinds <- runDest (Q.loadWatchKindsByReference r')
          if (elem wk doneKinds)
            then pure Sync.PreviouslyDone
            else do
              TL.SyncWatchResult li body <-
                either (throwError . DecodeError ErrWatchResult blob) pure $ runGetS S.decomposeWatchFormat blob
              li' <- bitraverse syncTextLiteral syncHashLiteral li
              when debug $ traceM $ "LocalIds for Source watch result " ++ show r ++ ": " ++ show li
              when debug $ traceM $ "LocalIds for Dest watch result " ++ show r' ++ ": " ++ show li'
              let blob' = runPutS $ S.recomposeWatchFormat (TL.SyncWatchResult li' body)
              runDest (Q.saveWatch wk r' blob')
              pure Sync.Done

    syncSecondaryHashes oId oId' =
      runSrc (Q.hashIdWithVersionForObject oId) >>= traverse_ (go oId')
      where
        go oId' (hId, hashVersion) = do
          hId' <- syncHashLiteral hId
          runDest $ Q.saveHashObject hId' oId' hashVersion

    isSyncedObject :: ObjectId -> m (Maybe ObjectId)
    isSyncedObject = Cache.applyDefined oCache \oId -> do
      hIds <- toList <$> runSrc (Q.hashIdsForObject oId)
      hIds' <- traverse syncHashLiteral hIds
      ( nubOrd . catMaybes
          <$> traverse (runDest . Q.maybeObjectIdForAnyHashId) hIds'
        )
        >>= \case
          [oId'] -> do
            when debug $ traceM $ "Source " ++ show oId ++ " is Dest " ++ show oId'
            pure $ Just oId'
          [] -> pure $ Nothing
          oIds' -> throwError (HashObjectCorrespondence oId hIds hIds' oIds')

    isSyncedCausal :: CausalHashId -> m (Maybe CausalHashId)
    isSyncedCausal = Cache.applyDefined cCache \chId -> do
      let hId = unCausalHashId chId
      hId' <- syncHashLiteral hId
      ifM
        (runDest $ Q.isCausalHash hId')
        (pure . Just $ CausalHashId hId')
        (pure Nothing)

runSrc,
  runDest ::
    (MonadError Error m, MonadReader Env m) =>
    ReaderT Connection (ExceptT Q.Integrity m) a ->
    m a
runSrc ma = Reader.reader srcDB >>= flip runDB ma
runDest ma = Reader.reader destDB >>= flip runDB ma

runDB ::
  MonadError Error m => Connection -> ReaderT Connection (ExceptT Q.Integrity m) a -> m a
runDB conn action =
  Except.runExceptT (Reader.runReaderT action conn) >>= \case
    Left e -> throwError (DbIntegrity e)
    Right a -> pure a

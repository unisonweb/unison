{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Sync22 where

import Control.Monad (filterM, join)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import qualified Control.Monad.Except as Except
import Control.Monad.Extra (ifM)
import Control.Monad.RWS (MonadIO, MonadReader, lift)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Validate (ValidateT, runValidateT)
import qualified Control.Monad.Validate as Validate
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bytes.Get (getWord8, runGetS)
import Data.Bytes.Put (putWord8, runPutS)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.List.Extra (nubOrd)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import Database.SQLite.Simple (Connection)
import qualified U.Codebase.Reference as Reference
import qualified U.Codebase.Sqlite.Branch.Format as BL
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.LocalIds as L
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Patch.Format as PL
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as Sqlite
import qualified U.Codebase.Sqlite.Reference as Sqlite.Reference
import qualified U.Codebase.Sqlite.Referent as Sqlite.Referent
import qualified U.Codebase.Sqlite.Serialization as S
import U.Codebase.Sync (Sync (Sync), TrySyncResult (Missing))
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import U.Util.Cache (Cache)
import qualified U.Util.Cache as Cache

data Entity = O ObjectId | C CausalHashId deriving (Eq, Ord, Show)

data DbTag = SrcDb | DestDb

data DecodeError
  = ErrTermComponent
  | ErrDeclComponent
  | -- | ErrTermFormat
    -- | ErrDeclFormat
    ErrBranchFormat
  | ErrPatchFormat
  | ErrBranchBody Word8
  | ErrPatchBody Word8
  | ErrWatchResult

type ErrString = String

data Error
  = DbIntegrity Q.Integrity
  | DecodeError DecodeError ByteString ErrString
  | -- | hashes corresponding to a single object in source codebase
    --  correspond to multiple objects in destination codebase
    HashObjectCorrespondence ObjectId [HashId] [ObjectId]

data Env = Env
  { srcDB :: Connection,
    destDB :: Connection,
    -- | there are three caches of this size
    idCacheSize :: Word
  }

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
  gc <- runSrc $ Q.getNurseryGeneration
  pure $ Sync roots (trySync tCache hCache oCache (succ gc))
  where
    roots = runSrc $ fmap (\h -> [C h]) Q.loadNamespaceRoot

trySync ::
  forall m.
  (MonadIO m, MonadError Error m, MonadReader Env m) =>
  Cache m TextId TextId ->
  Cache m HashId HashId ->
  Cache m ObjectId ObjectId ->
  Generation ->
  Entity ->
  m (TrySyncResult Entity)
trySync tCache hCache oCache gc = \case
  -- for causals, we need to get the value_hash_id of the thingo
  -- - maybe enqueue their parents
  -- - enqueue the self_ and value_ hashes
  -- - enqueue the namespace object, if present
  C chId -> do
    chId' <- syncCausalHash chId
    -- we're going to assume that if the dest has this in its
    -- causal table, then it's safe to short-circuit
    ifM
      (runDest $ Q.isCausalHash $ unCausalHashId chId')
      (pure Sync.PreviouslyDone)
      ( do
          bhId <- runSrc $ Q.loadCausalValueHashId chId
          bhId' <- syncBranchHashId bhId

          mayBoId <-
            runSrc . Q.maybeObjectIdForAnyHashId $
              unBranchHashId bhId
          mayBoId' <- join <$> traverse (isSyncedObject) mayBoId

          findMissingParents chId >>= \case
            [] ->
              -- if branch object is present at src and dest,
              -- or absent from both src and dest
              -- then we are done
              if isJust mayBoId == isJust mayBoId'
                then do
                  runDest $ Q.saveCausal chId' bhId'
                  pure Sync.Done
                else -- else it's present at src but not at dest.,
                -- so request it be copied, and revisit later
                  pure $ Missing [O $ fromJust mayBoId]
            missingParents ->
              -- if branch object is present at src and dest,
              -- or absent from both src and dest
              -- but there are parents missing,
              -- then request the parents be copied, and revisit later
              if isJust mayBoId == isJust mayBoId'
                then pure $ Missing missingParents
                else -- otherwise request the branch and the parents be copied
                  pure $ Missing $ (O $ fromJust mayBoId) : missingParents
      )
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
            -- note: this whole business with `fmt` is pretty weird, and will need to be
            -- revisited when there are more formats.
            -- (or maybe i'll learn something by implementing sync for patches and namespaces,
            -- which have two formats already)
            (fmt, unzip -> (localIds, bytes)) <-
              lift case flip runGetS bytes do
                tag <- getWord8
                component <- S.decomposeComponent
                pure (tag, component) of
                Right x -> pure x
                Left s -> throwError $ DecodeError ErrTermComponent bytes s
            -- iterate through the local ids looking for missing deps;
            -- then either enqueue the missing deps, or proceed to move the object
            localIds' <- traverse syncLocalIds localIds
            -- reassemble and save the reindexed term
            let bytes' =
                  runPutS $
                    putWord8 fmt >> S.recomposeComponent (zip localIds' bytes)
            oId' <- runDest $ Q.saveObject hId' objType bytes'
            -- copy reference-specific stuff
            lift $ for_ [0 .. length localIds - 1] \(fromIntegral -> idx) -> do
              -- sync watch results
              for_ [WK.RegularWatch, WK.TestWatch] \wk -> do
                let refH = Reference.Id hId idx
                    refH' = Reference.Id hId' idx
                runSrc (Q.loadWatch wk refH) >>= traverse_ \blob -> do
                  (L.LocalIds tIds hIds, termBytes) <-
                    case runGetS S.decomposeWatchResult blob of
                      Right x -> pure x
                      Left s -> throwError $ DecodeError ErrWatchResult blob s
                  tIds' <- traverse syncTextLiteral tIds
                  hIds' <- traverse syncHashLiteral hIds
                  let blob' = runPutS (S.recomposeWatchResult (L.LocalIds tIds' hIds', termBytes))
                  runDest (Q.saveWatch wk refH' blob')
              -- sync dependencies index
              let ref = Reference.Id oId idx
                  ref' = Reference.Id oId' idx
              let fromJust' = fromMaybe (error "missing objects should've been caught by `foldLocalIds` above")
              runSrc (Q.getDependenciesForDependent ref)
                >>= traverse (fmap fromJust' . isSyncedObjectReference)
                >>= runDest . traverse_ (flip Q.addToDependentsIndex ref')
              -- sync type index
              runSrc (Q.getTypeReferencesForComponent oId)
                >>= traverse (syncTypeIndexRow oId')
                >>= traverse_ (runDest . uncurry Q.addToTypeIndex)
              -- sync type mentions index
              runSrc (Q.getTypeMentionsReferencesForComponent oId)
                >>= traverse (syncTypeIndexRow oId')
                >>= traverse_ (runDest . uncurry Q.addToTypeMentionsIndex)
            pure oId'
          OT.DeclComponent -> do
            -- split up the localIds (parsed), decl blobs
            (fmt, unzip -> (localIds, declBytes)) <-
              case flip runGetS bytes do
                tag <- getWord8
                component <- S.decomposeComponent
                pure (tag, component) of
                Right x -> pure x
                Left s -> throwError $ DecodeError ErrDeclComponent bytes s
            -- iterate through the local ids looking for missing deps;
            -- then either enqueue the missing deps, or proceed to move the object
            localIds' <- traverse syncLocalIds localIds
            -- reassemble and save the reindexed term
            let bytes' =
                  runPutS $
                    putWord8 fmt
                      >> S.recomposeComponent (zip localIds' declBytes)
            oId' <- runDest $ Q.saveObject hId' objType bytes'
            -- copy per-element-of-the-component stuff
            lift $ for_ [0 .. length localIds - 1] \(fromIntegral -> idx) -> do
              -- sync dependencies index
              let ref = Reference.Id oId idx
                  ref' = Reference.Id oId' idx
              let fromJust' = fromMaybe (error "missing objects should've been caught by `foldLocalIds` above")
              runSrc (Q.getDependenciesForDependent ref)
                >>= traverse (fmap fromJust' . isSyncedObjectReference)
                >>= runDest . traverse_ (flip Q.addToDependentsIndex ref')
              -- sync type index
              runSrc (Q.getTypeReferencesForComponent oId)
                >>= traverse (syncTypeIndexRow oId')
                >>= traverse_ (runDest . uncurry Q.addToTypeIndex)
              -- sync type mentions index
              runSrc (Q.getTypeMentionsReferencesForComponent oId)
                >>= traverse (syncTypeIndexRow oId')
                >>= traverse_ (runDest . uncurry Q.addToTypeMentionsIndex)
            pure oId'
          OT.Namespace -> case BS.uncons bytes of
            -- full branch case
            Just (fmt@0, bytes) -> do
              (ids, body) <- case flip runGetS bytes S.decomposeBranchFull of
                Right x -> pure x
                Left s -> throwError $ DecodeError (ErrBranchBody fmt) bytes s
              ids' <- syncBranchLocalIds ids
              let bytes' = runPutS $ putWord8 0 *> S.recomposeBranchFull ids' body
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            -- branch diff case
            Just (fmt@1, bytes) -> do
              (boId, ids, body) <- case flip runGetS bytes S.decomposeBranchDiff of
                Right x -> pure x
                Left s -> throwError $ DecodeError (ErrBranchBody fmt) bytes s
              boId' <- syncLocalObjectId boId
              ids' <- syncBranchLocalIds ids
              let bytes' = runPutS $ putWord8 1 *> S.recomposeBranchDiff boId' ids' body
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            -- unrecognized tag case
            Just (tag, _) -> throwError $ DecodeError ErrBranchFormat bytes ("unrecognized branch format tag: " ++ show tag)
            Nothing -> throwError $ DecodeError ErrBranchFormat bytes "zero-byte branch object"
          OT.Patch -> case BS.uncons bytes of
            -- full branch case
            Just (fmt@0, bytes) -> do
              (ids, body) <- case flip runGetS bytes S.decomposePatchFull of
                Right x -> pure x
                Left s -> throwError $ DecodeError (ErrPatchBody fmt) bytes s
              ids' <- syncPatchLocalIds ids
              let bytes' = runPutS $ putWord8 0 *> S.recomposePatchFull ids' body
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            -- branch diff case
            Just (fmt@1, bytes) -> do
              (poId, ids, body) <- case flip runGetS bytes S.decomposePatchDiff of
                Right x -> pure x
                Left s -> throwError $ DecodeError (ErrPatchBody fmt) bytes s
              poId' <- syncLocalObjectId poId
              ids' <- syncPatchLocalIds ids
              let bytes' = runPutS $ putWord8 1 *> S.recomposePatchDiff poId' ids' body
              oId' <- runDest $ Q.saveObject hId' objType bytes'
              pure oId'
            -- error cases
            Just (tag, _) -> throwError $ DecodeError ErrBranchFormat bytes ("unrecognized patch format tag: " ++ show tag)
            Nothing -> throwError $ DecodeError ErrPatchFormat bytes "zero-byte patch object"
        case result of
          Left deps -> pure . Sync.Missing $ toList deps
          Right oId' -> do
            syncSecondaryHashes oId oId'
            Cache.insert oCache oId oId'
            pure Sync.Done
  where
    syncLocalObjectId :: ObjectId -> ValidateT (Set Entity) m ObjectId
    syncLocalObjectId oId =
      lift (isSyncedObject oId) >>= \case
        Just oId' -> pure oId'
        Nothing -> Validate.refute . Set.singleton $ O oId

    syncBranchObjectId :: BranchObjectId -> ValidateT (Set Entity) m BranchObjectId
    syncBranchObjectId = fmap BranchObjectId . syncLocalObjectId . unBranchObjectId

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
      pure $ PL.LocalIds tIds' hIds' oIds'

    syncBranchLocalIds :: BL.BranchLocalIds -> ValidateT (Set Entity) m BL.BranchLocalIds
    syncBranchLocalIds (BL.LocalIds tIds oIds poIds chboIds) = do
      oIds' <- traverse syncLocalObjectId oIds
      poIds' <- traverse (fmap PatchObjectId . syncLocalObjectId . unPatchObjectId) poIds
      chboIds' <- traverse (bitraverse syncBranchObjectId (lift . syncCausalHash)) chboIds
      tIds' <- lift $ traverse syncTextLiteral tIds
      pure $ BL.LocalIds tIds' oIds' poIds' chboIds'

    syncTypeIndexRow oId' = bitraverse syncHashReference (pure . rewriteTypeIndexReferent oId')
    rewriteTypeIndexReferent :: ObjectId -> Sqlite.Referent.Id -> Sqlite.Referent.Id
    rewriteTypeIndexReferent oId' = bimap (const oId') (const oId')

    syncTextLiteral :: TextId -> m TextId
    syncTextLiteral = Cache.apply tCache \tId -> do
      t <- runSrc $ Q.loadTextById tId
      runDest $ Q.saveText t

    syncHashLiteral :: HashId -> m HashId
    syncHashLiteral = Cache.apply hCache \hId -> do
      b32hex <- runSrc $ Q.loadHashById hId
      runDest $ Q.saveHash b32hex

    isSyncedObjectReference :: Sqlite.Reference -> m (Maybe Sqlite.Reference)
    isSyncedObjectReference = \case
      Reference.ReferenceBuiltin t ->
        Just . Reference.ReferenceBuiltin <$> syncTextLiteral t
      Reference.ReferenceDerived id ->
        fmap Reference.ReferenceDerived <$> isSyncedObjectReferenceId id

    isSyncedObjectReferenceId :: Sqlite.Reference.Id -> m (Maybe Sqlite.Reference.Id)
    isSyncedObjectReferenceId (Reference.Id oId idx) =
      isSyncedObject oId <&> fmap (\oId' -> Reference.Id oId' idx)

    syncHashReference :: Sqlite.ReferenceH -> m Sqlite.ReferenceH
    syncHashReference = bitraverse syncTextLiteral syncHashLiteral

    syncCausalHash :: CausalHashId -> m CausalHashId
    syncCausalHash = fmap CausalHashId . syncHashLiteral . unCausalHashId

    syncBranchHashId :: BranchHashId -> m BranchHashId
    syncBranchHashId = fmap BranchHashId . syncHashLiteral . unBranchHashId

    findMissingParents :: CausalHashId -> m [Entity]
    findMissingParents chId = do
      runSrc (Q.loadCausalParents chId) >>= filterM isMissing <&> fmap C
      where
        isMissing p =
          syncCausalHash p
            >>= runDest . Q.isCausalHash . unCausalHashId

    syncSecondaryHashes oId oId' =
      runSrc (Q.hashIdWithVersionForObject oId) >>= traverse_ (go oId')
      where
        go oId' (hId, hashVersion) = do
          hId' <- syncHashLiteral hId
          runDest $ Q.saveHashObject hId' oId' hashVersion

    isSyncedObject :: ObjectId -> m (Maybe ObjectId)
    isSyncedObject = Cache.applyDefined oCache \oId -> do
      hIds <- toList <$> runSrc (Q.hashIdsForObject oId)
      ( nubOrd . catMaybes
          <$> traverse (runDest . Q.maybeObjectIdForAnyHashId) hIds
        )
        >>= \case
          [oId'] -> pure $ Just oId'
          [] -> pure $ Nothing
          oIds' -> throwError (HashObjectCorrespondence oId hIds oIds')

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

-- syncs coming from git:
--  - pull a specified remote causal (Maybe CausalHash) into the local database
--  - and then maybe do some stuff
-- syncs coming from
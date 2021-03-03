{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Sync22 where

import Control.Monad (filterM, foldM, join)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Extra (ifM)
import Control.Monad.RWS (MonadIO, MonadReader)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import qualified Control.Monad.Writer as Writer
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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Data.Word (Word8)
import Database.SQLite.Simple (Connection)
import qualified U.Codebase.Reference as Reference
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.LocalIds as L
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Patch.Format as PL
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as Sqlite
import qualified U.Codebase.Sqlite.Reference as Sqlite.Reference
import qualified U.Codebase.Sqlite.Referent as Sqlite.Referent
import qualified U.Codebase.Sqlite.Serialization as S
import U.Codebase.Sync
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import U.Util.Cache (Cache)
import qualified U.Util.Cache as Cache
import qualified U.Util.Serialization as S

data Entity = O ObjectId | C CausalHashId

data DbTag = SrcDb | DestDb

data DecodeError
  = ErrTermComponent
  | ErrDeclComponent
  | -- | ErrTermFormat
    -- | ErrDeclFormat
    ErrBranchFormat
  | ErrPatchFormat
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
    destDB :: Connection
  }

-- data Mappings

-- We load an object from the source; it has a bunch of dependencies.
-- Some of these dependencies exist at the defination, some don't.
-- For the ones that do, look up their ids, and update the thingie as you write it
-- For the ones that don't, copy them (then you will know their ids), and update the thingie.
-- If you want, you can try to cache that lookup.

-- sync22 ::
--   ( MonadIO m,
--     MonadError Error m,
--     MonadReader TwoConnections m
--   ) =>
--   Sync m Entity
-- sync22 = Sync roots trySync
--   where
--     roots = runSrc $ fmap (\h -> [C h]) Q.loadNamespaceRoot

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
        result <- case objType of
          OT.TermComponent -> do
            -- split up the localIds (parsed), term, and type blobs
            -- note: this whole business with `fmt` is pretty weird, and will need to be
            -- revisited when there are more formats.
            -- (or maybe i'll learn something by implementing sync for patches and namespaces,
            -- which have two formats already)
            (fmt, unzip3 -> (localIds, termBytes, typeBytes)) <-
              case flip runGetS bytes do
                tag <- getWord8
                component <- S.decomposeTermComponent
                pure (tag, component) of
                Right x -> pure x
                Left s -> throwError $ DecodeError ErrTermComponent bytes s
            -- iterate through the local ids looking for missing deps;
            -- then either enqueue the missing deps, or proceed to move the object
            foldM foldLocalIds (Right mempty) localIds >>= \case
              Left missingDeps -> pure $ Left missingDeps
              Right (toList -> localIds') -> do
                -- reassemble and save the reindexed term
                let bytes' =
                      runPutS $
                        putWord8 fmt
                          >> S.recomposeTermComponent (zip3 localIds' termBytes typeBytes)
                oId' <- runDest $ Q.saveObject hId' objType bytes'
                -- copy reference-specific stuff
                for_ [0 .. length localIds - 1] \(fromIntegral -> idx) -> do
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
                pure $ Right oId'
          OT.DeclComponent -> do
            -- split up the localIds (parsed), decl blobs
            (fmt, unzip -> (localIds, declBytes)) <-
              case flip runGetS bytes do
                tag <- getWord8
                component <- S.decomposeDeclComponent
                pure (tag, component) of
                Right x -> pure x
                Left s -> throwError $ DecodeError ErrDeclComponent bytes s
            -- iterate through the local ids looking for missing deps;
            -- then either enqueue the missing deps, or proceed to move the object
            foldM foldLocalIds (Right mempty) localIds >>= \case
              Left missingDeps -> pure $ Left missingDeps
              Right (toList -> localIds') -> do
                -- reassemble and save the reindexed term
                let bytes' =
                      runPutS $
                        putWord8 fmt
                          >> S.recomposeDeclComponent (zip localIds' declBytes)
                oId' <- runDest $ Q.saveObject hId' objType bytes'
                -- copy per-element-of-the-component stuff
                for_ [0 .. length localIds - 1] \(fromIntegral -> idx) -> do
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
                pure $ Right oId'
          OT.Namespace -> case BS.uncons bytes of
            Just (0, bytes) -> error "todo"
            Just (1, bytes) -> error "todo"
            Just (tag, _) -> throwError $ DecodeError ErrBranchFormat bytes ("unrecognized branch format tag: " ++ show tag)
            Nothing -> throwError $ DecodeError ErrBranchFormat bytes "zero-byte branch object"
          OT.Patch -> case BS.uncons bytes of
            Just (fmt@0, bytes) -> do
              (ids, blob) <- case flip runGetS bytes do
                ids <- S.getPatchLocalIds
                blob <- S.getFramedByteString
                pure (ids, blob) of
                Right x -> pure x
                Left s -> throwError $ DecodeError (ErrPatchBody 0) bytes s
              syncPatchLocalIds ids >>= \case
                Left missingDeps -> pure $ Left missingDeps
                Right ids' -> do
                  let bytes' = runPutS do
                        putWord8 fmt
                        S.putPatchLocalIds ids'
                        S.putFramedByteString blob
                  oId' <- runDest $ Q.saveObject hId' objType bytes'
                  pure $ Right oId'
            Just (fmt@1, bytes) -> do
              (poId, ids, blob) <- case flip runGetS bytes do
                poId <- S.getVarInt
                ids <- S.getPatchLocalIds
                blob <- S.getFramedByteString
                pure (poId, ids, blob) of
                Right x -> pure x
                Left s -> throwError $ DecodeError (ErrPatchBody 0) bytes s
              mayPoId' <- isSyncedObject poId
              eitherIds' <- syncPatchLocalIds ids
              case (mayPoId', eitherIds') of
                (Nothing, Left missingDeps) -> pure $ Left (O poId Seq.<| missingDeps)
                (Nothing, Right {}) -> pure $ Left (Seq.singleton (O poId))
                (Just {}, Left missingDeps) -> pure $ Left missingDeps
                (Just poId', Right ids') -> do
                  let bytes' = runPutS do
                        putWord8 fmt
                        S.putVarInt poId'
                        S.putPatchLocalIds ids'
                        S.putFramedByteString blob
                  oId' <- runDest $ Q.saveObject hId' objType bytes'
                  pure $ Right oId'
            Just (tag, _) -> throwError $ DecodeError ErrBranchFormat bytes ("unrecognized patch format tag: " ++ show tag)
            Nothing -> throwError $ DecodeError ErrPatchFormat bytes "zero-byte patch object"
        case result of
          Left deps -> pure . Sync.Missing $ toList deps
          Right oId' -> do
            syncSecondaryHashes oId oId'
            Cache.insert oCache oId oId'
            pure Sync.Done
  where
    foldLocalIds :: Either (Seq Entity) (Seq L.LocalIds) -> L.LocalIds -> m (Either (Seq Entity) (Seq L.LocalIds))
    foldLocalIds (Left missing) (L.LocalIds _tIds oIds) =
      syncLocalObjectIds oIds <&> \case
        Left missing2 -> Left (missing <> missing2)
        Right _oIds' -> Left missing
    foldLocalIds (Right localIdss') (L.LocalIds tIds oIds) =
      syncLocalObjectIds oIds >>= \case
        Left missing -> pure $ Left missing
        Right oIds' -> do
          tIds' <- traverse syncTextLiteral tIds
          pure $ Right (localIdss' Seq.|> L.LocalIds tIds' oIds')

    -- I want to collect all the failures, rather than short-circuiting after the first
    syncLocalObjectIds :: Traversable t => t ObjectId -> m (Either (Seq Entity) (t ObjectId))
    syncLocalObjectIds oIds = do
      (mayOIds', missing) <- Writer.runWriterT do
        for oIds \oId ->
          lift (isSyncedObject oId) >>= \case
            Just oId' -> pure oId'
            Nothing -> do
              Writer.tell . Seq.singleton $ O oId
              pure $ error "Arya didn't think this would get eval'ed."
      if null missing
        then pure $ Right mayOIds'
        else pure $ Left missing


    syncPatchLocalIds :: PL.PatchLocalIds -> m (Either (Seq Entity) PL.PatchLocalIds)
    syncPatchLocalIds (PL.LocalIds tIds hIds oIds) = runExceptT do
      oIds' <- ExceptT $ syncLocalObjectIds oIds
      tIds' <- lift $ traverse syncTextLiteral tIds
      hIds' <- lift $ traverse syncHashLiteral hIds
      pure $ PL.LocalIds tIds' hIds' oIds'

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
      runSrc (Q.loadCausalParents chId)
        >>= filterM isMissing
        <&> fmap C
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

-- syncCausal chId = do
--   value

-- Q: Do we want to cache corresponding ID mappings?
-- A: Yes, but not yet

runSrc ::
  (MonadError Error m, MonadReader Env m) =>
  ReaderT Connection (ExceptT Q.Integrity m) a ->
  m a
runSrc = error "todo" -- withExceptT SrcDB . (reader fst >>=)

runDest ::
  (MonadError Error m, MonadReader Env m) =>
  ReaderT Connection (ExceptT Q.Integrity m) a ->
  m a
runDest = error "todo" -- withExceptT SrcDB . (reader fst >>=)

-- applyDefined

-- syncs coming from git:
--  - pull a specified remote causal (Maybe CausalHash) into the local database
--  - and then maybe do some stuff
-- syncs coming from
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Product
import qualified Data.Map as Map
import Data.Semigroup
import qualified Data.Set as Set
import Data.Set.Lens (setOf)
import Data.String.Here.Uninterpolated (here)
import qualified U.Codebase.HashTags as H
import qualified U.Codebase.Sqlite.Branch.Format as S.BranchFormat
import qualified U.Codebase.Sqlite.Branch.Full as DBBranch
import qualified U.Codebase.Sqlite.DbId as DB
import qualified U.Codebase.Sqlite.LocalizeObject as S.LocalizeObject
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Serialization as S
import qualified U.Codebase.Sync as Sync
import qualified U.Util.Serialization as S
import Unison.Codebase.SqliteCodebase.Migrations.Helpers (abortMigration)
import qualified Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers as Helpers
import qualified Unison.Debug as Debug
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Prelude hiding (log)

data MigrationState = MigrationState
  { -- A mapping from a causal hash to the _corrected_ and _canonicalized_ branch hash and
    -- object.
    _canonicalBranchForCausalHashId :: Map DB.CausalHashId (DB.BranchHashId, DB.BranchObjectId),
    -- A mapping of branch hashes which were found to be correct and don't need to be
    -- re-hashed/re-canonicalized, it allows us to skip some redundant work.
    _validBranchHashIds :: Map DB.BranchHashId DB.BranchObjectId,
    _numMigrated :: Int
  }
  deriving (Generic)

canonicalBranchForCausalHashId :: Lens' MigrationState (Map DB.CausalHashId (DB.BranchHashId, DB.BranchObjectId))
canonicalBranchForCausalHashId =
  field @"_canonicalBranchForCausalHashId"

validBranchHashIds :: Lens' MigrationState (Map DB.BranchHashId DB.BranchObjectId)
validBranchHashIds =
  field @"_validBranchHashIds"

numMigrated :: Lens' MigrationState Int
numMigrated =
  field @"_numMigrated"

-- | There was a bug in previous versions of UCM which incorrectly 'converted' branch hashes.
-- In previous versions, every namespace hash was assigned its causal hash instead.
-- This remained undetected because there was never a need for this hash to be verifiable,
-- and the hashes were still unique because the namespace hash was PART of the causal hash.
--
-- However, with the advent of Share and Sync, we now want to correctly verify these namespace
-- hashes. Also, correcting this may reduce codebase size slightly since it's possible some
-- identical namespace objects were given differing hashes if the same namespace was used in
-- different causals.
--
-- This migration fixes the issue by re-hashing all namespace objects, luckily this doesn't
-- affect any causal hashes, and there are actually no references to namespace objects in
-- other blobs, so we don't have any 'cleanup' to do after fixing a namespace hash.
migrateSchema3To4 :: Sqlite.Transaction ()
migrateSchema3To4 = do
  Q.expectSchemaVersion 3
  rootCausalHashId <- Q.expectNamespaceRoot
  totalCausals <- causalCount
  migrationState <- flip execStateT (MigrationState mempty mempty 0) $ Sync.sync migrationSync (migrationProgress totalCausals) [rootCausalHashId]
  let MigrationState {_canonicalBranchForCausalHashId = mapping} = migrationState
  let reachableCausalHashes = Map.keysSet mapping
  let reachableBranchObjIds = setOf (traversed . _2) mapping
  log $ "🛠  Cleaning up unreachable branches and causals..."
  dropUnreachableCausalsAndBranches reachableCausalHashes reachableBranchObjIds
  do
    assertMigrationSuccess
  Q.setSchemaVersion 4
  where
    causalCount :: Sqlite.Transaction Int
    causalCount = do
      Sqlite.queryOneCol_
        [here|
          SELECT count(*) FROM causal;
          |]

    assertMigrationSuccess :: Sqlite.Transaction ()
    assertMigrationSuccess = do
      badNamespaces <- Sqlite.queryListRow_ @(DB.BranchObjectId, DB.CausalHashId) findNamespacesWithCausalHashesSql
      when (not . null $ badNamespaces) $ abortMigration "Uh-oh, still found some causals with incorrect namespace hashes after migratiing."

migrationProgress :: Int -> Sync.Progress (StateT MigrationState Sqlite.Transaction) DB.CausalHashId
migrationProgress totalCausals =
  Sync.Progress {Sync.need, Sync.done, Sync.error, Sync.allDone}
  where
    need e = lift $ debugLog $ "Need " <> show e
    done _ =
      do
        numDone <- numMigrated <+= 1
        lift $ Sqlite.unsafeIO $ putStr $ "\r🏗  " <> show numDone <> " / ~" <> show totalCausals <> " entities migrated. 🚧"
    error e = lift . log $ "Error " <> show e
    allDone = lift . Sqlite.unsafeIO . putStrLn $ "\nFinished."

migrationSync :: Sync.Sync (StateT MigrationState Sqlite.Transaction) DB.CausalHashId
migrationSync =
  Sync.Sync \e -> do
    (runExceptT $ migrateCausal e) >>= \case
      Left syncResult -> pure syncResult
      Right _ -> pure Sync.Done

liftT :: Sqlite.Transaction a -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) a
liftT = lift . lift

dropUnreachableCausalsAndBranches :: Set DB.CausalHashId -> Set DB.BranchObjectId -> Sqlite.Transaction ()
dropUnreachableCausalsAndBranches reachableCausals reachableBranchObjs = do
  createReachabilityTables
  Sqlite.executeMany insertReachableCausalSql (Sqlite.Only <$> Set.toList reachableCausals)
  Sqlite.executeMany insertReachableBranchObjectSql (Sqlite.Only <$> Set.toList reachableBranchObjs)
  Sqlite.execute_ deleteUnreachableHashObjects
  Sqlite.execute_ deleteUnreachableBranchObjects
  Sqlite.execute_ deleteUnreachableCausalParents
  Sqlite.execute_ deleteUnreachableCausals
  where
    deleteUnreachableHashObjects =
      [here|
      DELETE FROM hash_object AS ho
        WHERE NOT EXISTS (SELECT 1 FROM reachable_branch_objects AS ro WHERE ho.object_id = ro.object_id)
      |]
    deleteUnreachableBranchObjects =
      [here|
      DELETE FROM object AS o
        WHERE
          o.type_id = 2 -- Filter for only branches
          AND NOT EXISTS (SELECT 1 FROM reachable_branch_objects AS ro WHERE o.id = ro.object_id)
      |]
    deleteUnreachableCausals =
      [here|
      DELETE FROM causal AS c
        WHERE NOT EXISTS (SELECT 1 FROM reachable_causals AS rc WHERE c.self_hash_id = rc.self_hash_id)
      |]
    deleteUnreachableCausalParents =
      [here|
      DELETE FROM causal_parent AS cp
        WHERE
          NOT EXISTS (SELECT 1 FROM reachable_causals AS rc WHERE cp.causal_id = rc.self_hash_id)
          OR NOT EXISTS (SELECT 1 FROM reachable_causals AS rc WHERE cp.parent_id = rc.self_hash_id)
      |]
    insertReachableCausalSql =
      [here|
      INSERT INTO reachable_causals (self_hash_id) VALUES (?)
        ON CONFLICT DO NOTHING
      |]
    insertReachableBranchObjectSql =
      [here|
      INSERT INTO reachable_branch_objects (object_id) VALUES (?)
        ON CONFLICT DO NOTHING
      |]
    createReachabilityTables = do
      Sqlite.execute_
        [here|
           CREATE TEMP TABLE IF NOT EXISTS reachable_branch_objects (
            object_id INTEGER PRIMARY KEY NOT NULL
           )
          |]
      Sqlite.execute_
        [here|
           CREATE TEMP TABLE IF NOT EXISTS reachable_causals (
            self_hash_id INTEGER PRIMARY KEY NOT NULL
           )
          |]

migrateCausal :: DB.CausalHashId -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) ()
migrateCausal causalHashId = do
  preuse (canonicalBranchForCausalHashId . ix causalHashId) >>= \case
    Just _ -> throwError Sync.PreviouslyDone
    Nothing -> do
      causalParents <- liftT $ Q.loadCausalParents causalHashId
      unmigratedParents <- flip filterM causalParents $ \parentHashId -> (uses canonicalBranchForCausalHashId (Map.notMember parentHashId))
      when (not . null $ unmigratedParents) $ throwError (Sync.Missing unmigratedParents)
      valueHashId <- liftT $ Q.expectCausalValueHashId causalHashId
      preuse (validBranchHashIds . ix valueHashId) >>= \case
        Nothing -> pure ()
        Just objId -> do
          canonicalBranchForCausalHashId . at causalHashId ?= (valueHashId, objId)
          throwError Sync.Done
      liftT (Q.loadBranchObjectIdByCausalHashId causalHashId) >>= \case
        Nothing -> do
          liftT . abortMigration $ "Missing object for child branch of causal: " <> show causalHashId
        Just branchObjId -> do
          rehashAndCanonicalizeNamespace causalHashId valueHashId branchObjId

rehashAndCanonicalizeNamespace :: DB.CausalHashId -> DB.BranchHashId -> DB.BranchObjectId -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) ()
rehashAndCanonicalizeNamespace causalHashId possiblyIncorrectNamespaceHashId objId = do
  dbBranch <- liftT $ Ops.expectDbBranch objId
  canonicalBranchForCausalMap <- use canonicalBranchForCausalHashId
  -- remap all of the object ID's of the child branches to the correct and canonical objects,
  -- get a list of any unmigrated children, and also track whether any re-mappings actually
  -- occurred, so we don't do extra work when nothing changed.
  let ((unmigratedChildren, Any changes), remappedBranch) =
        dbBranch & DBBranch.childrenHashes_ %%~ \(ids@(_childBranchObjId, childCausalHashId)) -> do
          case Map.lookup childCausalHashId canonicalBranchForCausalMap of
            Nothing -> (([childCausalHashId], Any False), ids)
            Just (_, canonicalObjId) -> (([], Any True), (canonicalObjId, childCausalHashId))
  when (not . null $ unmigratedChildren) $ throwError (Sync.Missing unmigratedChildren)
  when changes $ do
    liftT $ replaceBranch objId remappedBranch
  newBranchHash <- liftT $ Helpers.dbBranchHash remappedBranch
  liftT . debugLog $ "New branch hash: " <> show newBranchHash
  correctNamespaceHashId <- liftT $ Q.saveBranchHash (H.BranchHash newBranchHash)
  -- Only do the extra work if the namespace hash was previously incorrect.
  when (correctNamespaceHashId == possiblyIncorrectNamespaceHashId) $ do
    -- This mapping hasn't changed, but this marks the causal as migrated.
    canonicalBranchForCausalHashId . at causalHashId ?= (correctNamespaceHashId, objId)
    validBranchHashIds . at possiblyIncorrectNamespaceHashId ?= objId
    throwError Sync.Done
  -- Update the value_hash_id on the causal to the correct hash for the branch
  liftT $ Sqlite.execute updateCausalValueHash (correctNamespaceHashId, possiblyIncorrectNamespaceHashId)
  -- It's possible that an object already exists for this new hash
  mayCanonical <- getCanonicalObjectForHash correctNamespaceHashId
  liftT . debugLog $ "(objId, Canonical object ID):" <> show (objId, mayCanonical)
  liftT . debugLog $ "Updating causal value hash (from, to)" <> show (possiblyIncorrectNamespaceHashId, correctNamespaceHashId)
  canonicalObjId <- case mayCanonical of
    -- If there's an existing canonical object, record the mapping from this object id to
    -- that one.
    Just canonicalObjectId
      | canonicalObjectId /= objId -> do
        -- Found an existing object with this hash, so the current object is a duplicate and
        -- needs to be deleted.
        liftT . debugLog $ "Mapping objID: " <> show objId <> " to canonical: " <> show canonicalObjectId
        liftT . debugLog $ "Unilaterally deleting: " <> show objId
        -- Remove possible foreign-key references before deleting the objects themselves
        liftT $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objId)
        liftT $ Sqlite.execute deleteObjectById (Sqlite.Only objId)
        pure canonicalObjectId
      | otherwise -> do
        error $ "Corrected hash for bad namespace is somehow still mapped to the same causal hash. This shouldn't happen.: " <> show (objId, canonicalObjectId)
    Nothing -> do
      -- There's no existing canonical object, this object BECOMES the canonical one by
      -- reassigning its primary hash.
      liftT . debugLog $ "Updating in place: " <> show objId
      liftT $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objId)
      liftT $ Sqlite.execute updateHashIdForObject (correctNamespaceHashId, objId)
      liftT $ Q.saveHashObject (DB.unBranchHashId correctNamespaceHashId) (DB.unBranchObjectId objId) 2
      pure objId
  -- Save the canonical branch info for the causal for use in remappings.
  canonicalBranchForCausalHashId . at causalHashId ?= (correctNamespaceHashId, canonicalObjId)
  where
    updateCausalValueHash :: Sqlite.Sql
    updateCausalValueHash =
      [here|
            UPDATE causal
              SET value_hash_id = ?
              WHERE value_hash_id = ?
            |]

    getCanonicalObjectForHash ::
      DB.BranchHashId ->
      ExceptT
        (Sync.TrySyncResult DB.CausalHashId)
        (StateT MigrationState Sqlite.Transaction)
        (Maybe DB.BranchObjectId)
    getCanonicalObjectForHash namespaceHashId =
      liftT $ Sqlite.queryMaybeCol sql (Sqlite.Only $ DB.unBranchHashId namespaceHashId)
      where
        sql =
          [here|
            SELECT id
              FROM object
              WHERE primary_hash_id = ?
            |]

    updateHashIdForObject :: Sqlite.Sql
    updateHashIdForObject =
      [here|
          UPDATE object
            SET primary_hash_id = ?
            WHERE id = ?
          |]

    -- Replace the bytes payload of a given branch in-place.
    -- This does NOT update the hash of the object.
    replaceBranch :: DB.BranchObjectId -> DBBranch.DbBranch -> Sqlite.Transaction ()
    replaceBranch objId branch = do
      let (localBranchIds, localBranch) = S.LocalizeObject.localizeBranch branch
      let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full localBranchIds localBranch
      Sqlite.execute sql (bytes, objId)
      where
        sql =
          [here|
        UPDATE object
        SET bytes = ?
        WHERE id = ?
      |]

    deleteHashObjectsByObjectId :: Sqlite.Sql
    deleteHashObjectsByObjectId =
      [here|
          DELETE FROM hash_object
            WHERE object_id = ?
    |]

    deleteObjectById :: Sqlite.Sql
    deleteObjectById =
      [here|
          DELETE FROM object
            WHERE id = ?
          |]

log :: String -> Sqlite.Transaction ()
log = Sqlite.unsafeIO . putStrLn

debugLog :: String -> Sqlite.Transaction ()
debugLog = Debug.whenDebug Debug.Migration . Sqlite.unsafeIO . putStrLn

findNamespacesWithCausalHashesSql :: Sqlite.Sql
findNamespacesWithCausalHashesSql =
  [here|
        -- Find all namespace object IDs which actually have causal hashes instead of
        -- namespace hashes as their primary hash.
        SELECT DISTINCT o.id, o.primary_hash_id
          FROM causal c
          LEFT JOIN object o ON o.primary_hash_id = c.self_hash_id
          WHERE self_hash_id = self_hash_id
                AND type_id = 2 -- filter for namespaces just to be safe
        |]

-- fixBadNamespaceHashes :: Sqlite.Transaction ()
-- fixBadNamespaceHashes = do
--   badNamespaces <- Sqlite.queryListRow_ findNamespacesWithCausalHashesSql
--   let numToMigrate = (length badNamespaces)
--   -- Rehash all bad namespaces and return a re-mapping of object IDs to the canonical object for their hash.
--   BadNamespaceInfo objectIdRemapping remainingObjects <- flip execStateT (BadNamespaceInfo mempty mempty) $ do
--     ifor_ badNamespaces \i (objectId, causalHashId) -> do
--       rehashNamespace objectId causalHashId
--       lift $ Sqlite.unsafeIO . putStr $ "\r  🏗  " <> show (i + 1) <> " / " <> show numToMigrate <> " entities migrated. 🚧"
--   debugLog $ "object ID remappings: " <> show objectIdRemapping
--   for_ remainingObjects $ \objectId -> do
--     remapObjectIds objectId objectIdRemapping
--   where
--     remapObjectIds :: DB.BranchObjectId -> Map DB.BranchObjectId DB.BranchObjectId -> Sqlite.Transaction ()
--     remapObjectIds objId mapping = do
--       dbBranch <- Ops.expectDbBranch objId
--       let (Any changes, remappedBranch) =
--             dbBranch & DBBranch.childrenHashes_ %%~ \(childObjId, causalHash) -> do
--               case Map.lookup childObjId mapping of
--                 Just canonicalObjId ->
--                   (Any True, (canonicalObjId, causalHash))
--                 Nothing ->
--                   (Any False, (childObjId, causalHash))

--       when changes $ do
--         replaceBranch objId remappedBranch

--     replaceBranch :: DB.BranchObjectId -> DBBranch.DbBranch -> Sqlite.Transaction ()
--     replaceBranch objId branch = do
--       let (localBranchIds, localBranch) = S.LocalizeObject.localizeBranch branch
--       let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full localBranchIds localBranch
--       Sqlite.execute sql (bytes, objId)
--       where
--         sql =
--           [here|
--         UPDATE object
--         SET bytes = ?
--         WHERE id = ?
--       |]

--     rehashNamespace :: DB.BranchObjectId -> DB.CausalHashId -> StateT BadNamespaceInfo Sqlite.Transaction ()
--     rehashNamespace objectId possiblyIncorrectCausalHashId = do
--       lift . debugLog $ "Processing Branch Object ID:" <> show objectId
--       dbBranch <- lift $ Ops.expectDbBranch objectId
--       lift . debugLog $ "Successfully loaded dbBranch."
--       newBranchHash <- lift $ Helpers.dbBranchHash dbBranch
--       lift . debugLog $ "New branch hash: " <> show newBranchHash
--       correctNamespaceHashId <- lift $ Q.saveBranchHash (H.BranchHash newBranchHash)
--       lift . debugLog $ "New branch hash Id: " <> show newBranchHash
--       -- Update the value hash of the bad causal to the correct hash of its namespace.
--       lift . debugLog $ "Updating causal value hash (from, to)" <> show (possiblyIncorrectCausalHashId, correctNamespaceHashId)
--       lift $ Sqlite.execute updateCausalValueHash (correctNamespaceHashId, possiblyIncorrectCausalHashId)
--       mayCanonical <- lift $ Sqlite.queryMaybeCol getCanonicalObjectForHash (Sqlite.Only $ DB.unBranchHashId correctNamespaceHashId)
--       lift . debugLog $ "(objId, Canonical object ID):" <> show (objectId, mayCanonical)
--       case mayCanonical of
--         -- If there's an existing canonical object, record the mapping from this object id to
--         -- that one.
--         Just canonicalObjectId
--           | canonicalObjectId /= objectId -> do
--             lift . debugLog $ "Mapping objID: " <> show objectId <> " to canonical: " <> show canonicalObjectId
--             canonicalObjectRemapping . at objectId ?= canonicalObjectId
--             lift . debugLog $ "Unilaterally deleting: " <> show objectId
--             -- Remove possible foreign-key references before deleting the objects themselves
--             lift $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objectId)
--             lift $ Sqlite.execute deleteObjectById (Sqlite.Only objectId)
--           | otherwise -> do
--             error $ "Corrected hash for bad namespace is somehow still mapped to the same causal hash. This shouldn't happen.: " <> show (objectId, canonicalObjectId)

--         -- If there's no existing canonical object, this object becomes the canonical one by
--         -- reassigning its primary hash.
--         Nothing -> do
--           lift . debugLog $ "Updating in place: " <> show objectId
--           lift $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objectId)
--           lift $ Sqlite.execute updateHashIdForObject (correctNamespaceHashId, objectId)
--           lift $ updateHashObjects objectId correctNamespaceHashId
--           remainingBadObjects %= Set.insert objectId
--       pure ()
--     updateHashIdForObject :: Sqlite.Sql
--     updateHashIdForObject =
--       [here|
--           UPDATE object
--             SET primary_hash_id = ?
--             WHERE id = ?
--           |]
--     updateHashObjects (DB.BranchObjectId objId) (DB.BranchHashId newHashId) = do
--       Sqlite.execute deleteOldHashObjsSql (Sqlite.Only objId)
--       Q.saveHashObject newHashId objId 2
--       where
--         deleteOldHashObjsSql =
--           [here|
--           DELETE FROM hash_object
--             WHERE object_id = ?
--           |]
--     getCanonicalObjectForHash :: Sqlite.Sql
--     getCanonicalObjectForHash =
--       [here|
--           SELECT id
--             FROM object
--             WHERE primary_hash_id = ?
--           |]
--     updateCausalValueHash :: Sqlite.Sql
--     updateCausalValueHash =
--       [here|
--           UPDATE causal
--             SET value_hash_id = ?
--             WHERE value_hash_id = ?
--           |]
--     deleteHashObjectsByObjectId :: Sqlite.Sql
--     deleteHashObjectsByObjectId =
--       [here|
--           DELETE FROM hash_object
--             WHERE object_id = ?
--     |]
--     deleteObjectById :: Sqlite.Sql
--     deleteObjectById =
--       [here|
--           DELETE FROM object
--             WHERE id = ?
--           |]

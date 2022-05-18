{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Product
import qualified Data.Map as Map
import Data.Semigroup
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
import Unison.Codebase.IntegrityCheck (integrityCheckAllBranches, integrityCheckAllCausals)
import qualified Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers as Helpers
import qualified Unison.Debug as Debug
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Prelude hiding (log)

data MigrationState = MigrationState
  { _canonicalBranchForCausalHashId :: Map DB.CausalHashId (DB.BranchHashId, DB.BranchObjectId)
  }
  deriving (Generic)

canonicalBranchForCausalHashId :: Lens' MigrationState (Map DB.CausalHashId (DB.BranchHashId, DB.BranchObjectId))
canonicalBranchForCausalHashId =
  field @"_canonicalBranchForCausalHashId"

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
  createReachabilityTables
  Debug.whenDebug Debug.Integrity do
    log $ "Pre-migration integrity check..."
    integrityCheckAllBranches
    integrityCheckAllCausals
  flip execStateT (MigrationState mempty) $ Sync.sync migrationSync _migrationProgress [_rootCausal]
  do
    log $ "Checking namespace and causal integrity..."
    assertMigrationSuccess
    integrityCheckAllBranches
    integrityCheckAllCausals
  Q.setSchemaVersion 4
  where
    createReachabilityTables = do
      Sqlite.execute_
        [here|
           CREATE TEMP TABLE IF NOT EXISTS reachable_branch_objects (
            id INTEGER PRIMARY KEY NOT NULL REFERENCES object(id)
           )
          |]
      Sqlite.execute_
        [here|
           CREATE TEMP TABLE IF NOT EXISTS reachable_causals (
            self_hash_id INTEGER PRIMARY KEY NOT NULL REFERENCES hash(id)
           )
          |]

migrationSync :: Sync.Sync (StateT MigrationState Sqlite.Transaction) DB.CausalHashId
migrationSync =
  Sync.Sync \e -> do
    (runExceptT $ migrateCausal e) >>= \case
      Left syncResult -> pure syncResult
      Right _ -> pure Sync.Done

liftT :: Sqlite.Transaction a -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) a
liftT = lift . lift

migrateCausal :: DB.CausalHashId -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) ()
migrateCausal causalHashId = do
  (uses canonicalBranchForCausalHashId (Map.member causalHashId)) >>= \case
    True -> throwError Sync.PreviouslyDone
    False -> do
      causalParents <- liftT $ Q.loadCausalParents causalHashId
      unmigratedParents <- flip filterM causalParents $ \parentHashId -> (uses canonicalBranchForCausalHashId (Map.member parentHashId))
      when (not . null $ unmigratedParents) $ throwError (Sync.Missing unmigratedParents)
      branchObjId <- liftT $ Q.expectBranchObjectIdByCausalHashId causalHashId
      rehashAndCanonicalizeNamespace causalHashId branchObjId

-- preuse (correctedBranchHashIds . ix branchHashId) >>= \case
--   Nothing -> do
--     unmigratedBranchObjId <- DB.BranchObjectId <$> (lift $ Q.expectObjectIdForPrimaryHashId (DB.unBranchHashId branchHashId))
--     pure (Sync.Missing [Left unmigratedBranchObjId])
--   Just _ -> do
--     pure Sync.PreviouslyDone

-- preuse (causalHashIdToNamespaceHash . ix causalHashId) >>= \case
--   Just _ -> pure Sync.PreviouslyDone
--   Nothing -> do
--     branchHashId <- lift $ Q.expectCausalValueHashId causalHashId
--     preuse (correctedBranchHashIds . ix branchHashId) >>= \case
--       Just correctBranchHashId
--         | correctBranchHashId == branchHashId -> do
--           causalHashIdToNamespaceHash . at causalHashId ?= correctBranchHashId
--           pure Sync.Done
--         | otherwise -> do
--           -- lift $ Sqlite.execute updateCausalValueHash (correctBranchHashId, branchHashId)
--           causalHashIdToNamespaceHash . at causalHashId ?= correctBranchHashId
--           pure Sync.Done
--       Nothing -> do
--         -- We haven't re-hashed the attached branch yet, so we need to do that first so we
--         -- know the value hash ID.
--         branchObjId <- lift $ Q.expectBranchObjectIdByCausalHashId causalHashId
--         pure (Sync.Missing [Left branchObjId])

rehashAndCanonicalizeNamespace :: DB.CausalHashId -> DB.BranchObjectId -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) ()
rehashAndCanonicalizeNamespace causalHashId objId = do
  possiblyIncorrectNamespaceHashId <- DB.BranchHashId <$> (liftT $ Q.expectPrimaryHashIdForObject (DB.unBranchObjectId objId))
  dbBranch <- liftT $ Ops.expectDbBranch objId
  remapping <- use canonicalBranchForCausalHashId
  let ((unmigratedChildren, Any changes), remappedBranch) =
        dbBranch & DBBranch.childrenHashes_ %%~ \(ids@(_childBranchObjId, childCausalHashId)) -> do
          case Map.lookup childCausalHashId remapping of
            Nothing -> (([childCausalHashId], Any False), ids)
            Just (_, canonicalObjId) -> ((mempty, Any True), (canonicalObjId, childCausalHashId))
  when (not . null $ unmigratedChildren) $ throwError (Sync.Missing (unmigratedChildren))
  when changes $ do
    liftT $ replaceBranch objId remappedBranch
  newBranchHash <- liftT $ Helpers.dbBranchHash remappedBranch
  correctNamespaceHashId <- liftT $ Q.saveBranchHash (H.BranchHash newBranchHash)
  -- Only do the extra work if the namespace hash was incorrect.
  when (correctNamespaceHashId == possiblyIncorrectNamespaceHashId) $ throwError Sync.Done
  liftT . debugLog $ "New branch hash Id: " <> show newBranchHash
  -- Update the value hash of the bad causal to the correct hash of its namespace.
  mayCanonical <- liftT $ Sqlite.queryMaybeCol getCanonicalObjectForHash (Sqlite.Only $ DB.unBranchHashId correctNamespaceHashId)
  liftT . debugLog $ "(objId, Canonical object ID):" <> show (objId, mayCanonical)
  liftT . debugLog $ "Updating causal value hash (from, to)" <> show (possiblyIncorrectNamespaceHashId, correctNamespaceHashId)
  liftT $ Sqlite.execute updateCausalValueHash (correctNamespaceHashId, possiblyIncorrectNamespaceHashId)
  canonicalObjId <- case mayCanonical of
    -- If there's an existing canonical object, record the mapping from this object id to
    -- that one.
    Just canonicalObjectId
      | canonicalObjectId /= objId -> do
        liftT . debugLog $ "Mapping objID: " <> show objId <> " to canonical: " <> show canonicalObjectId
        liftT . debugLog $ "Unilaterally deleting: " <> show objId
        -- Remove possible foreign-key references before deleting the objects themselves
        liftT $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objId)
        liftT $ Sqlite.execute deleteObjectById (Sqlite.Only objId)
        pure canonicalObjectId
      | otherwise -> do
        error $ "Corrected hash for bad namespace is somehow still mapped to the same causal hash. This shouldn't happen.: " <> show (objId, canonicalObjectId)

    -- If there's no existing canonical object, this object becomes the canonical one by
    -- reassigning its primary hash.
    Nothing -> do
      liftT . debugLog $ "Updating in place: " <> show objId
      liftT $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objId)
      liftT $ Sqlite.execute updateHashIdForObject (correctNamespaceHashId, objId)
      liftT $ updateHashObjects objId correctNamespaceHashId
      pure objId
  canonicalBranchForCausalHashId . at causalHashId ?= (correctNamespaceHashId, canonicalObjId)
  where
    updateCausalValueHash :: Sqlite.Sql
    updateCausalValueHash =
      [here|
            UPDATE causal
              SET value_hash_id = ?
              WHERE value_hash_id = ?
            |]
    getCanonicalObjectForHash :: Sqlite.Sql
    getCanonicalObjectForHash =
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
    updateHashObjects (DB.BranchObjectId objId) (DB.BranchHashId newHashId) = do
      Sqlite.execute deleteOldHashObjsSql (Sqlite.Only objId)
      Q.saveHashObject newHashId objId 2
      where
        deleteOldHashObjsSql =
          [here|
          DELETE FROM hash_object
            WHERE object_id = ?
          |]

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

-- abortMigration :: String -> Sqlite.Transaction a
-- abortMigration msg = do
--   error msg

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

log :: String -> Sqlite.Transaction ()
log = Sqlite.unsafeIO . putStrLn

debugLog :: String -> Sqlite.Transaction ()
debugLog = Debug.whenDebug Debug.Migration . Sqlite.unsafeIO . putStrLn

assertMigrationSuccess :: Sqlite.Transaction ()
assertMigrationSuccess = do
  badNamespaces <- Sqlite.queryListRow_ @(DB.BranchObjectId, DB.CausalHashId) findNamespacesWithCausalHashesSql
  when (not . null $ badNamespaces) $ error "Uh-oh, still found bad namespaces after migration."

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

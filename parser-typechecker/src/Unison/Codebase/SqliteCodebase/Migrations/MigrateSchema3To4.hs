{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Lens
import Control.Monad.State
import Data.Generics.Product
import qualified Data.Map as Map
import Data.Semigroup
import qualified Data.Set as Set
import Data.String.Here.Uninterpolated (here)
import Data.Void
import qualified U.Codebase.HashTags as H
import qualified U.Codebase.Sqlite.Branch.Format as S.BranchFormat
import qualified U.Codebase.Sqlite.Branch.Full as DBBranch
import qualified U.Codebase.Sqlite.DbId as DB
import qualified U.Codebase.Sqlite.LocalizeObject as S.LocalizeObject
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Serialization as S
import qualified U.Util.Serialization as S
import qualified Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers as Helpers
import qualified Unison.Debug as Debug
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Prelude hiding (log)

data BadNamespaceInfo = BadNamespaceInfo
  { _canonicalObjectRemapping :: Map DB.BranchObjectId DB.BranchObjectId,
    _remainingBadObjects :: Set DB.BranchObjectId
  }
  deriving (Generic)

canonicalObjectRemapping :: Lens' BadNamespaceInfo (Map DB.BranchObjectId DB.BranchObjectId)
canonicalObjectRemapping =
  field @"_canonicalObjectRemapping"

remainingBadObjects :: Lens' BadNamespaceInfo (Set DB.BranchObjectId)
remainingBadObjects = field @"_remainingBadObjects"

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
  -- Debug.whenDebug Debug.Migration do
  --   integrityCheckAllBranches
  fixBadNamespaceHashes
  do
    log $ "Checking migration success..."
    assertMigrationSuccess
    log $ "Checking namespace and causal integrity..."
    integrityCheckAllBranches
  Q.setSchemaVersion 4

fixBadNamespaceHashes :: Sqlite.Transaction ()
fixBadNamespaceHashes = do
  badNamespaces <- Sqlite.queryListRow_ findNamespacesWithCausalHashesSql
  let numToMigrate = (length badNamespaces)
  -- Rehash all bad namespaces and return a re-mapping of object IDs to the canonical object for their hash.
  BadNamespaceInfo objectIdRemapping remainingObjects <- flip execStateT (BadNamespaceInfo mempty mempty) $ do
    ifor_ badNamespaces \i (objectId, causalHashId) -> do
      rehashNamespace objectId causalHashId
      lift $ Sqlite.unsafeIO . putStr $ "\r  🏗  " <> show (i + 1) <> " / " <> show numToMigrate <> " entities migrated. 🚧"
  debugLog $ "object ID remappings: " <> show objectIdRemapping
  for_ remainingObjects $ \objectId -> do
    remapObjectIds objectId objectIdRemapping
  where
    remapObjectIds :: DB.BranchObjectId -> Map DB.BranchObjectId DB.BranchObjectId -> Sqlite.Transaction ()
    remapObjectIds objId mapping = do
      dbBranch <- Ops.expectDbBranch objId
      let (Any changes, remappedBranch) =
            dbBranch & DBBranch.childrenHashes_ %%~ \(childObjId, causalHash) -> do
              case Map.lookup childObjId mapping of
                Just canonicalObjId ->
                  (Any True, (canonicalObjId, causalHash))
                Nothing ->
                  (Any False, (childObjId, causalHash))

      when changes $ do
        replaceBranch objId remappedBranch

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

    rehashNamespace :: DB.BranchObjectId -> DB.CausalHashId -> StateT BadNamespaceInfo Sqlite.Transaction ()
    rehashNamespace objectId possiblyIncorrectCausalHashId = do
      lift . debugLog $ "Processing Branch Object ID:" <> show objectId
      dbBranch <- lift $ Ops.expectDbBranch objectId
      lift . debugLog $ "Successfully loaded dbBranch."
      newBranchHash <- lift $ Helpers.dbBranchHash dbBranch
      lift . debugLog $ "New branch hash: " <> show newBranchHash
      correctNamespaceHashId <- lift $ Q.saveBranchHash (H.BranchHash newBranchHash)
      lift . debugLog $ "New branch hash Id: " <> show newBranchHash
      -- Update the value hash of the bad causal to the correct hash of its namespace.
      lift . debugLog $ "Updating causal value hash (from, to)" <> show (possiblyIncorrectCausalHashId, correctNamespaceHashId)
      lift $ Sqlite.execute updateCausalValueHash (correctNamespaceHashId, possiblyIncorrectCausalHashId)
      mayCanonical <- lift $ Sqlite.queryMaybeCol getCanonicalObjectForHash (Sqlite.Only $ DB.unBranchHashId correctNamespaceHashId)
      lift . debugLog $ "(objId, Canonical object ID):" <> show (objectId, mayCanonical)
      case mayCanonical of
        -- If there's an existing canonical object, record the mapping from this object id to
        -- that one.
        Just canonicalObjectId
          | canonicalObjectId /= objectId -> do
            lift . debugLog $ "Mapping objID: " <> show objectId <> " to canonical: " <> show canonicalObjectId
            canonicalObjectRemapping . at objectId ?= canonicalObjectId
            lift . debugLog $ "Unilaterally deleting: " <> show objectId
            -- Remove possible foreign-key references before deleting the objects themselves
            lift $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objectId)
            lift $ Sqlite.execute deleteObjectById (Sqlite.Only objectId)
          | otherwise -> do
            error $ "Corrected hash for bad namespace is somehow still mapped to the same causal hash. This shouldn't happen.: " <> show (objectId, canonicalObjectId)

        -- If there's no existing canonical object, this object becomes the canonical one by
        -- reassigning its primary hash.
        Nothing -> do
          lift . debugLog $ "Updating in place: " <> show objectId
          lift $ Sqlite.execute deleteHashObjectsByObjectId (Sqlite.Only objectId)
          lift $ Sqlite.execute updateHashIdForObject (correctNamespaceHashId, objectId)
          lift $ updateHashObjects objectId correctNamespaceHashId
          remainingBadObjects %= Set.insert objectId
      pure ()
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
    getCanonicalObjectForHash :: Sqlite.Sql
    getCanonicalObjectForHash =
      [here|
          SELECT id
            FROM object
            WHERE primary_hash_id = ?
          |]
    updateCausalValueHash :: Sqlite.Sql
    updateCausalValueHash =
      [here|
          UPDATE causal
            SET value_hash_id = ?
            WHERE value_hash_id = ?
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

assertMigrationSuccess :: Sqlite.Transaction ()
assertMigrationSuccess = do
  Debug.whenDebug Debug.Migration $ do
    badNamespaces <- Sqlite.queryListRow_ @(DB.BranchObjectId, DB.CausalHashId) findNamespacesWithCausalHashesSql
    when (not . null $ badNamespaces) $ error "Uh-oh, still found bad namespaces after migration."

integrityCheckAllBranches :: Sqlite.Transaction ()
integrityCheckAllBranches = do
  Sqlite.queryOneCol_ anyBadCausalsCheck
  branchObjIds <- Sqlite.queryListCol_ allBranchObjectIdsSql
  for_ branchObjIds integrityCheckBranch
  where
    allBranchObjectIdsSql :: Sqlite.Sql
    allBranchObjectIdsSql =
      [here|
          SELECT id FROM object WHERE type_id = 2;
          |]

    anyBadCausalsCheck :: Sqlite.Sql
    anyBadCausalsCheck =
      [here|
          SELECT count(*)
            FROM causal c
            WHERE NOT EXISTS (SELECT 1 from object o WHERE o.primary_hash_id = c.value_hash_id);
          |]

    integrityCheckBranch :: DB.BranchObjectId -> Sqlite.Transaction ()
    integrityCheckBranch objId = do
      debugLog $ "Checking integrity of " <> show objId
      dbBranch <- Ops.expectDbBranch objId
      expectedBranchHash <- Helpers.dbBranchHash dbBranch
      actualBranchHash <- Q.expectPrimaryHashByObjectId (DB.unBranchObjectId objId)
      when (expectedBranchHash /= actualBranchHash) $ do
        failure $ "Expected hash for namespace doesn't match actual hash for namespace: " <> show (expectedBranchHash, actualBranchHash)
      forOf_ DBBranch.childrenHashes_ dbBranch $ \(childObjId, childCausalHashId) -> do
        debugLog $ "checking child: " <> show (childObjId, childCausalHashId)
        -- Assert the child branch object exists
        Q.loadNamespaceObject @Void (DB.unBranchObjectId childObjId) (const $ Right ()) >>= \case
          Nothing -> failure $ "Expected namespace object for object ID: " <> show childObjId
          Just _ -> pure ()
        Sqlite.queryOneCol doesCausalExistForCausalHashId (Sqlite.Only childCausalHashId) >>= \case
          True -> pure ()
          False -> failure $ "Expected causal for causal hash ID, but none was found: " <> show childCausalHashId
        -- Assert the object for the causal hash ID matches the given object Id.
        Q.loadBranchObjectIdByCausalHashId childCausalHashId >>= \case
          Nothing -> failure $ "Expected branch object for causal hash ID: " <> show childCausalHashId
          Just foundBranchId
            | foundBranchId /= childObjId -> do
              failure $ "Expected child branch object to match canonical object ID for causal hash's namespace: " <> show (childCausalHashId, foundBranchId, childObjId)
            | otherwise -> pure ()
        pure ()
      where
        failure :: String -> Sqlite.Transaction ()
        failure msg = do
          -- error msg
          debugLog msg

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

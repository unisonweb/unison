{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Lens
import Control.Monad.State
import Data.Generics.Product
import qualified Data.Map as Map
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
  fixBadNamespaceHashes
  Q.setSchemaVersion 4

fixBadNamespaceHashes :: Sqlite.Transaction ()
fixBadNamespaceHashes = do
  badNamespaces <- Sqlite.queryListRow_ findNamespacesWithCausalHashes
  let numToMigrate = (length badNamespaces)
  -- Rehash all bad namespaces and return a re-mapping of object IDs to the canonical object for their hash.
  BadNamespaceInfo objectIdRemapping remainingObjects <- flip execStateT (BadNamespaceInfo mempty mempty) $ do
    ifor_ badNamespaces \i (objectId, causalHashId) -> do
      rehashNamespace objectId causalHashId
      lift $ Sqlite.unsafeIO . putStr $ "\r  🏗  " <> show (i + 1) <> " / " <> show numToMigrate <> " entities migrated. 🚧"
  for_ remainingObjects $ \objectId -> do
    remapObjectIds objectId objectIdRemapping

  Debug.whenDebug Debug.Migration $ do
    badNamespaces <- Sqlite.queryListRow_ @(DB.BranchObjectId, DB.CausalHashId) findNamespacesWithCausalHashes
    when (not . null $ badNamespaces) $ error "Uh-oh, still found bad namespaces after migration."
  where
    remapObjectIds :: DB.BranchObjectId -> Map DB.BranchObjectId DB.BranchObjectId -> Sqlite.Transaction ()
    remapObjectIds objId mapping = do
      dbBranch <- Ops.expectDbBranch objId
      let maybeRemappedBranch =
            dbBranch & DBBranch.childrenHashes_ %%~ \(childObjId, causalHash) -> do
              canonicalObjId <- Map.lookup childObjId mapping
              -- Sanity check that the object ID we're re-mapping to matches the causal.
              pure (canonicalObjId, causalHash)

      case maybeRemappedBranch of
        -- Nothing changed, no need to save.
        Nothing -> pure ()
        Just remappedBranch -> do
          -- Sanity check
          replaceBranch objId remappedBranch

      Debug.whenDebug Debug.Migration $ do
        -- Check the remapped branch if we have one, otherwise sanity check the untouched
        -- branch.
        let branch = fromMaybe dbBranch maybeRemappedBranch
        forOf_ DBBranch.childrenHashes_ branch $ \(childObjId, childCausalHashId) -> do
          -- Assert the objects all exist
          Q.loadNamespaceObject @Void (DB.unBranchObjectId childObjId) (const $ Right ()) >>= \case
            Nothing -> error $ "Expected namespace for object ID: " <> show childObjId
            Just _ -> pure ()
          -- Assert the object for the causal hash ID matches the given object Id.
          Q.loadBranchObjectIdByCausalHashId childCausalHashId >>= \case
            Nothing -> error $ "Expected branch object for causal hash ID: " <> show childCausalHashId
            Just foundBranchId
              | foundBranchId /= childObjId -> do
                error $ "Expected child branch object to match canonical object ID for causal hash's namespace: " <> show (childCausalHashId, foundBranchId, childObjId)
              | otherwise -> pure ()
          pure ()

    findNamespacesWithCausalHashes :: Sqlite.Sql
    findNamespacesWithCausalHashes =
      [here|
        -- Find all namespace object IDs which actually have causal hashes instead of
        -- namespace hashes as their primary hash.
        SELECT DISTINCT o.id, o.primary_hash_id
          FROM causal c
          LEFT JOIN object o ON o.primary_hash_id = c.self_hash_id
          WHERE self_hash_id = self_hash_id
                AND type_id = 2 -- filter for namespaces just to be safe
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

    rehashNamespace :: DB.BranchObjectId -> DB.CausalHashId -> StateT BadNamespaceInfo Sqlite.Transaction ()
    rehashNamespace objectId incorrectHashIdFromCausal = do
      dbBranch <- lift $ Ops.expectDbBranch objectId
      newBranchHash <- lift $ Helpers.dbBranchHash dbBranch
      correctNamespaceHashId <- lift $ Q.saveBranchHash (H.BranchHash newBranchHash)
      -- Update the value hash of the bad causal to the correct hash of its namespace.
      lift $ Sqlite.execute updateCausalValueHash (correctNamespaceHashId, incorrectHashIdFromCausal)
      mayCanonical <- lift $ Sqlite.queryMaybeCol getCanonicalObjectForHash (Sqlite.Only $ DB.unBranchHashId correctNamespaceHashId)
      case mayCanonical of
        -- If there's an existing canonical object, record the mapping from this object id to
        -- that one.
        Just canonicalObjectId
          | canonicalObjectId /= objectId -> do
            canonicalObjectRemapping . at objectId ?= canonicalObjectId
            lift $ Sqlite.execute deleteObjectById (Sqlite.Only objectId)
          | otherwise -> do
            error $ "Corrected hash for bad namespace is somehow still mapped to the same causal hash. This shouldn't happen.: " <> show (objectId, canonicalObjectId)

        -- If there's no existing canonical object, this object becomes the canonical one by
        -- reassigning its primary hash.
        Nothing -> do
          lift $ Sqlite.execute updateHashIdForObject (correctNamespaceHashId, objectId)
          remainingBadObjects %= Set.insert objectId
      pure ()
    updateHashIdForObject :: Sqlite.Sql
    updateHashIdForObject =
      [here|
          UPDATE object
            SET primary_hash_id = ?
            WHERE id = ?
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
    deleteObjectById :: Sqlite.Sql
    deleteObjectById =
      [here|
          DELETE FROM object
            WHERE id = ?
          |]

-- * Identify all bad causals/namespace objects

-- * Rehash all bad namespaces

-- * Insert each namespace, keeping track of which objects have conflicts and which don't; add objectID to 'canonical' objectID mapping if there's a conflict on primary hash.

-- * Delete all 'duplicate' namespaces

-- * Go through every namespace from the 'bad-namespaces' that didn't map to a canonical namespace, re-map their object IDs to the canonical ones.

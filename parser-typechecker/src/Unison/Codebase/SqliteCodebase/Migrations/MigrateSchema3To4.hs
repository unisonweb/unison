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

-- | There was a bug in previous versions of UCM which incorrectly used causal hashes as branch hashes.
-- This remained undetected because there was never a need for this hash to be verifiable,
-- and the hashes were still unique because the namespace hash was PART of the causal hash.
-- It did however result in many identical branches being stored multiple times under
-- different `primary_hash_id`s.
--
-- However, with the advent of Share and Sync, we now need to correctly verify these namespace
-- hashes.
--
-- This migration fixes the issue by re-hashing namespace objects where the value_hash_id of a
-- causal matches the self_hash_id.
-- Luckily this doesn't change any causal hashes.
--
-- However, due to the possibility of multiple identical objects stored under different
-- `primary_hash_id`s, we may now have multiple objects with the same `primary_hash_id`, which
-- our DB schema doesn't allow.
--
-- To address this, we keep exactly one 'canonical' object for each hash, then remap all
-- references to old objects into this canonical object instead. Unfortunately this requires
-- mapping over every branch object and traversing the child references.
--
-- It was also discovered that some developers had many branches which referenced objects
-- which weren't in their codebase. We're not yet sure how this happened, but it's unlikely
-- to be the case for most end users, and it turned out that these references were in causals
-- and branches which were unreachable from the root namespace. As a fix, this migration also
-- tracks every causal and branch which is reachable from the root namespace and deletes all
-- causals and namespaces which are unreachable. Note that this may orphan some definitions,
-- patches, etc. which were previously referenced in an 'unreachable' branch, but they were
-- already floating around in an unreachable state.
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
  Q.setSchemaVersion 4
  where
    causalCount :: Sqlite.Transaction Int
    causalCount = do
      Sqlite.queryOneCol_
        [here|
          SELECT count(*) FROM causal;
          |]

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
        WHERE
          NOT EXISTS (SELECT 1 FROM reachable_branch_objects AS ro WHERE ho.object_id = ro.object_id)
          -- Ensure hash objects we're deleting are for branch objects.
          AND EXISTS (SELECT 1 FROM object AS o WHERE o.id = ho.object_id AND type_id = 2)
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
          -- We only need to check the children, because if it's impossible for a parent to be
          -- unreachable if the child is reachable. A.k.a. reachable(child) =implies> reachable(parent)
          NOT EXISTS (SELECT 1 FROM reachable_causals AS rc WHERE cp.causal_id = rc.self_hash_id)
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
        dbBranch & DBBranch.childrenHashes_ %%~ \(ids@(childBranchObjId, childCausalHashId)) -> do
          case Map.lookup childCausalHashId canonicalBranchForCausalMap of
            Nothing -> (([childCausalHashId], Any False), ids)
            Just (_, canonicalObjId) ->
              let changed = canonicalObjId /= childBranchObjId
               in (([], Any changed), (canonicalObjId, childCausalHashId))
  when (not . null $ unmigratedChildren) $ throwError (Sync.Missing unmigratedChildren)
  when changes $ do
    liftT $ replaceBranch objId remappedBranch
  when (DB.unCausalHashId causalHashId /= DB.unBranchHashId possiblyIncorrectNamespaceHashId) $ do
    -- If the causal hash and value hash are already different, then we don't need to re-hash
    -- the branch.
    canonicalBranchForCausalHashId . at causalHashId ?= (possiblyIncorrectNamespaceHashId, objId)
    validBranchHashIds . at possiblyIncorrectNamespaceHashId ?= objId
    throwError Sync.Done
  newBranchHash <- liftT $ Helpers.dbBranchHash remappedBranch
  liftT . debugLog $ "New branch hash: " <> show newBranchHash
  correctNamespaceHashId <- liftT $ Q.saveBranchHash (H.BranchHash newBranchHash)
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

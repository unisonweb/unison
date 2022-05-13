{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Lens (ifor_)
import Data.String.Here.Uninterpolated (here)
import qualified U.Codebase.HashTags as H
import qualified U.Codebase.Sqlite.DbId as DB
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers as Helpers
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Prelude hiding (log)

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
  ifor_ badNamespaces \i (objectId, causalHashId) -> do
    rehashNamespace objectId causalHashId
    Sqlite.unsafeIO . putStr $ "\r  🏗  " <> show (i + 1) <> " / " <> show numToMigrate <> " entities migrated. 🚧"
  where
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
    rehashNamespace :: DB.BranchObjectId -> DB.CausalHashId -> Sqlite.Transaction ()
    rehashNamespace objectId incorrectHashIdFromCausal = do
      dbBranch <- Ops.expectDbBranch objectId
      newBranchHash <- Helpers.dbBranchHash dbBranch
      correctNamespaceHashId <- Q.saveBranchHash (H.BranchHash newBranchHash)
      Sqlite.execute updateHashIdForObject (correctNamespaceHashId, objectId)
      Sqlite.execute updateCausalValueHash (correctNamespaceHashId, incorrectHashIdFromCausal)
      pure ()
      where
        updateHashIdForObject =
          [here|
              UPDATE object
                SET primary_hash_id = ?
                WHERE id = ?
              |]
        updateCausalValueHash =
          [here|
              UPDATE causal
                SET value_hash_id = ?
                WHERE value_hash_id = ?
              |]

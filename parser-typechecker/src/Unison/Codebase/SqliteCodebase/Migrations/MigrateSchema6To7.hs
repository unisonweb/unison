{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema6To7 (migrateSchema6To7) where

import Control.Monad.Except
import Control.Monad.State
import U.Codebase.Branch.Type (NamespaceStats)
import U.Codebase.Sqlite.DbId qualified as DB
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sync qualified as Sync
import Unison.Debug qualified as Debug
import Unison.Sqlite qualified as Sqlite

-- | Adds a table for tracking namespace statistics
-- Adds stats for all existing namespaces, even though missing stats are computed on-demand if missing.
migrateSchema6To7 :: Sqlite.Transaction ()
migrateSchema6To7 = do
  Q.expectSchemaVersion 6
  Q.addNamespaceStatsTables
  addStatsToAllNamespaces
  Q.setSchemaVersion 7

addStatsToAllNamespaces :: Sqlite.Transaction ()
addStatsToAllNamespaces = do
  totalToMigrate <-
    Sqlite.queryOneCol
      [Sqlite.sql|
        SELECT COUNT(*)
          FROM object
          WHERE type_id = 2
      |]
  allBranchObjIds <-
    Sqlite.queryListCol
      [Sqlite.sql|
        SELECT id
          FROM object
          WHERE type_id = 2
      |]
  _ <- flip runStateT 0 $ Sync.sync migrationSync (migrationProgress totalToMigrate) allBranchObjIds
  pure ()

migrationSync :: Sync.Sync (StateT Int Sqlite.Transaction) DB.BranchObjectId
migrationSync =
  Sync.Sync (lift . addStatsForBranch)

addStatsForBranch :: DB.BranchObjectId -> Sqlite.Transaction (Sync.TrySyncResult DB.BranchObjectId)
addStatsForBranch boId = do
  bhId <- Db.BranchHashId <$> Q.expectPrimaryHashIdForObject (Db.unBranchObjectId boId)
  -- "expectNamespaceStatsByHashId" computes stats if they are missing.
  _ :: NamespaceStats <- Ops.expectNamespaceStatsByHashId bhId
  pure Sync.Done

debugLog :: String -> Sqlite.Transaction ()
debugLog = Debug.whenDebug Debug.Migration . Sqlite.unsafeIO . putStrLn

migrationProgress :: Int -> Sync.Progress (StateT Int Sqlite.Transaction) DB.BranchObjectId
migrationProgress totalBranches =
  Sync.Progress {Sync.need, Sync.done, Sync.error, Sync.allDone}
  where
    need e = lift $ debugLog $ "Need " <> show e
    done _ =
      do
        modify succ
        numDone <- get
        lift $ Sqlite.unsafeIO $ putStr $ "\r🏗  " <> show numDone <> " / ~" <> show totalBranches <> " entities migrated. 🚧"
    error e = lift . Sqlite.unsafeIO . putStrLn $ "Error " <> show e
    allDone = lift do
      -- In some corrupted codebases we don't necessarily process every causal, or there may
      -- be unreachable causals. We'll show the final number here just so everything looks
      -- good to users. It's okay since we'll process the other branches and clean them up in
      -- a batch step.
      Sqlite.unsafeIO $ putStrLn $ "\r🏗  " <> show totalBranches <> " / ~" <> show totalBranches <> " entities migrated. 🚧"
      Sqlite.unsafeIO . putStrLn $ "Finished."

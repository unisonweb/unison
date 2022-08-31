{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema5To6 (migrateSchema5To6) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Product (field)
import qualified Data.Map as Map
import Data.String.Here.Uninterpolated (here)
import U.Codebase.Branch (NamespaceStats (..))
import qualified U.Codebase.Sqlite.Branch.Full as Db
import qualified U.Codebase.Sqlite.DbId as DB
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sync as Sync
import qualified Unison.Debug as Debug
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Unison.Util.Monoid (foldMapM)

-- | Adds a table for tracking namespace statistics
migrateSchema5To6 :: Sqlite.Transaction ()
migrateSchema5To6 = do
  Q.expectSchemaVersion 5
  Q.addNamespaceStatsTables
  addStatsToAllNamespaces
  Q.setSchemaVersion 6

data MigrationState = MigrationState
  { statsForBranchHashId :: Map DB.BranchObjectId NamespaceStats,
    numMigrated :: Int
  }
  deriving (Generic)

addStatsToAllNamespaces :: Sqlite.Transaction ()
addStatsToAllNamespaces = do
  totalCausals <-
    Sqlite.queryOneCol_
      [here|
    SELECT COUNT(*)
      FROM object
      WHERE type_id = 2
    |]
  allBranchObjIds <-
    Sqlite.queryListCol_
      [here|
    SELECT id
      FROM object
      WHERE type_id = 2
    |]
  _state <- flip execStateT (MigrationState mempty 0) $ Sync.sync migrationSync (migrationProgress totalCausals) allBranchObjIds
  pure ()

migrationSync :: Sync.Sync (StateT MigrationState Sqlite.Transaction) DB.BranchObjectId
migrationSync =
  Sync.Sync \e -> do
    (runExceptT $ addStatsForBranch e) >>= \case
      Left syncResult -> pure syncResult
      Right _ -> pure Sync.Done

addStatsForBranch :: DB.BranchObjectId -> ExceptT (Sync.TrySyncResult DB.BranchObjectId) (StateT MigrationState Sqlite.Transaction) ()
addStatsForBranch boId = do
  lift (use (field @"statsForBranchHashId" . at boId)) >>= \case
    Just _ -> throwError $ Sync.PreviouslyDone
    Nothing -> pure ()
  branch <- lift . lift $ Ops.expectDbBranch boId
  (unmigratedChildren :: [DB.BranchObjectId], childStats :: NamespaceStats) <-
    Db.children branch & foldMapM \(boId, _chId) ->
      use (field @"statsForBranchHashId" . at boId) <&> \case
        Nothing -> ([boId], mempty)
        Just stats -> (mempty, stats)
  when (not . null $ unmigratedChildren) $ throwError $ Sync.Missing unmigratedChildren
  let branchStats =
        childStats
          <> NamespaceStats
            { numContainedTerms = lengthOf (folded . folded) (Db.terms branch),
              numContainedTypes = lengthOf (folded . folded) (Db.types branch),
              numContainedPatches = Map.size (Db.patches branch)
            }
  bhId <- Db.BranchHashId <$> (lift . lift $ Q.expectPrimaryHashIdForObject (Db.unBranchObjectId boId))
  lift . lift $ Q.saveNamespaceStats bhId branchStats
  field @"statsForBranchHashId" . at boId ?= branchStats

debugLog :: String -> Sqlite.Transaction ()
debugLog = Debug.whenDebug Debug.Migration . Sqlite.unsafeIO . putStrLn

migrationProgress :: Int -> Sync.Progress (StateT MigrationState Sqlite.Transaction) DB.BranchObjectId
migrationProgress totalBranches =
  Sync.Progress {Sync.need, Sync.done, Sync.error, Sync.allDone}
  where
    need e = lift $ debugLog $ "Need " <> show e
    done _ =
      do
        numDone <- field @"numMigrated" <+= 1
        lift $ Sqlite.unsafeIO $ putStr $ "\r🏗  " <> show numDone <> " / ~" <> show totalBranches <> " entities migrated. 🚧"
    error e = lift . Sqlite.unsafeIO . putStrLn $ "Error " <> show e
    allDone = do
      -- In some corrupted codebases we don't necessarily process every causal, or there may
      -- be unreachable causals. We'll show the final number here just so everything looks
      -- good to users. It's okay since we'll process the other branches and clean them up in
      -- a batch step.
      lift $ Sqlite.unsafeIO $ putStrLn $ "\r🏗  " <> show totalBranches <> " / ~" <> show totalBranches <> " entities migrated. 🚧"
      lift . Sqlite.unsafeIO . putStrLn $ "Finished."

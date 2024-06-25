{-# LANGUAGE MultiWayIf #-}

module Unison.Codebase.SqliteCodebase.Migrations where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.Regions qualified as Region
import System.FilePath ((</>))
import U.Codebase.Sqlite.DbId (SchemaVersion (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (CodebasePath)
import Unison.Codebase.Init (BackupStrategy (..), VacuumStrategy (..))
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (OpenCodebaseUnknownSchemaVersion))
import Unison.Codebase.Init.OpenCodebaseError qualified as Codebase
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema11To12 (migrateSchema11To12)
import Unison.Codebase.SqliteCodebase.Paths (backupCodebasePath)
import Unison.Codebase.Type (LocalOrRemote (..))
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Sqlite.Connection qualified as Sqlite.Connection
import UnliftIO qualified

-- | Mapping from schema version to the migration required to get there.
-- E.g. The migration at index 2 must be run on a codebase at version 1.
migrations :: Map SchemaVersion (Sqlite.Transaction ())
migrations =
  Map.fromList
    [ -- Recreates the name lookup tables because the primary key was missing the root hash id.
      sqlMigration 9 Q.fixScopedNameLookupTables,
      sqlMigration 10 Q.addProjectTables,
      sqlMigration 11 Q.addMostRecentBranchTable,
      (12, migrateSchema11To12),
      sqlMigration 13 Q.addMostRecentNamespaceTable,
      sqlMigration 14 Q.addSquashResultTable,
      sqlMigration 15 Q.addSquashResultTableIfNotExists,
      sqlMigration 16 Q.cdToProjectRoot
    ]
  where
    sqlMigration :: SchemaVersion -> Sqlite.Transaction () -> (SchemaVersion, Sqlite.Transaction ())
    sqlMigration ver migration =
      ( ver,
        do
          Q.expectSchemaVersion (ver - 1)
          migration
          Q.setSchemaVersion ver
      )

data CodebaseVersionStatus
  = CodebaseUpToDate
  | CodebaseUnknownSchemaVersion SchemaVersion
  | CodebaseRequiresMigration
      -- Current version
      SchemaVersion
      -- Required version
      SchemaVersion
  deriving stock (Eq, Ord, Show)

checkCodebaseIsUpToDate :: Sqlite.Transaction CodebaseVersionStatus
checkCodebaseIsUpToDate = do
  schemaVersion <- Q.schemaVersion
  -- The highest schema that this ucm knows how to migrate to.
  pure $
    if
        | schemaVersion == Q.currentSchemaVersion -> CodebaseUpToDate
        | schemaVersion < Q.currentSchemaVersion -> CodebaseRequiresMigration schemaVersion Q.currentSchemaVersion
        | otherwise -> CodebaseUnknownSchemaVersion schemaVersion

-- | Migrates a codebase up to the most recent version known to ucm.
-- This is a No-op if it's up to date
-- Returns an error if the schema version is newer than this ucm knows about.
ensureCodebaseIsUpToDate ::
  (MonadIO m) =>
  LocalOrRemote ->
  CodebasePath ->
  Bool ->
  BackupStrategy ->
  VacuumStrategy ->
  Sqlite.Connection ->
  m (Either Codebase.OpenCodebaseError ())
ensureCodebaseIsUpToDate localOrRemote root shouldPrompt backupStrategy vacuumStrategy conn =
  (liftIO . UnliftIO.try) do
    regionVar <- newEmptyMVar
    let finalizeRegion :: IO ()
        finalizeRegion =
          whenJustM (tryTakeMVar regionVar) \region -> do
            content <- Region.getConsoleRegion region
            Region.finishConsoleRegion region content

    Region.displayConsoleRegions do
      (`UnliftIO.finally` finalizeRegion) do
        let migs = migrations
        -- The highest schema that this ucm knows how to migrate to.
        let highestKnownSchemaVersion = fst . head $ Map.toDescList migs
        currentSchemaVersion <- Sqlite.runTransaction conn Q.schemaVersion
        when (currentSchemaVersion > highestKnownSchemaVersion) $ UnliftIO.throwIO $ OpenCodebaseUnknownSchemaVersion (fromIntegral currentSchemaVersion)
        backupCodebaseIfNecessary backupStrategy localOrRemote conn currentSchemaVersion highestKnownSchemaVersion root
        when shouldPrompt do
          putStrLn "Press <enter> to start the migration once all other ucm processes are shutdown..."
          void $ liftIO getLine
        ranMigrations <-
          Sqlite.runWriteTransaction conn \run -> do
            -- Get the schema version again now that we're in a transaction.
            currentSchemaVersion <- run Q.schemaVersion
            case Map.minViewWithKey migrations of
              Nothing -> error "No migrations found"
              Just ((minMigrationVersion, _), _) -> do
                when (currentSchemaVersion < (minMigrationVersion - 1)) do
                  putStrLn $ "🚨 Your codebase is at schema version " <> show currentSchemaVersion <> " but this UCM binary only knows how to migrate from codebases of at least version " <> show (minMigrationVersion - 1)
                  putStrLn $ "🚨 You may need to find a slightly older version of the UCM binary to bridge the gap. Please file an issue or contact the Unison team for help."
                  error $ "Migration aborted."

            let migrationsToRun = Map.filterWithKey (\v _ -> v > currentSchemaVersion) migs
            for_ (Map.toAscList migrationsToRun) $ \(SchemaVersion v, migration) -> do
              putStrLn $ "🔨 Migrating codebase to version " <> show v <> "..."
              run migration
            let ranMigrations = not (null migrationsToRun)
            pure ranMigrations
        when ranMigrations do
          region <- readMVar regionVar
          -- Vacuum once now that any migrations have taken place.
          Region.setConsoleRegion region ("✅ All good, cleaning up..." :: Text)
          case vacuumStrategy of
            Vacuum -> void $ Sqlite.Connection.vacuum conn
            NoVacuum -> pure ()
          Region.setConsoleRegion region ("🏁 Migrations complete 🏁" :: Text)

-- | If we need to make a backup,  then copy the sqlite database to a new file with a unique name based on current time.
backupCodebaseIfNecessary :: BackupStrategy -> LocalOrRemote -> Sqlite.Connection -> SchemaVersion -> SchemaVersion -> CodebasePath -> IO ()
backupCodebaseIfNecessary backupStrategy localOrRemote conn currentSchemaVersion highestKnownSchemaVersion root = do
  case (backupStrategy, localOrRemote) of
    (NoBackup, _) -> pure ()
    (_, Remote) -> pure ()
    (Backup, Local)
      | (currentSchemaVersion >= highestKnownSchemaVersion) -> pure ()
      | otherwise -> do
          backupPath <- getPOSIXTime <&> (\t -> root </> backupCodebasePath currentSchemaVersion t)
          Sqlite.vacuumInto conn backupPath
          -- vacuum-into clears the journal mode, so we need to set it again.
          Sqlite.withConnection "backup" backupPath \backupConn -> do
            Sqlite.trySetJournalMode backupConn Sqlite.JournalMode'WAL
          putStrLn ("📋 I backed up your codebase to " ++ (root </> backupPath))
          putStrLn "⚠️  Please close all other ucm processes and wait for the migration to complete before interacting with your codebase."

{-# LANGUAGE MultiWayIf #-}

module Unison.Codebase.SqliteCodebase.Migrations where

import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.Regions qualified as Region
import System.FilePath ((</>))
import Text.Printf (printf)
import U.Codebase.Reference qualified as C.Reference
import U.Codebase.Sqlite.DbId (HashVersion (..), SchemaVersion (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (CodebasePath)
import Unison.Codebase.Init (BackupStrategy (..), VacuumStrategy (..))
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (OpenCodebaseUnknownSchemaVersion))
import Unison.Codebase.Init.OpenCodebaseError qualified as Codebase
import Unison.Codebase.IntegrityCheck (IntegrityResult (..), integrityCheckAllBranches, integrityCheckAllCausals, prettyPrintIntegrityErrors)
import Unison.Codebase.SqliteCodebase.Migrations.DefinitionHashCheck (definitionHashCheck)
import Unison.Codebase.SqliteCodebase.Migrations.Helpers (abortMigration)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema11To12 (migrateSchema11To12)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2 (migrateSchema1To2)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema5To6 (migrateSchema5To6)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema6To7 (migrateSchema6To7)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema7To8 (migrateSchema7To8)
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops2
import Unison.Codebase.SqliteCodebase.Paths (backupCodebasePath)
import Unison.Codebase.Type (LocalOrRemote (..))
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Sqlite.Connection qualified as Sqlite.Connection
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as Pretty
import UnliftIO qualified

-- | Mapping from schema version to the migration required to get there.
-- E.g. The migration at index 2 must be run on a codebase at version 1.
migrations ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  TVar (Map Hash Ops2.TermBufferEntry) ->
  TVar (Map Hash Ops2.DeclBufferEntry) ->
  CodebasePath ->
  Map SchemaVersion (Sqlite.Transaction ())
migrations getDeclType termBuffer declBuffer rootCodebasePath =
  Map.fromList
    [ (2, migrateSchema1To2 getDeclType termBuffer declBuffer),
      -- The 1 to 2 migration kept around hash objects of hash version 1, unfortunately this
      -- caused an issue:
      --
      -- The migration would detect causals whose value hash did not have a corresponding branch
      -- object, this was caused by a race-condition in sync which could end up in a partial sync.
      -- When a branch object was determined to be missing, the migration would replace it with the
      -- empty branch. This worked well, but led to a situation where related parent or successors
      -- of that causal would have their hash objects mapped to the new v2 object which contained
      -- the empty branch in place of missing branches. This is fine, but, if a different codebase
      -- migrated the same branch and wasn't missing the branch in question it would migrate
      -- successfully and each database now have the same v1 hash object mapped to two distinct v2
      -- objects, which rightfully causes a crash when syncing.
      --
      -- This migration drops all the v1 hash objects to avoid this issue, since these hash objects
      -- weren't being used for anything anyways.
      sqlMigration 3 (Q.removeHashObjectsByHashingVersion (HashVersion 1)),
      (4, migrateSchema3To4),
      -- The 4 to 5 migration adds initial support for out-of-order sync i.e. Unison Share
      sqlMigration 5 Q.addTempEntityTables,
      (6, migrateSchema5To6 rootCodebasePath),
      (7, migrateSchema6To7),
      (8, migrateSchema7To8),
      -- Recreates the name lookup tables because the primary key was missing the root hash id.
      sqlMigration 9 Q.fixScopedNameLookupTables,
      sqlMigration 10 Q.addProjectTables,
      sqlMigration 11 Q.addMostRecentBranchTable,
      (12, migrateSchema11To12),
      sqlMigration 13 Q.addMostRecentNamespaceTable,
      sqlMigration 14 Q.addSquashResultTable,
      sqlMigration 15 Q.addSquashResultTableIfNotExists,
      (16, definitionHashCheck)
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
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  TVar (Map Hash Ops2.TermBufferEntry) ->
  TVar (Map Hash Ops2.DeclBufferEntry) ->
  Bool ->
  BackupStrategy ->
  VacuumStrategy ->
  Sqlite.Connection ->
  m (Either Codebase.OpenCodebaseError ())
ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer shouldPrompt backupStrategy vacuumStrategy conn =
  (liftIO . UnliftIO.try) do
    regionVar <- newEmptyMVar
    let finalizeRegion :: IO ()
        finalizeRegion =
          whenJustM (tryTakeMVar regionVar) \region -> do
            content <- Region.getConsoleRegion region
            Region.finishConsoleRegion region content

    Region.displayConsoleRegions do
      (`UnliftIO.finally` finalizeRegion) do
        let migs = migrations getDeclType termBuffer declBuffer root
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
            let migrationsToRun = Map.filterWithKey (\v _ -> v > currentSchemaVersion) migs
            -- This is a bit of a hack, hopefully we can remove this when we have a more
            -- reliable way to freeze old migration code in time.
            -- The problem is that 'saveObject' has been changed to flush temp entity tables,
            -- but old schema versions still use 'saveObject', but don't have the tables!
            -- We can create the tables no matter what, there won't be anything to flush, so
            -- everything still works as expected.
            --
            -- Hopefully we can remove this once we've got better methods of freezing migration
            -- code in time.
            when (currentSchemaVersion < 5) $ run Q.addTempEntityTables
            when (currentSchemaVersion < 6) $ run Q.addNamespaceStatsTables
            for_ (Map.toAscList migrationsToRun) $ \(SchemaVersion v, migration) -> do
              putStrLn $ "🔨 Migrating codebase to version " <> show v <> "..."
              run migration
            let ranMigrations = not (null migrationsToRun)
            when ranMigrations do
              region <-
                UnliftIO.mask_ do
                  region <- Region.openConsoleRegion Region.Linear
                  putMVar regionVar region
                  pure region
              result <- do
                -- Ideally we'd check everything here, but certain codebases are known to have objects
                -- with missing Hash Objects, we'll want to clean that up in a future migration.
                -- integrityCheckAllHashObjects,
                let checks =
                      Monoid.whenM
                        (currentSchemaVersion < 7) -- Only certain migrations actually make changes which reasonably need to be checked
                        [ integrityCheckAllBranches,
                          integrityCheckAllCausals
                        ]

                zip [(1 :: Int) ..] checks & foldMapM \(i, check) -> do
                  Region.setConsoleRegion
                    region
                    (Text.pack (printf "🕵️  Checking codebase integrity (step %d of %d)..." i (length checks)))
                  run check
              case result of
                NoIntegrityErrors -> pure ()
                IntegrityErrorDetected errs -> do
                  let msg = prettyPrintIntegrityErrors errs
                  let rendered = Pretty.toPlain 80 (Pretty.border 2 msg)
                  Region.setConsoleRegion region (Text.pack rendered)
                  run (abortMigration "Codebase integrity error detected.")
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

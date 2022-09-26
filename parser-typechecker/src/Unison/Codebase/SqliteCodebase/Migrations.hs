{-# LANGUAGE MultiWayIf #-}

module Unison.Codebase.SqliteCodebase.Migrations where

import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.Console.Regions as Region
import System.Directory (copyFile)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified U.Codebase.Reference as C.Reference
import U.Codebase.Sqlite.DbId (SchemaVersion (..))
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase (CodebasePath)
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (OpenCodebaseUnknownSchemaVersion))
import qualified Unison.Codebase.Init.OpenCodebaseError as Codebase
import Unison.Codebase.IntegrityCheck (IntegrityResult (..), integrityCheckAllBranches, integrityCheckAllCausals, prettyPrintIntegrityErrors)
import Unison.Codebase.SqliteCodebase.Migrations.Helpers (abortMigration)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2 (migrateSchema1To2)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema2To3 (migrateSchema2To3)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema4To5 (migrateSchema4To5)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema5To6 (migrateSchema5To6)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema6To7 (migrateSchema6To7)
import qualified Unison.Codebase.SqliteCodebase.Operations as Ops2
import Unison.Codebase.SqliteCodebase.Paths (backupCodebasePath, codebasePath)
import Unison.Codebase.Type (LocalOrRemote (..))
import qualified Unison.ConstructorType as CT
import Unison.Hash (Hash)
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sqlite.Connection as Sqlite.Connection
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Pretty as Pretty
import qualified UnliftIO

-- | Mapping from schema version to the migration required to get there.
-- Each migration may only be run on a schema of its immediate predecessor,
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
      (3, migrateSchema2To3),
      (4, migrateSchema3To4),
      (5, migrateSchema4To5),
      (6, migrateSchema5To6 rootCodebasePath),
      (7, migrateSchema6To7)
    ]

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
  MonadIO m =>
  LocalOrRemote ->
  CodebasePath ->
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  TVar (Map Hash Ops2.TermBufferEntry) ->
  TVar (Map Hash Ops2.DeclBufferEntry) ->
  Bool ->
  Sqlite.Connection ->
  m (Either Codebase.OpenCodebaseError ())
ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer shouldPrompt conn =
  (liftIO . UnliftIO.try) do
    regionVar <- newEmptyMVar
    let finalizeRegion :: IO ()
        finalizeRegion =
          whenJustM (tryTakeMVar regionVar) \region -> do
            content <- Region.getConsoleRegion region
            Region.finishConsoleRegion region content

    Region.displayConsoleRegions do
      (`UnliftIO.finally` finalizeRegion) do
        ranMigrations <-
          Sqlite.runWriteTransaction conn \run -> do
            schemaVersion <- run Q.schemaVersion
            let migs = migrations getDeclType termBuffer declBuffer root
            -- The highest schema that this ucm knows how to migrate to.
            let currentSchemaVersion = fst . head $ Map.toDescList migs
            when (schemaVersion > currentSchemaVersion) $ UnliftIO.throwIO $ OpenCodebaseUnknownSchemaVersion (fromIntegral schemaVersion)
            let migrationsToRun = Map.filterWithKey (\v _ -> v > schemaVersion) migs
            when (localOrRemote == Local && (not . null) migrationsToRun) $ backupCodebase root shouldPrompt
            -- This is a bit of a hack, hopefully we can remove this when we have a more
            -- reliable way to freeze old migration code in time.
            -- The problem is that 'saveObject' has been changed to flush temp entity tables,
            -- but old schema versions still use 'saveObject', but don't have the tables!
            -- We can create the tables no matter what, there won't be anything to flush, so
            -- everything still works as expected.
            --
            -- Hopefully we can remove this once we've got better methods of freezing migration
            -- code in time.
            when (schemaVersion < 5) $ run Q.addTempEntityTables
            when (schemaVersion < 6) $ run Q.addNamespaceStatsTables
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
                let checks =
                      [ -- Ideally we'd check everything here, but certain codebases are known to have objects
                        -- with missing Hash Objects, we'll want to clean that up in a future migration.
                        -- integrityCheckAllHashObjects,
                        integrityCheckAllBranches,
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
          _success <- Sqlite.Connection.vacuum conn
          Region.setConsoleRegion region ("🏁 Migrations complete 🏁" :: Text)

-- | Copy the sqlite database to a new file with a unique name based on current time.
backupCodebase :: CodebasePath -> Bool -> IO ()
backupCodebase root shouldPrompt = do
  backupPath <- backupCodebasePath <$> getPOSIXTime
  copyFile (root </> codebasePath) (root </> backupPath)
  putStrLn ("📋 I backed up your codebase to " ++ (root </> backupPath))
  putStrLn "⚠️  Please close all other ucm processes and wait for the migration to complete before interacting with your codebase."
  when shouldPrompt do
    putStrLn "Press <enter> to start the migration once all other ucm processes are shutdown..."
    void $ liftIO getLine

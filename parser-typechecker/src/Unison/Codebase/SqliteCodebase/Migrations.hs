module Unison.Codebase.SqliteCodebase.Migrations where

import Control.Concurrent.STM (TVar)
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (copyFile)
import System.FilePath ((</>))
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
import qualified Unison.Codebase.SqliteCodebase.Operations as Ops2
import Unison.Codebase.SqliteCodebase.Paths
import Unison.Codebase.Type (LocalOrRemote (..))
import qualified Unison.ConstructorType as CT
import Unison.Hash (Hash)
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sqlite.Connection as Sqlite.Connection
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
  Map SchemaVersion (Sqlite.Transaction ())
migrations getDeclType termBuffer declBuffer =
  Map.fromList
    [ (2, migrateSchema1To2 getDeclType termBuffer declBuffer),
      (3, migrateSchema2To3),
      (4, migrateSchema3To4),
      (5, migrateSchema4To5)
    ]

-- | Migrates a codebase up to the most recent version known to ucm.
-- This is a No-op if it's up to date
-- Returns an error if the schema version is newer than this ucm knows about.
ensureCodebaseIsUpToDate ::
  MonadUnliftIO m =>
  LocalOrRemote ->
  CodebasePath ->
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  TVar (Map Hash Ops2.TermBufferEntry) ->
  TVar (Map Hash Ops2.DeclBufferEntry) ->
  Sqlite.Connection ->
  m (Either Codebase.OpenCodebaseError ())
ensureCodebaseIsUpToDate localOrRemote root getDeclType termBuffer declBuffer conn =
  UnliftIO.try do
    liftIO do
      ranMigrations <-
        Sqlite.runWriteTransaction conn \run -> do
          schemaVersion <- run Q.schemaVersion
          let migs = migrations getDeclType termBuffer declBuffer
          -- The highest schema that this ucm knows how to migrate to.
          let currentSchemaVersion = fst . head $ Map.toDescList migs
          when (schemaVersion > currentSchemaVersion) $ UnliftIO.throwIO $ OpenCodebaseUnknownSchemaVersion (fromIntegral schemaVersion)
          let migrationsToRun =
                Map.filterWithKey (\v _ -> v > schemaVersion) migs
          when (localOrRemote == Local && (not . null) migrationsToRun) $ backupCodebase root
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
          for_ (Map.toAscList migrationsToRun) $ \(SchemaVersion v, migration) -> do
            putStrLn $ "🔨 Migrating codebase to version " <> show v <> "..."
            run migration
          let ranMigrations = not (null migrationsToRun)
          when ranMigrations $ do
            putStrLn $ "🕵️  Checking codebase integrity..."
            run do
              result <-
                fmap fold . sequenceA $
                  [ -- Ideally we'd check everything here, but certain codebases are known to have objects
                    -- with missing Hash Objects, we'll want to clean that up in a future migration.
                    -- integrityCheckAllHashObjects,
                    integrityCheckAllBranches,
                    integrityCheckAllCausals
                  ]
              case result of
                NoIntegrityErrors -> pure ()
                IntegrityErrorDetected errs -> do
                  let msg = prettyPrintIntegrityErrors errs
                  let rendered = Pretty.toPlain 80 (Pretty.border 2 msg)
                  Sqlite.unsafeIO $ putStrLn rendered
                  abortMigration "Codebase integrity error detected."
          pure ranMigrations
      when ranMigrations do
        -- Vacuum once now that any migrations have taken place.
        putStrLn $ "Cleaning up..."
        Sqlite.Connection.vacuum conn
        putStrLn $ "🏁 Migration complete 🏁"

-- | Copy the sqlite database to a new file with a unique name based on current time.
backupCodebase :: CodebasePath -> IO ()
backupCodebase root = do
  backupPath <- backupCodebasePath <$> getPOSIXTime
  copyFile (root </> codebasePath) (root </> backupPath)
  putStrLn ("📋 I backed up your codebase to " ++ (root </> backupPath))
  putStrLn "⚠️  Please close all other ucm processes and wait for the migration to complete before interacting with your codebase."
  putStrLn "Press <enter> to start the migration once all other ucm processes are shutdown..."
  void $ liftIO getLine

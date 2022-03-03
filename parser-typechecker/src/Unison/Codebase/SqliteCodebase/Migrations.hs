module Unison.Codebase.SqliteCodebase.Migrations where

import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (copyFile)
import System.FilePath ((</>))
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId (SchemaVersion (..))
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase (CodebasePath)
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (OpenCodebaseUnknownSchemaVersion))
import qualified Unison.Codebase.Init.OpenCodebaseError as Codebase
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2 (migrateSchema1To2)
import Unison.Codebase.SqliteCodebase.Paths
import Unison.Codebase.Type (Codebase, LocalOrRemote (..))
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Var (Var)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO

type Migration m a v = Connection -> Codebase m v a -> m ()

-- | The highest schema that this ucm knows how to migrate to.
currentSchemaVersion :: SchemaVersion
currentSchemaVersion = fst . head $ Map.toDescList (migrations @IO @Symbol @())

-- | Mapping from schema version to the migration required to get there.
-- Each migration may only be run on a schema of its immediate predecessor,
-- E.g. The migration at index 2 must be run on a codebase at version 1.
migrations :: forall m v a. (MonadUnliftIO m, Var v) => Map SchemaVersion (Migration m a v)
migrations =
  Map.fromList
    [ (2, migrateSchema1To2)
    ]

-- | Migrates a codebase up to the most recent version known to ucm.
-- This is a No-op if it's up to date
-- Returns an error if the schema version is newer than this ucm knows about.
ensureCodebaseIsUpToDate :: (MonadUnliftIO m, Var v) => LocalOrRemote -> CodebasePath -> Connection -> Codebase m v a -> m (Either Codebase.OpenCodebaseError ())
ensureCodebaseIsUpToDate localOrRemote root conn codebase = UnliftIO.try do
  schemaVersion <- runReaderT Q.schemaVersion conn
  when (schemaVersion > currentSchemaVersion) $ UnliftIO.throwIO $ OpenCodebaseUnknownSchemaVersion (fromIntegral schemaVersion)
  let migrationsToRun =
        Map.filterWithKey (\v _ -> v > schemaVersion) migrations
  when (localOrRemote == Local && (not . null) migrationsToRun) $ backupCodebase root
  for_ (Map.toAscList migrationsToRun) $ \(SchemaVersion v, migration) -> do
    liftIO . putStrLn $ "Migrating codebase to version " <> show v <> "..."
    migration conn codebase

-- | Copy the sqlite database to a new file with a unique name based on current time.
backupCodebase :: CodebasePath -> MonadIO m => m ()
backupCodebase root =
  liftIO do
    backupPath <- backupCodebasePath <$> getPOSIXTime
    copyFile (root </> codebasePath) (root </> backupPath)
    putStrLn ("📋 I backed up your codebase to " ++ (root </> backupPath))
    putStrLn "⚠️  Please close all other ucm processes and wait for the migration to complete before interacting with your codebase."
    putStrLn "Press <enter> to start the migration once all other ucm processes are shutdown..."
    void $ liftIO getLine

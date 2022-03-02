module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema2To3 (migrateSchema2To3) where

import Control.Monad.Reader
import U.Codebase.Sqlite.Connection (Connection)
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase (Codebase)
import Unison.Codebase.SqliteCodebase.Migrations.Errors (MigrationError (IncorrectStartingSchemaVersion))
import Unison.Var (Var)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO

migrateSchema2To3 :: forall a m v. (MonadUnliftIO m, Var v) => Connection -> Codebase m v a -> m (Either MigrationError ())
migrateSchema2To3 conn _ = UnliftIO.try . flip runReaderT conn $
  Q.withSavepoint "MIGRATE_SCHEMA_2_TO_3" $ \_rollback -> do
    version <- Q.schemaVersion
    when (version /= 2) $ UnliftIO.throwIO (IncorrectStartingSchemaVersion version)
    Q.removeHashObjectsByHashingVersion 1
    Q.setSchemaVersion 3

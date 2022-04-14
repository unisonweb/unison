module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Monad.Reader
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId (SchemaVersion (..))
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase (Codebase)
import Unison.Codebase.SqliteCodebase.Migrations.Errors (MigrationError (IncorrectStartingSchemaVersion))
import Unison.Prelude
import Unison.Var (Var)
import qualified UnliftIO

-- | The 3 to 4 migration adds initial support for out-of-order sync i.e. Unison Share
migrateSchema3To4 :: forall a m v. (MonadUnliftIO m, Var v) => Connection -> Codebase m v a -> m (Either MigrationError ())
migrateSchema3To4 conn _ = UnliftIO.try . flip runReaderT conn $
  Q.withSavepoint "MIGRATE_SCHEMA_3_TO_4" $ \_rollback -> do
    version <- Q.schemaVersion
    when (version /= 2) $ UnliftIO.throwIO (IncorrectStartingSchemaVersion version)
    Q.addTempEntityTables
    Q.setSchemaVersion (SchemaVersion 4)

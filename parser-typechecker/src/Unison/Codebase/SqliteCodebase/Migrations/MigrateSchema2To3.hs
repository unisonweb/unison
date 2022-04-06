module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema2To3 (migrateSchema2To3) where

import Control.Monad.Reader
import U.Codebase.Sqlite.DbId (HashVersion (..), SchemaVersion (..))
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase (Codebase)
import Unison.Codebase.SqliteCodebase.Migrations.Errors (MigrationError (IncorrectStartingSchemaVersion))
import qualified Unison.Sqlite as Sqlite
import Unison.Var (Var)
import Unison.Prelude
import qualified UnliftIO

-- | The 1 to 2 migration kept around hash objects of hash version 1, unfortunately this
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
migrateSchema2To3 :: forall a m v. (MonadUnliftIO m, Var v) => Sqlite.Connection -> Codebase m v a -> m (Either MigrationError ())
migrateSchema2To3 conn _ = UnliftIO.try . flip runReaderT conn $
  undefined
  -- Sqlite.withSavepoint "MIGRATE_SCHEMA_2_TO_3" $ \_rollback -> do
  --   version <- Q.schemaVersion
  --   when (version /= 2) $ UnliftIO.throwIO (IncorrectStartingSchemaVersion version)
  --   Q.removeHashObjectsByHashingVersion (HashVersion 1)
  --   Q.setSchemaVersion (SchemaVersion 3)

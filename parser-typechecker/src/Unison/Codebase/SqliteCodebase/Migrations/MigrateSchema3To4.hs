module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Sqlite as Sqlite

-- | The 3 to 4 migration adds initial support for out-of-order sync i.e. Unison Share
migrateSchema3To4 :: Sqlite.Transaction ()
migrateSchema3To4 = do
  Q.expectSchemaVersion 3
  Q.addTempEntityTables
  Q.setSchemaVersion 4

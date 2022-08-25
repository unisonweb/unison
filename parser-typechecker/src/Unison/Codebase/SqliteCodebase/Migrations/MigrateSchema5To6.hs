module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema5To6 (migrateSchema5To6) where

import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Sqlite as Sqlite

-- | The 5 to 6 migration adds the reflog as a table in the DB
migrateSchema5To6 :: Sqlite.Transaction ()
migrateSchema5To6 = do
  Q.expectSchemaVersion 5
  Q.addReflogTable
  Q.setSchemaVersion 6

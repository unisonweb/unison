module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema9To10 (migrateSchema9To10) where

import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Sqlite as Sqlite

migrateSchema9To10 :: Sqlite.Transaction ()
migrateSchema9To10 = do
  Queries.expectSchemaVersion 9
  Queries.addProjectTables
  Queries.setSchemaVersion 10

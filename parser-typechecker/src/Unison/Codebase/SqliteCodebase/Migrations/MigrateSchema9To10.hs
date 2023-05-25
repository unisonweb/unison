module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema9To10 (migrateSchema9To10) where

import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Sqlite qualified as Sqlite

migrateSchema9To10 :: Sqlite.Transaction ()
migrateSchema9To10 = do
  Queries.expectSchemaVersion 9
  Queries.addProjectTables
  Queries.setSchemaVersion 10

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema8To9 (migrateSchema8To9) where

import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Sqlite as Sqlite

migrateSchema8To9 :: Sqlite.Transaction ()
migrateSchema8To9 = do
  Queries.expectSchemaVersion 8
  Queries.addProjectTables
  Queries.setSchemaVersion 9

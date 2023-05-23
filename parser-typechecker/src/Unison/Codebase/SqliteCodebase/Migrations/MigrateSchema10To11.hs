module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema10To11 (migrateSchema10To11) where

import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Sqlite as Sqlite

migrateSchema10To11 :: Sqlite.Transaction ()
migrateSchema10To11 = do
  Queries.expectSchemaVersion 10
  Queries.addMostRecentBranchTable
  Queries.setSchemaVersion 11

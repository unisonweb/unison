module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema10To11 (migrateSchema10To11) where

import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Sqlite qualified as Sqlite

migrateSchema10To11 :: Sqlite.Transaction ()
migrateSchema10To11 = do
  Queries.expectSchemaVersion 10
  Queries.addMostRecentBranchTable
  Queries.setSchemaVersion 11

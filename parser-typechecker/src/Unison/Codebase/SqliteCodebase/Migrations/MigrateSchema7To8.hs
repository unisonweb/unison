-- | @004-add-project-tables.sql@
module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema7To8 (migrateSchema7To8) where

import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Sqlite as Sqlite

migrateSchema7To8 :: Sqlite.Transaction ()
migrateSchema7To8 = do
  Queries.expectSchemaVersion 7
  Queries.addProjectTables
  Queries.setSchemaVersion 8

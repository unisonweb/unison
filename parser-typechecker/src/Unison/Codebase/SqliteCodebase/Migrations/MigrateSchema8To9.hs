module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema8To9 (migrateSchema8To9) where

import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Sqlite qualified as Sqlite

-- | Recreates the name lookup tables because the primary key was missing the root hash id.
migrateSchema8To9 :: Sqlite.Transaction ()
migrateSchema8To9 = do
  Q.expectSchemaVersion 8
  Q.fixScopedNameLookupTables
  Q.setSchemaVersion 9

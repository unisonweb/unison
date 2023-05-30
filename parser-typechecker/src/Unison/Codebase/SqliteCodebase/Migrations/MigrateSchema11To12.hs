{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema11To12 (migrateSchema11To12) where

import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Debug qualified as Debug
import Unison.Sqlite qualified as Sqlite
import Prelude hiding (log)

-- | This migration just deletes all the old name lookups, it doesn't recreate them.
-- On share we'll rebuild only the required name lookups from scratch.
migrateSchema11To12 :: Sqlite.Transaction ()
migrateSchema11To12 = do
  Queries.expectSchemaVersion 11
  dropOldNameLookupTables
  deleteAllNameLookups
  Debug.debugLogM Debug.Migration "Adding name lookup mount tables"
  Queries.addNameLookupMountTables
  Queries.setSchemaVersion 12

-- | These are old name lookups from before we switched to a branch-hash keyed
-- approach. It can be dropped now to reclaim space.
dropOldNameLookupTables :: Sqlite.Transaction ()
dropOldNameLookupTables = do
  Debug.debugLogM Debug.Migration "Dropping old name lookup tables"
  Sqlite.execute2
    [Sqlite.sql2|
      DROP TABLE IF EXISTS term_name_lookup
    |]
  Sqlite.execute2
    [Sqlite.sql2|
      DROP TABLE IF EXISTS type_name_lookup
    |]

-- | These are old name lookups from before we switched to a branch-hash keyed
-- approach. It can be dropped now to reclaim space.
deleteAllNameLookups :: Sqlite.Transaction ()
deleteAllNameLookups = do
  Debug.debugLogM Debug.Migration "Deleting all name lookups"
  -- Bare deletes are optimized into table truncations by sqlite
  Sqlite.execute2
    [Sqlite.sql2|
      DELETE FROM scoped_term_name_lookup
    |]
  Sqlite.execute2
    [Sqlite.sql2|
      DELETE FROM scoped_type_name_lookup
    |]
  Sqlite.execute2
    [Sqlite.sql2|
      DELETE FROM name_lookups
    |]

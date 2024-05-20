{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema16To17 (migrateSchema16To17) where

import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Sqlite qualified as Sqlite

-- | This migration adds the causal_object_id column to the project_branches table.
migrateSchema16To17 :: Sqlite.Transaction ()
migrateSchema16To17 = do
  Queries.expectSchemaVersion 16
  error "Impelement MigrateSchema16To17.migrateSchema16To17"
  Queries.setSchemaVersion 17

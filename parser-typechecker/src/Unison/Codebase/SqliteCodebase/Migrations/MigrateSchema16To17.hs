{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema16To17 (migrateSchema16To17) where

import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Sqlite qualified as Sqlite

-- | This migration adds the causal_object_id column to the project_branches table.
migrateSchema16To17 :: Sqlite.Transaction ()
migrateSchema16To17 = do
  Q.expectSchemaVersion 16
  Q.addCurrentProjectPathTable
  Q.setSchemaVersion 17

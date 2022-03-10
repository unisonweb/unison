{-# LANGUAGE DeriveAnyClass #-}

module Unison.Codebase.SqliteCodebase.Migrations.Errors where

import U.Codebase.Sqlite.DbId (SchemaVersion)
import Unison.Prelude

data MigrationError
  = IncorrectStartingSchemaVersion SchemaVersion
  deriving (Show, Eq, Ord)
  deriving anyclass (Exception)

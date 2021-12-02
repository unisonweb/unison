module Unison.Codebase.SqliteCodebase.GitError where

import U.Codebase.Sqlite.DbId (SchemaVersion)
import Unison.Codebase.Editor.RemoteRepo (ReadRepo)
import Unison.CodebasePath (CodebasePath)

data GitSqliteCodebaseError
  = GitCouldntParseRootBranchHash ReadRepo String
  | NoDatabaseFile ReadRepo CodebasePath
  | UnrecognizedSchemaVersion ReadRepo CodebasePath SchemaVersion
  deriving (Show)

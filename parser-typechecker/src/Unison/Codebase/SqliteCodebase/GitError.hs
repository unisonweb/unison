module Unison.Codebase.SqliteCodebase.GitError where

import U.Codebase.Sqlite.DbId (SchemaVersion)
import Unison.Codebase.Editor.RemoteRepo (ReadGitRepo)
import Unison.CodebasePath (CodebasePath)

data GitSqliteCodebaseError
  = GitCouldntParseRootBranchHash ReadGitRepo String
  | NoDatabaseFile ReadGitRepo CodebasePath
  | UnrecognizedSchemaVersion ReadGitRepo CodebasePath SchemaVersion
  deriving (Show)

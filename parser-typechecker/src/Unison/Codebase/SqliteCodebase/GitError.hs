module Unison.Codebase.SqliteCodebase.GitError where

import Unison.Codebase.Editor.RemoteRepo (ReadRepo)
import Unison.CodebasePath (CodebasePath)
import U.Codebase.Sqlite.DbId (SchemaVersion)

data GitSqliteCodebaseError
  = GitCouldntParseRootBranchHash ReadRepo String
  | UnrecognizedSchemaVersion ReadRepo CodebasePath SchemaVersion
  deriving Show
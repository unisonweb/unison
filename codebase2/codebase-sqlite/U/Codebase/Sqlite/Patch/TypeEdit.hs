module U.Codebase.Sqlite.Patch.TypeEdit where

import U.Codebase.Sqlite.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

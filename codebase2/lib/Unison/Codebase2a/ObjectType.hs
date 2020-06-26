module Unison.Codebase2a.DbType where

import           Database.SQLite.Simple

-- We can only append to this list, not insert into it.
data ObjectType = Term | TermType | DataDecl | EffectDecl | Namespace | Patch
  deriving (Eq, Ord, Show, Enum)

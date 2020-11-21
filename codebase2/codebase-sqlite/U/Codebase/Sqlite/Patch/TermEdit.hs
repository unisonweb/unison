module U.Codebase.Sqlite.Patch.TermEdit where

import U.Codebase.Reference (Reference')
import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalTextId)

type TermEdit = TermEdit' Db.TextId Db.ObjectId

type LocalTermEdit = TermEdit' LocalTextId LocalDefnId

data TermEdit' t h = Replace (Reference' t h) Typing | Deprecate
  deriving (Eq, Ord, Show)

-- Replacements with the Same type can be automatically propagated.
-- Replacements with a Subtype can be automatically propagated but may result in dependents getting more general types, so requires re-inference.
-- Replacements of a Different type need to be manually propagated by the programmer.
data Typing = Same | Subtype | Different
  deriving (Eq, Ord, Show)

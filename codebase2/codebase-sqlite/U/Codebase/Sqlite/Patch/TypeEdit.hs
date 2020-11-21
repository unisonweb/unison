module U.Codebase.Sqlite.Patch.TypeEdit where

import U.Codebase.Reference (Reference')
import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalTextId)

type LocalTypeEdit = TypeEdit' LocalTextId LocalDefnId

type TypeEdit = TypeEdit' Db.TextId Db.ObjectId

data TypeEdit' t h = Replace (Reference' t h) | Deprecate
  deriving (Eq, Ord, Show)

module U.Codebase.Sqlite.Reference where

import U.Codebase.Sqlite.DbId
import U.Codebase.Reference (Reference', Id')

type Reference = Reference' TextId ObjectId
type Id = Id' ObjectId

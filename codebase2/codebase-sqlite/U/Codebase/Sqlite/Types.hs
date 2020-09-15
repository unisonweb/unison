module U.Codebase.Sqlite.Types where

import U.Codebase.Sqlite.DbId
import U.Codebase.Referent (Referent')
import U.Codebase.Reference (Reference')

type Reference = Reference' TextId ObjectId
type Referent = Referent' Reference Reference

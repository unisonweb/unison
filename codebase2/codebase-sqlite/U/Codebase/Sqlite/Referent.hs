module U.Codebase.Sqlite.Referent where

import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Referent (Referent')

type Referent = Referent' Reference Reference

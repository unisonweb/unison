module U.Codebase.Sqlite.Patch.Full where

import Data.Map (Map)
import U.Codebase.Sqlite.Types
import U.Codebase.Sqlite.Patch.TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit

data Patch = Patch {
  termEdits :: Map Referent TermEdit,
  typeEdits :: Map Reference TypeEdit
}

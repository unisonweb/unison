module U.Codebase.Sqlite.Patch.Full where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Sqlite.Patch.TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit
import U.Codebase.Sqlite.Types

data Patch = Patch
  { termEdits :: Map Referent (Set TermEdit),
    typeEdits :: Map Reference (Set TypeEdit)
  }

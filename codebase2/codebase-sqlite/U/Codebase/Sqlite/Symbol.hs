module U.Codebase.Sqlite.Symbol where

import Data.Word (Word64)
import Data.Text (Text)
data Symbol = Symbol !Word64 !Text deriving (Eq, Ord, Show)
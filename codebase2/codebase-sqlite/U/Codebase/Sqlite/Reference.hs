module U.Codebase.Sqlite.Reference where

import U.Codebase.Sqlite.DbId
import Data.Word (Word64)

data Reference = Builtin TextId | Derived Id
  deriving (Eq, Ord, Show)

data Id = Id ObjectId ComponentIndex
  deriving (Eq, Ord, Show)

newtype ComponentIndex = ComponentIndex Word64
  deriving (Eq, Ord, Show)

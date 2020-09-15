module U.Codebase.Sqlite.Referent where

import Data.Word (Word64)
import U.Codebase.Sqlite.Reference (Reference)

data Referent = Ref Reference | Con Reference ConstructorIndex
  deriving (Eq, Ord, Show)

newtype ConstructorIndex = ConstructorIndex Word64
  deriving (Eq, Ord, Show)


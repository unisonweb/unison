module Unison.Codebase2a.ObjectType where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple (SQLData(SQLInteger))


--import           Database.SQLite.Simple

-- We can only append to this list, not insert into it.
data ObjectType = Term | TermType | DataDecl | EffectDecl | Namespace | Patch
  deriving (Eq, Ord, Show, Enum)

instance ToField ObjectType where
  toField = SQLInteger . fromIntegral . fromEnum

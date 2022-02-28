module U.Codebase.Sqlite.ObjectType where

import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple (SQLData(SQLInteger))

-- |Don't reorder these, they are part of the database,
-- and the ToField and FromField implementation currently
-- depends on the derived Enum implementation.
data ObjectType
  = TermComponent -- 0
  | DeclComponent -- 1
  | Namespace -- 2
  | Patch -- 3
  deriving (Eq, Ord, Show, Enum)

instance ToField ObjectType where
  toField = SQLInteger . fromIntegral . fromEnum

instance FromField ObjectType where
  fromField = fmap toEnum . fromField

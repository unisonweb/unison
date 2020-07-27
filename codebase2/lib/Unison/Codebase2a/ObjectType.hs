{-# LANGUAGE TypeApplications #-}

module Unison.Codebase2a.ObjectType where

import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple (SQLData(SQLInteger))
import Database.SQLite.Simple.FromField (FromField(..))


--import           Database.SQLite.Simple

-- Don't reorder these, they are part of the database
data ObjectType
  = TermCycle   -- 0
  | TermType    -- 1
  | DeclCycle   -- 2
  | Namespace   -- 4
  | Patch       -- 5
  deriving (Eq, Ord, Show, Enum)

instance ToField ObjectType where
  toField = SQLInteger . fromIntegral . fromEnum

instance FromField ObjectType where
  fromField = fmap toEnum . fromField

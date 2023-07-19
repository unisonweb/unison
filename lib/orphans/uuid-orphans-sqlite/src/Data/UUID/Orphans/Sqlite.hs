{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UUID.Orphans.Sqlite () where

import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.SQLite.Simple.FromField qualified as Sqlite
import Database.SQLite.Simple.ToField qualified as Sqlite

instance Sqlite.FromField UUID where
  fromField field = do
    text <- Sqlite.fromField field
    case UUID.fromText text of
      Nothing -> Sqlite.returnError Sqlite.ConversionFailed field "invalid UUID"
      Just uuid -> pure uuid

instance Sqlite.ToField UUID where
  toField = Sqlite.toField . UUID.toText

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UUID.Orphans.Sqlite () where

import Data.UUID (UUID)
import qualified Data.UUID as UUID

import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite

instance Sqlite.FromField UUID where
  fromField field = do
    text <- Sqlite.fromField field
    case UUID.fromText text of
      Nothing -> Sqlite.returnError Sqlite.ConversionFailed field "invalid UUID"
      Just uuid -> pure uuid

instance Sqlite.ToField UUID where
  toField = Sqlite.toField . UUID.toText

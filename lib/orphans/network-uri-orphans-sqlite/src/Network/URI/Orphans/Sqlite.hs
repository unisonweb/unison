{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Orphans.Sqlite () where

import Database.SQLite.Simple.FromField qualified as Sqlite
import Database.SQLite.Simple.ToField qualified as Sqlite
import Network.URI (URI)
import Network.URI qualified as URI

instance Sqlite.FromField URI where
  fromField field = do
    string <- Sqlite.fromField field
    case URI.parseURI string of
      Nothing -> Sqlite.returnError Sqlite.ConversionFailed field "invalid URI"
      Just uri -> pure uri

instance Sqlite.ToField URI where
  toField uri = Sqlite.toField (URI.uriToString id uri "")

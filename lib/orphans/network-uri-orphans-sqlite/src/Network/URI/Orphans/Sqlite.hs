{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Orphans.Sqlite () where

import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import Network.URI (URI)
import qualified Network.URI as URI

instance Sqlite.FromField URI where
  fromField field = do
    string <- Sqlite.fromField field
    case URI.parseURI string of
      Nothing -> Sqlite.returnError Sqlite.ConversionFailed field "invalid URI"
      Just uri -> pure uri

instance Sqlite.ToField URI where
  toField uri = Sqlite.toField (URI.uriToString id uri "")

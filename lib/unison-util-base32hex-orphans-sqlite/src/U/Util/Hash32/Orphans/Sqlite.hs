{-# OPTIONS_GHC -fno-warn-orphans #-}

module U.Util.Hash32.Orphans.Sqlite () where

import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import U.Util.Base32Hex (Base32Hex (..))
import U.Util.Hash32 (Hash32 (..))

deriving via Text instance ToField Hash32

deriving via Text instance FromField Hash32

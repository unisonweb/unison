module Unison.Sqlite.DataVersion
  ( DataVersion (..),
    getDataVersion,
  )
where

import Unison.Prelude
import Unison.Sqlite.Transaction

newtype DataVersion
  = DataVersion Int64
  deriving stock (Eq)
  deriving newtype (Show)

getDataVersion :: Transaction DataVersion
getDataVersion =
  coerce @(Transaction Int64) (queryOneOne_ "PRAGMA data_version")

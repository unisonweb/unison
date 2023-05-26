{-# LANGUAGE QuasiQuotes #-}

module Unison.Sqlite.DataVersion
  ( DataVersion (..),
    getDataVersion,
  )
where

import Unison.Prelude
import Unison.Sqlite.Sql2 (sql2)
import Unison.Sqlite.Transaction

newtype DataVersion
  = DataVersion Int64
  deriving stock (Eq)
  deriving newtype (Show)

getDataVersion :: Transaction DataVersion
getDataVersion =
  coerce @(Transaction Int64) (queryOneCol2 [sql2| PRAGMA data_version |])

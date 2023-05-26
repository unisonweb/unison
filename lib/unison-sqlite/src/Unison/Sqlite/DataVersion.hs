{-# LANGUAGE QuasiQuotes #-}

module Unison.Sqlite.DataVersion
  ( DataVersion (..),
    getDataVersion,
  )
where

import Unison.Prelude
import Unison.Sqlite.Sql2 (sql)
import Unison.Sqlite.Transaction

newtype DataVersion
  = DataVersion Int64
  deriving stock (Eq)
  deriving newtype (Show)

getDataVersion :: Transaction DataVersion
getDataVersion =
  coerce @(Transaction Int64) (queryOneCol [sql| PRAGMA data_version |])

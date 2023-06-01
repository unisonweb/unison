module Unison.Sqlite.Connection.Internal
  ( Connection (..),
  )
where

import Database.SQLite.Simple qualified as Sqlite

-- | A /non-thread safe/ connection to a SQLite database.
data Connection = Connection
  { name :: String,
    file :: FilePath,
    conn :: Sqlite.Connection
  }

instance Show Connection where
  show (Connection name file _conn) =
    "Connection { name = " ++ show name ++ ", file = " ++ show file ++ " }"

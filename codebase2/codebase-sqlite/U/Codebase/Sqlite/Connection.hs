{- ORMOLU_DISABLE -}
module U.Codebase.Sqlite.Connection where

import qualified Database.SQLite.Simple as Sqlite

data Connection = Connection {name :: String, file :: FilePath, underlying :: Sqlite.Connection}

instance Show Connection where
  show (Connection name file underlying) =
    "Connection " ++ show name
      ++ (if showFile then " " ++ file else mempty)
      ++ (if showHandle then " " ++ show (Sqlite.connectionHandle underlying) else mempty)

showFile, showHandle :: Bool
showFile = False
showHandle = False

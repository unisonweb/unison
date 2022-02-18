{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Codebase.Sqlite.JournalMode where

data JournalMode = DELETE | TRUNCATE | PERSIST | MEMORY | WAL | OFF
  deriving Show

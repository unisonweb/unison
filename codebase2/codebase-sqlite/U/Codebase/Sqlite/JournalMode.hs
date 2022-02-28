module U.Codebase.Sqlite.JournalMode where

data JournalMode = DELETE | TRUNCATE | PERSIST | MEMORY | WAL | OFF
  deriving Show

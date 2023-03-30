-- | Module for exposing the backup API of SQLite3.
--
-- https://www.sqlite.org/backup.html
-- https://www.sqlite.org/c3ref/backup_finish.html
module Unison.Sqlite.Backup (backupInto) where

import qualified Data.Text as Text
import Database.SQLite3 as SQLite3
import Unison.Prelude
import qualified UnliftIO

-- | Backup a database to another database.
backupInto ::
  -- | Source .sqlite3 file path
  FilePath ->
  -- | Destination .sqlite3 file path
  FilePath ->
  IO ()
backupInto (Text.pack -> srcPath) (Text.pack -> destPath) = do
  UnliftIO.bracket (SQLite3.open srcPath) SQLite3.close $ \srcDb -> do
    UnliftIO.bracket (SQLite3.open destPath) SQLite3.close $ \destDb -> do
      UnliftIO.bracket (SQLite3.backupInit destDb destPath srcDb srcPath) SQLite3.backupFinish $ \backup -> do
        void $ SQLite3.backupStep backup (-1) -- -1 means copy the whole db at once

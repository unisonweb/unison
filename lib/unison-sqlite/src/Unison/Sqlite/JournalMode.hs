{-# LANGUAGE QuasiQuotes #-}

module Unison.Sqlite.JournalMode
  ( JournalMode (..),
    trySetJournalMode,
    SetJournalModeException (..),
  )
where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite
import Unison.Prelude
import Unison.Sqlite.Exception (SqliteExceptionReason)
import Unison.Sqlite.Sql
import Unison.Sqlite.Transaction

-- | https://www.sqlite.org/pragma.html#pragma_journal_mode
data JournalMode
  = JournalMode'DELETE
  | JournalMode'TRUNCATE
  | JournalMode'PERSIST
  | JournalMode'MEMORY
  | JournalMode'WAL
  | JournalMode'OFF
  deriving stock (Eq, Show)

journalModeFromText :: Text -> Maybe JournalMode
journalModeFromText = \case
  "DELETE" -> Just JournalMode'DELETE
  "TRUNCATE" -> Just JournalMode'TRUNCATE
  "PERSIST" -> Just JournalMode'PERSIST
  "MEMORY" -> Just JournalMode'MEMORY
  "WAL" -> Just JournalMode'WAL
  "OFF" -> Just JournalMode'OFF
  _ -> Nothing

unsafeJournalModeFromText :: HasCallStack => Text -> JournalMode
unsafeJournalModeFromText s =
  fromMaybe (error ("Unknown journal mode: " ++ Text.unpack s)) (journalModeFromText s)

trySetJournalMode :: JournalMode -> Transaction ()
trySetJournalMode mode0 = do
  queryOneRowCheck_
    (setJournalModeSql mode0)
    \(Sqlite.Only mode1s) ->
      let mode1 = unsafeJournalModeFromText mode1s
       in if mode0 /= mode1
            then
              Left
                SetJournalModeException
                  { currentJournalMode = mode1,
                    couldntSetTo = mode0
                  }
            else Right ()

setJournalModeSql :: JournalMode -> Sql
setJournalModeSql = \case
  JournalMode'DELETE -> [sql| PRAGMA journal_mode = DELETE |]
  JournalMode'TRUNCATE -> [sql| PRAGMA journal_mode = TRUNCATE |]
  JournalMode'PERSIST -> [sql| PRAGMA journal_mode = PERSIST |]
  JournalMode'MEMORY -> [sql| PRAGMA journal_mode = MEMORY |]
  JournalMode'WAL -> [sql| PRAGMA journal_mode = WAL |]
  JournalMode'OFF -> [sql| PRAGMA journal_mode = OFF |]

data SetJournalModeException = SetJournalModeException
  { currentJournalMode :: JournalMode,
    couldntSetTo :: JournalMode
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

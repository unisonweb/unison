module Unison.Sqlite.JournalMode
  ( JournalMode (..),
    trySetJournalMode,
    SetJournalModeException (..),
  )
where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite
import Unison.Prelude
import Unison.Sqlite.Connection
import Unison.Sqlite.Exception (SqliteExceptionReason)
import Unison.Sqlite.Sql

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
  "delete" -> Just JournalMode'DELETE
  "truncate" -> Just JournalMode'TRUNCATE
  "persist" -> Just JournalMode'PERSIST
  "memory" -> Just JournalMode'MEMORY
  "wal" -> Just JournalMode'WAL
  "off" -> Just JournalMode'OFF
  _ -> Nothing

unsafeJournalModeFromText :: HasCallStack => Text -> JournalMode
unsafeJournalModeFromText s =
  fromMaybe (error ("Unknown journal mode: " ++ Text.unpack s)) (journalModeFromText s)

journalModeToText :: JournalMode -> Text
journalModeToText = \case
  JournalMode'DELETE -> "delete"
  JournalMode'TRUNCATE -> "truncate"
  JournalMode'PERSIST -> "persist"
  JournalMode'MEMORY -> "memory"
  JournalMode'WAL -> "wal"
  JournalMode'OFF -> "off"

trySetJournalMode :: MonadIO m => Connection -> JournalMode -> m ()
trySetJournalMode conn mode0 = liftIO $ do
  queryOneRowCheck_
    conn
    (Sql ("PRAGMA journal_mode = " <> journalModeToText mode0))
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

data SetJournalModeException = SetJournalModeException
  { currentJournalMode :: JournalMode,
    couldntSetTo :: JournalMode
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

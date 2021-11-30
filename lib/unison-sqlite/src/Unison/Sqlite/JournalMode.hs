module Unison.Sqlite.JournalMode
  ( JournalMode (..),
    trySetJournalMode,
    SetJournalModeException (..),
  )
where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite
import Unison.Prelude
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

journalModeToText :: JournalMode -> Text
journalModeToText = \case
  JournalMode'DELETE -> "DELETE"
  JournalMode'TRUNCATE -> "TRUNCATE"
  JournalMode'PERSIST -> "PERSIST"
  JournalMode'MEMORY -> "MEMORY"
  JournalMode'WAL -> "WAL"
  JournalMode'OFF -> "OFF"

trySetJournalMode :: JournalMode -> Transaction ()
trySetJournalMode mode0 = do
  queryOneCheck_
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

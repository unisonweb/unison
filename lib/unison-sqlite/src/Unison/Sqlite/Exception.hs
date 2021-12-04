-- | Sqlite exception utils
module Unison.Sqlite.Exception
  ( SqliteException (..),
    SqliteExceptionReason,
    SomeSqliteExceptionReason (..),
    SqliteExceptionInfo (..),
    throwSqliteException,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import qualified Database.SQLite.Simple as Sqlite
import Debug.RecoverRTTI (anythingToString)
import Unison.Prelude
import UnliftIO.Exception

data SqliteExceptionInfo params connection = SqliteExceptionInfo
  { sql :: Text,
    params :: Maybe params,
    exception :: SomeSqliteExceptionReason,
    connection :: connection
  }

throwSqliteException :: Show connection => SqliteExceptionInfo params connection -> IO a
throwSqliteException SqliteExceptionInfo {connection, exception, params, sql} = do
  threadId <- myThreadId
  throwIO
    SqliteException
      { sql,
        params = maybe "" anythingToString params,
        exception,
        connection = show connection,
        threadId
      }

-- | A type that is intended to be used as additional context for a sqlite-related exception.
class (Show e, Typeable e) => SqliteExceptionReason e

instance SqliteExceptionReason Sqlite.SQLError

data SomeSqliteExceptionReason
  = forall e. SqliteExceptionReason e => SomeSqliteExceptionReason e
  deriving anyclass (SqliteExceptionReason)

instance Show SomeSqliteExceptionReason where
  show (SomeSqliteExceptionReason x) = show x

-- | A @SqliteException@ represents an exception paired with some context that resulted in the exception.
--
-- A @SqliteException@ may result from a number of different conditions:
--
-- * The underlying sqlite library threw an exception, as when establishing a connection to a non-existent database.
-- * A postcondition violation of a function like 'queryMaybe', which asserts that the resulting relation will have
--   certain number of rows,
-- * A postcondition violation of a function like 'queryListCheck', which takes a user-defined check as an argument.
--
-- A @SqliteException@ should not be inspected or used for control flow when run in a trusted environment, where the
-- database can be assumed to be uncorrupt. Rather, wherever possible, the user of this library should write code that
-- is guaranteed not to throw exceptions, by checking the necessary preconditions first. If that is not possible, it
-- should be considered a bug in this library.
--
-- When actions are run on an untrusted codebase, e.g. one downloaded from a remote server, it is sufficient to catch
-- just one exception type, @SqliteException@.
data SqliteException = SqliteException
  { sql :: Text,
    params :: String,
    -- | The inner exception. It is intentionally not 'SomeException', so that calling code cannot accidentally
    -- 'throwIO' domain-specific exception types, but must instead use a @*Check@ query variant.
    exception :: SomeSqliteExceptionReason,
    connection :: String,
    threadId :: ThreadId
  }
  deriving stock (Show)
  deriving anyclass (Exception)

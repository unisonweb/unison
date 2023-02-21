{-# LANGUAGE ImplicitParams #-}

-- | Sqlite exception utils.
module Unison.Sqlite.Exception
  ( -- * @SomeSqliteException@
    SomeSqliteException (..),
    isCantOpenException,

    -- ** @SqliteConnectException@
    SqliteConnectException (..),
    rethrowAsSqliteConnectException,

    -- ** @SqliteQueryException@
    SqliteQueryException (..),
    pattern SqliteBusyException,
    isSqliteBusyException,
    SqliteQueryExceptionInfo (..),
    throwSqliteQueryException,
    SomeSqliteExceptionReason (..),
    SqliteExceptionReason,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Data.Typeable (cast)
import qualified Database.SQLite.Simple as Sqlite
import Debug.RecoverRTTI (anythingToString)
import GHC.Stack (currentCallStack)
import Unison.Prelude
import Unison.Sqlite.Connection.Internal (Connection)
import Unison.Sqlite.Sql
import UnliftIO.Exception

------------------------------------------------------------------------------------------------------------------------
-- SomeSqliteException

-- | The root exception for all exceptions thrown by this library.
--
-- @
-- SomeException (from base)
--   └── SomeSqliteException
--         └── SqliteConnectException
--         └── SqliteQueryException
-- @
--
-- A @SomeSqliteException@ should not be inspected or used for control flow when run in a trusted environment, where the
-- database can be assumed to be uncorrupt. Rather, wherever possible, the user of this library should write code that
-- is guaranteed not to throw exceptions, by checking the necessary preconditions first. If that is not possible, it
-- should be considered a bug in this library.
--
-- When actions are run on an untrusted codebase, e.g. one downloaded from a remote server, it is sufficient to catch
-- just one exception type, @SomeSqliteException@.
data SomeSqliteException
  = forall e. Exception e => SomeSqliteException e
  deriving anyclass (Exception)

instance Show SomeSqliteException where
  show (SomeSqliteException e) = show e

isCantOpenException :: SomeSqliteException -> Bool
isCantOpenException (SomeSqliteException exception) =
  case cast exception of
    Just SqliteConnectException {exception = Sqlite.SQLError Sqlite.ErrorCan'tOpen _ _} -> True
    _ -> False

------------------------------------------------------------------------------------------------------------------------
-- SomeSqliteException
--   └── SqliteConnectException

-- | An exception thrown during establishing a connection.
data SqliteConnectException = SqliteConnectException
  { threadId :: ThreadId,
    name :: String,
    file :: FilePath,
    exception :: Sqlite.SQLError
  }
  deriving stock (Show)

instance Exception SqliteConnectException where
  toException = toException . SomeSqliteException
  fromException = fromException >=> \(SomeSqliteException e) -> cast e

rethrowAsSqliteConnectException :: String -> FilePath -> Sqlite.SQLError -> IO a
rethrowAsSqliteConnectException name file exception = do
  threadId <- myThreadId
  throwIO SqliteConnectException {exception, file, name, threadId}

------------------------------------------------------------------------------------------------------------------------
-- SomeSqliteException
--   └── SqliteQueryException

-- | A @SqliteQueryException@ represents an exception thrown during processing a query, paired with some context that
-- resulted in the exception.
--
-- A @SqliteQueryException@ may result from a number of different conditions:
--
-- * The underlying sqlite library threw an exception.
-- * A postcondition violation of a function like 'Unison.Sqlite.queryMaybeRow', which asserts that the resulting
--   relation will have certain number of rows,
-- * A postcondition violation of a function like 'Unison.Sqlite.queryListRowCheck', which takes a user-defined check as
--   an argument.
--
-- A @SqliteQueryException@ should not be inspected or used for control flow when run in a trusted environment, where
-- the database can be assumed to be uncorrupt. Rather, wherever possible, the user of this library should write code
-- that is guaranteed not to throw exceptions, by checking the necessary preconditions first. If that is not possible,
-- it should be considered a bug in this library.
--
-- When actions are run on an untrusted codebase, e.g. one downloaded from a remote server, it is sufficient to catch
-- just one exception type, @SqliteQueryException@.
data SqliteQueryException = SqliteQueryException
  { sql :: Sql,
    params :: String,
    -- | The inner exception. It is intentionally not 'SomeException', so that calling code cannot accidentally
    -- 'throwIO' domain-specific exception types, but must instead use a @*Check@ query variant.
    exception :: SomeSqliteExceptionReason,
    callStack :: [String],
    connection :: Connection,
    threadId :: ThreadId
  }
  deriving stock (Show)

instance Exception SqliteQueryException where
  toException = toException . SomeSqliteException
  fromException = fromException >=> \(SomeSqliteException e) -> cast e

pattern SqliteBusyException :: SqliteQueryException
pattern SqliteBusyException <- (isSqliteBusyException -> True)

isSqliteBusyException :: SqliteQueryException -> Bool
isSqliteBusyException SqliteQueryException {exception = SomeSqliteExceptionReason reason} =
  case cast reason of
    Just (Sqlite.SQLError Sqlite.ErrorBusy _ _) -> True
    _ -> False

data SqliteQueryExceptionInfo params = SqliteQueryExceptionInfo
  { connection :: Connection,
    sql :: Sql,
    params :: Maybe params,
    exception :: SomeSqliteExceptionReason
  }

throwSqliteQueryException :: SqliteQueryExceptionInfo params -> IO a
throwSqliteQueryException SqliteQueryExceptionInfo {connection, exception, params, sql} = do
  threadId <- myThreadId
  callStack <- currentCallStack
  throwIO
    SqliteQueryException
      { sql,
        params = maybe "" anythingToString params,
        exception,
        callStack,
        connection,
        threadId
      }

data SomeSqliteExceptionReason
  = forall e. SqliteExceptionReason e => SomeSqliteExceptionReason e
  deriving anyclass (SqliteExceptionReason)

instance Show SomeSqliteExceptionReason where
  show (SomeSqliteExceptionReason x) = show x

-- | A type that is intended to be used as additional context for a sqlite-related exception.
class (Show e, Typeable e) => SqliteExceptionReason e

instance SqliteExceptionReason Sqlite.SQLError

instance SqliteExceptionReason Void

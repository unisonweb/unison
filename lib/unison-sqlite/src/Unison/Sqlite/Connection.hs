module Unison.Sqlite.Connection
  ( -- * Connection management
    Connection (..),
    withConnection,

    -- * Executing queries

    -- ** Without results

    -- *** With parameters
    execute,
    executeMany,

    -- *** Without parameters
    execute_,

    -- ** With results

    -- *** With parameters
    queryListRow,
    queryListCol,
    queryMaybeRow,
    queryMaybeCol,
    queryOneRow,
    queryOneCol,

    -- **** With checks
    queryListRowCheck,
    queryListColCheck,
    queryMaybeRowCheck,
    queryMaybeColCheck,
    queryOneRowCheck,
    queryOneColCheck,

    -- *** Without parameters
    queryListRow_,
    queryListCol_,
    queryMaybeRow_,
    queryMaybeCol_,
    queryOneRow_,
    queryOneCol_,

    -- **** With checks
    queryListRowCheck_,
    queryListColCheck_,
    queryMaybeRowCheck_,
    queryMaybeColCheck_,
    queryOneRowCheck_,
    queryOneColCheck_,

    -- * Low-level operations
    withSavepoint,
    withStatement,

    -- * Exceptions
    SqliteException (..),
    SomeShowTypeable (..),
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite3.Direct as Sqlite (Database (..))
import Debug.RecoverRTTI (anythingToString)
import Unison.Prelude
import Unison.Sqlite.Sql
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception

-- | A /non-thread safe/ connection to a SQLite database.
data Connection = Connection
  { name :: String,
    file :: FilePath,
    conn :: Sqlite.Connection
  }

instance Show Connection where
  show (Connection name file (Sqlite.Connection (Sqlite.Database conn))) =
    "Connection " ++ show name ++ " " ++ show file ++ " " ++ show conn

-- | Perform an action with a connection to a SQLite database.
--
-- Note: the connection is created with @PRAGMA foreign_keys = ON@ automatically, to work around the fact that SQLite
-- does not automatically enforce foreign key integrity, because it elected to maintain backwards compatibility with
-- code that was written before the foreign key integrity feature was implemented.
withConnection ::
  MonadUnliftIO m =>
  -- | Connection name, for debugging.
  String ->
  -- | Path to SQLite database file.
  FilePath ->
  (Connection -> m a) ->
  m a
withConnection name file =
  bracket (openConnection name file) closeConnection

-- Open a connection to a SQLite database.
openConnection ::
  MonadIO m =>
  -- Connection name, for debugging.
  String ->
  -- Path to SQLite database file.
  FilePath ->
  m Connection
openConnection name file = do
  conn0 <- liftIO (Sqlite.open file)
  let conn = Connection {conn = conn0, file, name}
  liftIO (execute_ conn "PRAGMA foreign_keys = ON")
  pure conn

-- Close a connection opened with 'openConnection'.
closeConnection :: MonadIO m => Connection -> m ()
closeConnection (Connection _ _ conn) =
  liftIO (Sqlite.close conn)

-- Without results, with parameters

execute :: Sqlite.ToRow a => Connection -> Sql -> a -> IO ()
execute conn@(Connection _ _ conn0) s params =
  Sqlite.execute conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeShowTypeable exception,
          params = Just params,
          sql = s
        }

executeMany :: Sqlite.ToRow a => Connection -> Sql -> [a] -> IO ()
executeMany conn@(Connection _ _ conn0) s params =
  Sqlite.executeMany conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeShowTypeable exception,
          params = Just params,
          sql = s
        }

-- Without results, without parameters

execute_ :: Connection -> Sql -> IO ()
execute_ conn@(Connection _ _ conn0) s =
  Sqlite.execute_ conn0 (coerce s) `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeShowTypeable exception,
          params = Nothing,
          sql = s
        }

-- With results, with parameters, without checks

queryListRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryListRow conn@(Connection _ _ conn0) s params =
  Sqlite.query conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeShowTypeable exception,
          params = Just params,
          sql = s
        }

queryListCol :: forall a b. (Sqlite.FromField b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryListCol conn s params =
  coerce @(IO [Sqlite.Only b]) @(IO [b]) (queryListRow conn s params)

queryMaybeRow :: (Sqlite.ToRow a, Sqlite.FromRow b) => Connection -> Sql -> a -> IO (Maybe b)
queryMaybeRow conn s params =
  queryListRowCheck conn s params \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (ExpectedAtMostOneRowException (anythingToString xs))

queryMaybeCol :: forall a b. (Sqlite.ToRow a, Sqlite.FromField b) => Connection -> Sql -> a -> IO (Maybe b)
queryMaybeCol conn s params =
  coerce @(IO (Maybe (Sqlite.Only b))) @(IO (Maybe b)) (queryMaybeRow conn s params)

queryOneRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO b
queryOneRow conn s params =
  queryListRowCheck conn s params \case
    [x] -> Right x
    xs -> Left (ExpectedExactlyOneRowException (anythingToString xs))

queryOneCol :: forall a b. (Sqlite.FromField b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO b
queryOneCol conn s params = do
  coerce @(IO (Sqlite.Only b)) @(IO b) (queryOneRow conn s params)

-- With results, with parameters, with checks

queryListRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either e r) ->
  IO r
queryListRowCheck conn s params check =
  gqueryListCheck conn s params (mapLeft SomeShowTypeable . check)

gqueryListCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either SomeShowTypeable r) ->
  IO r
gqueryListCheck conn s params check = do
  xs <- queryListRow conn s params
  case check xs of
    Left exception ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception,
            params = Just params,
            sql = s
          }
    Right result -> pure result

queryListColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either e r) ->
  IO r
queryListColCheck conn s params check =
  queryListRowCheck conn s params (coerce @([b] -> Either e r) @([Sqlite.Only b] -> Either e r) check)

queryMaybeRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  IO r
queryMaybeRowCheck conn s params check =
  gqueryListCheck conn s params \case
    [] -> mapLeft SomeShowTypeable (check Nothing)
    [x] -> mapLeft SomeShowTypeable (check (Just x))
    xs -> Left (SomeShowTypeable (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  IO r
queryMaybeColCheck conn s params check =
  queryMaybeRowCheck conn s params (coerce @(Maybe b -> Either e r) @(Maybe (Sqlite.Only b) -> Either e r) check)

queryOneRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO r
queryOneRowCheck conn s params check =
  gqueryListCheck conn s params \case
    [x] -> mapLeft SomeShowTypeable (check x)
    xs -> Left (SomeShowTypeable (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO r
queryOneColCheck conn s params check =
  queryOneRowCheck conn s params (coerce @(b -> Either e r) @(Sqlite.Only b -> Either e r) check)

-- With results, without parameters, without checks

queryListRow_ :: Sqlite.FromRow a => Connection -> Sql -> IO [a]
queryListRow_ conn@(Connection _ _ conn0) s =
  Sqlite.query_ conn0 (coerce s) `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeShowTypeable exception,
          params = Nothing,
          sql = s
        }

queryListCol_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO [a]
queryListCol_ conn s =
  coerce @(IO [Sqlite.Only a]) @(IO [a]) (queryListRow_ conn s)

queryMaybeRow_ :: Sqlite.FromRow a => Connection -> Sql -> IO (Maybe a)
queryMaybeRow_ conn s =
  queryListRowCheck_ conn s \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (SomeShowTypeable (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeCol_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO (Maybe a)
queryMaybeCol_ conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybeRow_ conn s)

queryOneRow_ :: Sqlite.FromRow a => Connection -> Sql -> IO a
queryOneRow_ conn s =
  queryListRowCheck_ conn s \case
    [x] -> Right x
    xs -> Left (SomeShowTypeable (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneCol_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO a
queryOneCol_ conn s =
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOneRow_ conn s)

-- With results, without parameters, with checks

queryListRowCheck_ :: (Sqlite.FromRow a, Show e, Typeable e) => Connection -> Sql -> ([a] -> Either e r) -> IO r
queryListRowCheck_ conn s check =
  gqueryListCheck_ conn s (mapLeft SomeShowTypeable . check)

gqueryListCheck_ :: Sqlite.FromRow a => Connection -> Sql -> ([a] -> Either SomeShowTypeable r) -> IO r
gqueryListCheck_ conn s check = do
  xs <- queryListRow_ conn s
  case check xs of
    Left exception ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception,
            params = Nothing,
            sql = s
          }
    Right result -> pure result

queryListColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  ([a] -> Either e r) ->
  IO r
queryListColCheck_ conn s check =
  queryListRowCheck_ conn s (coerce @([a] -> Either e r) @([Sqlite.Only a] -> Either e r) check)

queryMaybeRowCheck_ :: (Sqlite.FromRow a, Show e, Typeable e) => Connection -> Sql -> (Maybe a -> Either e r) -> IO r
queryMaybeRowCheck_ conn s check =
  gqueryListCheck_ conn s \case
    [] -> mapLeft SomeShowTypeable (check Nothing)
    [x] -> mapLeft SomeShowTypeable (check (Just x))
    xs -> Left (SomeShowTypeable (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  (Maybe a -> Either e r) ->
  IO r
queryMaybeColCheck_ conn s check =
  queryMaybeRowCheck_ conn s (coerce @(Maybe a -> Either e r) @(Maybe (Sqlite.Only a) -> Either e r) check)

queryOneRowCheck_ :: (Sqlite.FromRow a, Show e, Typeable e) => Connection -> Sql -> (a -> Either e r) -> IO r
queryOneRowCheck_ conn s check =
  gqueryListCheck_ conn s \case
    [x] -> mapLeft SomeShowTypeable (check x)
    xs -> Left (SomeShowTypeable (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO r
queryOneColCheck_ conn s check =
  queryOneRowCheck_ conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

-- Low-level

-- | Perform an action within a named savepoint. The action is provided a rollback action.
withSavepoint :: Connection -> Text -> (IO () -> IO a) -> IO a
withSavepoint conn name action = do
  uninterruptibleMask \restore -> do
    execute_ conn (Sql ("SAVEPOINT " <> name))
    result <-
      restore (action rollback) `onException` do
        rollback
        release
    release
    pure result
  where
    rollback = execute_ conn (Sql ("ROLLBACK TO " <> name))
    release = execute_ conn (Sql ("RELEASE " <> name))

withStatement :: (Sqlite.FromRow a, Sqlite.ToRow b) => Connection -> Sql -> b -> (IO (Maybe a) -> IO c) -> IO c
withStatement conn@(Connection _ _ conn0) s params callback =
  thing `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeShowTypeable exception,
          params = Just params,
          sql = s
        }
  where
    thing =
      bracket (Sqlite.openStatement conn0 (coerce s)) Sqlite.closeStatement \statement -> do
        Sqlite.bind statement params
        callback (Sqlite.nextRow statement)

------------------------------------------------------------------------------------------------------------------------
-- Exceptions

-- | A query was expected to return exactly one row, but it did not. The exception carries a string representation of
-- the rows that were actually returned.
newtype ExpectedExactlyOneRowException = ExpectedExactlyOneRowException
  { rows :: String
  }
  deriving stock (Show)

-- | A query was expected to return exactly one row, but it did not. The exception carries a string representation of
-- the rows that were actually returned.
newtype ExpectedAtMostOneRowException = ExpectedAtMostOneRowException
  { rows :: String
  }
  deriving stock (Show)

data SqliteExceptionInfo a = SqliteExceptionInfo
  { sql :: Sql,
    params :: Maybe a,
    exception :: SomeShowTypeable,
    connection :: Connection
  }

throwSqliteException :: SqliteExceptionInfo params -> IO a
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

data SomeShowTypeable
  = forall e. (Show e, Typeable e) => SomeShowTypeable e

instance Show SomeShowTypeable where
  show (SomeShowTypeable x) = show x

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
  { sql :: Sql,
    params :: String,
    -- | The inner exception. It is intentionally not 'SomeException', so that calling code cannot accidentally
    -- 'throwIO' domain-specific exception types, but must instead use a @*Check@ query variant.
    exception :: SomeShowTypeable,
    connection :: String,
    threadId :: ThreadId
  }
  deriving stock (Show)
  deriving anyclass (Exception)

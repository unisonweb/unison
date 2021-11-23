module Unison.Sqlite.Connection
  ( -- * Connection management
    Connection (..),
    withConnection,
    openConnection,
    closeConnection,

    -- * Executing queries

    -- ** Without results

    -- *** With parameters
    execute,
    executeMany,

    -- *** Without parameters
    execute_,

    -- ** With results

    -- *** With parameters
    queryList,
    queryListOne,
    queryMaybe,
    queryMaybeOne,
    queryOne,
    queryOneOne,

    -- **** With checks
    queryOneCheck,

    -- *** Without parameters
    queryList_,
    queryListOne_,
    queryMaybe_,
    queryMaybeOne_,
    queryOne_,
    queryOneOne_,

    -- **** With checks
    queryOneCheck_,

    -- * Low-level operations
    withSavepoint,
    rollbackTo,
    withStatement,

    -- * Exceptions
    SqliteException (..),
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

-- | A non-thread safe, named connection to a SQLite database.
data Connection = Connection
  { name :: String,
    file :: FilePath,
    conn :: Sqlite.Connection
  }

instance Show Connection where
  show (Connection name file (Sqlite.Connection (Sqlite.Database conn))) =
    "Connection " ++ show name ++ " " ++ show file ++ " " ++ show conn

-- | Perform an action with a connection to a SQLite database.
withConnection :: MonadUnliftIO m => String -> FilePath -> (Connection -> m a) -> m a
withConnection name file =
  bracket (openConnection name file) closeConnection

-- | Open a connection to a SQLite database. Prefer 'withConnection'.
openConnection :: MonadIO m => String -> FilePath -> m Connection
openConnection name file = do
  conn <- liftIO (Sqlite.open file)
  liftIO (Sqlite.execute_ conn "PRAGMA foreign_keys = ON")
  pure Connection {name, file, conn}

-- | Close a connection opened with 'openConnection'.
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
          exception,
          params = Just params,
          sql = s
        }

executeMany :: Sqlite.ToRow a => Connection -> Sql -> [a] -> IO ()
executeMany conn@(Connection _ _ conn0) s params =
  Sqlite.executeMany conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception,
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
          exception,
          params = Nothing,
          sql = s
        }

-- With results, with parameters, without checks

queryList :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryList conn@(Connection _ _ conn0) s params =
  Sqlite.query conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception,
          params = Just params,
          sql = s
        }

queryListOne :: forall a b. (Sqlite.FromField b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryListOne conn s params =
  coerce @(IO [Sqlite.Only b]) @(IO [b]) (queryList conn s params)

queryMaybe :: (Sqlite.ToRow a, Sqlite.FromRow b) => Connection -> Sql -> a -> IO (Maybe b)
queryMaybe conn s params =
  queryList conn s params >>= \case
    [] -> pure Nothing
    [x] -> pure (Just x)
    xs ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = ExpectedAtMostOneRowException (anythingToString xs),
            params = Nothing,
            sql = s
          }

queryMaybeOne :: forall a b. (Sqlite.ToRow a, Sqlite.FromField b) => Connection -> Sql -> a -> IO (Maybe b)
queryMaybeOne conn s params =
  coerce @(IO (Maybe (Sqlite.Only b))) @(IO (Maybe b)) (queryMaybe conn s params)

queryOne :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO b
queryOne conn s params =
  queryList conn s params >>= \case
    [x] -> pure x
    xs ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = ExpectedExactlyOneRowException (anythingToString xs),
            params = Just params,
            sql = s
          }

queryOneOne :: forall a b. (Sqlite.FromField b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO b
queryOneOne conn s params = do
  coerce @(IO (Sqlite.Only b)) @(IO b) (queryOne conn s params)

-- With results, with parameters, with checks

queryOneCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO r
queryOneCheck conn s params check =
  queryList conn s params >>= \case
    [x] ->
      case check x of
        Left ex ->
          throwSqliteException
            SqliteExceptionInfo
              { connection = conn,
                exception = ex,
                params = Just params,
                sql = s
              }
        Right y -> pure y
    xs ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = ExpectedExactlyOneRowException (anythingToString xs),
            params = Just params,
            sql = s
          }

-- With results, without parameters, without checks

queryList_ :: Sqlite.FromRow a => Connection -> Sql -> IO [a]
queryList_ conn@(Connection _ _ conn0) s =
  Sqlite.query_ conn0 (coerce s) `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception,
          params = Nothing,
          sql = s
        }

queryListOne_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO [a]
queryListOne_ conn s =
  coerce @(IO [Sqlite.Only a]) @(IO [a]) (queryList_ conn s)

queryMaybe_ :: Sqlite.FromRow a => Connection -> Sql -> IO (Maybe a)
queryMaybe_ conn s =
  queryList_ conn s >>= \case
    [] -> pure Nothing
    [x] -> pure (Just x)
    xs ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = ExpectedAtMostOneRowException (anythingToString xs),
            params = Nothing,
            sql = s
          }

queryMaybeOne_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO (Maybe a)
queryMaybeOne_ conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybe_ conn s)

queryOne_ :: Sqlite.FromRow a => Connection -> Sql -> IO a
queryOne_ conn s =
  queryList_ conn s >>= \case
    [x] -> pure x
    xs ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = ExpectedExactlyOneRowException (anythingToString xs),
            params = Nothing,
            sql = s
          }

queryOneOne_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO a
queryOneOne_ conn s =
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOne_ conn s)

-- With results, without parameters, with checks

queryOneCheck_ :: (Sqlite.FromRow a, Show e, Typeable e) => Connection -> Sql -> (a -> Either e r) -> IO r
queryOneCheck_ conn s check =
  queryList_ conn s >>= \case
    [x] ->
      case check x of
        Left ex ->
          throwSqliteException
            SqliteExceptionInfo
              { connection = conn,
                exception = ex,
                params = Nothing,
                sql = s
              }
        Right y -> pure y
    xs ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = ExpectedExactlyOneRowException (anythingToString xs),
            params = Nothing,
            sql = s
          }

-- Low-level

withSavepoint :: Connection -> Text -> IO a -> IO a
withSavepoint conn name =
  bracket_
    (execute_ conn (Sql ("SAVEPOINT " <> name)))
    (execute_ conn (Sql ("RELEASE " <> name)))

rollbackTo :: Connection -> Text -> IO ()
rollbackTo conn name =
  execute_ conn (Sql ("ROLLBACK TO " <> name))

withStatement :: (Sqlite.FromRow a, Sqlite.ToRow b) => Connection -> Sql -> b -> (IO (Maybe a) -> IO c) -> IO c
withStatement conn@(Connection _ _ conn0) s params callback =
  thing `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception,
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

newtype ExpectedExactlyOneRowException = ExpectedExactlyOneRowException
  { rows :: String
  }
  deriving stock (Show)

newtype ExpectedAtMostOneRowException = ExpectedAtMostOneRowException
  { rows :: String
  }
  deriving stock (Show)

data SqliteExceptionInfo a e = SqliteExceptionInfo
  { sql :: Sql,
    params :: Maybe a,
    exception :: e,
    connection :: Connection
  }

throwSqliteException :: (Show exception, Typeable exception) => SqliteExceptionInfo params exception -> IO a
throwSqliteException SqliteExceptionInfo {connection, exception, params, sql} = do
  threadId <- myThreadId
  throwIO
    SqliteException
      { sql,
        params = maybe "" anythingToString params,
        exception,
        connection,
        threadId
      }

data SqliteException e = SqliteException
  { sql :: Sql,
    params :: String,
    exception :: e,
    connection :: Connection,
    threadId :: ThreadId
  }
  deriving stock (Show)
  deriving anyclass (Exception)

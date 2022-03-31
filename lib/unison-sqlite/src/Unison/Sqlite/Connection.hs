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
    withSavepointIO,
    savepoint,
    rollback,
    release,
    withStatement,

    -- * Exceptions
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
  )
where

import Data.Bifunctor (bimap)
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import Debug.RecoverRTTI (anythingToString)
import qualified Unison.Debug as Debug
import Unison.Prelude
import Unison.Sqlite.Connection.Internal (Connection (..))
import Unison.Sqlite.Exception
import Unison.Sqlite.Sql
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Exception

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
  bracket (liftIO (openConnection name file)) (liftIO . closeConnection)

-- Open a connection to a SQLite database.
openConnection ::
  -- Connection name, for debugging.
  String ->
  -- Path to SQLite database file.
  FilePath ->
  IO Connection
openConnection name file = do
  conn0 <- Sqlite.open file `catch` rethrowAsSqliteConnectException name file
  let conn = Connection {conn = conn0, file, name}
  execute_ conn "PRAGMA foreign_keys = ON"
  pure conn

-- Close a connection opened with 'openConnection'.
closeConnection :: Connection -> IO ()
closeConnection (Connection _ _ conn) =
  -- FIXME if this throws an exception, it won't be under `SomeSqliteException`
  -- Possible fixes:
  --   1. Add close exception to the hierarchy, e.g. `SqliteCloseException`
  --   2. Always ignore exceptions thrown by `close` (Mitchell prefers this one)
  Sqlite.close conn

-- An internal type, for making prettier debug logs

data Query = Query
  { sql :: Sql,
    params :: Maybe String,
    result :: Maybe String
  }

instance Show Query where
  show Query {sql, params, result} =
    concat
      [ "Query { sql = ",
        show sql,
        maybe "" (\p -> ", params = " ++ show p) params,
        maybe "" (\r -> ", results = " ++ show r) result,
        " }"
      ]

logQuery :: Sql -> Maybe a -> Maybe b -> IO ()
logQuery sql params result =
  Debug.debugM Debug.Sqlite "SQL query" (Query sql (anythingToString <$> params) (anythingToString <$> result))

-- Without results, with parameters

execute :: Sqlite.ToRow a => Connection -> Sql -> a -> IO ()
execute conn@(Connection _ _ conn0) s params = do
  logQuery s (Just params) Nothing
  Sqlite.execute conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Just params,
          sql = s
        }

executeMany :: Sqlite.ToRow a => Connection -> Sql -> [a] -> IO ()
executeMany conn@(Connection _ _ conn0) s params = do
  logQuery s (Just params) Nothing
  Sqlite.executeMany conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Just params,
          sql = s
        }

-- Without results, without parameters

execute_ :: Connection -> Sql -> IO ()
execute_ conn@(Connection _ _ conn0) s = do
  logQuery s Nothing Nothing
  Sqlite.execute_ conn0 (coerce s) `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Nothing,
          sql = s
        }

-- With results, with parameters, without checks

queryListRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryListRow conn@(Connection _ _ conn0) s params = do
  result <-
    Sqlite.query conn0 (coerce s) params
      `catch` \(exception :: Sqlite.SQLError) ->
        throwSqliteQueryException
          SqliteQueryExceptionInfo
            { connection = conn,
              exception = SomeSqliteExceptionReason exception,
              params = Just params,
              sql = s
            }
  logQuery s (Just params) (Just result)
  pure result

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
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either e r) ->
  IO r
queryListRowCheck conn s params check =
  gqueryListCheck conn s params (mapLeft SomeSqliteExceptionReason . check)

gqueryListCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either SomeSqliteExceptionReason r) ->
  IO r
gqueryListCheck conn s params check = do
  xs <- queryListRow conn s params
  case check xs of
    Left exception ->
      throwSqliteQueryException
        SqliteQueryExceptionInfo
          { connection = conn,
            exception,
            params = Just params,
            sql = s
          }
    Right result -> pure result

queryListColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either e r) ->
  IO r
queryListColCheck conn s params check =
  queryListRowCheck conn s params (coerce @([b] -> Either e r) @([Sqlite.Only b] -> Either e r) check)

queryMaybeRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO (Maybe r)
queryMaybeRowCheck conn s params check =
  gqueryListCheck conn s params \case
    [] -> pure Nothing
    [x] -> bimap SomeSqliteExceptionReason Just (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO (Maybe r)
queryMaybeColCheck conn s params check =
  queryMaybeRowCheck conn s params (coerce @(b -> Either e r) @(Sqlite.Only b -> Either e r) check)

queryOneRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO r
queryOneRowCheck conn s params check =
  gqueryListCheck conn s params \case
    [x] -> mapLeft SomeSqliteExceptionReason (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  (b -> Either e r) ->
  IO r
queryOneColCheck conn s params check =
  queryOneRowCheck conn s params (coerce @(b -> Either e r) @(Sqlite.Only b -> Either e r) check)

-- With results, without parameters, without checks

queryListRow_ :: Sqlite.FromRow a => Connection -> Sql -> IO [a]
queryListRow_ conn@(Connection _ _ conn0) s = do
  result <-
    Sqlite.query_ conn0 (coerce s)
      `catch` \(exception :: Sqlite.SQLError) ->
        throwSqliteQueryException
          SqliteQueryExceptionInfo
            { connection = conn,
              exception = SomeSqliteExceptionReason exception,
              params = Nothing,
              sql = s
            }
  logQuery s Nothing (Just result)
  pure result

queryListCol_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO [a]
queryListCol_ conn s =
  coerce @(IO [Sqlite.Only a]) @(IO [a]) (queryListRow_ conn s)

queryMaybeRow_ :: Sqlite.FromRow a => Connection -> Sql -> IO (Maybe a)
queryMaybeRow_ conn s =
  queryListRowCheck_ conn s \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeCol_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO (Maybe a)
queryMaybeCol_ conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybeRow_ conn s)

queryOneRow_ :: Sqlite.FromRow a => Connection -> Sql -> IO a
queryOneRow_ conn s =
  queryListRowCheck_ conn s \case
    [x] -> Right x
    xs -> Left (SomeSqliteExceptionReason (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneCol_ :: forall a. Sqlite.FromField a => Connection -> Sql -> IO a
queryOneCol_ conn s =
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOneRow_ conn s)

-- With results, without parameters, with checks

queryListRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Connection -> Sql -> ([a] -> Either e r) -> IO r
queryListRowCheck_ conn s check =
  gqueryListCheck_ conn s (mapLeft SomeSqliteExceptionReason . check)

gqueryListCheck_ :: Sqlite.FromRow a => Connection -> Sql -> ([a] -> Either SomeSqliteExceptionReason r) -> IO r
gqueryListCheck_ conn s check = do
  xs <- queryListRow_ conn s
  case check xs of
    Left exception ->
      throwSqliteQueryException
        SqliteQueryExceptionInfo
          { connection = conn,
            exception,
            params = Nothing,
            sql = s
          }
    Right result -> pure result

queryListColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  ([a] -> Either e r) ->
  IO r
queryListColCheck_ conn s check =
  queryListRowCheck_ conn s (coerce @([a] -> Either e r) @([Sqlite.Only a] -> Either e r) check)

queryMaybeRowCheck_ ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeRowCheck_ conn s check =
  gqueryListCheck_ conn s \case
    [] -> pure Nothing
    [x] -> bimap SomeSqliteExceptionReason Just (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeColCheck_ conn s check =
  queryMaybeRowCheck_ conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

queryOneRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Connection -> Sql -> (a -> Either e r) -> IO r
queryOneRowCheck_ conn s check =
  gqueryListCheck_ conn s \case
    [x] -> mapLeft SomeSqliteExceptionReason (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO r
queryOneColCheck_ conn s check =
  queryOneRowCheck_ conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

-- Low-level

-- | Perform an action within a named savepoint. The action is provided a rollback action.
withSavepoint :: MonadUnliftIO m => Connection -> Text -> (m () -> m a) -> m a
withSavepoint conn name action =
  withRunInIO \runInIO ->
    withSavepointIO conn name \rollback ->
      runInIO (action (liftIO rollback))

withSavepointIO :: Connection -> Text -> (IO () -> IO a) -> IO a
withSavepointIO conn name action = do
  uninterruptibleMask \restore -> do
    savepoint conn name
    result <-
      restore (action doRollback) `onException` do
        doRollback
        doRelease
    doRelease
    pure result
  where
    doRollback = rollback conn name
    doRelease = release conn name

-- | @SAVEPOINT@
savepoint :: Connection -> Text -> IO ()
savepoint conn name =
  execute_ conn (Sql ("SAVEPOINT " <> name))

-- | @ROLLBACK TO@
rollback :: Connection -> Text -> IO ()
rollback conn name =
  execute_ conn (Sql ("ROLLBACK TO " <> name))

-- | @RELEASE@
release :: Connection -> Text -> IO ()
release conn name =
  execute_ conn (Sql ("RELEASE " <> name))

withStatement :: (Sqlite.FromRow a, Sqlite.ToRow b) => Connection -> Sql -> b -> (IO (Maybe a) -> IO c) -> IO c
withStatement conn@(Connection _ _ conn0) s params callback =
  thing `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
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
  deriving anyclass (SqliteExceptionReason)

-- | A query was expected to return exactly one row, but it did not. The exception carries a string representation of
-- the rows that were actually returned.
newtype ExpectedAtMostOneRowException = ExpectedAtMostOneRowException
  { rows :: String
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

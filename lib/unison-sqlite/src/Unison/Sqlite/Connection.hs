{-# OPTIONS_GHC -Wno-deprecations #-}

module Unison.Sqlite.Connection
  ( -- * Connection management
    Connection (..),
    withConnection,

    -- * Executing queries

    -- ** Without results
    execute2,
    executeStatements,

    -- ** With results
    queryStreamRow,
    queryStreamCol,
    queryListRow2,
    queryListCol2,
    queryMaybeRow2,
    queryMaybeCol2,
    queryOneRow2,
    queryOneCol2,
    queryManyListRow,

    -- *** With checks
    queryListRowCheck,
    queryListColCheck,
    queryMaybeRowCheck2,
    queryMaybeColCheck2,
    queryOneRowCheck2,
    queryOneColCheck2,

    -- * Rows modified
    rowsModified,

    -- * Vacuum (into)
    vacuum,
    vacuumInto,

    -- * Low-level operations

    -- ** Transaction
    begin,
    beginImmediate,
    commit,
    rollback,

    -- ** Savepoint
    withSavepoint,
    withSavepointIO,
    savepoint,
    rollbackTo,
    release,

    -- * Exceptions
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
  )
where

import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.FromField qualified as Sqlite
import Database.SQLite3 qualified as Direct.Sqlite
import Debug.Pretty.Simple (pTraceShowM)
import Debug.RecoverRTTI (anythingToString)
import GHC.Stack (currentCallStack)
import Unison.Debug qualified as Debug
import Unison.Prelude
import Unison.Sqlite.Connection.Internal (Connection (..))
import Unison.Sqlite.Exception
import Unison.Sqlite.Sql
import Unison.Sqlite.Sql2 (Sql2 (..))
import UnliftIO.Exception

-- | Perform an action with a connection to a SQLite database.
--
-- Note: the connection is created with @PRAGMA foreign_keys = ON@ automatically, to work around the fact that SQLite
-- does not automatically enforce foreign key integrity, because it elected to maintain backwards compatibility with
-- code that was written before the foreign key integrity feature was implemented.
withConnection ::
  (MonadUnliftIO m) =>
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
  execute_ conn "PRAGMA busy_timeout = 60000"
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
    result :: Maybe String,
    callStack :: [String]
  }

instance Show Query where
  show Query {sql, params, result, callStack} =
    concat
      [ "Query { sql = ",
        show sql,
        maybe "" (\p -> ", params = " ++ show p) params,
        maybe "" (\r -> ", results = " ++ show r) result,
        if null callStack then "" else ", callStack = " ++ show callStack,
        " }"
      ]

logQuery :: Sql -> Maybe a -> Maybe b -> IO ()
logQuery sql params result =
  Debug.whenDebug Debug.Sqlite do
    callStack <- currentCallStack
    pTraceShowM
      Query
        { sql,
          params = anythingToString <$> params,
          result = anythingToString <$> result,
          callStack
        }

-- Will replace `Query` when `sql2` replaces `sql` everywhere.
data Query2 = Query2
  { sql :: Sql,
    params :: [Sqlite.SQLData],
    result :: Maybe String,
    callStack :: [String]
  }

instance Show Query2 where
  show Query2 {sql, params, result, callStack} =
    concat
      [ "Query { sql = ",
        show sql,
        if null params then "" else ", params = " ++ show params,
        maybe "" (\r -> ", results = " ++ show r) result,
        if null callStack then "" else ", callStack = " ++ show callStack,
        " }"
      ]

-- Will replace `logQuery` when `sql2` replaces `sql` everywhere.
logQuery2 :: Sql -> [Sqlite.SQLData] -> Maybe b -> IO ()
logQuery2 sql params result =
  Debug.whenDebug Debug.Sqlite do
    callStack <- currentCallStack
    pTraceShowM
      Query2
        { sql,
          params,
          result = anythingToString <$> result,
          callStack
        }

-- Without results, with parameters

execute :: (Sqlite.ToRow a) => Connection -> Sql -> a -> IO ()
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

execute2 :: Connection -> Sql2 -> IO ()
execute2 conn@(Connection _ _ conn0) (Sql2 s params) = do
  logQuery2 (Sql s) params Nothing
  doExecute `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException2
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Just params,
          sql = Sql s
        }
  where
    doExecute :: IO ()
    doExecute =
      Sqlite.withStatement conn0 (coerce s) \(Sqlite.Statement statement) -> do
        bindParameters statement params
        void (Direct.Sqlite.step statement)

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

-- | Execute one or more semicolon-delimited statements.
--
-- This function does not support parameters, and is mostly useful for executing DDL and migrations.
executeStatements :: Connection -> Text -> IO ()
executeStatements conn@(Connection _ _ (Sqlite.Connection database)) s = do
  logQuery (Sql s) Nothing Nothing
  Direct.Sqlite.exec database (coerce s) `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Nothing,
          sql = Sql s
        }

-- With results, without checks

queryStreamRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> (IO (Maybe b) -> IO r) -> IO r
queryStreamRow conn@(Connection _ _ conn0) s params callback =
  run `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Just params,
          sql = s
        }
  where
    run =
      bracket (Sqlite.openStatement conn0 (coerce s)) Sqlite.closeStatement \statement -> do
        Sqlite.bind statement params
        callback (Sqlite.nextRow statement)

queryStreamCol ::
  forall a b r.
  (Sqlite.FromField b, Sqlite.ToRow a) =>
  Connection ->
  Sql ->
  a ->
  (IO (Maybe b) -> IO r) ->
  IO r
queryStreamCol =
  coerce
    @(Connection -> Sql -> a -> (IO (Maybe (Sqlite.Only b)) -> IO r) -> IO r)
    @(Connection -> Sql -> a -> (IO (Maybe b) -> IO r) -> IO r)
    queryStreamRow

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

queryListRow2 :: forall a. (Sqlite.FromRow a) => Connection -> Sql2 -> IO [a]
queryListRow2 conn@(Connection _ _ conn0) (Sql2 s params) = do
  result <-
    doQuery
      `catch` \(exception :: Sqlite.SQLError) ->
        throwSqliteQueryException2
          SqliteQueryExceptionInfo
            { connection = conn,
              exception = SomeSqliteExceptionReason exception,
              params = Just params,
              sql = Sql s
            }
  logQuery2 (Sql s) params (Just result)
  pure result
  where
    doQuery :: IO [a]
    doQuery =
      Sqlite.withStatement conn0 (coerce s) \statement -> do
        bindParameters (coerce statement) params
        let loop :: [a] -> IO [a]
            loop rows =
              Sqlite.nextRow statement >>= \case
                Nothing -> pure (reverse rows)
                Just row -> loop (row : rows)
        loop []

queryListCol2 :: forall a. (Sqlite.FromField a) => Connection -> Sql2 -> IO [a]
queryListCol2 =
  coerce @(Connection -> Sql2 -> IO [Sqlite.Only a]) @(Connection -> Sql2 -> IO [a]) queryListRow2

queryMaybeRow2 :: (Sqlite.FromRow a) => Connection -> Sql2 -> IO (Maybe a)
queryMaybeRow2 conn s =
  queryListRowCheck2 conn s \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (ExpectedAtMostOneRowException (anythingToString xs))

queryMaybeCol2 :: forall a. (Sqlite.FromField a) => Connection -> Sql2 -> IO (Maybe a)
queryMaybeCol2 conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybeRow2 conn s)

queryOneRow2 :: (Sqlite.FromRow a) => Connection -> Sql2 -> IO a
queryOneRow2 conn s =
  queryListRowCheck2 conn s \case
    [x] -> Right x
    xs -> Left (ExpectedExactlyOneRowException (anythingToString xs))

queryOneCol2 :: forall a. (Sqlite.FromField a) => Connection -> Sql2 -> IO a
queryOneCol2 conn s = do
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOneRow2 conn s)

-- | Run a query many times using a prepared statement.
queryManyListRow :: forall q r. (Sqlite.ToRow q, Sqlite.FromRow r) => Connection -> Sql -> [q] -> IO [[r]]
queryManyListRow conn@(Connection _ _ conn0) s params = case params of
  [] -> pure []
  _ -> handle handler do
    logQuery s (Just params) Nothing
    Sqlite.withStatement conn0 (coerce s) \stmt -> do
      for params \p ->
        Sqlite.withBind stmt p $ exhaustQuery stmt
  where
    handler :: Sqlite.SQLError -> IO a
    handler exception =
      throwSqliteQueryException
        SqliteQueryExceptionInfo
          { connection = conn,
            exception = SomeSqliteExceptionReason exception,
            params = Just params,
            sql = s
          }
    exhaustQuery :: Sqlite.Statement -> IO [r]
    exhaustQuery stmt = do
      Sqlite.nextRow stmt >>= \case
        Just a -> (a :) <$> exhaustQuery stmt
        Nothing -> pure []

-- With results, with checks

queryListRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  ([b] -> Either e r) ->
  IO r
queryListRowCheck conn s params check =
  gqueryListCheck conn s params (mapLeft SomeSqliteExceptionReason . check)

queryListRowCheck2 ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  ([a] -> Either e r) ->
  IO r
queryListRowCheck2 conn s check =
  gqueryListCheck2 conn s (mapLeft SomeSqliteExceptionReason . check)

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

gqueryListCheck2 ::
  (Sqlite.FromRow a) =>
  Connection ->
  Sql2 ->
  ([a] -> Either SomeSqliteExceptionReason r) ->
  IO r
gqueryListCheck2 conn s@(Sql2 sql params) check = do
  xs <- queryListRow2 conn s
  case check xs of
    Left exception ->
      throwSqliteQueryException2
        SqliteQueryExceptionInfo
          { connection = conn,
            exception,
            params = Just params,
            sql = Sql sql
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

queryMaybeRowCheck2 ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeRowCheck2 conn s check =
  gqueryListCheck2 conn s \case
    [] -> pure Nothing
    [x] -> bimap SomeSqliteExceptionReason Just (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck2 ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeColCheck2 conn s check =
  queryMaybeRowCheck2 conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

queryOneRowCheck2 ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  (a -> Either e r) ->
  IO r
queryOneRowCheck2 conn s check =
  gqueryListCheck2 conn s \case
    [x] -> mapLeft SomeSqliteExceptionReason (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneColCheck2 ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  (a -> Either e r) ->
  IO r
queryOneColCheck2 conn s check =
  queryOneRowCheck2 conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

-- Rows modified

rowsModified :: Connection -> IO Int
rowsModified (Connection _ _ conn) =
  Sqlite.changes conn

-- Vacuum

-- | @VACUUM@, and return whether or not the vacuum succeeded. A vacuum fails if the connection has any open
-- transactions.
vacuum :: Connection -> IO Bool
vacuum conn =
  try (execute_ conn "VACUUM") >>= \case
    Left SqliteBusyException -> pure False
    Left exception -> throwIO exception
    Right () -> pure True

-- | @VACUUM INTO@
vacuumInto :: Connection -> FilePath -> IO ()
vacuumInto conn file =
  execute conn "VACUUM INTO ?" (Sqlite.Only file)

-- Low-level

-- | @BEGIN@
begin :: Connection -> IO ()
begin conn =
  execute_ conn "BEGIN"

-- | @BEGIN IMMEDIATE@
beginImmediate :: Connection -> IO ()
beginImmediate conn =
  execute_ conn "BEGIN IMMEDIATE"

-- | @COMMIT@
commit :: Connection -> IO ()
commit conn =
  execute_ conn "COMMIT"

-- | @ROLLBACK@
rollback :: Connection -> IO ()
rollback conn =
  execute_ conn "ROLLBACK"

-- | Perform an action within a named savepoint. The action is provided a rollback action.
withSavepoint :: (MonadUnliftIO m) => Connection -> Text -> (m () -> m a) -> m a
withSavepoint conn name action =
  withRunInIO \runInIO ->
    withSavepointIO conn name \rollback ->
      runInIO (action (liftIO rollback))

withSavepointIO :: Connection -> Text -> (IO () -> IO a) -> IO a
withSavepointIO conn name action = do
  uninterruptibleMask \restore -> do
    savepoint conn name
    result <-
      restore (action doRollbackTo) `onException` do
        doRollbackTo
        doRelease
    doRelease
    pure result
  where
    doRollbackTo = rollbackTo conn name
    doRelease = release conn name

-- | @SAVEPOINT@
savepoint :: Connection -> Text -> IO ()
savepoint conn name =
  execute_ conn (Sql ("SAVEPOINT " <> name))

-- | @ROLLBACK TO@
rollbackTo :: Connection -> Text -> IO ()
rollbackTo conn name =
  execute_ conn (Sql ("ROLLBACK TO " <> name))

-- | @RELEASE@
release :: Connection -> Text -> IO ()
release conn name =
  execute_ conn (Sql ("RELEASE " <> name))

-----------------------------------------------------------------------------------------------------------------------
-- Utils

bindParameters :: Direct.Sqlite.Statement -> [Sqlite.SQLData] -> IO ()
bindParameters statement =
  loop 1
  where
    loop :: Direct.Sqlite.ParamIndex -> [Sqlite.SQLData] -> IO ()
    loop !i = \case
      [] -> pure ()
      p : ps -> do
        Direct.Sqlite.bindSQLData statement i p
        loop (i + 1) ps

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

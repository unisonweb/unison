{-# OPTIONS_GHC -Wno-deprecations #-}

module Unison.Sqlite.Connection
  ( -- * Connection management
    Connection (..),
    withConnection,

    -- * Executing queries

    -- ** Without results

    -- *** With parameters
    execute,
    execute2,
    executeMany,

    -- *** Without parameters
    execute_,

    -- ** With results

    -- *** With parameters
    queryStreamRow,
    queryStreamCol,
    queryListRow,
    queryListRow2,
    queryListCol,
    queryListCol2,
    queryMaybeRow,
    queryMaybeRow2,
    queryMaybeCol,
    queryMaybeCol2,
    queryOneRow,
    queryOneRow2,
    queryOneCol,
    queryOneCol2,
    queryManyListRow,

    -- **** With checks
    queryListRowCheck,
    queryListColCheck,
    queryMaybeRowCheck,
    queryMaybeRowCheck2,
    queryMaybeColCheck,
    queryMaybeColCheck2,
    queryOneRowCheck,
    queryOneRowCheck2,
    queryOneColCheck,
    queryOneColCheck2,

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

import Data.Bifunctor (bimap)
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite3 as Direct.Sqlite
import Debug.Pretty.Simple (pTraceShowM)
import Debug.RecoverRTTI (anythingToString)
import GHC.Stack (currentCallStack)
import qualified Unison.Debug as Debug
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
logQuery2 :: Sql -> [Either Sqlite.SQLData [Sqlite.SQLData]] -> Maybe b -> IO ()
logQuery2 sql params result =
  Debug.whenDebug Debug.Sqlite do
    callStack <- currentCallStack
    pTraceShowM
      Query2
        { sql,
          params = flattenParameters params,
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
          params = Just (foldMap (either pure id) params),
          sql = Sql s
        }
  where
    doExecute :: IO ()
    doExecute =
      Sqlite.withStatement conn0 (coerce s) \(Sqlite.Statement statement) -> do
        bindParameters statement params
        void (Direct.Sqlite.step statement)

executeMany :: (Sqlite.ToRow a) => Connection -> Sql -> [a] -> IO ()
executeMany conn@(Connection _ _ conn0) s = \case
  [] -> pure ()
  params -> do
    logQuery s (Just params) Nothing
    Sqlite.executeMany conn0 (coerce s) params `catch` \(exception :: Sqlite.SQLError) ->
      throwSqliteQueryException
        SqliteQueryExceptionInfo
          { connection = conn,
            exception = SomeSqliteExceptionReason exception,
            params = Just params,
            sql = s
          }

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
              params = Just (flattenParameters params),
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

queryListCol :: forall a b. (Sqlite.FromField b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryListCol =
  coerce @(Connection -> Sql -> a -> IO [Sqlite.Only b]) @(Connection -> Sql -> a -> IO [b]) queryListRow

queryListCol2 :: forall a. (Sqlite.FromField a) => Connection -> Sql2 -> IO [a]
queryListCol2 =
  coerce @(Connection -> Sql2 -> IO [Sqlite.Only a]) @(Connection -> Sql2 -> IO [a]) queryListRow2

queryMaybeRow :: (Sqlite.ToRow a, Sqlite.FromRow b) => Connection -> Sql -> a -> IO (Maybe b)
queryMaybeRow conn s params =
  queryListRowCheck conn s params \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (ExpectedAtMostOneRowException (anythingToString xs))

queryMaybeRow2 :: (Sqlite.FromRow a) => Connection -> Sql2 -> IO (Maybe a)
queryMaybeRow2 conn s =
  queryListRowCheck2 conn s \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (ExpectedAtMostOneRowException (anythingToString xs))

queryMaybeCol :: forall a b. (Sqlite.ToRow a, Sqlite.FromField b) => Connection -> Sql -> a -> IO (Maybe b)
queryMaybeCol conn s params =
  coerce @(IO (Maybe (Sqlite.Only b))) @(IO (Maybe b)) (queryMaybeRow conn s params)

queryMaybeCol2 :: forall a. (Sqlite.FromField a) => Connection -> Sql2 -> IO (Maybe a)
queryMaybeCol2 conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybeRow2 conn s)

queryOneRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO b
queryOneRow conn s params =
  queryListRowCheck conn s params \case
    [x] -> Right x
    xs -> Left (ExpectedExactlyOneRowException (anythingToString xs))

queryOneRow2 :: (Sqlite.FromRow a) => Connection -> Sql2 -> IO a
queryOneRow2 conn s =
  queryListRowCheck2 conn s \case
    [x] -> Right x
    xs -> Left (ExpectedExactlyOneRowException (anythingToString xs))

queryOneCol :: forall a b. (Sqlite.FromField b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO b
queryOneCol conn s params = do
  coerce @(IO (Sqlite.Only b)) @(IO b) (queryOneRow conn s params)

queryOneCol2 :: forall a. (Sqlite.FromField a) => Connection -> Sql2 -> IO a
queryOneCol2 conn s = do
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOneRow2 conn s)

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

queryMaybeColCheck2 ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeColCheck2 conn s check =
  queryMaybeRowCheck2 conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

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

queryOneColCheck2 ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql2 ->
  (a -> Either e r) ->
  IO r
queryOneColCheck2 conn s check =
  queryOneRowCheck2 conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

-- With results, without parameters, without checks

queryListRow_ :: (Sqlite.FromRow a) => Connection -> Sql -> IO [a]
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

queryListCol_ :: forall a. (Sqlite.FromField a) => Connection -> Sql -> IO [a]
queryListCol_ conn s =
  coerce @(IO [Sqlite.Only a]) @(IO [a]) (queryListRow_ conn s)

queryMaybeRow_ :: (Sqlite.FromRow a) => Connection -> Sql -> IO (Maybe a)
queryMaybeRow_ conn s =
  queryListRowCheck_ conn s \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeCol_ :: forall a. (Sqlite.FromField a) => Connection -> Sql -> IO (Maybe a)
queryMaybeCol_ conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybeRow_ conn s)

queryOneRow_ :: (Sqlite.FromRow a) => Connection -> Sql -> IO a
queryOneRow_ conn s =
  queryListRowCheck_ conn s \case
    [x] -> Right x
    xs -> Left (SomeSqliteExceptionReason (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneCol_ :: forall a. (Sqlite.FromField a) => Connection -> Sql -> IO a
queryOneCol_ conn s =
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOneRow_ conn s)

-- With results, without parameters, with checks

queryListRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Connection -> Sql -> ([a] -> Either e r) -> IO r
queryListRowCheck_ conn s check =
  gqueryListCheck_ conn s (mapLeft SomeSqliteExceptionReason . check)

gqueryListCheck_ :: (Sqlite.FromRow a) => Connection -> Sql -> ([a] -> Either SomeSqliteExceptionReason r) -> IO r
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

bindParameters :: Direct.Sqlite.Statement -> [Either Sqlite.SQLData [Sqlite.SQLData]] -> IO ()
bindParameters statement =
  loop1 1
  where
    loop1 :: Direct.Sqlite.ParamIndex -> [Either Sqlite.SQLData [Sqlite.SQLData]] -> IO ()
    loop1 !i = \case
      [] -> pure ()
      Left p : ps -> do
        Direct.Sqlite.bindSQLData statement i p
        loop1 (i + 1) ps
      Right ps : qs -> do
        j <- loop2 i ps
        loop1 j qs
    loop2 :: Direct.Sqlite.ParamIndex -> [Sqlite.SQLData] -> IO Direct.Sqlite.ParamIndex
    loop2 !i = \case
      [] -> pure i
      p : ps -> do
        Direct.Sqlite.bindSQLData statement i p
        loop2 (i + 1) ps

-- On happy paths we can just bind all of the parameters in this Either form - no need to pay for flattening. But for
-- logging or reporting exceptions, we flatten.
flattenParameters :: [Either Sqlite.SQLData [Sqlite.SQLData]] -> [Sqlite.SQLData]
flattenParameters =
  foldMap (either pure id)

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

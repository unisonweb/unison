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
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
  )
where

import Control.Monad.IO.Unlift
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite3 as Sqlite.Direct (StepResult, step)
import qualified Database.SQLite3.Direct as Sqlite.Direct (Database (..))
import Debug.RecoverRTTI (anythingToString)
import Unison.Prelude
import Unison.Sqlite.Exception
import Unison.Sqlite.Sql
import UnliftIO.Exception (bracket, catch, mask_, onException, uninterruptibleMask)

-- | A /non-thread safe/ connection to a SQLite database.
data Connection = Connection
  { name :: String,
    file :: FilePath,
    conn :: Sqlite.Connection,
    cacheRef :: IORef (IntMap Sqlite.Statement)
  }

instance Show Connection where
  show (Connection name file (Sqlite.Connection (Sqlite.Direct.Database conn)) _) =
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
openConnection name file = liftIO do
  conn0 <- Sqlite.open file
  cacheRef <- newIORef IntMap.empty
  let conn = Connection {cacheRef, conn = conn0, file, name}
  executeRaw_ conn "PRAGMA foreign_keys = ON"
  pure conn

-- Close a connection opened with 'openConnection'.
closeConnection :: MonadIO m => Connection -> m ()
closeConnection (Connection _ _ conn _) =
  liftIO (Sqlite.close conn)

-- Without results, with parameters

execute :: Sqlite.ToRow a => Connection -> Sql -> a -> IO ()
execute conn s params =
  prepareBind conn s params \(Sqlite.Statement statement) -> do
    _ :: Sqlite.Direct.StepResult <- Sqlite.Direct.step statement
    pure ()

executeMany :: Sqlite.ToRow a => Connection -> Sql -> [a] -> IO ()
executeMany conn s params =
  prepareBinds conn s params \(Sqlite.Statement statement) -> do
    _ :: Sqlite.Direct.StepResult <- Sqlite.Direct.step statement
    pure ()

-- Without results, without parameters

execute_ :: Connection -> Sql -> IO ()
execute_ conn s =
  prepare conn s \(Sqlite.Statement statement) -> do
    _ :: Sqlite.Direct.StepResult <- Sqlite.Direct.step statement
    pure ()

executeRaw_ :: Connection -> Text -> IO ()
executeRaw_ conn@(Connection _ _ conn0 _) s =
  Sqlite.execute_ conn0 (Sqlite.Query s) `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteException
      SqliteExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          params = Nothing,
          sql = s
        }

-- With results, with parameters, without checks

queryListRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Connection -> Sql -> a -> IO [b]
queryListRow conn s params =
  withStatement conn s params drawRows

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
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception,
            params = Just params,
            sql = sqlToText s
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
  (Maybe b -> Either e r) ->
  IO r
queryMaybeRowCheck conn s params check =
  gqueryListCheck conn s params \case
    [] -> mapLeft SomeSqliteExceptionReason (check Nothing)
    [x] -> mapLeft SomeSqliteExceptionReason (check (Just x))
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck ::
  forall a b e r.
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  IO r
queryMaybeColCheck conn s params check =
  queryMaybeRowCheck conn s params (coerce @(Maybe b -> Either e r) @(Maybe (Sqlite.Only b) -> Either e r) check)

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
queryListRow_ conn s =
  withStatement_ conn s drawRows

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
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception,
            params = Nothing,
            sql = sqlToText s
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

queryMaybeRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Connection -> Sql -> (Maybe a -> Either e r) -> IO r
queryMaybeRowCheck_ conn s check =
  gqueryListCheck_ conn s \case
    [] -> mapLeft SomeSqliteExceptionReason (check Nothing)
    [x] -> mapLeft SomeSqliteExceptionReason (check (Just x))
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck_ ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (Maybe a -> Either e r) ->
  IO r
queryMaybeColCheck_ conn s check =
  queryMaybeRowCheck_ conn s (coerce @(Maybe a -> Either e r) @(Maybe (Sqlite.Only a) -> Either e r) check)

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
withSavepoint :: Connection -> Text -> (IO () -> IO a) -> IO a
withSavepoint conn name action = do
  uninterruptibleMask \restore -> do
    executeRaw_ conn ("SAVEPOINT " <> name)
    result <-
      restore (action rollback) `onException` do
        rollback
        release
    release
    pure result
  where
    rollback = executeRaw_ conn ("ROLLBACK TO " <> name)
    release = executeRaw_ conn ("RELEASE " <> name)

withStatement :: (Sqlite.FromRow a, Sqlite.ToRow b) => Connection -> Sql -> b -> (IO (Maybe a) -> IO c) -> IO c
withStatement conn s params callback =
  prepareBind conn s params \statement ->
    -- FIXME sqlite-simple's withRow is needlessly slow, it calls 'length' on every row
    callback (Sqlite.nextRow statement)

withStatement_ :: Sqlite.FromRow a => Connection -> Sql -> (IO (Maybe a) -> IO c) -> IO c
withStatement_ conn s callback =
  prepare conn s \statement ->
    callback (Sqlite.nextRow statement)

------------------------------------------------------------------------------------------------------------------------
-- Statement cache

-- TODO rename
prepare :: Connection -> Sql -> (Sqlite.Statement -> IO a) -> IO a
prepare conn s action = do
  catch
    ( do
        statement <- openStatement conn s
        action statement
    )
    \(exception :: Sqlite.SQLError) ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = SomeSqliteExceptionReason exception,
            params = Nothing,
            sql = sqlToText s
          }

prepareBind :: Sqlite.ToRow params => Connection -> Sql -> params -> (Sqlite.Statement -> IO a) -> IO a
prepareBind conn s params action = do
  catch
    ( do
        statement <- openStatement conn s
        Sqlite.bind statement params
        result <- action statement
        Sqlite.reset statement
        pure result
    )
    \(exception :: Sqlite.SQLError) ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = SomeSqliteExceptionReason exception,
            params = Just params,
            sql = sqlToText s
          }

prepareBinds :: Sqlite.ToRow params => Connection -> Sql -> [params] -> (Sqlite.Statement -> IO ()) -> IO ()
prepareBinds conn s paramss action = do
  statement <-
    openStatement conn s `catch` \(exception :: Sqlite.SQLError) ->
      throwSqliteException
        SqliteExceptionInfo
          { connection = conn,
            exception = SomeSqliteExceptionReason exception,
            params = Nothing,
            sql = sqlToText s
          }
  for_ paramss \params -> do
    catch
      ( do
          Sqlite.bind statement params
          result <- action statement
          Sqlite.reset statement
          pure result
      )
      \(exception :: Sqlite.SQLError) ->
        throwSqliteException
          SqliteExceptionInfo
            { connection = conn,
              exception = SomeSqliteExceptionReason exception,
              params = Just params,
              sql = sqlToText s
            }

openStatement :: Connection -> Sql -> IO Sqlite.Statement
openStatement Connection {cacheRef, conn} Sql {uniqueId, string} = do
  cache <- readIORef cacheRef
  case IntMap.lookup uniqueId cache of
    Nothing ->
      mask_ do
        statement <- Sqlite.openStatement conn (Sqlite.Query string)
        writeIORef cacheRef $! IntMap.insert uniqueId statement cache
        pure statement
    Just statement -> pure statement

drawRows :: Sqlite.FromRow a => IO (Maybe a) -> IO [a]
drawRows get =
  loop []
  where
    loop acc =
      get >>= \case
        Nothing -> pure (reverse acc)
        Just row -> loop (row : acc)

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

-- | A type class interface to SQLite.
module Unison.Sqlite.DB
  ( -- * Type-class
    DB,
    runDB,
    runTransaction,

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
  )
where

import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import Unison.Prelude
import Unison.Sqlite.Connection (Connection)
import qualified Unison.Sqlite.Connection as Connection
import Unison.Sqlite.Exception (SqliteExceptionReason)
import Unison.Sqlite.Sql (Sql (..))
import Unison.Sqlite.Transaction (Transaction)
import qualified Unison.Sqlite.Transaction as Transaction

type DB m =
  (MonadIO m, MonadReader Connection m)

runDB :: MonadIO m => Connection -> ReaderT Connection m a -> m a
runDB conn action =
  runReaderT action conn

runTransaction :: DB m => Transaction a -> m a
runTransaction transaction = do
  conn <- ask
  Transaction.runTransaction conn transaction

-- Without results, with parameters

execute :: (DB m, Sqlite.ToRow a) => Sql -> a -> m ()
execute s params = do
  conn <- ask
  liftIO (Connection.execute conn s params)

executeMany :: (DB m, Sqlite.ToRow a) => Sql -> [a] -> m ()
executeMany s params = do
  conn <- ask
  liftIO (Connection.executeMany conn s params)

-- Without results, without parameters

execute_ :: DB m => Sql -> m ()
execute_ s = do
  conn <- ask
  liftIO (Connection.execute_ conn s)

-- With results, with parameters, without checks

queryListRow :: (DB m, Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> m [a]
queryListRow s params = do
  conn <- ask
  liftIO (Connection.queryListRow conn s params)

queryListCol :: (DB m, Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> m [a]
queryListCol s params = do
  conn <- ask
  liftIO (Connection.queryListCol conn s params)

queryMaybeRow :: (DB m, Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> m (Maybe a)
queryMaybeRow s params = do
  conn <- ask
  liftIO (Connection.queryMaybeRow conn s params)

queryMaybeCol :: (DB m, Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> m (Maybe a)
queryMaybeCol s params = do
  conn <- ask
  liftIO (Connection.queryMaybeCol conn s params)

queryOneRow :: (DB m, Sqlite.FromRow b, Sqlite.ToRow a) => Sql -> a -> m b
queryOneRow s params = do
  conn <- ask
  liftIO (Connection.queryOneRow conn s params)

queryOneCol :: (DB m, Sqlite.FromField b, Sqlite.ToRow a) => Sql -> a -> m b
queryOneCol s params = do
  conn <- ask
  liftIO (Connection.queryOneCol conn s params)

-- With results, with parameters, with checks

queryListRowCheck ::
  (DB m, Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  ([b] -> Either e r) ->
  m r
queryListRowCheck s params check = do
  conn <- ask
  liftIO (Connection.queryListRowCheck conn s params check)

queryListColCheck ::
  (DB m, Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  ([b] -> Either e r) ->
  m r
queryListColCheck s params check = do
  conn <- ask
  liftIO (Connection.queryListColCheck conn s params check)

queryMaybeRowCheck ::
  (DB m, Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  m r
queryMaybeRowCheck s params check = do
  conn <- ask
  liftIO (Connection.queryMaybeRowCheck conn s params check)

queryMaybeColCheck ::
  (DB m, Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  m r
queryMaybeColCheck s params check = do
  conn <- ask
  liftIO (Connection.queryMaybeColCheck conn s params check)

queryOneRowCheck ::
  (DB m, Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  m r
queryOneRowCheck s params check = do
  conn <- ask
  liftIO (Connection.queryOneRowCheck conn s params check)

queryOneColCheck ::
  (DB m, Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  m r
queryOneColCheck s params check = do
  conn <- ask
  liftIO (Connection.queryOneColCheck conn s params check)

-- With results, without parameters, without checks

queryListRow_ :: (DB m, Sqlite.FromRow a) => Sql -> m [a]
queryListRow_ s = do
  conn <- ask
  liftIO (Connection.queryListRow_ conn s)

queryListCol_ :: (DB m, Sqlite.FromField a) => Sql -> m [a]
queryListCol_ s = do
  conn <- ask
  liftIO (Connection.queryListCol_ conn s)

queryMaybeRow_ :: (DB m, Sqlite.FromRow a) => Sql -> m (Maybe a)
queryMaybeRow_ s = do
  conn <- ask
  liftIO (Connection.queryMaybeRow_ conn s)

queryMaybeCol_ :: (DB m, Sqlite.FromField a) => Sql -> m (Maybe a)
queryMaybeCol_ s = do
  conn <- ask
  liftIO (Connection.queryMaybeCol_ conn s)

queryOneRow_ :: (DB m, Sqlite.FromRow a) => Sql -> m a
queryOneRow_ s = do
  conn <- ask
  liftIO (Connection.queryOneRow_ conn s)

queryOneCol_ :: (DB m, Sqlite.FromField a) => Sql -> m a
queryOneCol_ s = do
  conn <- ask
  liftIO (Connection.queryOneCol_ conn s)

-- With results, without parameters, with checks

queryListRowCheck_ :: (DB m, Sqlite.FromRow a, SqliteExceptionReason e) => Sql -> ([a] -> Either e r) -> m r
queryListRowCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryListRowCheck_ conn s check)

queryListColCheck_ :: (DB m, Sqlite.FromField a, SqliteExceptionReason e) => Sql -> ([a] -> Either e r) -> m r
queryListColCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryListColCheck_ conn s check)

queryMaybeRowCheck_ :: (DB m, Sqlite.FromRow a, SqliteExceptionReason e) => Sql -> (Maybe a -> Either e r) -> m r
queryMaybeRowCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryMaybeRowCheck_ conn s check)

queryMaybeColCheck_ :: (DB m, Sqlite.FromField a, SqliteExceptionReason e) => Sql -> (Maybe a -> Either e r) -> m r
queryMaybeColCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryMaybeColCheck_ conn s check)

queryOneRowCheck_ :: (DB m, Sqlite.FromRow a, SqliteExceptionReason e) => Sql -> (a -> Either e r) -> m r
queryOneRowCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryOneRowCheck_ conn s check)

queryOneColCheck_ :: (DB m, Sqlite.FromField a, SqliteExceptionReason e) => Sql -> (a -> Either e r) -> m r
queryOneColCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryOneColCheck_ conn s check)

-- Low-level

-- | Perform an action within a named savepoint. The action is provided a rollback action.
withSavepoint :: (DB m, MonadUnliftIO m) => Text -> (m () -> m a) -> m a
withSavepoint name action = do
  conn <- ask
  withRunInIO \unlift ->
    liftIO (Connection.withSavepoint conn name (unlift . action . liftIO))

withStatement :: (DB m, MonadUnliftIO m, Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> (m (Maybe a) -> m c) -> m c
withStatement s params callback = do
  conn <- ask
  withRunInIO \unlift ->
    Connection.withStatement conn s params (unlift . callback . liftIO)

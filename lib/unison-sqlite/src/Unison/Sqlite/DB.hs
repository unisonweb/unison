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
    queryList,
    queryListOne,
    queryMaybe,
    queryMaybeOne,
    queryOne,
    queryOneOne,

    -- **** With checks
    queryListCheck,
    queryListOneCheck,
    queryMaybeCheck,
    queryMaybeOneCheck,
    queryOneCheck,
    queryOneOneCheck,

    -- *** Without parameters
    queryList_,
    queryListOne_,
    queryMaybe_,
    queryMaybeOne_,
    queryOne_,
    queryOneOne_,

    -- **** With checks
    queryListCheck_,
    queryListOneCheck_,
    queryMaybeCheck_,
    queryMaybeOneCheck_,
    queryOneCheck_,
    queryOneOneCheck_,

    -- * Low-level operations
    withSavepoint,
    rollbackTo,
    withStatement,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import Unison.Prelude
import Unison.Sqlite.Connection (Connection)
import qualified Unison.Sqlite.Connection as Connection
import Unison.Sqlite.Sql (Sql (..))
import Unison.Sqlite.Transaction (Transaction)
import qualified Unison.Sqlite.Transaction as Transaction
import UnliftIO.Exception (bracket_)

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

queryList :: (DB m, Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> m [a]
queryList s params = do
  conn <- ask
  liftIO (Connection.queryList conn s params)

queryListOne :: (DB m, Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> m [a]
queryListOne s params = do
  conn <- ask
  liftIO (Connection.queryListOne conn s params)

queryMaybe :: (DB m, Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> m (Maybe a)
queryMaybe s params = do
  conn <- ask
  liftIO (Connection.queryMaybe conn s params)

queryMaybeOne :: (DB m, Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> m (Maybe a)
queryMaybeOne s params = do
  conn <- ask
  liftIO (Connection.queryMaybeOne conn s params)

queryOne :: (DB m, Sqlite.FromRow b, Sqlite.ToRow a) => Sql -> a -> m b
queryOne s params = do
  conn <- ask
  liftIO (Connection.queryOne conn s params)

queryOneOne :: (DB m, Sqlite.FromField b, Sqlite.ToRow a) => Sql -> a -> m b
queryOneOne s params = do
  conn <- ask
  liftIO (Connection.queryOneOne conn s params)

-- With results, with parameters, with checks

queryListCheck ::
  (DB m, Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  ([b] -> Either e r) ->
  m r
queryListCheck s params check = do
  conn <- ask
  liftIO (Connection.queryListCheck conn s params check)

queryListOneCheck ::
  (DB m, Sqlite.FromField b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  ([b] -> Either e r) ->
  m r
queryListOneCheck s params check = do
  conn <- ask
  liftIO (Connection.queryListOneCheck conn s params check)

queryMaybeCheck ::
  (DB m, Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  m r
queryMaybeCheck s params check = do
  conn <- ask
  liftIO (Connection.queryMaybeCheck conn s params check)

queryMaybeOneCheck ::
  (DB m, Sqlite.FromField b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  (Maybe b -> Either e r) ->
  m r
queryMaybeOneCheck s params check = do
  conn <- ask
  liftIO (Connection.queryMaybeOneCheck conn s params check)

queryOneCheck ::
  (DB m, Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  m r
queryOneCheck s params check = do
  conn <- ask
  liftIO (Connection.queryOneCheck conn s params check)

queryOneOneCheck ::
  (DB m, Sqlite.FromField b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  m r
queryOneOneCheck s params check = do
  conn <- ask
  liftIO (Connection.queryOneOneCheck conn s params check)

-- With results, without parameters, without checks

queryList_ :: (DB m, Sqlite.FromRow a) => Sql -> m [a]
queryList_ s = do
  conn <- ask
  liftIO (Connection.queryList_ conn s)

queryListOne_ :: (DB m, Sqlite.FromField a) => Sql -> m [a]
queryListOne_ s = do
  conn <- ask
  liftIO (Connection.queryListOne_ conn s)

queryMaybe_ :: (DB m, Sqlite.FromRow a) => Sql -> m (Maybe a)
queryMaybe_ s = do
  conn <- ask
  liftIO (Connection.queryMaybe_ conn s)

queryMaybeOne_ :: (DB m, Sqlite.FromField a) => Sql -> m (Maybe a)
queryMaybeOne_ s = do
  conn <- ask
  liftIO (Connection.queryMaybeOne_ conn s)

queryOne_ :: (DB m, Sqlite.FromRow a) => Sql -> m a
queryOne_ s = do
  conn <- ask
  liftIO (Connection.queryOne_ conn s)

queryOneOne_ :: (DB m, Sqlite.FromField a) => Sql -> m a
queryOneOne_ s = do
  conn <- ask
  liftIO (Connection.queryOneOne_ conn s)

-- With results, without parameters, with checks

queryListCheck_ :: (DB m, (Sqlite.FromRow a, Show e, Typeable e)) => Sql -> ([a] -> Either e r) -> m r
queryListCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryListCheck_ conn s check)

queryListOneCheck_ :: (DB m, (Sqlite.FromField a, Show e, Typeable e)) => Sql -> ([a] -> Either e r) -> m r
queryListOneCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryListOneCheck_ conn s check)

queryMaybeCheck_ :: (DB m, (Sqlite.FromRow a, Show e, Typeable e)) => Sql -> (Maybe a -> Either e r) -> m r
queryMaybeCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryMaybeCheck_ conn s check)

queryMaybeOneCheck_ :: (DB m, (Sqlite.FromField a, Show e, Typeable e)) => Sql -> (Maybe a -> Either e r) -> m r
queryMaybeOneCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryMaybeOneCheck_ conn s check)

queryOneCheck_ :: (DB m, (Sqlite.FromRow a, Show e, Typeable e)) => Sql -> (a -> Either e r) -> m r
queryOneCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryOneCheck_ conn s check)

queryOneOneCheck_ :: (DB m, (Sqlite.FromField a, Show e, Typeable e)) => Sql -> (a -> Either e r) -> m r
queryOneOneCheck_ s check = do
  conn <- ask
  liftIO (Connection.queryOneOneCheck_ conn s check)

-- Low-level

withSavepoint :: (DB m, MonadUnliftIO m) => Text -> m a -> m a
withSavepoint name action = do
  conn <- ask
  bracket_
    (liftIO (Connection.execute_ conn (Sql ("SAVEPOINT " <> name))))
    (liftIO (Connection.execute_ conn (Sql ("RELEASE " <> name))))
    action

rollbackTo :: DB m => Text -> m ()
rollbackTo name = do
  conn <- ask
  liftIO (Connection.execute_ conn (Sql ("ROLLBACK TO " <> name)))

withStatement :: (DB m, MonadUnliftIO m, Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> (m (Maybe a) -> m c) -> m c
withStatement s params callback = do
  conn <- ask
  withRunInIO \runInIO ->
    Connection.withStatement conn s params (runInIO . callback . liftIO)

module Unison.Sqlite.Transaction
  ( -- * Transaction management
    Transaction,
    runTransaction,
    savepoint,
    idempotentIO,

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
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (fromException), onException, throwIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified System.Random as Random
import Unison.Prelude
import Unison.Sqlite.Connection (Connection (..))
import qualified Unison.Sqlite.Connection as Connection
import Unison.Sqlite.Exception (SqliteExceptionReason, SqliteQueryException, pattern SqliteBusyException)
import Unison.Sqlite.Sql
import UnliftIO.Exception (catchAny, trySyncOrAsync, uninterruptibleMask)

newtype Transaction a
  = Transaction (Connection -> IO a)
  -- Omit MonadIO instance because transactions may be retried
  -- Omit MonadThrow instance so we always throw SqliteException (via *Check) with lots of context
  deriving (Applicative, Functor, Monad) via (ReaderT Connection IO)

-- | Run a transaction on the given connection.
runTransaction :: MonadIO m => Connection -> Transaction a -> m a
runTransaction conn (Transaction f) = liftIO do
  uninterruptibleMask \restore -> do
    Connection.execute_ conn "BEGIN"
    result <-
      -- Catch all exceptions (sync or async), because we want to ROLLBACK the BEGIN no matter what.
      trySyncOrAsync @_ @SomeException (restore (f conn)) >>= \case
        Left exception -> do
          ignoringExceptions rollback
          case fromException exception of
            Just SqliteBusyException ->
              let loop microseconds = do
                    restore (threadDelay microseconds)
                    try @_ @SqliteQueryException (Connection.execute_ conn "BEGIN IMMEDIATE") >>= \case
                      Left SqliteBusyException -> loop (microseconds * 2)
                      Left exception -> throwIO exception
                      Right () -> restore (f conn) `onException` ignoringExceptions rollback
               in loop 100_000
            _ -> throwIO exception
        Right result -> pure result
    Connection.execute_ conn "COMMIT"
    pure result
  where
    rollback :: IO ()
    rollback =
      Connection.execute_ conn "ROLLBACK"

    ignoringExceptions :: IO () -> IO ()
    ignoringExceptions action =
      action `catchAny` \_ -> pure ()

-- | Perform an atomic sub-computation within a transaction; if it returns 'Left', it's rolled back.
savepoint :: Transaction (Either a a) -> Transaction a
savepoint (Transaction action) = do
  Transaction \conn -> do
    -- Generate a random name for the savepoint, so the caller isn't burdened with coming up with a name. Seems
    -- extremely unlikely for this to go wrong (i.e. some super nested withSavepoint call that ends up generating the
    -- same savepoint name twice in a single scope).
    name <- Text.pack <$> replicateM 10 (Random.randomRIO ('a', 'z'))
    Connection.withSavepointIO conn name \rollback ->
      action conn >>= \case
        Left result -> do
          rollback
          pure result
        Right result -> pure result

-- | Perform IO inside a transaction, which should be idempotent, because it may be run more than once.
idempotentIO :: IO a -> Transaction a
idempotentIO action =
  Transaction \_ -> action

-- Without results, with parameters

execute :: Sqlite.ToRow a => Sql -> a -> Transaction ()
execute s params = do
  Transaction \conn -> Connection.execute conn s params

executeMany :: Sqlite.ToRow a => Sql -> [a] -> Transaction ()
executeMany s params =
  Transaction \conn -> Connection.executeMany conn s params

-- Without results, without parameters

execute_ :: Sql -> Transaction ()
execute_ s =
  Transaction \conn -> Connection.execute_ conn s

-- With results, with parameters, without checks

queryListRow :: (Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> Transaction [a]
queryListRow s params =
  Transaction \conn -> Connection.queryListRow conn s params

queryListCol :: (Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> Transaction [a]
queryListCol s params =
  Transaction \conn -> Connection.queryListCol conn s params

queryMaybeRow :: (Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> Transaction (Maybe a)
queryMaybeRow s params =
  Transaction \conn -> Connection.queryMaybeRow conn s params

queryMaybeCol :: (Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> Transaction (Maybe a)
queryMaybeCol s params =
  Transaction \conn -> Connection.queryMaybeCol conn s params

queryOneRow :: (Sqlite.FromRow b, Sqlite.ToRow a) => Sql -> a -> Transaction b
queryOneRow s params =
  Transaction \conn -> Connection.queryOneRow conn s params

queryOneCol :: (Sqlite.FromField b, Sqlite.ToRow a) => Sql -> a -> Transaction b
queryOneCol s params =
  Transaction \conn -> Connection.queryOneCol conn s params

-- With results, with parameters, with checks

queryListRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  ([b] -> Either e r) ->
  Transaction r
queryListRowCheck s params check =
  Transaction \conn -> Connection.queryListRowCheck conn s params check

queryListColCheck ::
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  ([b] -> Either e r) ->
  Transaction r
queryListColCheck s params check =
  Transaction \conn -> Connection.queryListColCheck conn s params check

queryMaybeRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  Transaction (Maybe r)
queryMaybeRowCheck s params check =
  Transaction \conn -> Connection.queryMaybeRowCheck conn s params check

queryMaybeColCheck ::
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  Transaction (Maybe r)
queryMaybeColCheck s params check =
  Transaction \conn -> Connection.queryMaybeColCheck conn s params check

queryOneRowCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  Transaction r
queryOneRowCheck s params check =
  Transaction \conn -> Connection.queryOneRowCheck conn s params check

queryOneColCheck ::
  (Sqlite.FromField b, Sqlite.ToRow a, SqliteExceptionReason e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  Transaction r
queryOneColCheck s params check =
  Transaction \conn -> Connection.queryOneColCheck conn s params check

-- With results, without parameters, without checks

queryListRow_ :: Sqlite.FromRow a => Sql -> Transaction [a]
queryListRow_ s =
  Transaction \conn -> Connection.queryListRow_ conn s

queryListCol_ :: Sqlite.FromField a => Sql -> Transaction [a]
queryListCol_ s =
  Transaction \conn -> Connection.queryListCol_ conn s

queryMaybeRow_ :: Sqlite.FromRow a => Sql -> Transaction (Maybe a)
queryMaybeRow_ s =
  Transaction \conn -> Connection.queryMaybeRow_ conn s

queryMaybeCol_ :: Sqlite.FromField a => Sql -> Transaction (Maybe a)
queryMaybeCol_ s =
  Transaction \conn -> Connection.queryMaybeCol_ conn s

queryOneRow_ :: Sqlite.FromRow a => Sql -> Transaction a
queryOneRow_ s =
  Transaction \conn -> Connection.queryOneRow_ conn s

queryOneCol_ :: Sqlite.FromField a => Sql -> Transaction a
queryOneCol_ s =
  Transaction \conn -> Connection.queryOneCol_ conn s

-- With results, without parameters, with checks

queryListRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Sql -> ([a] -> Either e r) -> Transaction r
queryListRowCheck_ s check =
  Transaction \conn -> Connection.queryListRowCheck_ conn s check

queryListColCheck_ :: (Sqlite.FromField a, SqliteExceptionReason e) => Sql -> ([a] -> Either e r) -> Transaction r
queryListColCheck_ s check =
  Transaction \conn -> Connection.queryListColCheck_ conn s check

queryMaybeRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Sql -> (a -> Either e r) -> Transaction (Maybe r)
queryMaybeRowCheck_ s check =
  Transaction \conn -> Connection.queryMaybeRowCheck_ conn s check

queryMaybeColCheck_ :: (Sqlite.FromField a, SqliteExceptionReason e) => Sql -> (a -> Either e r) -> Transaction (Maybe r)
queryMaybeColCheck_ s check =
  Transaction \conn -> Connection.queryMaybeColCheck_ conn s check

queryOneRowCheck_ :: (Sqlite.FromRow a, SqliteExceptionReason e) => Sql -> (a -> Either e r) -> Transaction r
queryOneRowCheck_ s check =
  Transaction \conn -> Connection.queryOneRowCheck_ conn s check

queryOneColCheck_ :: (Sqlite.FromField a, SqliteExceptionReason e) => Sql -> (a -> Either e r) -> Transaction r
queryOneColCheck_ s check =
  Transaction \conn -> Connection.queryOneColCheck_ conn s check

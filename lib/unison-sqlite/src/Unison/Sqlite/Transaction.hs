module Unison.Sqlite.Transaction
  ( -- * Transaction management
    Transaction,
    runTransaction,
    runReadOnlyTransaction,
    runWriteTransaction,
    unsafeUnTransaction,
    savepoint,
    unsafeIO,
    unsafeGetConnection,

    -- * Executing queries

    -- ** Without results

    -- *** With parameters
    execute,
    executeMany,

    -- *** Without parameters
    execute_,

    -- ** With results

    -- *** With parameters
    queryStreamRow,
    queryStreamCol,
    queryListRow,
    queryListCol,
    queryMaybeRow,
    queryMaybeCol,
    queryOneRow,
    queryOneCol,
    queryManyListRow,

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

    -- * Rows modified
    rowsModified,
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
import UnliftIO.Exception (bracketOnError_, catchAny, trySyncOrAsync, uninterruptibleMask)

newtype Transaction a
  = Transaction (Connection -> IO a)
  -- Omit MonadIO instance because transactions may be retried
  -- Omit MonadThrow instance so we always throw SqliteException (via *Check) with lots of context
  deriving (Applicative, Functor, Monad) via (ReaderT Connection IO)

unsafeGetConnection :: Transaction Connection
unsafeGetConnection = Transaction pure

-- | Run a transaction on the given connection.
runTransaction :: (MonadIO m) => Connection -> Transaction a -> m a
runTransaction conn (Transaction f) = liftIO do
  uninterruptibleMask \restore -> do
    Connection.begin conn
    -- Catch all exceptions (sync or async), because we want to ROLLBACK the BEGIN no matter what.
    trySyncOrAsync @_ @SomeException (restore (f conn)) >>= \case
      Left exception -> do
        ignoringExceptions (Connection.rollback conn)
        case fromException exception of
          Just SqliteBusyException -> do
            restore (threadDelay transactionRetryDelay)
            runWriteTransaction_ restore conn (f conn)
          _ -> throwIO exception
      Right result -> do
        Connection.commit conn
        pure result
{-# SPECIALIZE runTransaction :: Connection -> Transaction a -> IO a #-}

-- | Run a transaction that is known to only perform reads.
--
-- The action is provided a function that peels off the 'Transaction' newtype without sending the corresponding
-- BEGIN/COMMIT statements.
--
-- The transaction is never retried, so it is (more) safe to interleave arbitrary IO actions. If the transaction does
-- attempt a write and gets SQLITE_BUSY, it's your fault!
runReadOnlyTransaction :: (MonadUnliftIO m) => Connection -> ((forall x. Transaction x -> m x) -> m a) -> m a
runReadOnlyTransaction conn f =
  withRunInIO \runInIO ->
    runReadOnlyTransaction_ conn (runInIO (f (\transaction -> liftIO (unsafeUnTransaction transaction conn))))
{-# SPECIALIZE runReadOnlyTransaction :: Connection -> ((forall x. Transaction x -> IO x) -> IO a) -> IO a #-}

runReadOnlyTransaction_ :: Connection -> IO a -> IO a
runReadOnlyTransaction_ conn action = do
  bracketOnError_
    (Connection.begin conn)
    (ignoringExceptions (Connection.rollback conn))
    ( do
        result <- action
        Connection.commit conn
        pure result
    )

-- | Run a transaction that is known to perform at least one write.
--
-- The action is provided a function that peels off the 'Transaction' newtype without sending the corresponding
-- BEGIN/COMMIT statements.
--
-- The transaction is never retried, so it is (more) safe to interleave arbitrary IO actions.
runWriteTransaction :: (MonadUnliftIO m) => Connection -> ((forall x. Transaction x -> m x) -> m a) -> m a
runWriteTransaction conn f =
  withRunInIO \runInIO ->
    uninterruptibleMask \restore ->
      runWriteTransaction_
        restore
        conn
        (runInIO (f (\transaction -> liftIO (unsafeUnTransaction transaction conn))))
{-# SPECIALIZE runWriteTransaction :: Connection -> ((forall x. Transaction x -> IO x) -> IO a) -> IO a #-}

runWriteTransaction_ :: (forall x. IO x -> IO x) -> Connection -> IO a -> IO a
runWriteTransaction_ restore conn transaction = do
  keepTryingToBeginImmediate restore conn
  result <- restore transaction `onException` ignoringExceptions (Connection.rollback conn)
  Connection.commit conn
  pure result

-- @BEGIN IMMEDIATE@ until success.
keepTryingToBeginImmediate :: (forall x. IO x -> IO x) -> Connection -> IO ()
keepTryingToBeginImmediate restore conn =
  let loop =
        try @_ @SqliteQueryException (Connection.beginImmediate conn) >>= \case
          Left SqliteBusyException -> do
            restore (threadDelay transactionRetryDelay)
            loop
          Left exception -> throwIO exception
          Right () -> pure ()
   in loop

ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

-- | Unwrap the transaction newtype, throwing away the sending of BEGIN/COMMIT + automatic retry.
unsafeUnTransaction :: Transaction a -> Connection -> IO a
unsafeUnTransaction (Transaction action) =
  action

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

-- | Perform IO inside a transaction, which should be idempotent, because it may be run more than once if the
-- transaction needs to retry.
--
-- /Warning/: attempting to run a transaction inside a transaction will cause an exception!
unsafeIO :: IO a -> Transaction a
unsafeIO action =
  Transaction \_ -> action

-- Without results, with parameters

execute :: (Sqlite.ToRow a) => Sql -> a -> Transaction ()
execute s params = do
  Transaction \conn -> Connection.execute conn s params

executeMany :: (Sqlite.ToRow a) => Sql -> [a] -> Transaction ()
executeMany s params =
  Transaction \conn -> Connection.executeMany conn s params

-- Without results, without parameters

execute_ :: Sql -> Transaction ()
execute_ s =
  Transaction \conn -> Connection.execute_ conn s

-- | Run a query many times using a prepared statement.
queryManyListRow :: (Sqlite.FromRow r, Sqlite.ToRow q) => Sql -> [q] -> Transaction [[r]]
queryManyListRow s params =
  Transaction \conn -> Connection.queryManyListRow conn s params

-- With results, with parameters, without checks

queryStreamRow ::
  (Sqlite.FromRow a, Sqlite.ToRow b) =>
  Sql ->
  b ->
  (Transaction (Maybe a) -> Transaction r) ->
  Transaction r
queryStreamRow s params callback =
  Transaction \conn ->
    Connection.queryStreamRow conn s params \next ->
      unsafeUnTransaction (callback (unsafeIO next)) conn

queryStreamCol ::
  forall a b r.
  (Sqlite.FromField a, Sqlite.ToRow b) =>
  Sql ->
  b ->
  (Transaction (Maybe a) -> Transaction r) ->
  Transaction r
queryStreamCol =
  coerce
    @(Sql -> b -> (Transaction (Maybe (Sqlite.Only a)) -> Transaction r) -> Transaction r)
    @(Sql -> b -> (Transaction (Maybe a) -> Transaction r) -> Transaction r)
    queryStreamRow

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

queryListRow_ :: (Sqlite.FromRow a) => Sql -> Transaction [a]
queryListRow_ s =
  Transaction \conn -> Connection.queryListRow_ conn s

queryListCol_ :: (Sqlite.FromField a) => Sql -> Transaction [a]
queryListCol_ s =
  Transaction \conn -> Connection.queryListCol_ conn s

queryMaybeRow_ :: (Sqlite.FromRow a) => Sql -> Transaction (Maybe a)
queryMaybeRow_ s =
  Transaction \conn -> Connection.queryMaybeRow_ conn s

queryMaybeCol_ :: (Sqlite.FromField a) => Sql -> Transaction (Maybe a)
queryMaybeCol_ s =
  Transaction \conn -> Connection.queryMaybeCol_ conn s

queryOneRow_ :: (Sqlite.FromRow a) => Sql -> Transaction a
queryOneRow_ s =
  Transaction \conn -> Connection.queryOneRow_ conn s

queryOneCol_ :: (Sqlite.FromField a) => Sql -> Transaction a
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

-- Rows modified

rowsModified :: Transaction Int
rowsModified =
  Transaction Connection.rowsModified

transactionRetryDelay :: Int
transactionRetryDelay = 100_000

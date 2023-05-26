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

    -- *** With checks
    queryListRowCheck2,
    queryListColCheck2,
    queryMaybeRowCheck2,
    queryMaybeColCheck2,
    queryOneRowCheck2,
    queryOneColCheck2,

    -- * Rows modified
    rowsModified,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (fromException), onException, throwIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.FromField qualified as Sqlite
import System.Random qualified as Random
import Unison.Prelude
import Unison.Sqlite.Connection (Connection (..))
import Unison.Sqlite.Connection qualified as Connection
import Unison.Sqlite.Exception (SqliteExceptionReason, SqliteQueryException, pattern SqliteBusyException)
import Unison.Sqlite.Sql2 (Sql2)
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

-- Without results

execute2 :: Sql2 -> Transaction ()
execute2 s =
  Transaction \conn -> Connection.execute2 conn s

executeStatements :: Text -> Transaction ()
executeStatements s =
  Transaction \conn -> Connection.executeStatements conn s

-- With results, without checks

queryStreamRow ::
  (Sqlite.FromRow a) =>
  Sql2 ->
  (Transaction (Maybe a) -> Transaction r) ->
  Transaction r
queryStreamRow sql callback =
  Transaction \conn ->
    Connection.queryStreamRow conn sql \next ->
      unsafeUnTransaction (callback (unsafeIO next)) conn

queryStreamCol ::
  forall a r.
  (Sqlite.FromField a) =>
  Sql2 ->
  (Transaction (Maybe a) -> Transaction r) ->
  Transaction r
queryStreamCol =
  coerce
    @(Sql2 -> (Transaction (Maybe (Sqlite.Only a)) -> Transaction r) -> Transaction r)
    @(Sql2 -> (Transaction (Maybe a) -> Transaction r) -> Transaction r)
    queryStreamRow

queryListRow2 :: (Sqlite.FromRow a) => Sql2 -> Transaction [a]
queryListRow2 s =
  Transaction \conn -> Connection.queryListRow2 conn s

queryListCol2 :: (Sqlite.FromField a) => Sql2 -> Transaction [a]
queryListCol2 s =
  Transaction \conn -> Connection.queryListCol2 conn s

queryMaybeRow2 :: (Sqlite.FromRow a) => Sql2 -> Transaction (Maybe a)
queryMaybeRow2 s =
  Transaction \conn -> Connection.queryMaybeRow2 conn s

queryMaybeCol2 :: (Sqlite.FromField a) => Sql2 -> Transaction (Maybe a)
queryMaybeCol2 s =
  Transaction \conn -> Connection.queryMaybeCol2 conn s

queryOneRow2 :: (Sqlite.FromRow a) => Sql2 -> Transaction a
queryOneRow2 s =
  Transaction \conn -> Connection.queryOneRow2 conn s

queryOneCol2 :: (Sqlite.FromField a) => Sql2 -> Transaction a
queryOneCol2 s =
  Transaction \conn -> Connection.queryOneCol2 conn s

-- With results, with parameters, with checks

queryListRowCheck2 ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Sql2 ->
  ([a] -> Either e r) ->
  Transaction r
queryListRowCheck2 sql check =
  Transaction \conn -> Connection.queryListRowCheck2 conn sql check

queryListColCheck2 ::
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Sql2 ->
  ([a] -> Either e r) ->
  Transaction r
queryListColCheck2 sql check =
  Transaction \conn -> Connection.queryListColCheck2 conn sql check

queryMaybeRowCheck2 ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Sql2 ->
  (a -> Either e r) ->
  Transaction (Maybe r)
queryMaybeRowCheck2 s check =
  Transaction \conn -> Connection.queryMaybeRowCheck2 conn s check

queryMaybeColCheck2 ::
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Sql2 ->
  (a -> Either e r) ->
  Transaction (Maybe r)
queryMaybeColCheck2 s check =
  Transaction \conn -> Connection.queryMaybeColCheck2 conn s check

queryOneRowCheck2 ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Sql2 ->
  (a -> Either e r) ->
  Transaction r
queryOneRowCheck2 s check =
  Transaction \conn -> Connection.queryOneRowCheck2 conn s check

queryOneColCheck2 ::
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Sql2 ->
  (a -> Either e r) ->
  Transaction r
queryOneColCheck2 s check =
  Transaction \conn -> Connection.queryOneColCheck2 conn s check

-- Rows modified

rowsModified :: Transaction Int
rowsModified =
  Transaction Connection.rowsModified

transactionRetryDelay :: Int
transactionRetryDelay = 100_000

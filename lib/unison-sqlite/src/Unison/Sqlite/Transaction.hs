module Unison.Sqlite.Transaction
  ( -- * Transaction management
    Transaction,
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
  )
where

import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import Unison.Prelude
import Unison.Sqlite.Connection (Connection (..))
import qualified Unison.Sqlite.Connection as Connection
import Unison.Sqlite.Sql

newtype Transaction a = Transaction {unTransaction :: Connection -> IO a}
  -- Omit MonadIO instance because transactions may be retried
  -- Omit MonadThrow instance so we always throw SqliteException (via *Check) with lots of context
  deriving (Applicative, Functor, Monad) via (ReaderT Connection IO)

-- | Run a transaction on the given connection.
runTransaction :: MonadIO m => Connection -> Transaction a -> m a
runTransaction conn@(Connection _ _ conn0) (Transaction f) =
  -- TODO retry on busy
  liftIO do
    Sqlite.execute_ conn0 "BEGIN"
    result <- f conn
    Sqlite.execute_ conn0 "COMMIT"
    pure result

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

queryList :: (Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> Transaction [a]
queryList s params =
  Transaction \conn -> Connection.queryList conn s params

queryListOne :: (Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> Transaction [a]
queryListOne s params =
  Transaction \conn -> Connection.queryListOne conn s params

queryMaybe :: (Sqlite.FromRow a, Sqlite.ToRow b) => Sql -> b -> Transaction (Maybe a)
queryMaybe s params =
  Transaction \conn -> Connection.queryMaybe conn s params

queryMaybeOne :: (Sqlite.FromField a, Sqlite.ToRow b) => Sql -> b -> Transaction (Maybe a)
queryMaybeOne s params =
  Transaction \conn -> Connection.queryMaybeOne conn s params

queryOne :: (Sqlite.FromRow b, Sqlite.ToRow a) => Sql -> a -> Transaction b
queryOne s params =
  Transaction \conn -> Connection.queryOne conn s params

queryOneOne :: (Sqlite.FromField b, Sqlite.ToRow a) => Sql -> a -> Transaction b
queryOneOne s params =
  Transaction \conn -> Connection.queryOneOne conn s params

-- With results, with parameters, with checks

queryOneCheck ::
  (Sqlite.FromRow b, Sqlite.ToRow a, Show e, Typeable e) =>
  Sql ->
  a ->
  (b -> Either e r) ->
  Transaction r
queryOneCheck s params check =
  Transaction \conn -> Connection.queryOneCheck conn s params check

-- With results, without parameters, without checks

queryList_ :: Sqlite.FromRow a => Sql -> Transaction [a]
queryList_ s =
  Transaction \conn -> Connection.queryList_ conn s

queryListOne_ :: Sqlite.FromField a => Sql -> Transaction [a]
queryListOne_ s =
  Transaction \conn -> Connection.queryListOne_ conn s

queryMaybe_ :: Sqlite.FromRow a => Sql -> Transaction (Maybe a)
queryMaybe_ s =
  Transaction \conn -> Connection.queryMaybe_ conn s

queryMaybeOne_ :: Sqlite.FromField a => Sql -> Transaction (Maybe a)
queryMaybeOne_ s =
  Transaction \conn -> Connection.queryMaybeOne_ conn s

queryOne_ :: Sqlite.FromRow a => Sql -> Transaction a
queryOne_ s =
  Transaction \conn -> Connection.queryOne_ conn s

queryOneOne_ :: Sqlite.FromField a => Sql -> Transaction a
queryOneOne_ s =
  Transaction \conn -> Connection.queryOneOne_ conn s

-- With results, without parameters, with checks

queryOneCheck_ :: (Sqlite.FromRow a, Show e, Typeable e) => Sql -> (a -> Either e r) -> Transaction r
queryOneCheck_ s check =
  Transaction \conn -> Connection.queryOneCheck_ conn s check

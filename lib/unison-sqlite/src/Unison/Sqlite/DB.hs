-- | A monadic interface to SQLite.
module Unison.Sqlite.DB
  ( DB,
    runDB,
    runTransaction,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Unison.Prelude
import Unison.Sqlite.Connection (Connection)
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

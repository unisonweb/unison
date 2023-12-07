-- | Utility for caching transaction calls
module U.Codebase.CacheTransaction
  ( cacheTransaction,
  )
where

import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Cache (Cache)
import Unison.Util.Cache qualified as Cache

cacheTransaction :: forall k v. Cache k v -> (k -> Transaction v) -> (k -> Transaction v)
cacheTransaction cache f k =
  coerce @(TransactionWithMonadIO v) (Cache.apply cache (coerce f) k)

newtype TransactionWithMonadIO a
  = TransactionWithMonadIO (Transaction a)
  deriving newtype (Applicative, Functor, Monad)

instance MonadIO TransactionWithMonadIO where
  liftIO :: forall a. IO a -> TransactionWithMonadIO a
  liftIO = coerce @(IO a -> Transaction a) Sqlite.unsafeIO

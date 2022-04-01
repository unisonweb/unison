module Unison.Sync.HTTP where

import Control.Monad.Reader
import qualified Network.HTTP.Client as HTTP
import Unison.Sync
import Unison.Sync.Types
import Servant.Client

newtype SyncHTTP a = Sync (ReaderT HTTP.Manager IO a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadSync SyncHTTP where
  getCausalHashForPath = _
  pushForce = _
  uploadToRepo = _
  downloadEntities = _

runSyncHTTP :: MonadIO m => Auth.AuthorizedHTTPClient -> SyncHTTP a -> m a
runSyncHTTP (AuthorizedHTTPClient manager) (Sync m) = liftIO $ do
  runReaderT m manager

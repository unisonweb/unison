{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An instance of MonadSync using HTTP.
module Unison.Sync.HTTP
  ( SyncT,
    runSyncT,
  )
where

import Control.Monad.Reader
import Servant.API
import Servant.Client
import qualified Unison.Auth.HTTPClient as Auth
import qualified Unison.Auth.Types as Auth
import Unison.Prelude
import qualified Unison.Sync.API as Sync
import Unison.Sync.Class
import Unison.Sync.Types

newtype SyncT m a = SyncT (ReaderT (SyncHandlers IO) m a)
  deriving newtype (Functor, Applicative, Monad)

liftHandler :: MonadIO m => ((SyncHandlers IO -> t -> IO a) -> t -> SyncT m a)
liftHandler handler req = SyncT do
  f <- asks handler
  liftIO $ f req

instance MonadIO m => MonadSync (SyncT m) where
  getCausalHashByPath = liftHandler getPathHandler
  updatePath = liftHandler updatePathHandler
  uploadEntities = liftHandler uploadEntitiesHandler
  downloadEntities = liftHandler downloadEntitiesHandler

data SyncError
  = InvalidHost Auth.Host
  | ClientErr ClientError
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncHandlers m = SyncHandlers
  { getPathHandler :: GetCausalHashByPathRequest -> m GetCausalHashByPathResponse,
    updatePathHandler :: UpdatePathRequest -> m UpdatePathResponse,
    downloadEntitiesHandler :: DownloadEntitiesRequest -> m DownloadEntitiesResponse,
    uploadEntitiesHandler :: UploadEntitiesRequest -> m UploadEntitiesResponse
  }

runSyncT :: MonadUnliftIO m => Auth.AuthorizedHttpClient -> BaseUrl -> SyncT m a -> m (Either SyncError a)
runSyncT (Auth.AuthorizedHttpClient manager) baseUrl (SyncT m) = try $ do
  let clientEnv = mkClientEnv manager baseUrl
  let hoistToIO :: forall a. ClientM a -> IO a
      hoistToIO = (throwEitherMWith ClientErr . flip runClientM clientEnv)
  let ( getPathHandler
          :<|> updatePathHandler
          :<|> downloadEntitiesHandler
          :<|> uploadEntitiesHandler
        ) = hoistClient Sync.api hoistToIO (client Sync.api)
  runReaderT m (SyncHandlers {..})

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
import qualified Data.SOP as SOP
import Servant.API
import Servant.Client
import qualified Unison.Auth.HTTPClient as Auth
import Unison.Prelude
import qualified Unison.Sync.API as Sync
import Unison.Sync.Class
import Unison.Sync.Types
import Unison.Util.Servant.Client (HasConstructor, collectUnion)

newtype SyncT m a = SyncT (ReaderT (SyncHandlers IO) m a)
  deriving newtype (Functor, Applicative, Monad)

-- | Helper to interpret a simple (non-union) method.
liftHandler :: MonadIO m => ((SyncHandlers IO -> t -> IO a) -> t -> SyncT m a)
liftHandler handler req = SyncT do
  f <- asks handler
  liftIO $ f req

-- | Helper to interpret a method with a union return-type.
liftHandlerUnion ::
  forall result xs req m.
  (MonadIO m, SOP.All (HasConstructor result) xs) =>
  (SyncHandlers IO -> req -> IO (Union xs)) ->
  req ->
  SyncT m result
liftHandlerUnion handler req = SyncT do
  f <- asks handler
  resp <- liftIO $ f req
  pure $ collectUnion resp

instance MonadIO m => MonadSync (SyncT m) where
  getCausalHashByPath = liftHandler getPathHandler
  updatePath = liftHandlerUnion updatePathHandler
  uploadEntities = liftHandlerUnion uploadEntitiesHandler
  downloadEntities = liftHandler downloadEntitiesHandler

data SyncError
  = ClientErr ClientError
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncHandlers m = SyncHandlers
  { getPathHandler :: GetCausalHashByPathRequest -> m GetCausalHashByPathResponse,
    updatePathHandler ::
      ( UpdatePathRequest ->
        m
          ( Union
              '[ WithStatus 204 (),
                 WithStatus 404 (NeedDependencies Hash),
                 WithStatus 412 HashMismatch
               ]
          )
      ),
    downloadEntitiesHandler ::
      ( DownloadEntitiesRequest ->
        m DownloadEntitiesResponse
      ),
    uploadEntitiesHandler ::
      ( UploadEntitiesRequest ->
        m
          ( Union
              '[ WithStatus 200 (),
                 WithStatus 202 (NeedDependencies Hash)
               ]
          )
      )
  }

-- | Run a SyncT
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

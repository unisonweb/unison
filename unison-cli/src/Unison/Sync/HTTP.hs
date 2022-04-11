{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Sync.HTTP
  ( getPathHandler,
    updatePathHandler,
    downloadEntitiesHandler,
    uploadEntitiesHandler,
  )
where

import Control.Monad.Reader
import Servant.API
import Servant.Client
import qualified Unison.Auth.HTTPClient as Auth
import Unison.Prelude
import qualified Unison.Sync.API as Sync
import Unison.Sync.Types

data SyncError
  = ClientErr ClientError
  deriving stock (Show)
  deriving anyclass (Exception)

getPathHandler :: Auth.AuthorizedHttpClient -> BaseUrl -> GetCausalHashByPathRequest -> IO GetCausalHashByPathResponse
updatePathHandler :: Auth.AuthorizedHttpClient -> BaseUrl -> UpdatePathRequest -> IO UpdatePathResponse
downloadEntitiesHandler :: Auth.AuthorizedHttpClient -> BaseUrl -> DownloadEntitiesRequest -> IO DownloadEntitiesResponse
uploadEntitiesHandler :: Auth.AuthorizedHttpClient -> BaseUrl -> UploadEntitiesRequest -> IO UploadEntitiesResponse
( getPathHandler,
  updatePathHandler,
  downloadEntitiesHandler,
  uploadEntitiesHandler
  ) =
    let ( getPathHandler
            :<|> updatePathHandler
            :<|> downloadEntitiesHandler
            :<|> uploadEntitiesHandler
          ) = hoistClient Sync.api hoist (client Sync.api)
     in (uncurryReaderT getPathHandler, uncurryReaderT updatePathHandler, uncurryReaderT downloadEntitiesHandler, uncurryReaderT uploadEntitiesHandler)
    where
      hoist :: forall a. ClientM a -> ReaderT (Auth.AuthorizedHttpClient, BaseUrl) IO a
      hoist m = do
        (Auth.AuthorizedHttpClient manager, baseUrl) <- ask
        let clientEnv = mkClientEnv manager baseUrl
        resp <- liftIO . throwEitherMWith ClientErr $ (runClientM m clientEnv)
        pure resp

      uncurryReaderT :: forall req resp. (req -> ReaderT (Auth.AuthorizedHttpClient, BaseUrl) IO resp) -> Auth.AuthorizedHttpClient -> BaseUrl -> req -> IO resp
      uncurryReaderT f httpClient baseURL req =
        runReaderT (f req) (httpClient, baseURL)

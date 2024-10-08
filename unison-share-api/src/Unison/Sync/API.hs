{-# LANGUAGE DataKinds #-}

module Unison.Sync.API (API) where

import Servant.API
import Unison.Sync.Types

type API =
  "path" :> "get" :> GetCausalHashByPathEndpoint
    :<|> "entities" :> "download" :> DownloadEntitiesEndpoint
    :<|> "entities" :> "upload" :> UploadEntitiesEndpoint

type GetCausalHashByPathEndpoint =
  ReqBody '[JSON] GetCausalHashByPathRequest
    :> Post '[JSON] GetCausalHashByPathResponse

type DownloadEntitiesEndpoint =
  ReqBody '[JSON] DownloadEntitiesRequest
    :> Post '[JSON] DownloadEntitiesResponse

type UploadEntitiesEndpoint =
  ReqBody '[JSON] UploadEntitiesRequest
    :> Post '[JSON] UploadEntitiesResponse

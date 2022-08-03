{-# LANGUAGE DataKinds #-}

module Unison.Sync.API (API, api) where

import Data.Proxy
import Servant.API
import Unison.Sync.Types
import Unison.Util.MessagePack (MessagePackCT)

api :: Proxy API
api = Proxy

type API =
  "path" :> "get" :> GetCausalHashByPathEndpoint
    :<|> "path" :> "fast-forward" :> FastForwardPathEndpoint
    :<|> "path" :> "update" :> UpdatePathEndpoint
    :<|> "entities" :> "download" :> DownloadEntitiesEndpoint
    :<|> "entities" :> "upload" :> UploadEntitiesEndpoint

type GetCausalHashByPathEndpoint =
  ReqBody '[JSON] GetCausalHashByPathRequest
    :> Post '[JSON] GetCausalHashByPathResponse

type FastForwardPathEndpoint =
  ReqBody '[JSON] FastForwardPathRequest
    :> Post '[JSON] FastForwardPathResponse

type UpdatePathEndpoint =
  ReqBody '[JSON] UpdatePathRequest
    :> Post '[JSON] UpdatePathResponse

type DownloadEntitiesEndpoint =
  ReqBody '[MessagePackCT, JSON] DownloadEntitiesRequest
    :> Post '[MessagePackCT, JSON] DownloadEntitiesResponse

type UploadEntitiesEndpoint =
  ReqBody '[JSON] UploadEntitiesRequest
    :> Post '[JSON] UploadEntitiesResponse

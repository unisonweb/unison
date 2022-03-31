{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Sync.API (API) where

import Servant.API
import Unison.Sync.Types

type API =
  ("get-path" :> GetCausalHashByPathEndpoint)
    :<|> ("push" :> PushEndpoint)
    :<|> ("download" :> DownloadEntitiesEndpoint)
    :<|> ("upload" :> UploadEntitiesEndpoint)

type GetCausalHashByPathEndpoint =
  ReqBody '[JSON] GetCausalHashByPathRequest
    :> Post '[JSON] GetCausalHashByPathResponse

type PushEndpoint =
  ReqBody '[JSON] PushRequest
    :> UVerb 'POST '[JSON] '[WithStatus 204 NoContent, WithStatus 404 (NeedDependencies HashJWT), WithStatus 412 OutOfDateHash]

type DownloadEntitiesEndpoint =
  ReqBody '[JSON] DownloadEntitiesRequest
    :> Post '[JSON] DownloadEntitiesResponse

type UploadEntitiesEndpoint =
  ReqBody '[JSON] UploadEntitiesRequest
    :> UVerb 'POST '[JSON] '[WithStatus 200 NoContent, WithStatus 202 (NeedDependencies Hash)]

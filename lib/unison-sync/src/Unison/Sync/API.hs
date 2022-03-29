{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Sync.API (API) where

import Servant.API
import Servant.API.ContentTypes.CBOR
import Unison.Sync.Types

type API =
  ("get-path" :> GetCausalHashByPathEndpoint)
    :<|> ("push" :> PushEndpoint)
    :<|> ("download" :> DownloadEntitiesEndpoint)
    :<|> ("upload" :> UploadEntitiesEndpoint)

type GetCausalHashByPathEndpoint =
  ReqBody '[JSON, CBOR] GetCausalHashByPathRequest
    :> Post '[JSON, CBOR] GetCausalHashByPathResponse

type PushEndpoint =
  ReqBody '[JSON, CBOR] PushRequest
    :> UVerb 'POST '[JSON, CBOR] '[WithStatus 204 NoContent, WithStatus 404 (NeedDependencies HashJWT), WithStatus 412 OutOfDateHash]

type DownloadEntitiesEndpoint =
  ReqBody '[JSON, CBOR] DownloadEntitiesRequest
    :> Post '[JSON, CBOR] DownloadEntitiesResponse

type UploadEntitiesEndpoint =
  ReqBody '[JSON, CBOR] UploadEntitiesRequest
    :> UVerb 'POST '[JSON, CBOR] '[WithStatus 200 NoContent, WithStatus 202 (NeedDependencies Hash)]

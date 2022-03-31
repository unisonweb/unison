{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Sync.API (API) where

import Servant.API
import Unison.Sync.Types

type API =
  "path" :> "get" :> GetCausalHashByPathEndpoint
    :<|> "path" :> "update" :> UpdatePathEndpoint
    :<|> "entities" :> "download" :> DownloadEntitiesEndpoint
    :<|> "entities" :> "upload" :> UploadEntitiesEndpoint

type GetCausalHashByPathEndpoint =
  ReqBody '[JSON] GetCausalHashByPathRequest
    :> Post '[JSON] GetCausalHashByPathResponse

type UpdatePathEndpoint =
  ReqBody '[JSON] UpdatePathRequest
    :> UVerb 'POST '[JSON] '[WithStatus 204 NoContent, WithStatus 404 (NeedDependencies HashJWT), WithStatus 412 HashMismatch]

type DownloadEntitiesEndpoint =
  ReqBody '[JSON] DownloadEntitiesRequest
    :> Post '[JSON] DownloadEntitiesResponse

type UploadEntitiesEndpoint =
  ReqBody '[JSON] UploadEntitiesRequest
    :> UVerb 'POST '[JSON] '[WithStatus 200 NoContent, WithStatus 202 (NeedDependencies Hash)]

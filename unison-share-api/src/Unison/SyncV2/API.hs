{-# LANGUAGE DataKinds #-}

module Unison.SyncV2.API
  ( API,
    api,
    Routes (..),
  )
where

import Data.ByteString (ByteString)
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Unison.SyncV2.Types
import Unison.Util.Servant.CBOR (CBOR)

api :: Proxy API
api = Proxy

type API = NamedRoutes Routes

type DownloadEntitiesStream =
  -- | The causal hash the client needs. The server should provide it and all of its dependencies
  ReqBody '[CBOR] DownloadEntitiesRequest
    :> StreamPost NoFraming OctetStream (SourceIO ByteString)

type UploadEntitiesStream =
  ReqBody '[CBOR] UploadEntitiesRequest
    :> StreamPost NoFraming OctetStream (SourceIO ByteString)

type GetCausalHashEndpoint =
  ReqBody '[CBOR] GetCausalHashRequest
    :> Post '[CBOR] GetCausalHashResponse

data Routes mode = Routes
  { getCausalHash :: mode :- "path" :> "get" :> GetCausalHashEndpoint,
    downloadEntitiesStream :: mode :- "entities" :> "download" :> DownloadEntitiesStream,
    uploadEntitiesStream :: mode :- "entities" :> "upload" :> UploadEntitiesStream
  }
  deriving stock (Generic)

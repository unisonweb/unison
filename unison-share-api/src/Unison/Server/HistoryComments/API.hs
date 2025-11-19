{-# LANGUAGE DataKinds #-}

module Unison.Server.HistoryComments.API (api, API, Routes (..)) where

import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Unison.Server.HistoryComments.Types
import Unison.SyncV2.Types
import Unison.Util.Servant.CBOR (CBOR)

api :: Proxy API
api = Proxy

type API = NamedRoutes Routes

type DownloadCommentsStream =
  -- | The causal hash the client needs. The server should provide it and all of its dependencies
  ReqBody '[CBOR, JSON] DownloadCommentsRequest
    :> StreamPost NoFraming OctetStream (SourceIO (CBORStream HistoryCommentChunk))

type UploadCommentsStream =
  StreamBody NoFraming OctetStream (SourceIO (CBORStream HistoryCommentChunk))
    :> Post '[JSON] UploadCommentsResponse

data Routes mode = Routes
  { uploadComments :: mode :- "history-comments" :> "upload" :> UploadCommentsStream,
    downloadComments :: mode :- "history-comments" :> "download" :> DownloadCommentsStream
  }
  deriving stock (Generic)

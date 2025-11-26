{-# LANGUAGE DataKinds #-}

module Unison.Server.HistoryComments.API (api, API, Routes (..)) where

import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.API.WebSocket

api :: Proxy API
api = Proxy

type API = NamedRoutes Routes

type DownloadCommentsStream = WebSocket

type UploadCommentsStream = WebSocket

data Routes mode = Routes
  { uploadHistoryComments :: mode :- "history-comments" :> "upload" :> UploadCommentsStream,
    downloadHistoryComments :: mode :- "history-comments" :> "download" :> DownloadCommentsStream
  }
  deriving stock (Generic)

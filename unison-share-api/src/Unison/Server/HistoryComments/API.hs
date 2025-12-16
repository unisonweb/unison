{-# LANGUAGE DataKinds #-}

module Unison.Server.HistoryComments.API (api, API, Routes (..)) where

import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.API.WebSocket
import Unison.Server.Types (BranchRef, RequiredQueryParam)

api :: Proxy API
api = Proxy

type API = NamedRoutes Routes

type DownloadCommentsStream = WebSocket

type UploadCommentsStream =
  RequiredQueryParam "branchRef" BranchRef
    :> WebSocket

data Routes mode = Routes
  { uploadHistoryComments :: mode :- "upload" :> UploadCommentsStream,
    downloadHistoryComments :: mode :- "download" :> DownloadCommentsStream
  }
  deriving stock (Generic)

module Unison.Share.HistoryComments (uploadCommentsClient) where

import Control.Monad.Reader
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Network.WebSockets qualified as WS
import Servant.API
import Servant.Client qualified as Servant
import Unison.Auth.Tokens (TokenProvider, newTokenProvider)
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Server.HistoryComments.API qualified as HistoryCommentsAPI
import Unison.Server.HistoryComments.Types
import Unison.Server.Types
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Util.Websockets

type HistoryCommentsAPI = ("ucm" :> "v1" :> "history-comments" :> HistoryCommentsAPI.API)

historyCommentsAPI :: Proxy HistoryCommentsAPI
historyCommentsAPI = Proxy @HistoryCommentsAPI

-- downloadCommentsClientM :: BranchRef -> WS.Connection -> Servant.ClientM ()
-- uploadCommentsClientM :: BranchRef -> WS.Connection -> Servant.ClientM ()
-- HistoryCommentsAPI.Routes
--   { uploadHistoryComments = downloadCommentsClientM,
--     downloadHistoryComments = uploadCommentsClientM
--   } = Servant.client historyCommentsAPI

msgBufferSize :: Int
msgBufferSize = 100

uploadCommentsClient ::
  -- | The Unison Share URL.
  Codeserver.CodeserverURI ->
  -- | The branch to download from.
  BranchRef ->
  Cli ()
uploadCommentsClient codeserver branchRef = do
  Cli.Env {codebase, credentialManager} <- ask
  let path = "/ucm/v1/history-comments/upload?branchRef=" <> Text.unpack (toQueryParam branchRef)
  -- Enable compression
  let tokenProvider = newTokenProvider credentialManager
  result <- liftIO $ withCodeserverWebsocket @IO @HistoryCommentChunk @() msgBufferSize codeserver tokenProvider path \Queues {send, receive} -> do
    error "Send comments"

  case result of
    Left _err -> error "handle err"
    Right () -> pure ()

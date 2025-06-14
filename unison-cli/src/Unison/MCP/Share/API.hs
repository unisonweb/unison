-- A subset of the Share API which we expose as MCP tools
module Unison.MCP.Share.API (shareSearch) where

import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client qualified as Servant
import Unison.Auth.HTTPClient (AuthenticatedHttpClient (..))
import Unison.Prelude
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Types (codeserverBaseURL)

shareSearch ::
  AuthenticatedHttpClient ->
  Text ->
  (IO (Either Servant.ClientError Aeson.Value))
shareSearch authedHTTPClient query = runClientM authedHTTPClient $ httpSearch (Just query)

-- https://api.unison-lang.org/search?query=%40http
type ShareAPI =
  ("search" :> QueryParam "query" Text :> Get '[JSON] Aeson.Value)
    :<|> ("search" :> QueryParam "query" Text :> Get '[JSON] Aeson.Value)

httpSearch :: Maybe Text -> Servant.ClientM Aeson.Value
_httpSearch :: Maybe Text -> Servant.ClientM Aeson.Value
( httpSearch
    :<|> _httpSearch
  ) =
    let pp :: Proxy ShareAPI
        pp = Proxy
     in (Servant.client pp)

runClientM :: AuthenticatedHttpClient -> Servant.ClientM a -> IO (Either Servant.ClientError a)
runClientM (AuthenticatedHttpClient httpClient) clientM = do
  let clientEnv = (Servant.mkClientEnv httpClient (codeserverBaseURL Codeserver.defaultCodeserver))
  Servant.runClientM clientM clientEnv

-- A subset of the Share API which we expose as MCP tools
module Unison.MCP.Share.API
  ( shareSearch,
    shareProjectReadme,
    ReadmeResponse (..),
  )
where

import Data.Aeson (FromJSON)
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

shareProjectReadme ::
  AuthenticatedHttpClient ->
  Text ->
  Text ->
  (IO (Either Servant.ClientError ReadmeResponse))
shareProjectReadme authedHTTPClient ownerHandle projectSlug =
  runClientM authedHTTPClient $ httpProjectReadme ownerHandle projectSlug

data ReadmeResponse = ReadmeResponse
  { markdownReadMe :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadmeResponse where
  parseJSON = do
    Aeson.withObject "ReadmeResponse" $ \o -> do
      markdownReadMe <- o Aeson..: "markdownReadMe"
      pure ReadmeResponse {markdownReadMe}

-- https://api.unison-lang.org/search?query=%40http
type ShareAPI =
  ("search" :> QueryParam "query" Text :> Get '[JSON] Aeson.Value)
    :<|> ("users" :> Capture "owner-handle" Text :> "projects" :> Capture "project-slug" Text :> "readme" :> Get '[JSON] ReadmeResponse)

httpSearch :: Maybe Text -> Servant.ClientM Aeson.Value
httpProjectReadme :: Text -> Text -> Servant.ClientM ReadmeResponse
( httpSearch
    :<|> httpProjectReadme
  ) =
    let pp :: Proxy ShareAPI
        pp = Proxy
     in (Servant.client pp)

runClientM :: AuthenticatedHttpClient -> Servant.ClientM a -> IO (Either Servant.ClientError a)
runClientM (AuthenticatedHttpClient httpClient) clientM = do
  let clientEnv = (Servant.mkClientEnv httpClient (codeserverBaseURL Codeserver.defaultCodeserver))
  Servant.runClientM clientM clientEnv

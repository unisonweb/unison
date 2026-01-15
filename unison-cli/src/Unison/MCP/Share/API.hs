-- A subset of the Share API which we expose as MCP tools
module Unison.MCP.Share.API
  ( shareSearch,
    shareProjectReadme,
    shareProjectInfo,
    ReadmeResponse (..),
    ProjectInfoResponse (..),
  )
where

import Data.Aeson (FromJSON, (.:), (.:?))
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

shareProjectInfo ::
  AuthenticatedHttpClient ->
  Text ->
  (IO (Either Servant.ClientError ProjectInfoResponse))
shareProjectInfo authedHTTPClient projectName =
  runClientM authedHTTPClient $ httpGetProject (Just projectName)

data ReadmeResponse = ReadmeResponse
  { markdownReadMe :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadmeResponse where
  parseJSON = do
    Aeson.withObject "ReadmeResponse" $ \o -> do
      markdownReadMe <- o Aeson..: "markdownReadMe"
      pure ReadmeResponse {markdownReadMe}

-- | Response from the GetProject API endpoint
-- The API returns a sum type with "type" and "payload" fields
data ProjectInfoResponse = ProjectInfoResponse
  { projectId :: Text,
    projectName :: Text,
    latestRelease :: Maybe Text,
    defaultBranch :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON ProjectInfoResponse where
  toJSON ProjectInfoResponse {projectId, projectName, latestRelease, defaultBranch} =
    Aeson.object
      [ "projectId" Aeson..= projectId,
        "projectName" Aeson..= projectName,
        "latestRelease" Aeson..= latestRelease,
        "defaultBranch" Aeson..= defaultBranch
      ]

instance FromJSON ProjectInfoResponse where
  parseJSON = Aeson.withObject "ProjectInfoResponse" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "success" -> do
        payload <- o .: "payload"
        projectId <- payload .: "project-id"
        projectName <- payload .: "project-name"
        latestRelease <- payload .:? "latest-release"
        defaultBranch <- payload .:? "default-branch"
        pure ProjectInfoResponse {projectId, projectName, latestRelease, defaultBranch}
      "not-found" -> do
        payload <- o .: "payload"
        msg <- payload .: "message"
        fail $ "Project not found: " <> show (msg :: Text)
      "unauthorized" -> do
        payload <- o .: "payload"
        msg <- payload .: "message"
        fail $ "Unauthorized: " <> show (msg :: Text)
      other -> fail $ "Unknown response type: " <> show other

-- https://api.unison-lang.org/search?query=%40http
type ShareAPI =
  ("search" :> QueryParam "query" Text :> Get '[JSON] Aeson.Value)
    :<|> ("users" :> Capture "owner-handle" Text :> "projects" :> Capture "project-slug" Text :> "readme" :> Get '[JSON] ReadmeResponse)
    :<|> ("ucm" :> "v1" :> "projects" :> "project" :> QueryParam "name" Text :> Get '[JSON] ProjectInfoResponse)

httpSearch :: Maybe Text -> Servant.ClientM Aeson.Value
httpProjectReadme :: Text -> Text -> Servant.ClientM ReadmeResponse
httpGetProject :: Maybe Text -> Servant.ClientM ProjectInfoResponse
( httpSearch
    :<|> httpProjectReadme
    :<|> httpGetProject
  ) =
    let pp :: Proxy ShareAPI
        pp = Proxy
     in (Servant.client pp)

runClientM :: AuthenticatedHttpClient -> Servant.ClientM a -> IO (Either Servant.ClientError a)
runClientM (AuthenticatedHttpClient httpClient) clientM = do
  let clientEnv = (Servant.mkClientEnv httpClient (codeserverBaseURL Codeserver.defaultCodeserver))
  Servant.runClientM clientM clientEnv

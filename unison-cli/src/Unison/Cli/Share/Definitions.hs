{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The Share API calls behind the @share.*@ commands: project search, READMEs, and definitions by name.
module Unison.Cli.Share.Definitions
  ( -- * API calls
    SearchHit (..),
    searchProjects,
    getReadme,
    getDefinitions,
  )
where

import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Servant.API ((:<|>) (..), (:>))
import Servant.API qualified as Servant
import Servant.Client qualified as Servant
import Unison.Auth.HTTPClient (AuthenticatedHttpClient (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Share.Projects (hardCodedBaseUrl)
import Unison.Cli.Share.Ref
import Unison.Prelude
import Unison.Server.Types (DefinitionDisplayResults)

data SearchHit = SearchHit
  { projectRef :: Text,
    summary :: Maybe Text
  }

instance FromJSON SearchHit where
  parseJSON = Aeson.withObject "SearchHit" \o -> SearchHit <$> o .: "projectRef" <*> o .:? "summary"

newtype ReadmeResponse = ReadmeResponse Text

instance FromJSON ReadmeResponse where
  parseJSON = Aeson.withObject "ReadmeResponse" \o -> ReadmeResponse <$> o .: "markdownReadMe"

data ProjectInfo = ProjectInfo
  { latestRelease :: Maybe Text,
    defaultBranch :: Text
  }

instance FromJSON ProjectInfo where
  parseJSON = Aeson.withObject "ProjectInfo" \o -> ProjectInfo <$> o .:? "latestRelease" <*> o .: "defaultBranch"

type ShareAPI =
  ("search" :> Servant.QueryParam "query" Text :> Servant.Get '[Servant.JSON] [SearchHit])
    :<|> Project (Servant.Get '[Servant.JSON] ProjectInfo)
    :<|> Project ("readme" :> Servant.Get '[Servant.JSON] ReadmeResponse)
    :<|> Project ("branches" :> Servant.Capture "branch" Text :> DefinitionByName)
    :<|> Project ("releases" :> Servant.Capture "version" Text :> DefinitionByName)

type Project api =
  "users" :> Servant.Capture "handle" Text :> "projects" :> Servant.Capture "slug" Text :> api

type DefinitionByName =
  "definitions" :> "by-name" :> Servant.Capture "name" Text :> Servant.Get '[Servant.JSON] DefinitionDisplayResults

httpSearch :: Maybe Text -> Servant.ClientM [SearchHit]
httpProjectInfo :: Text -> Text -> Servant.ClientM ProjectInfo
httpReadme :: Text -> Text -> Servant.ClientM ReadmeResponse
httpBranchDefinition :: Text -> Text -> Text -> Text -> Servant.ClientM DefinitionDisplayResults
httpReleaseDefinition :: Text -> Text -> Text -> Text -> Servant.ClientM DefinitionDisplayResults
( httpSearch
    :<|> httpProjectInfo
    :<|> httpReadme
    :<|> httpBranchDefinition
    :<|> httpReleaseDefinition
  ) = Servant.client (Proxy @ShareAPI)

run :: Servant.ClientM a -> Cli (Either Servant.ClientError a)
run action = do
  Cli.Env {authHTTPClient = AuthenticatedHttpClient httpManager} <- ask
  liftIO $ Servant.runClientM action (Servant.mkClientEnv httpManager hardCodedBaseUrl)

searchProjects :: Text -> Cli (Either Servant.ClientError [SearchHit])
searchProjects query = run (httpSearch (Just query))

-- | The project's README, as markdown.
getReadme :: ShareRef -> Cli (Either Servant.ClientError Text)
getReadme ShareRef {handle, slug} =
  run (httpReadme handle slug) <&> fmap \(ReadmeResponse md) -> md

-- | Look up a definition by name. Names that don't exist are in `missingDefinitions`, not an error.
getDefinitions :: ShareRef -> Text -> Cli (Either Servant.ClientError DefinitionDisplayResults)
getDefinitions ShareRef {handle, slug, branch} name =
  case branch of
    ShareRelease version -> run (httpReleaseDefinition handle slug version name)
    ShareBranchNamed branchName -> run (httpBranchDefinition handle slug branchName name)
    ShareLatest ->
      run (httpProjectInfo handle slug) >>= \case
        Left err -> pure (Left err)
        Right ProjectInfo {latestRelease, defaultBranch} ->
          case latestRelease of
            Just version -> run (httpReleaseDefinition handle slug version name)
            Nothing -> run (httpBranchDefinition handle slug defaultBranch name)

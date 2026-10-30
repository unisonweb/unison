{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The Share API calls behind the @share.*@ commands: project search, READMEs, and definitions by name.
module Unison.Cli.Share.Definitions
  ( -- * API calls
    SearchHit (..),
    searchEntities,
    searchDefinitions,
    ReleaseInfo (..),
    listReleases,
    getReadme,
    getDefinitions,
  )
where

import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
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
import Unison.Server.Types (DefinitionDisplayResults, DefinitionSearchResult, DefinitionSearchResults (..))

-- | One match of an entity search: a project, or a user or organization.
data SearchHit
  = -- | The project, like @\@unison/base@, and its summary.
    ProjectHit Text (Maybe Text)
  | -- | The handle, with the @\@@, and the name.
    UserHit Text (Maybe Text)
  | OrgHit Text (Maybe Text)

instance FromJSON SearchHit where
  parseJSON = Aeson.withObject "SearchHit" \o ->
    o .: "tag" >>= \case
      "project" -> ProjectHit <$> o .: "projectRef" <*> o .:? "summary"
      "user" -> UserHit . ("@" <>) <$> o .: "handle" <*> o .:? "name"
      "org" -> do
        user <- o .: "user"
        OrgHit <$> (("@" <>) <$> user .: "handle") <*> user .:? "name"
      (tag :: Text) -> fail ("unknown kind of search hit: " <> Text.unpack tag)

-- | The hits we understand; Share may well add kinds we don't.
newtype SearchHits = SearchHits [SearchHit]

instance FromJSON SearchHits where
  parseJSON = Aeson.withArray "SearchHits" \hits ->
    SearchHits . catMaybes . toList <$> traverse (\hit -> (Just <$> parseJSON hit) <|> pure Nothing) hits

newtype ReadmeResponse = ReadmeResponse Text

instance FromJSON ReadmeResponse where
  parseJSON = Aeson.withObject "ReadmeResponse" \o -> ReadmeResponse <$> o .: "markdownReadMe"

data ProjectInfo = ProjectInfo
  { latestRelease :: Maybe Text,
    defaultBranch :: Text
  }

instance FromJSON ProjectInfo where
  parseJSON = Aeson.withObject "ProjectInfo" \o -> ProjectInfo <$> o .:? "latestRelease" <*> o .: "defaultBranch"

-- | One release of a project: its version, when it was published, and its status if it isn't published.
data ReleaseInfo = ReleaseInfo
  { version :: Text,
    date :: Text,
    status :: Text
  }

instance FromJSON ReleaseInfo where
  parseJSON = Aeson.withObject "ReleaseInfo" \o -> do
    version <- o .: "version"
    created <- o .: "createdAt"
    status <- o .: "status"
    statusName <- status .: "status"
    published <- status .:? "publishedAt"
    pure ReleaseInfo {version, date = Text.takeWhile (/= 'T') (fromMaybe created published), status = statusName}

newtype ReleasesResponse = ReleasesResponse [ReleaseInfo]

instance FromJSON ReleasesResponse where
  parseJSON = Aeson.withObject "ReleasesResponse" \o -> ReleasesResponse <$> o .: "items"

type ShareAPI =
  ("search" :> Servant.QueryParam "query" Text :> Servant.Get '[Servant.JSON] SearchHits)
    :<|> ( "search-definitions"
             :> Servant.QueryParam "query" Text
             :> Servant.QueryParam "project-filter" Text
             :> Servant.QueryParam "user-filter" Text
             :> Servant.Get '[Servant.JSON] DefinitionSearchResults
         )
    :<|> Project (Servant.Get '[Servant.JSON] ProjectInfo)
    :<|> Project ("releases" :> Servant.QueryParam "limit" Int :> Servant.Get '[Servant.JSON] ReleasesResponse)
    :<|> Project ("readme" :> Servant.Get '[Servant.JSON] ReadmeResponse)
    :<|> Project ("branches" :> Servant.Capture "branch" Text :> DefinitionByName)
    :<|> Project ("releases" :> Servant.Capture "version" Text :> DefinitionByName)

type Project api =
  "users" :> Servant.Capture "handle" Text :> "projects" :> Servant.Capture "slug" Text :> api

type DefinitionByName =
  "definitions" :> "by-name" :> Servant.Capture "name" Text :> Servant.Get '[Servant.JSON] DefinitionDisplayResults

httpSearch :: Maybe Text -> Servant.ClientM SearchHits
httpSearchDefinitions :: Maybe Text -> Maybe Text -> Maybe Text -> Servant.ClientM DefinitionSearchResults
httpProjectInfo :: Text -> Text -> Servant.ClientM ProjectInfo
httpReleases :: Text -> Text -> Maybe Int -> Servant.ClientM ReleasesResponse
httpReadme :: Text -> Text -> Servant.ClientM ReadmeResponse
httpBranchDefinition :: Text -> Text -> Text -> Text -> Servant.ClientM DefinitionDisplayResults
httpReleaseDefinition :: Text -> Text -> Text -> Text -> Servant.ClientM DefinitionDisplayResults
( httpSearch
    :<|> httpSearchDefinitions
    :<|> httpProjectInfo
    :<|> httpReleases
    :<|> httpReadme
    :<|> httpBranchDefinition
    :<|> httpReleaseDefinition
  ) = Servant.client (Proxy @ShareAPI)

run :: Servant.ClientM a -> Cli (Either Servant.ClientError a)
run action = do
  Cli.Env {authHTTPClient = AuthenticatedHttpClient httpManager} <- ask
  liftIO $ Servant.runClientM action (Servant.mkClientEnv httpManager hardCodedBaseUrl)

-- | Search for projects (and, for a query that starts with an @\@@, users and organizations).
searchEntities :: Text -> Cli (Either Servant.ClientError [SearchHit])
searchEntities query = run (httpSearch (Just query)) <&> fmap \(SearchHits hits) -> hits

-- | Search for terms and types, by name or by type, in all of Share or in part of it.
searchDefinitions :: Text -> Maybe SearchFilter -> Cli (Either Servant.ClientError [DefinitionSearchResult])
searchDefinitions query filter_ =
  run (httpSearchDefinitions (Just query) projectFilter userFilter) <&> fmap \(DefinitionSearchResults results) -> results
  where
    projectFilter = case filter_ of Just (FilterProject project) -> Just project; _ -> Nothing
    userFilter = case filter_ of Just (FilterUser user) -> Just user; _ -> Nothing

-- | The releases of a project, newest first (at most 100).
listReleases :: Text -> Text -> Cli (Either Servant.ClientError [ReleaseInfo])
listReleases handle slug =
  run (httpReleases handle slug (Just 100)) <&> fmap \(ReleasesResponse releases) -> releases

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

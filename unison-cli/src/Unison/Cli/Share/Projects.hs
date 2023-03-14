{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains Share API calls related to projects, wrapped in the Cli monad.
--
-- Here, we also validate inputs from Share that the API itself does not. For example, in the API,
-- a project name is just a Text. But because our client requires a richer structure for project names, we try parsing
-- them into a ProjectName, and fail right away if parsing fails.
module Unison.Cli.Share.Projects
  ( -- * API types
    RemoteProject (..),

    -- * API functions
    getProjectById,
    getProjectByName,
    createProject,
    getProjectBranchById,
    getProjectBranchByName,
    createProjectBranch,
    setProjectBranchHead,

    -- * Temporary special hard-coded base url
    hardCodedBaseUrl,
    hardCodedUri,
  )
where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.Proxy
import Network.URI (URI)
import qualified Network.URI as URI
import Servant.API ((:<|>) (..), (:>))
import Servant.Client
import U.Codebase.Sqlite.DbId (RemoteProjectBranchId (..), RemoteProjectId (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Auth.HTTPClient as Auth
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Share.API.Projects as Share.API
import Unison.Share.Codeserver (defaultCodeserver)
import Unison.Share.Types (codeserverBaseURL)
import Witch (unsafeFrom)

-- | A remote project.
data RemoteProject = RemoteProject
  { projectId :: RemoteProjectId,
    projectName :: ProjectName
  }
  deriving stock (Eq, Generic, Show)

-- | Get a project by id.
--
-- On success, update the `remote_project` table.
getProjectById :: RemoteProjectId -> Cli (Maybe RemoteProject)
getProjectById (RemoteProjectId projectId) = do
  response <- servantClientToCli (getProject0 (Just projectId) Nothing)
  onGetProjectResponse response

-- | Get a project by name.
--
-- On success, update the `remote_project` table.
getProjectByName :: ProjectName -> Cli (Maybe RemoteProject)
getProjectByName projectName = do
  response <- servantClientToCli (getProject0 Nothing (Just (into @Text projectName)))
  onGetProjectResponse response

-- | Create a new project.
--
-- On success, update the `remote_project` table.
createProject :: ProjectName -> Cli (Maybe RemoteProject)
createProject projectName = do
  let request = Share.API.CreateProjectRequest {projectName = into @Text projectName}
  servantClientToCli (createProject0 request) >>= \case
    Share.API.CreateProjectResponseNotFound {} -> pure Nothing
    Share.API.CreateProjectResponseUnauthorized x -> unauthorized x
    Share.API.CreateProjectResponseSuccess project -> Just <$> onProject project

-- | Get a project branch by id.
--
-- On success, update the `remote_project_branch` table.
getProjectBranchById :: ProjectAndBranch RemoteProjectId RemoteProjectBranchId -> Cli Share.API.GetProjectBranchResponse
getProjectBranchById (ProjectAndBranch (RemoteProjectId projectId) (RemoteProjectBranchId branchId)) = do
  response <- servantClientToCli (getProjectBranch0 projectId (Just branchId) Nothing)
  onGetProjectBranchResponse response
  pure response

-- | Get a project branch by name.
--
-- On success, update the `remote_project_branch` table.
getProjectBranchByName :: ProjectAndBranch RemoteProjectId ProjectBranchName -> Cli Share.API.GetProjectBranchResponse
getProjectBranchByName (ProjectAndBranch (RemoteProjectId projectId) branchName) = do
  response <- servantClientToCli (getProjectBranch0 projectId Nothing (Just (into @Text branchName)))
  onGetProjectBranchResponse response
  pure response

-- | Create a new project branch.
--
-- On success, update the `remote_project_branch` table.
createProjectBranch :: Share.API.CreateProjectBranchRequest -> Cli Share.API.CreateProjectBranchResponse
createProjectBranch request = do
  response <- servantClientToCli (createProjectBranch0 request)
  case response of
    Share.API.CreateProjectBranchResponseMissingCausalHash {} -> pure ()
    Share.API.CreateProjectBranchResponseNotFound {} -> pure ()
    Share.API.CreateProjectBranchResponseUnauthorized {} -> pure ()
    Share.API.CreateProjectBranchResponseSuccess branch -> onProjectBranch branch
  pure response

-- | Set a project branch head (can be a fast-forward or force-push).
setProjectBranchHead :: Share.API.SetProjectBranchHeadRequest -> Cli Share.API.SetProjectBranchHeadResponse
setProjectBranchHead request =
  servantClientToCli (setProjectBranchHead0 request)

------------------------------------------------------------------------------------------------------------------------
-- Database manipulation callbacks

onGetProjectResponse :: Share.API.GetProjectResponse -> Cli (Maybe RemoteProject)
onGetProjectResponse = \case
  Share.API.GetProjectResponseNotFound {} -> pure Nothing
  Share.API.GetProjectResponseUnauthorized x -> unauthorized x
  Share.API.GetProjectResponseSuccess project -> Just <$> onProject project

onGetProjectBranchResponse :: Share.API.GetProjectBranchResponse -> Cli ()
onGetProjectBranchResponse = \case
  Share.API.GetProjectBranchResponseBranchNotFound {} -> pure ()
  Share.API.GetProjectBranchResponseProjectNotFound {} -> pure ()
  Share.API.GetProjectBranchResponseUnauthorized {} -> pure ()
  Share.API.GetProjectBranchResponseSuccess branch -> onProjectBranch branch

onProject :: Share.API.Project -> Cli RemoteProject
onProject project = do
  let projectId = RemoteProjectId (project ^. #projectId)
  let projectName = unsafeFrom @Text (project ^. #projectName)
  Cli.runTransaction (Queries.ensureRemoteProject projectId hardCodedUri projectName)
  pure RemoteProject {projectId, projectName}

onProjectBranch :: Share.API.ProjectBranch -> Cli ()
onProjectBranch branch =
  Cli.runTransaction do
    Queries.ensureRemoteProjectBranch
      (RemoteProjectId (branch ^. #projectId))
      hardCodedUri
      (RemoteProjectBranchId (branch ^. #branchId))
      (unsafeFrom @Text (branch ^. #branchName))

unauthorized :: Share.API.Unauthorized -> Cli void
unauthorized (Share.API.Unauthorized message) =
  Cli.returnEarly (Output.Unauthorized message)

------------------------------------------------------------------------------------------------------------------------
-- Low-level servant client generation and wrapping

-- For now, since there's no syntax for specifying an alternative share server in any of the UCM commands, we
-- just hard-code the default codeserver here.
hardCodedBaseUrl :: BaseUrl
hardCodedBaseUrl =
  codeserverBaseURL defaultCodeserver

-- Like hardCodedBaseUri using an isomorphic-ish type
hardCodedUri :: URI
hardCodedUri =
  case URI.parseURI (showBaseUrl hardCodedBaseUrl) of
    Nothing -> error ("BaseUrl is an invalid URI: " ++ showBaseUrl hardCodedBaseUrl)
    Just uri -> uri

servantClientToCli :: ClientM a -> Cli a
servantClientToCli action = do
  Cli.Env {authHTTPClient = Auth.AuthenticatedHttpClient httpManager} <- ask

  let clientEnv :: ClientEnv
      clientEnv =
        mkClientEnv httpManager hardCodedBaseUrl

  liftIO (runClientM action clientEnv) & onLeftM \err -> do
    liftIO (print err)
    liftIO (putStrLn "FIXME: ^ make this prettier")
    Cli.returnEarlyWithoutOutput

getProject0 :: Maybe Text -> Maybe Text -> ClientM Share.API.GetProjectResponse
createProject0 :: Share.API.CreateProjectRequest -> ClientM Share.API.CreateProjectResponse
getProjectBranch0 :: Text -> Maybe Text -> Maybe Text -> ClientM Share.API.GetProjectBranchResponse
createProjectBranch0 :: Share.API.CreateProjectBranchRequest -> ClientM Share.API.CreateProjectBranchResponse
setProjectBranchHead0 :: Share.API.SetProjectBranchHeadRequest -> ClientM Share.API.SetProjectBranchHeadResponse
( getProject0
    :<|> createProject0
    :<|> getProjectBranch0
    :<|> createProjectBranch0
    :<|> setProjectBranchHead0
  ) =
    client (Proxy :: Proxy ("ucm" :> "v1" :> "projects" :> Share.API.ProjectsAPI))

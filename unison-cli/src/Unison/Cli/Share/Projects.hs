{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains Share API calls related to projects, wrapped in the Cli monad.
module Unison.Cli.Share.Projects
  ( -- * API functions
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
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Share.API.Projects as Share.API
import Unison.Share.Codeserver (defaultCodeserver)
import Unison.Share.Types (codeserverBaseURL)
import Witch (unsafeFrom)

-- | Get a project by id.
--
-- On success, update the `remote_project` table.
getProjectById :: RemoteProjectId -> Cli Share.API.GetProjectResponse
getProjectById (RemoteProjectId projectId) = do
  response <- servantClientToCli (getProject0 (Just projectId) Nothing)
  onGetProjectResponse response
  pure response

-- | Get a project by name.
--
-- On success, update the `remote_project` table.
getProjectByName :: ProjectName -> Cli Share.API.GetProjectResponse
getProjectByName projectName = do
  response <- servantClientToCli (getProject0 Nothing (Just (into @Text projectName)))
  onGetProjectResponse response
  pure response

-- | Create a new project.
--
-- On success, update the `remote_project` table.
createProject :: Share.API.CreateProjectRequest -> Cli Share.API.CreateProjectResponse
createProject request = do
  response <- servantClientToCli (createProject0 request)
  case response of
    Share.API.CreateProjectResponseNotFound {} -> pure ()
    Share.API.CreateProjectResponseUnauthorized {} -> pure ()
    Share.API.CreateProjectResponseSuccess project -> onProject project
  pure response

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

onGetProjectResponse :: Share.API.GetProjectResponse -> Cli ()
onGetProjectResponse = \case
  Share.API.GetProjectResponseNotFound {} -> pure ()
  Share.API.GetProjectResponseUnauthorized {} -> pure ()
  Share.API.GetProjectResponseSuccess project -> onProject project

onGetProjectBranchResponse :: Share.API.GetProjectBranchResponse -> Cli ()
onGetProjectBranchResponse = \case
  Share.API.GetProjectBranchResponseBranchNotFound {} -> pure ()
  Share.API.GetProjectBranchResponseProjectNotFound {} -> pure ()
  Share.API.GetProjectBranchResponseUnauthorized {} -> pure ()
  Share.API.GetProjectBranchResponseSuccess branch -> onProjectBranch branch

onProject :: Share.API.Project -> Cli ()
onProject project =
  Cli.runTransaction do
    Queries.ensureRemoteProject
      (RemoteProjectId (project ^. #projectId))
      hardCodedUri
      (unsafeFrom @Text (project ^. #projectName))

onProjectBranch :: Share.API.ProjectBranch -> Cli ()
onProjectBranch branch =
  Cli.runTransaction do
    Queries.ensureRemoteProjectBranch
      (RemoteProjectId (branch ^. #projectId))
      hardCodedUri
      (RemoteProjectBranchId (branch ^. #branchId))
      (unsafeFrom @Text (branch ^. #branchName))

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

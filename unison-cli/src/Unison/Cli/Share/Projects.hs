{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains Share API calls related to projects, wrapped in the Cli monad.
module Unison.Cli.Share.Projects
  ( getProjectById,
    getProjectByName,
    createProject,
    getProjectBranchById,
    getProjectBranchByName,
    createProjectBranch,
    setProjectBranchHead,
  )
where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.Proxy
import qualified Data.Text as Text
import Servant.API ((:<|>) (..), (:>))
import Servant.Client
import U.Codebase.Sqlite.DbId (RemoteProjectBranchId (..), RemoteProjectId (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.Auth.HTTPClient as Auth
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.API.Projects
import Unison.Share.Codeserver (defaultCodeserver)
import Unison.Share.Types (codeserverBaseURL)

-- | Get a project by id.
--
-- On success, update the `remote_project` table.
getProjectById :: RemoteProjectId -> Cli GetProjectResponse
getProjectById (RemoteProjectId projectId) = do
  response <- servantClientToCli (getProject0 (Just projectId) Nothing)
  onGetProjectResponse response
  pure response

-- | Get a project by name.
--
-- On success, update the `remote_project` table.
getProjectByName :: ProjectName -> Cli GetProjectResponse
getProjectByName projectName = do
  response <- servantClientToCli (getProject0 Nothing (Just (into @Text projectName)))
  onGetProjectResponse response
  pure response

-- | Create a new project.
--
-- On success, update the `remote_project` table.
createProject :: CreateProjectRequest -> Cli CreateProjectResponse
createProject request = do
  response <- servantClientToCli (createProject0 request)
  case response of
    CreateProjectResponseNotFound {} -> pure ()
    CreateProjectResponseUnauthorized {} -> pure ()
    CreateProjectResponseSuccess project -> onProject project
  pure response

-- | Get a project branch by id.
getProjectBranchById :: ProjectAndBranch RemoteProjectId RemoteProjectBranchId -> Cli GetProjectBranchResponse
getProjectBranchById (ProjectAndBranch (RemoteProjectId projectId) (RemoteProjectBranchId branchId)) =
  servantClientToCli (getProjectBranch0 projectId (Just branchId) Nothing)

-- | Get a project branch by name.
getProjectBranchByName :: ProjectAndBranch RemoteProjectId ProjectBranchName -> Cli GetProjectBranchResponse
getProjectBranchByName (ProjectAndBranch (RemoteProjectId projectId) branchName) =
  servantClientToCli (getProjectBranch0 projectId Nothing (Just (into @Text branchName)))

-- | Create a new project branch.
createProjectBranch :: CreateProjectBranchRequest -> Cli CreateProjectBranchResponse
createProjectBranch request =
  servantClientToCli (createProjectBranch0 request)

-- | Set a project branch head (can be a fast-forward or force-push).
setProjectBranchHead :: SetProjectBranchHeadRequest -> Cli SetProjectBranchHeadResponse
setProjectBranchHead request =
  servantClientToCli (setProjectBranchHead0 request)

------------------------------------------------------------------------------------------------------------------------
-- Database manipulation callbacks

onGetProjectResponse :: GetProjectResponse -> Cli ()
onGetProjectResponse = \case
  GetProjectResponseNotFound {} -> pure ()
  GetProjectResponseUnauthorized {} -> pure ()
  GetProjectResponseSuccess project -> onProject project

onProject :: Project -> Cli ()
onProject project =
  Cli.runTransaction do
    Queries.ensureRemoteProject
      (RemoteProjectId (project ^. #projectId))
      (Text.pack (showBaseUrl hardCodedBaseUrl))
      (project ^. #projectName)

------------------------------------------------------------------------------------------------------------------------
-- Low-level servant client generation and wrapping

-- For now, since there's no syntax for specifying an alternative share server in any of the UCM commands, we
-- just hard-code the default codeserver here.
hardCodedBaseUrl :: BaseUrl
hardCodedBaseUrl =
  codeserverBaseURL defaultCodeserver

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

getProject0 :: Maybe Text -> Maybe Text -> ClientM GetProjectResponse
createProject0 :: CreateProjectRequest -> ClientM CreateProjectResponse
getProjectBranch0 :: Text -> Maybe Text -> Maybe Text -> ClientM GetProjectBranchResponse
createProjectBranch0 :: CreateProjectBranchRequest -> ClientM CreateProjectBranchResponse
setProjectBranchHead0 :: SetProjectBranchHeadRequest -> ClientM SetProjectBranchHeadResponse
( getProject0
    :<|> createProject0
    :<|> getProjectBranch0
    :<|> createProjectBranch0
    :<|> setProjectBranchHead0
  ) =
    client (Proxy :: Proxy ("ucm" :> "v1" :> "projects" :> ProjectsAPI))

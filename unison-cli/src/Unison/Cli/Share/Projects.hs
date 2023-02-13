-- | Share API calls related to projects.
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

import Control.Monad.Reader (ask)
import Data.Proxy
import Servant.API ((:<|>) (..))
import Servant.Client
import U.Codebase.Sqlite.Queries (RemoteBranchId (..), RemoteProjectId (..))
import qualified Unison.Auth.HTTPClient as Auth
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Prelude
import Unison.Share.API.Projects
import Unison.Share.Codeserver (defaultCodeserver)
import Unison.Share.Types (codeserverBaseURL)

getProjectById :: RemoteProjectId -> Cli GetProjectResponse
getProjectById (RemoteProjectId projectId) =
  servantClientToCli (getProject0 (Just projectId) Nothing)

getProjectByName :: Text -> Cli GetProjectResponse
getProjectByName projectName =
  servantClientToCli (getProject0 Nothing (Just projectName))

createProject :: CreateProjectRequest -> Cli CreateProjectResponse
createProject request =
  servantClientToCli (createProject0 request)

getProjectBranchById :: RemoteProjectId -> RemoteBranchId -> Cli GetProjectBranchResponse
getProjectBranchById (RemoteProjectId projectId) (RemoteBranchId branchId) =
  servantClientToCli (getProjectBranch0 projectId (Just branchId) Nothing)

getProjectBranchByName :: RemoteProjectId -> Text -> Cli GetProjectBranchResponse
getProjectBranchByName (RemoteProjectId projectId) branchName =
  servantClientToCli (getProjectBranch0 projectId Nothing (Just branchName))

createProjectBranch :: CreateProjectBranchRequest -> Cli CreateProjectBranchResponse
createProjectBranch request =
  servantClientToCli (createProjectBranch0 request)

setProjectBranchHead :: SetProjectBranchHeadRequest -> Cli SetProjectBranchHeadResponse
setProjectBranchHead request =
  servantClientToCli (setProjectBranchHead0 request)

------------------------------------------------------------------------------------------------------------------------
-- Low-level servant client generation and wrapping

servantClientToCli :: ClientM a -> Cli a
servantClientToCli action = do
  Cli.Env {authHTTPClient = Auth.AuthenticatedHttpClient httpManager} <- ask

  let clientEnv :: ClientEnv
      clientEnv =
        -- For now, since there's no syntax for specifying an alternative share server in any of the UCM commands, we
        -- just hard-code the default codeserver here.
        mkClientEnv httpManager (codeserverBaseURL defaultCodeserver)

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
    client (Proxy :: Proxy ProjectsAPI)

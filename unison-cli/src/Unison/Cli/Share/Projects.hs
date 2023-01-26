-- | Share API calls related to projects.
module Unison.Cli.Share.Projects where

import Data.Proxy
import Servant.API ((:<|>) (..))
import Servant.Client
import Unison.Prelude
import Unison.Share.API.Projects

getProject0 :: Maybe Text -> Maybe Text -> ClientM GetProjectResponse
createProject :: CreateProjectRequest -> ClientM CreateProjectResponse
getProjectBranch :: Text -> Maybe Text -> Maybe Text -> ClientM GetProjectBranchResponse
createProjectBranch :: CreateProjectBranchRequest -> ClientM CreateProjectBranchResponse
setProjectBranchHead :: SetProjectBranchHeadRequest -> ClientM SetProjectBranchHeadResponse
( getProject0
    :<|> createProject
    :<|> getProjectBranch
    :<|> createProjectBranch
    :<|> setProjectBranchHead
  ) =
    hoistClient projectsAPI id (client projectsAPI)
    where
      projectsAPI :: Proxy ProjectsAPI
      projectsAPI = Proxy

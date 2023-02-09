-- | @project.push@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectPush
  ( projectPush,
  )
where

import Control.Lens ((^.))
import Data.Text as Text
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase.Editor.HandleInput.AuthLogin as AuthLogin
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch,
    ProjectBranchName,
    ProjectName,
    prependUserSlugToProjectBranchName,
    prependUserSlugToProjectName,
  )
import qualified Unison.Share.API.Projects as Share.API
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Sqlite as Sqlite
import Witch (unsafeFrom)

-- | Push a project branch.
projectPush :: Maybe (ProjectAndBranch ProjectName (Maybe ProjectBranchName)) -> Cli ()
projectPush maybeProjectAndBranch = do
  (projectId, branchId) <-
    getCurrentProjectBranch & onNothingM do
      loggeth ["Not currently on a branch"]
      Cli.returnEarlyWithoutOutput

  case maybeProjectAndBranch of
    Nothing -> do
      Cli.runTransaction oinkResolveRemoteIds >>= \case
        Nothing -> do
          loggeth ["We don't have a remote branch mapping for this branch or any ancestor"]
          loggeth ["Getting current logged-in user on Share"]
          myUserHandle <- oinkGetLoggedInUser
          loggeth ["Got current logged-in user on Share: ", myUserHandle]
          (project, branch) <-
            Cli.runTransaction do
              project <- Queries.expectProject projectId
              branch <- Queries.expectProjectBranch projectId branchId
              pure (project, branch)
          let localProjectName = unsafeFrom @Text (project ^. #name)
          let remoteProjectName = prependUserSlugToProjectName myUserHandle localProjectName
          response <- do
            let request = Share.API.CreateProjectRequest {projectName = into @Text remoteProjectName}
            loggeth ["Making create-project request for project"]
            loggeth [tShow request]
            Share.createProject request & onLeftM \err -> do
              loggeth ["Creating a project failed"]
              loggeth [tShow err]
              Cli.returnEarlyWithoutOutput
          remoteProject <-
            case response of
              Share.API.CreateProjectResponseBadRequest -> do
                loggeth ["Share says: bad request"]
                Cli.returnEarlyWithoutOutput
              Share.API.CreateProjectResponseUnauthorized -> do
                loggeth ["Share says: unauthorized"]
                Cli.returnEarlyWithoutOutput
              Share.API.CreateProjectResponseSuccess remoteProject -> pure remoteProject
          loggeth ["Share says: success!"]
          loggeth [tShow remoteProject]
          let localBranchName = unsafeFrom @Text (branch ^. #name)
          let remoteBranchName = prependUserSlugToProjectBranchName myUserHandle localBranchName
          loggeth ["Making create-branch request for branch", into @Text remoteProjectName]
          response <- do
            let request =
                  Share.API.CreateProjectBranchRequest
                    { projectId = remoteProject ^. #projectId,
                      branchName = into @Text remoteBranchName,
                      branchCausalHash = wundefined,
                      branchMergeTarget = wundefined
                    }
            loggeth ["Making create-project request for project"]
            loggeth [tShow request]
            Share.createProjectBranch request & onLeftM \err -> do
              loggeth ["Creating a branch failed"]
              loggeth [tShow err]
              Cli.returnEarlyWithoutOutput
          remoteBranch <-
            case response of
              Share.API.CreateProjectBranchResponseBadRequest -> do
                loggeth ["Share says: bad request"]
                Cli.returnEarlyWithoutOutput
              Share.API.CreateProjectBranchResponseUnauthorized -> do
                loggeth ["Share says: unauthorized"]
                Cli.returnEarlyWithoutOutput
              Share.API.CreateProjectBranchResponseSuccess remoteBranch -> pure remoteBranch
          loggeth ["Share says: success!"]
          loggeth [tShow remoteBranch]
        Just projectAndBranch ->
          case projectAndBranch ^. #branch of
            Nothing -> do
              let ancestorRemoteProjectId = projectAndBranch ^. #project
              loggeth ["We don't have a remote branch mapping, but our ancestor maps to project: ", ancestorRemoteProjectId]
              loggeth ["Creating remote branch not implemented"]
              Cli.returnEarlyWithoutOutput
            Just remoteBranchId -> do
              let remoteProjectId = projectAndBranch ^. #project
              loggeth ["Found remote branch mapping: ", remoteProjectId, ":", remoteBranchId]
              loggeth ["Pushing to existing branch not implemented"]
              Cli.returnEarlyWithoutOutput
    Just projectAndBranch -> do
      let _projectName = projectAndBranch ^. #project
      let _branchName = fromMaybe (unsafeFrom @Text "main") (projectAndBranch ^. #branch)
      loggeth ["Specifying project/branch to push to not implemented"]
      Cli.returnEarlyWithoutOutput

oinkResolveRemoteIds :: Sqlite.Transaction (Maybe (ProjectAndBranch Text (Maybe Text)))
oinkResolveRemoteIds = undefined

oinkGetLoggedInUser :: Cli Text
oinkGetLoggedInUser = do
  AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  wundefined

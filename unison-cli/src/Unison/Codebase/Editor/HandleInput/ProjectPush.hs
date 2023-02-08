-- | @project.push@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectPush
  ( projectPush,
  )
where

import Control.Lens ((^.))
import Data.Text as Text
import Data.Text.IO as Text
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName)
import Witch (unsafeFrom)

-- | Push a project branch.
projectPush :: Maybe (ProjectAndBranch ProjectName (Maybe ProjectBranchName)) -> Cli ()
projectPush maybeProjectAndBranch = do
  (projectId, currentBranchId) <-
    getCurrentProjectBranch & onNothingM do
      loggeth ["Not currently on a branch"]
      Cli.returnEarlyWithoutOutput

  -- Resolve where to push:
  --   if (project/branch names provided)
  --     if (ids in remote_project / remote_project_branch tables)
  --       use those
  --     else
  --       ask Share
  --   else if (default push location exists),
  --     if (its remote branch id is non-null)
  --       use that
  --     else
  --       if (this branch name exists in that project)
  --         use that
  --       else
  --         create a branch with this name
  --   else
  --     ask Share for my username
  --     if (I'm not logged in)
  --       fail -- don't know where to push
  --     else
  --

  case maybeProjectAndBranch of
    Nothing -> do
      maybeRemoteIds <- undefined :: Cli (Either Text (Text, Text))
      case maybeRemoteIds of
        Left ancestorRemoteProjectId -> do
          loggeth ["We don't have a remote branch mapping, but our ancestor maps to project: ", ancestorRemoteProjectId]
          loggeth ["Creating remote branch not implemented"]
          Cli.returnEarlyWithoutOutput
        Right (remoteProjectId, remoteBranchId) -> do
          loggeth ["Found remote branch mapping: ", remoteProjectId, ":", remoteBranchId]
          loggeth ["Pushing to existing branch not implemented"]
          Cli.returnEarlyWithoutOutput
    Just projectAndBranch -> do
      let _projectName = projectAndBranch ^. #project
      let _branchName = fromMaybe (unsafeFrom @Text "main") (projectAndBranch ^. #branch)
      loggeth ["Specifying project/branch to push to not implemented"]
      Cli.returnEarlyWithoutOutput

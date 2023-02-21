-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth, projectBranchPath)
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite
import Witch (unsafeFrom)

-- | Switch to (or create) a project or project branch.
projectSwitch :: These ProjectName ProjectBranchName -> Cli ()
projectSwitch = \case
  These projectName branchName ->
    switchToProjectAndBranch (ProjectAndBranch projectName branchName)
  This projectName -> switchToProject projectName
  That branchName -> switchToBranch branchName

-- Switch to a project name (going to branch "main" for now, but ideally we'd store the last branch a user was on).
switchToProject :: ProjectName -> Cli ()
switchToProject projectName =
  switchToProjectAndBranch
    ProjectAndBranch
      { project = projectName,
        branch = unsafeFrom @Text "main"
      }

data SwitchToBranchOutcome
  = SwitchedToExistingBranch
  | SwitchedToNewBranch

-- Switch to a branch in the current project. If it doesn't exist, create it.
switchToBranch :: ProjectBranchName -> Cli ()
switchToBranch branchName = do
  ProjectAndBranch {project = projectId, branch = currentBranchId} <-
    getCurrentProjectBranch & onNothingM do
      loggeth ["Not currently on a branch"]
      Cli.returnEarlyWithoutOutput
  (outcome, newBranchId) <-
    Cli.runTransaction do
      Queries.loadProjectBranchByName projectId (into @Text branchName) >>= \case
        Nothing -> do
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch projectId newBranchId (into @Text branchName)
          Queries.markProjectBranchChild projectId currentBranchId newBranchId
          pure (SwitchedToNewBranch, newBranchId)
        Just branch -> pure (SwitchedToExistingBranch, branch ^. #branchId)
  let path = projectBranchPath ProjectAndBranch {project = projectId, branch = newBranchId}
  case outcome of
    SwitchedToExistingBranch -> loggeth ["I just switch to a new branch"]
    SwitchedToNewBranch -> do
      Cli.stepAt "project.switch" (Path.unabsolute path, id) -- id creates empty branch
      loggeth ["I just created a new branch"]
  Cli.cd path

-- Switch to a project+branch.
switchToProjectAndBranch :: ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
switchToProjectAndBranch ProjectAndBranch {project = projectName, branch = branchName} = do
  projectAndBranch <- do
    maybeProjectAndBranch <-
      Cli.runTransaction do
        Queries.loadProjectByName (into @Text projectName) >>= \case
          Nothing -> pure Nothing
          Just project -> do
            let projectId = project ^. #projectId
            Queries.loadProjectBranchByName projectId (into @Text branchName) <&> \case
              Nothing -> Nothing
              Just branch -> Just ProjectAndBranch {project = projectId, branch = branch ^. #branchId}
    maybeProjectAndBranch & onNothing do
      loggeth ["Not found: ", into @Text projectName, ":", into @Text branchName]
      Cli.returnEarlyWithoutOutput

  Cli.cd (projectBranchPath projectAndBranch)

-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens (over, (^.))
import qualified Data.Text as Text
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (getBranch0At, stepAt)
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth, projectBranchPath)
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite
import Witch (unsafeFrom)

-- | Switch to (or create) a project or project branch.
projectSwitch :: These ProjectName ProjectBranchName -> Cli ()
projectSwitch = \case
  These projectName branchName -> switchToProjectAndBranch (ProjectAndBranch projectName branchName)
  This projectName -> switchToProjectAndBranch (ProjectAndBranch projectName (unsafeFrom @Text "main"))
  That branchName -> do
    projectAndBranch <-
      getCurrentProjectBranch & onNothingM do
        loggeth ["Not currently on a branch"]
        Cli.returnEarlyWithoutOutput
    let projectId = projectAndBranch ^. #project
    project <- Cli.runTransaction (Queries.expectProject projectId)
    switchToProjectAndBranch2 (ProjectAndBranch (projectId, project ^. #name) branchName) (Just projectAndBranch)

-- Switch to a project+branch.
switchToProjectAndBranch :: ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
switchToProjectAndBranch projectAndBranch = do
  project <-
    Cli.runTransaction (Queries.loadProjectByName (projectAndBranch ^. #project)) & onNothingM do
      loggeth ["no such project"]
      Cli.returnEarlyWithoutOutput
  let projectId = project ^. #projectId
  maybeCurrentProject <- getCurrentProjectBranch
  switchToProjectAndBranch2 (over #project (projectId,) projectAndBranch) maybeCurrentProject

data SwitchToBranchOutcome
  = SwitchedToExistingBranch
  | SwitchedToNewBranchFrom Sqlite.ProjectBranch -- branch we switched from

-- Switch to a project+branch.
switchToProjectAndBranch2 ::
  ProjectAndBranch (ProjectId, ProjectName) ProjectBranchName ->
  Maybe (ProjectAndBranch ProjectId ProjectBranchId) ->
  Cli ()
switchToProjectAndBranch2 (ProjectAndBranch (projectId, projectName) branchName) maybeCurrentProject = do
  (outcome, branchId) <-
    Cli.runTransaction do
      Queries.loadProjectBranchByName projectId branchName >>= \case
        Just branch -> pure (SwitchedToExistingBranch, branch ^. #branchId)
        Nothing -> do
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch projectId newBranchId branchName
          fromBranch <-
            case maybeCurrentProject of
              Just (ProjectAndBranch currentProjectId currentBranchId)
                | projectId == currentProjectId ->
                    Queries.expectProjectBranch currentProjectId currentBranchId
              _ -> do
                -- For now, we treat switching to a new branch from outside of a project as equivalent to switching to a
                -- new branch from the branch called "main" in that project. Eventually, we should probably instead
                -- use the default project branch
                Queries.loadProjectBranchByName projectId (unsafeFrom @Text "main") >>= \case
                  Nothing ->
                    error $
                      reportBug "E469471" $
                        "No branch called 'main' in project "
                          ++ Text.unpack (into @Text projectName)
                          ++ " (id = "
                          ++ show projectId
                          ++ "). We (currently) require 'main' to exist."
                  Just branch -> pure branch
          Queries.markProjectBranchChild projectId (fromBranch ^. #branchId) newBranchId
          pure (SwitchedToNewBranchFrom fromBranch, newBranchId)
  let path = projectBranchPath (ProjectAndBranch projectId branchId)
  case outcome of
    SwitchedToExistingBranch -> pure ()
    SwitchedToNewBranchFrom fromBranch -> do
      fromBranch0 <- Cli.getBranch0At (projectBranchPath (ProjectAndBranch projectId (fromBranch ^. #branchId)))
      Cli.stepAt "project.switch" (Path.unabsolute path, const fromBranch0)
      Cli.respond (Output.CreatedProjectBranch (fromBranch ^. #name) branchName)
  Cli.cd path

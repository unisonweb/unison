-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import qualified Data.UUID.V4 as UUID
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, projectBranchPath)
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite
import Witch (into, unsafeFrom)

-- | Switch to (or create) a project or project branch.
projectSwitch :: ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) -> Cli ()
projectSwitch projectAndBranch =
  case (projectAndBranch ^. #project, projectAndBranch ^. #branch) of
    (Just projectName, Just branchName) -> switchToProjectAndBranch projectName branchName
    (Just projectName, Nothing) -> switchToProject projectName
    (Nothing, Just branchName) -> switchToBranch branchName
    -- Kind of an impossible case :shrug:
    (Nothing, Nothing) -> pure ()

-- Switch to a project name (going to branch "main" for now, but ideally we'd store the last branch a user was on).
switchToProject :: ProjectName -> Cli ()
switchToProject projectName =
  switchToProjectAndBranch projectName (unsafeFrom @Text "main")

data SwitchToBranchOutcome
  = SwitchedToExistingBranch
  | SwitchedToNewBranch

-- Switch to a branch in the current project. If it doesn't exist, create it.
switchToBranch :: ProjectBranchName -> Cli ()
switchToBranch branchName = do
  (projectId, currentBranchId) <- getCurrentProjectBranch & onNothingM (error "not on project branch")
  (outcome, newBranchId) <-
    Cli.runTransaction do
      Queries.loadProjectBranchByName projectId (into @Text branchName) >>= \case
        Nothing -> do
          newBranchId <- Sqlite.unsafeIO (Queries.BranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch projectId newBranchId (into @Text branchName)
          Queries.markProjectBranchChild projectId currentBranchId newBranchId
          pure (SwitchedToNewBranch, newBranchId)
        Just branch -> pure (SwitchedToExistingBranch, branch ^. #branchId)
  let path = projectBranchPath projectId newBranchId
  case outcome of
    SwitchedToExistingBranch -> pure ()
    SwitchedToNewBranch -> do
      Cli.stepAt "project.create-branch" (Path.unabsolute path, id) -- id creates empty branch
      -- TODO print "I created a new branch"
      pure ()
  Cli.cd path

-- Switch to a project+branch.
switchToProjectAndBranch :: ProjectName -> ProjectBranchName -> Cli ()
switchToProjectAndBranch projectName branchName = do
  (projectId, branchId) <- do
    maybeIds <-
      Cli.runTransaction do
        Queries.loadProjectByName (into @Text projectName) >>= \case
          Nothing -> pure Nothing
          Just project -> do
            let projectId = project ^. #projectId
            Queries.loadProjectBranchByName projectId (into @Text branchName) <&> \case
              Nothing -> Nothing
              Just branch -> Just (projectId, branch ^. #branchId)
    case maybeIds of
      Just ids -> pure ids
      Nothing -> error "project not found"

  Cli.cd (projectBranchPath projectId branchId)

-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens (view, (^.))
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (getBranchAt, updateAt)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Branch as Branch (empty)
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite

data SwitchToBranchOutcome
  = SwitchedToExistingBranch
  | SwitchedToNewBranch (Maybe Sqlite.ProjectBranch) -- parent

-- | Switch to (or create) a project or project branch.
projectSwitch :: These ProjectName ProjectBranchName -> Cli ()
projectSwitch projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName branchName) <- ProjectUtils.hydrateNames projectAndBranchNames0
  project <-
    Cli.runTransaction (Queries.loadProjectByName projectName) & onNothingM do
      Cli.returnEarly (Output.LocalProjectBranchDoesntExist projectAndBranchNames)
  let projectId = project ^. #projectId
  maybeCurrentProject <- ProjectUtils.getCurrentProjectBranch
  (outcome, branchId) <-
    Cli.runTransaction do
      Queries.loadProjectBranchByName projectId branchName >>= \case
        Just branch -> pure (SwitchedToExistingBranch, branch ^. #branchId)
        Nothing -> do
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          -- If we create a new branch from outside its project, then the new branch is "detached" (no history, no
          -- parent).
          let parentBranch = do
                ProjectAndBranch currentProject currentBranch <- maybeCurrentProject
                guard (projectId == currentProject ^. #projectId)
                Just currentBranch
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId,
                branchId = newBranchId,
                name = branchName,
                parentBranchId = view #branchId <$> parentBranch
              }
          pure (SwitchedToNewBranch parentBranch, newBranchId)
  let path = ProjectUtils.projectBranchPath (ProjectAndBranch projectId branchId)
  case outcome of
    SwitchedToExistingBranch -> pure ()
    SwitchedToNewBranch maybeFromBranch -> do
      fromBranchObject <-
        case maybeFromBranch of
          Nothing -> pure Branch.empty
          Just fromBranch ->
            Cli.getBranchAt (ProjectUtils.projectBranchPath (ProjectAndBranch projectId (fromBranch ^. #branchId)))
      _ <- Cli.updateAt "project.switch" path (const fromBranchObject)
      Cli.respond $
        Output.CreatedProjectBranch
          ( case maybeFromBranch of
              Nothing -> Output.CreatedProjectBranchFrom'Nothingness
              Just fromBranch -> Output.CreatedProjectBranchFrom'ParentBranch (fromBranch ^. #name)
          )
          projectAndBranchNames
  Cli.cd path

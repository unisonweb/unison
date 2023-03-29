-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (getBranch0At, stepAt)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite

data SwitchToBranchOutcome
  = SwitchedToExistingBranch
  | SwitchedToNewBranchFrom Sqlite.ProjectBranch -- branch we switched from

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
    Cli.runEitherTransaction do
      Queries.loadProjectBranchByName projectId branchName >>= \case
        Just branch -> pure (Right (SwitchedToExistingBranch, branch ^. #branchId))
        Nothing -> do
          -- We don't support creating a new branch from outside its project, because we want to require that the user
          -- picks some parent branch to start from
          case maybeCurrentProject of
            Just (ProjectAndBranch currentProject currentBranch) | projectId == currentProject ^. #projectId -> do
              newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
              Queries.insertProjectBranch projectId newBranchId branchName
              Queries.markProjectBranchChild projectId (currentBranch ^. #branchId) newBranchId
              pure (Right (SwitchedToNewBranchFrom currentBranch, newBranchId))
            _ -> pure (Left (Output.RefusedToCreateProjectBranch projectAndBranchNames))
  let path = ProjectUtils.projectBranchPath (ProjectAndBranch projectId branchId)
  case outcome of
    SwitchedToExistingBranch -> pure ()
    SwitchedToNewBranchFrom fromBranch -> do
      fromBranch0 <-
        Cli.getBranch0At (ProjectUtils.projectBranchPath (ProjectAndBranch projectId (fromBranch ^. #branchId)))
      Cli.stepAt "project.switch" (Path.unabsolute path, const fromBranch0)
      Cli.respond (Output.CreatedProjectBranch (fromBranch ^. #name) branchName)
  Cli.cd path

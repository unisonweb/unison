-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import qualified Data.Text as Text
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
import Witch (unsafeFrom)

data SwitchToBranchOutcome
  = SwitchedToExistingBranch
  | SwitchedToNewBranchFrom Sqlite.ProjectBranch -- branch we switched from

-- | Switch to (or create) a project or project branch.
projectSwitch :: These ProjectName ProjectBranchName -> Cli ()
projectSwitch projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName branchName) <- ProjectUtils.hydrateNames projectAndBranchNames0
  project <-
    Cli.runTransaction (Queries.loadProjectByName (projectAndBranchNames ^. #project)) & onNothingM do
      Cli.returnEarly (Output.LocalProjectBranchDoesntExist projectAndBranchNames)
  let projectId = project ^. #projectId
  maybeCurrentProject <- ProjectUtils.getCurrentProjectBranch
  (outcome, branchId) <-
    Cli.runTransaction do
      Queries.loadProjectBranchByName projectId branchName >>= \case
        Just branch -> pure (SwitchedToExistingBranch, branch ^. #branchId)
        Nothing -> do
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch projectId newBranchId branchName
          fromBranch <-
            case maybeCurrentProject of
              Just (ProjectAndBranch currentProject currentBranch)
                | projectId == currentProject ^. #projectId -> pure currentBranch
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
  let path = ProjectUtils.projectBranchPath (ProjectAndBranch projectId branchId)
  case outcome of
    SwitchedToExistingBranch -> pure ()
    SwitchedToNewBranchFrom fromBranch -> do
      fromBranch0 <-
        Cli.getBranch0At (ProjectUtils.projectBranchPath (ProjectAndBranch projectId (fromBranch ^. #branchId)))
      Cli.stepAt "project.switch" (Path.unabsolute path, const fromBranch0)
      Cli.respond (Output.CreatedProjectBranch (fromBranch ^. #name) branchName)
  Cli.cd path

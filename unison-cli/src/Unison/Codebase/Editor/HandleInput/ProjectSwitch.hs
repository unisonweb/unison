-- | @project.switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)

-- | Switch to an existing project or project branch.
projectSwitch :: These ProjectName ProjectBranchName -> Cli ()
projectSwitch projectAndBranchNames0 = do
  projectAndBranchNames@(ProjectAndBranch projectName branchName) <- ProjectUtils.hydrateNames projectAndBranchNames0
  branch <-
    Cli.runTransaction (Queries.loadProjectBranchByNames projectName branchName) & onNothingM do
      Cli.returnEarly (Output.LocalProjectBranchDoesntExist projectAndBranchNames)
  Cli.cd (ProjectUtils.projectBranchPath (ProjectAndBranch (branch ^. #projectId) (branch ^. #branchId)))

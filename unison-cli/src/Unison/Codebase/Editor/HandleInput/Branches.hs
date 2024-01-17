-- | @branches@ input handler
module Unison.Codebase.Editor.HandleInput.Branches
  ( handleBranches,
  )
where

import Control.Lens (mapped, over, (^.), _2)
import Data.Map.Strict qualified as Map
import Network.URI (URI)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)

handleBranches :: Maybe ProjectName -> Cli ()
handleBranches maybeProjectName = do
  maybeCurrentProjectIds <- ProjectUtils.getCurrentProjectIds
  (project, branches) <-
    Cli.runTransactionWithRollback \rollback -> do
      project <-
        case maybeProjectName of
          Just projectName -> do
            Queries.loadProjectByName projectName & onNothingM do
              rollback (Output.LocalProjectDoesntExist projectName)
          Nothing -> do
            ProjectAndBranch projectId _ <- maybeCurrentProjectIds & onNothing (rollback Output.NotOnProjectBranch)
            Queries.expectProject projectId
      branches <- Queries.loadAllProjectBranchInfo (project ^. #projectId)
      pure (project, branches)
  Cli.respondNumbered (Output.ListBranches (project ^. #name) (f branches))
  where
    f ::
      Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName)) ->
      [(ProjectBranchName, [(URI, ProjectName, ProjectBranchName)])]
    f =
      over (mapped . _2) (map (\(h, (p, b)) -> (h, p, b)) . Map.toList) . Map.toList

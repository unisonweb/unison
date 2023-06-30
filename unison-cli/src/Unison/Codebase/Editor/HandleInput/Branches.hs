-- | @branches@ input handler
module Unison.Codebase.Editor.HandleInput.Branches
  ( handleBranches,
  )
where

import Control.Lens (mapped, over, (^.), _2)
import Data.Map.Strict qualified as Map
import Network.URI (URI)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Sqlite qualified as Sqlite

handleBranches :: Maybe ProjectName -> Cli ()
handleBranches maybeProjectName = do
  maybeCurrentProjectIds <- ProjectUtils.getCurrentProjectIds
  (project, branches) <-
    Cli.runEitherTransaction do
      let loadProject :: Sqlite.Transaction (Either Output.Output Sqlite.Project)
          loadProject =
            case maybeProjectName of
              Just projectName -> do
                Queries.loadProjectByName projectName <&> \case
                  Nothing -> Left (Output.LocalProjectDoesntExist projectName)
                  Just project -> Right project
              Nothing ->
                case maybeCurrentProjectIds of
                  Just (ProjectAndBranch projectId _) -> Right <$> Queries.expectProject projectId
                  Nothing -> pure (Left Output.NotOnProjectBranch)
      loadProject >>= \case
        Left err -> pure (Left err)
        Right project -> do
          branches <- Queries.loadAllProjectBranchInfo (project ^. #projectId)
          pure (Right (project, branches))
  Cli.respondNumbered (Output.ListBranches (project ^. #name) (f branches))
  where
    f ::
      Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName)) ->
      [(ProjectBranchName, [(URI, ProjectName, ProjectBranchName)])]
    f =
      over (mapped . _2) (map (\(h, (p, b)) -> (h, p, b)) . Map.toList) . Map.toList

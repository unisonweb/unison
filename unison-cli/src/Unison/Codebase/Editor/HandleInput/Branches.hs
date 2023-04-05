-- | @branches@ input handler
module Unison.Codebase.Editor.HandleInput.Branches
  ( handleBranches,
  )
where

import Control.Lens (mapped, over, (^.), _2)
import qualified Data.Map.Strict as Map
import Network.URI (URI)
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Prelude
import Unison.Project (ProjectBranchName, ProjectName)

handleBranches :: Cli ()
handleBranches = do
  project <- ProjectUtils.expectCurrentProject
  branches <- Cli.runTransaction (Queries.loadAllProjectBranchInfo (project ^. #projectId))
  Cli.respondNumbered (Output.ListBranches (project ^. #name) (f branches))
  where
    f ::
      Map ProjectBranchName (Map URI (ProjectName, ProjectBranchName)) ->
      [(ProjectBranchName, [(URI, ProjectName, ProjectBranchName)])]
    f =
      over (mapped . _2) (map (\(h, (p, b)) -> (h, p, b)) . Map.toList) . Map.toList

module Unison.Codebase.Editor.HandleInput.Global (forAllProjectBranches) where

import Control.Monad.Reader
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Core.Project
import Unison.Prelude
import Unison.Util.Monoid (foldMapM)

-- | Map over ALL project branches in the codebase.
-- This is a _very_ big hammer, that you should basically never use, except for things like debugging or migrations.
forAllProjectBranches :: (Monoid r) => ((ProjectAndBranch ProjectName ProjectBranchName, ProjectAndBranch ProjectId ProjectBranchId) -> Branch IO -> Cli r) -> Cli r
forAllProjectBranches f = do
  Cli.Env {codebase} <- ask
  projectBranches <- Cli.runTransaction Q.loadAllProjectBranchNamePairs
  projectBranches & foldMapM \(names, ids@(ProjectAndBranch projId branchId)) -> do
    b <- liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
    f (names, ids) b

-- | @delete.project@ input handler
module Unison.Codebase.Editor.HandleInput.DeleteProject
  ( handleDeleteProject,
  )
where

import Control.Lens
import Data.List qualified as List
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))
import Unison.Sqlite qualified as Sqlite

-- | Delete a project
handleDeleteProject :: ProjectName -> Cli ()
handleDeleteProject projectName = do
  ProjectPath currentProject _ _ <- Cli.getCurrentProjectPath

  projectToDelete <-
    Cli.runTransactionWithRollback \rollback -> do
      Queries.loadProjectByName projectName & onNothingM do
        rollback (Output.LocalProjectDoesntExist projectName)

  when (projectToDelete.projectId == currentProject.projectId) do
    nextLoc <- Cli.runTransaction $ findAnyBranchInCodebaseNotInProject (projectToDelete.projectId) `whenNothingM` createDummyProjectExcept projectToDelete.name
    Cli.switchProject nextLoc

  Cli.runTransaction do
    Queries.deleteProject (projectToDelete ^. #projectId)
  where
    findAnyBranchInCodebaseNotInProject :: ProjectId -> Sqlite.Transaction (Maybe (ProjectAndBranch ProjectId ProjectBranchId))
    findAnyBranchInCodebaseNotInProject exceptProjectId = do
      Queries.loadAllProjectBranchNamePairs
        <&> List.find (\(_, ProjectAndBranch projId _) -> projId /= exceptProjectId)
        <&> fmap \(_, pbIds) -> pbIds

    createDummyProjectExcept :: ProjectName -> Sqlite.Transaction (ProjectAndBranch ProjectId ProjectBranchId)
    createDummyProjectExcept (UnsafeProjectName "scratch") = do
      (_, emptyCausalHashId) <- Codebase.emptyCausalHash
      Ops.insertProjectAndBranch (UnsafeProjectName "scratch2") (UnsafeProjectBranchName "main") emptyCausalHashId
        <&> \(proj, branch) -> ProjectAndBranch proj.projectId branch.branchId
    createDummyProjectExcept _ = do
      (_, emptyCausalHashId) <- Codebase.emptyCausalHash
      Ops.insertProjectAndBranch (UnsafeProjectName "scratch") (UnsafeProjectBranchName "main") emptyCausalHashId
        <&> \(proj, branch) -> ProjectAndBranch proj.projectId branch.branchId

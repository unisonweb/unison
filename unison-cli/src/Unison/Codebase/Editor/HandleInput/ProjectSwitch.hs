-- | @switch@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectSwitch
  ( projectSwitch,
  )
where

import Control.Lens ((^.))
import Data.These (These (..))
import U.Codebase.Sqlite.Project as SqliteProject
import U.Codebase.Sqlite.ProjectBranch as SqliteProjectBranch
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.CommandLine.FuzzySelect qualified as Fuzzy
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectName)
import Witch (unsafeFrom)

-- | Switch to an existing project or project branch, with a flexible syntax that does not require prefixing branch
-- names with forward slashes (though doing so makes the command unambiguous).
--
-- When the argument is ambiguous, when outside of a project, "switch foo" means switch to project "foo", not branch
-- "foo" (since there is no current project). And when inside a project, "switch foo" means one of:
--
--   1. Switch to project "foo", since there isn't a branch "foo" in the current project
--   2. Switch to branch "foo", since there isn't a project "foo"
--   3. Complain, because there's both a project "foo" and a branch "foo" in the current project
projectSwitch :: Maybe ProjectAndBranchNames -> Cli ()
projectSwitch mayNames = do
  projectNames <- case mayNames of
    Nothing -> do
      fuzzySelectProjectBranch >>= \case
        Nothing -> Cli.returnEarlyWithoutOutput
        Just (ProjectAndBranch p b) -> pure (ProjectAndBranchNames'Unambiguous $ These p b)
    Just names -> pure names
  case projectNames of
    ProjectAndBranchNames'Ambiguous projectName branchName ->
      ProjectUtils.getCurrentProjectBranch >>= \case
        Nothing -> switchToProjectAndBranchByTheseNames (This projectName)
        Just (ProjectAndBranch currentProject _currentBranch, _restPath) -> do
          let currentProjectName = currentProject ^. #name
          (projectExists, branchExists) <-
            Cli.runTransaction do
              (,)
                <$> Queries.projectExistsByName projectName
                <*> Queries.projectBranchExistsByName (currentProject ^. #projectId) branchName
          case (projectExists, branchExists) of
            (False, False) -> Cli.respond (Output.LocalProjectNorProjectBranchExist projectName branchName)
            (False, True) -> switchToProjectAndBranchByTheseNames (These currentProjectName branchName)
            (True, False) -> switchToProjectAndBranchByTheseNames (This projectName)
            (True, True) ->
              Cli.respondNumbered $
                Output.AmbiguousSwitch
                  projectName
                  (ProjectAndBranch currentProjectName branchName)
    ProjectAndBranchNames'Unambiguous projectAndBranchNames0 ->
      switchToProjectAndBranchByTheseNames projectAndBranchNames0

switchToProjectAndBranchByTheseNames :: These ProjectName ProjectBranchName -> Cli ()
switchToProjectAndBranchByTheseNames projectAndBranchNames0 = do
  branch <- case projectAndBranchNames0 of
    This projectName -> Cli.runEitherTransaction do
      Queries.loadProjectByName projectName >>= \case
        Nothing -> pure (Left (Output.LocalProjectDoesntExist projectName))
        Just project ->
          Queries.loadMostRecentBranch (project ^. #projectId) >>= \case
            Nothing ->
              let branchName = unsafeFrom @Text "main"
               in Queries.loadProjectBranchByName (project ^. #projectId) branchName >>= \case
                    Nothing -> pure (Left (Output.LocalProjectBranchDoesntExist (ProjectAndBranch projectName branchName)))
                    Just branch -> Right <$> setMostRecentBranch branch
            Just branchId ->
              Queries.loadProjectBranch (project ^. #projectId) branchId >>= \case
                Nothing -> error "impossible"
                Just branch -> pure (Right branch)
    _ -> do
      projectAndBranchNames@(ProjectAndBranch projectName branchName) <- ProjectUtils.hydrateNames projectAndBranchNames0
      Cli.runEitherTransaction do
        Queries.loadProjectBranchByNames projectName branchName >>= \case
          Nothing -> pure (Left (Output.LocalProjectBranchDoesntExist projectAndBranchNames))
          Just branch -> Right <$> setMostRecentBranch branch
  Cli.cd (ProjectUtils.projectBranchPath (ProjectAndBranch (branch ^. #projectId) (branch ^. #branchId)))
  where
    setMostRecentBranch branch = do
      Queries.setMostRecentBranch (branch ^. #projectId) (branch ^. #branchId)
      pure branch

-- | Select a project branch from a list of all project branches
fuzzySelectProjectBranch :: Cli (Maybe (ProjectAndBranch ProjectName ProjectBranchName))
fuzzySelectProjectBranch = do
  mayCurrentPB <-
    ProjectUtils.getCurrentProjectBranch
      <&> \case
        Nothing -> Nothing
        Just (ProjectAndBranch p b, _path) -> Just (ProjectAndBranch (SqliteProject.projectId p) (SqliteProjectBranch.branchId b))
  allBranches <-
    Cli.runTransaction Queries.loadAllProjectBranchNamePairs
      <&> filter \(_names, ids) -> Just ids /= mayCurrentPB
  result <-
    liftIO $
      Fuzzy.fuzzySelect
        Fuzzy.defaultOptions {Fuzzy.allowMultiSelect = False}
        (\(names, _) -> into @Text names)
        allBranches
  case result of
    Just ((names, _ids) : _) ->
      pure . Just $ names
    _ -> pure Nothing

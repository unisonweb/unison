-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( -- * Project/path helpers
    getCurrentProject,
    expectCurrentProject,
    getCurrentProjectIds,
    getCurrentProjectBranch,
    expectCurrentProjectBranch,
    projectPath,
    projectBranchesPath,
    projectBranchPath,
    projectBranchSegment,
    projectBranchPathPrism,

    -- * Name hydration
    hydrateNames,

    -- * Loading local project info
    expectProjectAndBranchByIds,
    getProjectAndBranchByTheseNames,
    expectProjectAndBranchByTheseNames,
    expectLooseCodeOrProjectBranch,

    -- * Loading remote project info
    expectRemoteProjectByName,
    expectRemoteProjectBranchById,
    loadRemoteProjectBranchByName,
    expectRemoteProjectBranchByName,
    loadRemoteProjectBranchByNames,
    expectRemoteProjectBranchByNames,
    expectRemoteProjectBranchByTheseNames,
  )
where

import Control.Lens
import Data.These (These (..))
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase.Editor.Input (LooseCodeOrProject)
import Unison.Codebase.Editor.Output (Output (LocalProjectBranchDoesntExist))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Project.Util
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

-- | Get the current project that a user is on.
getCurrentProject :: Cli (Maybe Sqlite.Project)
getCurrentProject = do
  path <- Cli.getCurrentPath
  case preview projectBranchPathPrism path of
    Nothing -> pure Nothing
    Just (ProjectAndBranch projectId _branchId, _restPath) ->
      Cli.runTransaction do
        project <- Queries.expectProject projectId
        pure (Just project)

-- | Like 'getCurrentProject', but fails with a message if the user is not on a project branch.
expectCurrentProject :: Cli Sqlite.Project
expectCurrentProject = do
  getCurrentProject & onNothingM (Cli.returnEarly Output.NotOnProjectBranch)

-- | Get the current project ids that a user is on.
getCurrentProjectIds :: Cli (Maybe (ProjectAndBranch ProjectId ProjectBranchId))
getCurrentProjectIds =
  fmap fst . preview projectBranchPathPrism <$> Cli.getCurrentPath

-- | Get the current project+branch+branch path that a user is on.
getCurrentProjectBranch :: Cli (Maybe (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch, Path.Path))
getCurrentProjectBranch = do
  path <- Cli.getCurrentPath
  case preview projectBranchPathPrism path of
    Nothing -> pure Nothing
    Just (ProjectAndBranch projectId branchId, restPath) ->
      Cli.runTransaction do
        project <- Queries.expectProject projectId
        branch <- Queries.expectProjectBranch projectId branchId
        pure (Just (ProjectAndBranch project branch, restPath))

-- | Like 'getCurrentProjectBranch', but fails with a message if the user is not on a project branch.
expectCurrentProjectBranch :: Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch, Path.Path)
expectCurrentProjectBranch =
  getCurrentProjectBranch & onNothingM (Cli.returnEarly Output.NotOnProjectBranch)

-- We often accept a `These ProjectName ProjectBranchName` from the user, so they can leave off either a project or
-- branch name, which we infer. This helper "hydrates" such a type to a `(ProjectName, BranchName)`, using the following
-- defaults if a name is missing:
--
--   * The project at the current path
--   * The branch named "main"
hydrateNames :: These ProjectName ProjectBranchName -> Cli (ProjectAndBranch ProjectName ProjectBranchName)
hydrateNames = \case
  This projectName -> pure (ProjectAndBranch projectName (unsafeFrom @Text "main"))
  That branchName -> do
    (ProjectAndBranch project _branch, _restPath) <- expectCurrentProjectBranch
    pure (ProjectAndBranch (project ^. #name) branchName)
  These projectName branchName -> pure (ProjectAndBranch projectName branchName)

-- Expect a local project+branch by ids.
expectProjectAndBranchByIds ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Sqlite.Transaction (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
expectProjectAndBranchByIds (ProjectAndBranch projectId branchId) = do
  project <- Queries.expectProject projectId
  branch <- Queries.expectProjectBranch projectId branchId
  pure (ProjectAndBranch project branch)

-- Get a local project branch by a "these names", using the following defaults if a name is missing:
--
--   * The project at the current path
--   * The branch named "main"
getProjectAndBranchByTheseNames ::
  These ProjectName ProjectBranchName ->
  Cli (Maybe (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
getProjectAndBranchByTheseNames = \case
  This projectName -> getProjectAndBranchByTheseNames (These projectName (unsafeFrom @Text "main"))
  That branchName -> runMaybeT do
    (ProjectAndBranch project _branch, _restPath) <- MaybeT getCurrentProjectBranch
    branch <- MaybeT (Cli.runTransaction (Queries.loadProjectBranchByName (project ^. #projectId) branchName))
    pure (ProjectAndBranch project branch)
  These projectName branchName -> do
    Cli.runTransaction do
      runMaybeT do
        project <- MaybeT (Queries.loadProjectByName projectName)
        branch <- MaybeT (Queries.loadProjectBranchByName (project ^. #projectId) branchName)
        pure (ProjectAndBranch project branch)

-- Expect a local project branch by a "these names", using the following defaults if a name is missing:
--
--   * The project at the current path
--   * The branch named "main"
expectProjectAndBranchByTheseNames ::
  These ProjectName ProjectBranchName ->
  Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
expectProjectAndBranchByTheseNames = \case
  This projectName -> expectProjectAndBranchByTheseNames (These projectName (unsafeFrom @Text "main"))
  That branchName -> do
    (ProjectAndBranch project _branch, _restPath) <- expectCurrentProjectBranch
    branch <-
      Cli.runTransaction (Queries.loadProjectBranchByName (project ^. #projectId) branchName) & onNothingM do
        Cli.returnEarly (LocalProjectBranchDoesntExist (ProjectAndBranch (project ^. #name) branchName))
    pure (ProjectAndBranch project branch)
  These projectName branchName -> do
    maybeProjectAndBranch <-
      Cli.runTransaction do
        runMaybeT do
          project <- MaybeT (Queries.loadProjectByName projectName)
          branch <- MaybeT (Queries.loadProjectBranchByName (project ^. #projectId) branchName)
          pure (ProjectAndBranch project branch)
    maybeProjectAndBranch & onNothing do
      Cli.returnEarly (LocalProjectBranchDoesntExist (ProjectAndBranch projectName branchName))

-- | Expect/resolve a possibly-ambiguous "loose code or project", with the following rules:
--
--   1. If we have an unambiguous `/branch` or `project/branch`, look up in the database.
--   2. If we have an unambiguous `loose.code.path`, just return it.
--   3. If we have an ambiguous `foo`, *because we do not currently have an unambiguous syntax for relative paths*,
--      we elect to treat it as a loose code path (because `/branch` can be selected with a leading forward slash).
expectLooseCodeOrProjectBranch ::
  These Path' (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) ->
  Cli (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
expectLooseCodeOrProjectBranch =
  _Right expectProjectAndBranchByTheseNames . f
  where
    f :: LooseCodeOrProject -> Either Path' (These ProjectName ProjectBranchName) -- (Maybe ProjectName, ProjectBranchName)
    f = \case
      This path -> Left path
      That (ProjectAndBranch Nothing branch) -> Right (That branch)
      That (ProjectAndBranch (Just project) branch) -> Right (These project branch)
      These path _ -> Left path -- (3) above

------------------------------------------------------------------------------------------------------------------------
-- Remote project utils

expectRemoteProjectByName :: ProjectName -> Cli Share.RemoteProject
expectRemoteProjectByName remoteProjectName = do
  Share.getProjectByName remoteProjectName & onNothingM do
    Cli.returnEarly (Output.RemoteProjectDoesntExist Share.hardCodedUri remoteProjectName)

expectRemoteProjectBranchById ::
  ProjectAndBranch (RemoteProjectId, ProjectName) (RemoteProjectBranchId, ProjectBranchName) ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchById projectAndBranch = do
  Share.getProjectBranchById projectAndBranchIds >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> remoteProjectBranchDoesntExist projectAndBranchNames
    Share.GetProjectBranchResponseProjectNotFound -> remoteProjectBranchDoesntExist projectAndBranchNames
    Share.GetProjectBranchResponseSuccess branch -> pure branch
  where
    projectAndBranchIds = projectAndBranch & over #project fst & over #branch fst
    projectAndBranchNames = projectAndBranch & over #project snd & over #branch snd

loadRemoteProjectBranchByName ::
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli (Maybe Share.RemoteProjectBranch)
loadRemoteProjectBranchByName projectAndBranch =
  Share.getProjectBranchByName projectAndBranch <&> \case
    Share.GetProjectBranchResponseBranchNotFound -> Nothing
    Share.GetProjectBranchResponseProjectNotFound -> Nothing
    Share.GetProjectBranchResponseSuccess branch -> Just branch

expectRemoteProjectBranchByName ::
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByName projectAndBranch =
  Share.getProjectBranchByName (projectAndBranch & over #project fst) >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> doesntExist
    Share.GetProjectBranchResponseProjectNotFound -> doesntExist
    Share.GetProjectBranchResponseSuccess branch -> pure branch
  where
    doesntExist =
      remoteProjectBranchDoesntExist (projectAndBranch & over #project snd)

loadRemoteProjectBranchByNames ::
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli (Maybe Share.RemoteProjectBranch)
loadRemoteProjectBranchByNames (ProjectAndBranch projectName branchName) =
  runMaybeT do
    project <- MaybeT (Share.getProjectByName projectName)
    MaybeT (loadRemoteProjectBranchByName (ProjectAndBranch (project ^. #projectId) branchName))

expectRemoteProjectBranchByNames ::
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByNames (ProjectAndBranch projectName branchName) = do
  project <- expectRemoteProjectByName projectName
  expectRemoteProjectBranchByName (ProjectAndBranch (project ^. #projectId, project ^. #projectName) branchName)

-- Expect a remote project branch by a "these names".
--
--   If both names are provided, use them.
--
--   If only a project name is provided, use branch name "main".
--
--   If only a branch name is provided, use the current branch's remote mapping (falling back to its parent, etc) to get
--   the project.
expectRemoteProjectBranchByTheseNames :: These ProjectName ProjectBranchName -> Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByTheseNames = \case
  This remoteProjectName -> do
    remoteProject <- expectRemoteProjectByName remoteProjectName
    let remoteProjectId = remoteProject ^. #projectId
    let remoteBranchName = unsafeFrom @Text "main"
    expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
  That branchName -> do
    (ProjectAndBranch localProject localBranch, _restPath) <- expectCurrentProjectBranch
    let localProjectId = localProject ^. #projectId
    let localBranchId = localBranch ^. #branchId
    Cli.runTransaction (Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId) >>= \case
      Just (remoteProjectId, _maybeProjectBranchId) -> do
        remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
        expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, remoteProjectName) branchName)
      Nothing -> do
        Cli.returnEarly $
          Output.NoAssociatedRemoteProject
            Share.hardCodedUri
            (ProjectAndBranch (localProject ^. #name) (localBranch ^. #name))
  These projectName branchName -> do
    remoteProject <- expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, projectName) branchName)

remoteProjectBranchDoesntExist :: ProjectAndBranch ProjectName ProjectBranchName -> Cli void
remoteProjectBranchDoesntExist projectAndBranch =
  Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri projectAndBranch)

-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( -- * Project/path helpers
    expectProjectBranchByName,
    resolveBranchRelativePath,

    -- * Name hydration
    hydrateNames,

    -- * Loading local project info
    expectProjectAndBranchByIds,
    getProjectAndBranchByTheseNames,
    expectProjectAndBranchByTheseNames,
    expectLooseCodeOrProjectBranch,

    -- * Loading remote project info
    expectRemoteProjectById,
    expectRemoteProjectByName,
    expectRemoteProjectBranchById,
    loadRemoteProjectBranchByName,
    expectRemoteProjectBranchByName,
    loadRemoteProjectBranchByNames,
    expectRemoteProjectBranchByNames,
    expectRemoteProjectBranchByTheseNames,

    -- * Other helpers
    findTemporaryBranchName,
    expectLatestReleaseBranchName,
  )
where

import Control.Lens
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.These (These (..))
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Share.Projects (IncludeSquashedHead)
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase.Editor.Input (LooseCodeOrProject)
import Unison.Codebase.Editor.Output (Output (LocalProjectBranchDoesntExist))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..))
import Unison.Core.Project (ProjectBranchName (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

resolveBranchRelativePath :: BranchRelativePath -> Cli PP.ProjectPathCtx
resolveBranchRelativePath brp = do
  case brp of
    BranchPathInCurrentProject projBranchName path -> do
      projectAndBranch <- expectProjectAndBranchByTheseNames (That projBranchName)
      pure $ PP.ctxFromProjectAndBranch projectAndBranch path
    QualifiedBranchPath projName projBranchName path -> do
      projectAndBranch <- expectProjectAndBranchByTheseNames (These projName projBranchName)
      pure $ PP.ctxFromProjectAndBranch projectAndBranch path
    UnqualifiedPath newPath' -> do
      ppCtx <- Cli.getProjectPathCtx
      pure $ ppCtx & PP.absPath_ %~ \curPath -> Path.resolve curPath newPath'

-- @findTemporaryBranchName projectId preferred@ finds some unused branch name in @projectId@ with a name
-- like @preferred@.
findTemporaryBranchName :: ProjectId -> ProjectBranchName -> Transaction ProjectBranchName
findTemporaryBranchName projectId preferred = do
  allBranchNames <-
    fmap (Set.fromList . map snd) do
      Queries.loadAllProjectBranchesBeginningWith projectId Nothing

  let -- all branch name candidates in order of preference:
      --   prefix
      --   prefix-2
      --   prefix-3
      --   ...
      allCandidates :: [ProjectBranchName]
      allCandidates =
        preferred : do
          n <- [(2 :: Int) ..]
          pure (unsafeFrom @Text (into @Text preferred <> "-" <> tShow n))

  pure (fromJust (List.find (\name -> not (Set.member name allBranchNames)) allCandidates))

-- | Get the current project+branch+branch path that a user is on.
getCurrentProjectBranch :: Cli (Maybe (PP.ProjectPath Sqlite.Project Sqlite.ProjectBranch))
getCurrentProjectBranch = runMaybeT do
  ppCtx <- lift Cli.getProjectPathCtx
  mapMaybeT Cli.runTransaction $ do
    proj <- MaybeT $ Queries.loadProject (ppCtx ^. PP.ctxAsIds_ . PP.project_)
    branch <- MaybeT $ Queries.loadProjectBranch (proj ^. #projectId) (ppCtx ^. PP.ctxAsIds_ . PP.branch_)
    pure $ PP.ProjectPath proj branch (ppCtx ^. PP.absPath_)

expectProjectBranchByName :: Sqlite.Project -> ProjectBranchName -> Cli Sqlite.ProjectBranch
expectProjectBranchByName project branchName =
  Cli.runTransaction (Queries.loadProjectBranchByName (project ^. #projectId) branchName) & onNothingM do
    Cli.returnEarly (LocalProjectBranchDoesntExist (ProjectAndBranch (project ^. #name) branchName))

-- | Like 'getCurrentProjectBranch', but fails with a message if the user is not on a project branch.
expectCurrentProjectBranch :: Cli (PP.ProjectPath Sqlite.Project Sqlite.ProjectBranch)
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
    ppCtx <- Cli.getProjectPathCtx
    pure (ProjectAndBranch (ppCtx ^. PP.ctxAsNames_ . PP.project_) branchName)
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
    currentProjectBranch <- MaybeT getCurrentProjectBranch
    branch <- MaybeT (Cli.runTransaction (Queries.loadProjectBranchByName (currentProjectBranch ^. PP.project_ . #projectId) branchName))
    pure (ProjectAndBranch (currentProjectBranch ^. PP.project_) branch)
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
    PP.ProjectPath project _branch _restPath <- expectCurrentProjectBranch
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

-- | Expect a remote project by id. Its latest-known name is also provided, for error messages.
expectRemoteProjectById :: RemoteProjectId -> ProjectName -> Cli Share.RemoteProject
expectRemoteProjectById remoteProjectId remoteProjectName = do
  Share.getProjectById remoteProjectId & onNothingM do
    Cli.returnEarly (Output.RemoteProjectDoesntExist Share.hardCodedUri remoteProjectName)

expectRemoteProjectByName :: ProjectName -> Cli Share.RemoteProject
expectRemoteProjectByName remoteProjectName = do
  Share.getProjectByName remoteProjectName & onNothingM do
    Cli.returnEarly (Output.RemoteProjectDoesntExist Share.hardCodedUri remoteProjectName)

expectRemoteProjectBranchById ::
  IncludeSquashedHead ->
  ProjectAndBranch (RemoteProjectId, ProjectName) (RemoteProjectBranchId, ProjectBranchName) ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchById includeSquashed projectAndBranch = do
  Share.getProjectBranchById includeSquashed projectAndBranchIds >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> remoteProjectBranchDoesntExist projectAndBranchNames
    Share.GetProjectBranchResponseProjectNotFound -> remoteProjectBranchDoesntExist projectAndBranchNames
    Share.GetProjectBranchResponseSuccess branch -> pure branch
  where
    projectAndBranchIds = projectAndBranch & over #project fst & over #branch fst
    projectAndBranchNames = projectAndBranch & over #project snd & over #branch snd

loadRemoteProjectBranchByName ::
  IncludeSquashedHead ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli (Maybe Share.RemoteProjectBranch)
loadRemoteProjectBranchByName includeSquashed projectAndBranch =
  Share.getProjectBranchByName includeSquashed projectAndBranch <&> \case
    Share.GetProjectBranchResponseBranchNotFound -> Nothing
    Share.GetProjectBranchResponseProjectNotFound -> Nothing
    Share.GetProjectBranchResponseSuccess branch -> Just branch

expectRemoteProjectBranchByName ::
  IncludeSquashedHead ->
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByName includeSquashed projectAndBranch =
  Share.getProjectBranchByName includeSquashed (projectAndBranch & over #project fst) >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> doesntExist
    Share.GetProjectBranchResponseProjectNotFound -> doesntExist
    Share.GetProjectBranchResponseSuccess branch -> pure branch
  where
    doesntExist =
      remoteProjectBranchDoesntExist (projectAndBranch & over #project snd)

loadRemoteProjectBranchByNames ::
  IncludeSquashedHead ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli (Maybe Share.RemoteProjectBranch)
loadRemoteProjectBranchByNames includeSquashed (ProjectAndBranch projectName branchName) =
  runMaybeT do
    project <- MaybeT (Share.getProjectByName projectName)
    MaybeT (loadRemoteProjectBranchByName includeSquashed (ProjectAndBranch (project ^. #projectId) branchName))

expectRemoteProjectBranchByNames ::
  IncludeSquashedHead ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByNames includeSquashed (ProjectAndBranch projectName branchName) = do
  project <- expectRemoteProjectByName projectName
  expectRemoteProjectBranchByName includeSquashed (ProjectAndBranch (project ^. #projectId, project ^. #projectName) branchName)

-- Expect a remote project branch by a "these names".
--
--   If both names are provided, use them.
--
--   If only a project name is provided, use branch name "main".
--
--   If only a branch name is provided, use the current branch's remote mapping (falling back to its parent, etc) to get
--   the project.
expectRemoteProjectBranchByTheseNames :: IncludeSquashedHead -> These ProjectName ProjectBranchName -> Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByTheseNames includeSquashed = \case
  This remoteProjectName -> do
    remoteProject <- expectRemoteProjectByName remoteProjectName
    let remoteProjectId = remoteProject ^. #projectId
    let remoteBranchName = unsafeFrom @Text "main"
    expectRemoteProjectBranchByName includeSquashed (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
  That branchName -> do
    PP.ProjectPath localProject localBranch _restPath <- expectCurrentProjectBranch
    let localProjectId = localProject ^. #projectId
    let localBranchId = localBranch ^. #branchId
    Cli.runTransaction (Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId) >>= \case
      Just (remoteProjectId, _maybeProjectBranchId) -> do
        remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
        expectRemoteProjectBranchByName includeSquashed (ProjectAndBranch (remoteProjectId, remoteProjectName) branchName)
      Nothing -> do
        Cli.returnEarly $
          Output.NoAssociatedRemoteProject
            Share.hardCodedUri
            (ProjectAndBranch (localProject ^. #name) (localBranch ^. #name))
  These projectName branchName -> do
    remoteProject <- expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    expectRemoteProjectBranchByName includeSquashed (ProjectAndBranch (remoteProjectId, projectName) branchName)

remoteProjectBranchDoesntExist :: ProjectAndBranch ProjectName ProjectBranchName -> Cli void
remoteProjectBranchDoesntExist projectAndBranch =
  Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri projectAndBranch)

-- | Expect the given remote project to have a latest release, and return it as a valid branch name.
expectLatestReleaseBranchName :: Share.RemoteProject -> Cli ProjectBranchName
expectLatestReleaseBranchName remoteProject =
  case remoteProject.latestRelease of
    Nothing -> Cli.returnEarly (Output.ProjectHasNoReleases remoteProject.projectName)
    Just semver -> pure (UnsafeProjectBranchName ("releases/" <> into @Text semver))

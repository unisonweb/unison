-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( -- * Project/path helpers
    expectProjectBranchByName,
    resolveBranchRelativePath,
    resolveProjectBranch,
    resolveProjectBranchInProject,

    -- * Name hydration
    hydrateNames,

    -- * Loading local project info
    expectProjectAndBranchByIds,
    getProjectAndBranchByTheseNames,
    getProjectAndBranchByNames,
    expectProjectAndBranchByTheseNames,
    getProjectBranchCausalHash,

    -- * Loading remote project info
    expectRemoteProjectById,
    expectRemoteProjectByName,
    expectRemoteProjectBranchById,
    loadRemoteProjectBranchByName,
    expectRemoteProjectBranchByName,
    loadRemoteProjectBranchByNames,
    expectRemoteProjectBranchByNames,
    expectRemoteProjectBranchByTheseNames,

    -- * Projecting out common things
    justTheIds,
    justTheIds',
    justTheNames,

    -- * Other helpers
    findTemporaryBranchName,
    expectLatestReleaseBranchName,

    -- * Merge/upgrade branch utils
    getMergeBranchParent,
    getUpgradeBranchParent,
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Share.Projects (IncludeSquashedHead)
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase.Editor.Output (Output (LocalProjectBranchDoesntExist))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..))
import Unison.Core.Project (ProjectBranchName (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectName)
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

resolveBranchRelativePath :: BranchRelativePath -> Cli PP.ProjectPath
resolveBranchRelativePath brp = do
  case brp of
    BranchPathInCurrentProject projBranchName path -> do
      projectAndBranch <- expectProjectAndBranchByTheseNames (That projBranchName)
      pure $ PP.fromProjectAndBranch projectAndBranch path
    QualifiedBranchPath projName projBranchName path -> do
      projectAndBranch <- expectProjectAndBranchByTheseNames (These projName projBranchName)
      pure $ PP.fromProjectAndBranch projectAndBranch path
    UnqualifiedPath newPath' -> do
      pp <- Cli.getCurrentProjectPath
      pure $ pp & PP.absPath_ %~ \curPath -> Path.resolve curPath newPath'

justTheIds :: ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> ProjectAndBranch ProjectId ProjectBranchId
justTheIds x =
  ProjectAndBranch x.project.projectId x.branch.branchId

justTheIds' :: Sqlite.ProjectBranch -> ProjectAndBranch ProjectId ProjectBranchId
justTheIds' branch =
  ProjectAndBranch branch.projectId branch.branchId

justTheNames :: ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> ProjectAndBranch ProjectName ProjectBranchName
justTheNames x =
  ProjectAndBranch x.project.name x.branch.name

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

expectProjectBranchByName :: Sqlite.Project -> ProjectBranchName -> Cli Sqlite.ProjectBranch
expectProjectBranchByName project branchName =
  Cli.runTransaction (Queries.loadProjectBranchByName (project ^. #projectId) branchName) & onNothingM do
    Cli.returnEarly (LocalProjectBranchDoesntExist (ProjectAndBranch (project ^. #name) branchName))

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
    pp <- Cli.getCurrentProjectPath
    pure (ProjectAndBranch (pp ^. #project . #name) branchName)
  These projectName branchName -> pure (ProjectAndBranch projectName branchName)

getProjectAndBranchByNames :: ProjectAndBranch ProjectName ProjectBranchName -> Sqlite.Transaction (Maybe (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
getProjectAndBranchByNames (ProjectAndBranch projectName branchName) =
  runMaybeT do
    project <- MaybeT (Queries.loadProjectByName projectName)
    branch <- MaybeT (Queries.loadProjectBranchByName (project ^. #projectId) branchName)
    pure (ProjectAndBranch project branch)

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
    (PP.ProjectPath proj _branch _path) <- lift Cli.getCurrentProjectPath
    branch <- MaybeT (Cli.runTransaction (Queries.loadProjectBranchByName (proj ^. #projectId) branchName))
    pure (ProjectAndBranch proj branch)
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
    PP.ProjectPath project _branch _restPath <- Cli.getCurrentProjectPath
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

-- | Expect/resolve branch reference with the following rules:
--
--   1. If the project is missing, use the provided project.
--   2. If we have an unambiguous `/branch` or `project/branch`, resolve it using the provided
--      project, defaulting to 'main' if branch is unspecified.
resolveProjectBranchInProject :: Project -> ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) -> Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
resolveProjectBranchInProject defaultProj (ProjectAndBranch mayProjectName mayBranchName) = do
  let branchName = fromMaybe (unsafeFrom @Text "main") mayBranchName
  let projectName = fromMaybe (defaultProj ^. #name) mayProjectName
  projectAndBranch <- expectProjectAndBranchByTheseNames (These projectName branchName)
  pure projectAndBranch

-- | Expect/resolve branch reference with the following rules:
--
--   1. If the project is missing, use the current project.
--   2. If we have an unambiguous `/branch` or `project/branch`, resolve it using the current
--      project, defaulting to 'main' if branch is unspecified.
resolveProjectBranch :: ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) -> Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
resolveProjectBranch pab = do
  pp <- Cli.getCurrentProjectPath
  resolveProjectBranchInProject (pp ^. #project) pab

-- | Get the causal hash of a project branch.
getProjectBranchCausalHash :: ProjectBranch -> Transaction CausalHash
getProjectBranchCausalHash ProjectBranch {projectId, branchId} = do
  causalHashId <- Q.expectProjectBranchHead projectId branchId
  Q.expectCausalHash causalHashId

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
    PP.ProjectPath localProject localBranch _restPath <- Cli.getCurrentProjectPath
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

-- | @getMergeBranchParent branch@ returns the parent branch of a "merge" branch.
--
-- When a merge fails, we put you on a branch called `merge-<source>-into-<target>`. That's a "merge" branch. It's not
-- currently distinguished in the database, so we first just switch on whether its name begins with "merge-". If it
-- does, then we get the branch's parent, which should exist, but perhaps wouldn't if the user had manually made a
-- parentless branch called "merge-whatever" for whatever reason.
getMergeBranchParent :: Sqlite.ProjectBranch -> Maybe ProjectBranchId
getMergeBranchParent branch = do
  guard ("merge-" `Text.isPrefixOf` into @Text branch.name)
  branch.parentBranchId

-- | @getUpgradeBranchParent branch@ returns the parent branch of an "upgrade" branch.
--
-- When an upgrade fails, we put you on a branch called `upgrade-<old>-to-<new>`. That's an "upgrade" branch. It's not
-- currently distinguished in the database, so we first just switch on whether its name begins with "upgrade-". If it
-- does, then we get the branch's parent, which should exist, but perhaps wouldn't if the user had manually made a
-- parentless branch called "upgrade-whatever" for whatever reason.
getUpgradeBranchParent :: Sqlite.ProjectBranch -> Maybe ProjectBranchId
getUpgradeBranchParent branch = do
  guard ("upgrade-" `Text.isPrefixOf` into @Text branch.name)
  branch.parentBranchId

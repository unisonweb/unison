-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( -- * Project/path helpers
    getCurrentProjectBranch,
    expectCurrentProjectBranch,
    projectPath,
    projectBranchPath,
    projectBranchPathPrism,

    -- * Name hydration
    hydrateNames,

    -- * Loading local project info
    expectProjectAndBranchByIds,
    expectProjectAndBranchByTheseNames,

    -- * Loading remote project info
    expectRemoteProjectByName,
    expectRemoteProjectBranchById,
    expectRemoteProjectBranchByName,

    -- ** Temp
    loggeth,
  )
where

import Control.Lens
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.These (These (..))
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Project as Sqlite
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Cli.Share.Projects as Share
import Unison.Codebase.Editor.Output (Output (LocalProjectBranchDoesntExist))
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Sqlite as Sqlite
import Witch (unsafeFrom)

-- | Get the current project+branch that a user is on.
--
-- Note that if a user has (somehow) cd'd into a namespace *within* a branch, this function will return Nothing; that
-- is, it only returns Just if the user's current namespace is the root of a branch, and no deeper.
--
-- This should be fine: we don't want users to be able to cd around willy-nilly within projects (right?...)
getCurrentProjectBranch :: Cli (Maybe (ProjectAndBranch ProjectId ProjectBranchId))
getCurrentProjectBranch = do
  path <- Cli.getCurrentPath
  pure (preview projectBranchPathPrism path)

-- | Like 'getCurrentProjectBranch', but fails with a message if the user is not on a project branch.
expectCurrentProjectBranch :: Cli (ProjectAndBranch ProjectId ProjectBranchId)
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
    ProjectAndBranch projectId _branchId <- expectCurrentProjectBranch
    Cli.runTransaction do
      project <- Queries.expectProject projectId
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
    ProjectAndBranch projectId _branchId <- expectCurrentProjectBranch
    project <- Cli.runTransaction (Queries.expectProject projectId)
    branch <-
      Cli.runTransaction (Queries.loadProjectBranchByName projectId branchName) & onNothingM do
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

------------------------------------------------------------------------------------------------------------------------
-- Remote project utils

expectRemoteProjectByName :: ProjectName -> Cli Share.RemoteProject
expectRemoteProjectByName remoteProjectName = do
  Share.getProjectByName remoteProjectName & onNothingM do
    loggeth ["remote project doesn't exist"]
    Cli.returnEarlyWithoutOutput

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

remoteProjectBranchDoesntExist :: ProjectAndBranch ProjectName ProjectBranchName -> Cli void
remoteProjectBranchDoesntExist projectAndBranch =
  Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri projectAndBranch)

------------------------------------------------------------------------------------------------------------------------

-- | Get the path that a project is stored at. Users aren't supposed to go here.
--
-- >>> projectPath "ABCD"
-- .__projects._ABCD
projectPath :: ProjectId -> Path.Absolute
projectPath projectId =
  review projectPathPrism projectId

-- | Get the path that a branch is stored at. Users aren't supposed to go here.
--
-- >>> projectBranchPath ProjectAndBranch { project = "ABCD", branch = "DEFG" }
-- .__projects._ABCD.branches._DEFG
projectBranchPath :: ProjectAndBranch ProjectId ProjectBranchId -> Path.Absolute
projectBranchPath =
  review projectBranchPathPrism

pattern UUIDNameSegment :: UUID -> NameSegment
pattern UUIDNameSegment uuid <-
  NameSegment (Text.uncons -> Just ('_', UUID.fromText . Text.map (\c -> if c == '_' then '-' else c) -> Just uuid))
  where
    UUIDNameSegment uuid = NameSegment (Text.cons '_' (Text.map (\c -> if c == '-' then '_' else c) (UUID.toText uuid)))

-- | The prism between paths like
--
-- @
-- .__projects._XX_XX
-- @
--
-- and the project id
--
-- @
-- XX-XX
-- @
projectPathPrism :: Prism' Path.Absolute ProjectId
projectPathPrism =
  prism' toPath toId
  where
    toPath :: ProjectId -> Path.Absolute
    toPath projectId =
      Path.Absolute $
        Path.fromList
          [ "__projects",
            UUIDNameSegment (unProjectId projectId)
          ]

    toId :: Path.Absolute -> Maybe ProjectId
    toId path =
      case Path.toList (Path.unabsolute path) of
        ["__projects", UUIDNameSegment projectId] -> Just (ProjectId projectId)
        _ -> Nothing

-- | The prism between paths like
--
-- @
-- .__projects._XX_XX.branches._YY_YY
-- @
--
-- and the @(project id, branch id)@ pair
--
-- @
-- (XX-XX, YY-YY)
-- @
projectBranchPathPrism :: Prism' Path.Absolute (ProjectAndBranch ProjectId ProjectBranchId)
projectBranchPathPrism =
  prism' toPath toIds
  where
    toPath :: ProjectAndBranch ProjectId ProjectBranchId -> Path.Absolute
    toPath ProjectAndBranch {project = projectId, branch = branchId} =
      Path.Absolute $
        Path.fromList
          [ "__projects",
            UUIDNameSegment (unProjectId projectId),
            "branches",
            UUIDNameSegment (unProjectBranchId branchId)
          ]

    toIds :: Path.Absolute -> Maybe (ProjectAndBranch ProjectId ProjectBranchId)
    toIds path =
      case Path.toList (Path.unabsolute path) of
        ["__projects", UUIDNameSegment projectId, "branches", UUIDNameSegment branchId] ->
          Just ProjectAndBranch {project = ProjectId projectId, branch = ProjectBranchId branchId}
        _ -> Nothing

-- Dumb temporary debug logger to use for the new project commands
loggeth :: [Text] -> Cli ()
loggeth =
  liftIO . Text.putStrLn . Text.concat . ("[coolbeans] " :)

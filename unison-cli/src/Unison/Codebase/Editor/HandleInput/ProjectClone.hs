-- | @clone@-related input handlers
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( handleClone,
  )
where

import Control.Lens (over, (^.), _2)
import Control.Monad.Reader (ask)
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..))
import qualified U.Codebase.Sqlite.DbId as Sqlite
import qualified U.Codebase.Sqlite.Project as Sqlite (Project)
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (projectBranchPath)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleInput.Pull as HandleInput.Pull
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectName, projectNameUserSlug)
import qualified Unison.Share.API.Hash as Share.API
import qualified Unison.Share.Sync as Share (downloadEntities)
import qualified Unison.Share.Sync.Types as Share
import qualified Unison.Sqlite as Sqlite
import Unison.Sync.Common (hash32ToCausalHash)
import qualified Unison.Sync.Types as Share
import Witch (unsafeFrom)

data LocalProjectKey
  = LocalProjectKey'Name ProjectName
  | LocalProjectKey'Project Sqlite.Project

data RemoteProjectKey
  = RemoteProjectKey'Id Sqlite.RemoteProjectId
  | RemoteProjectKey'Name ProjectName

-- | Clone a remote branch.
handleClone :: ProjectAndBranchNames -> Maybe ProjectAndBranchNames -> Cli ()
handleClone remoteNames0 maybeLocalNames0 = do
  maybeCurrentProjectBranch <- ProjectUtils.getCurrentProjectBranch
  resolvedRemoteNames <- resolveRemoteNames maybeCurrentProjectBranch remoteNames0
  localNames1 <- resolveLocalNames maybeCurrentProjectBranch resolvedRemoteNames maybeLocalNames0
  cloneInto localNames1 (resolvedRemoteNames ^. #branch)

data ResolvedRemoteNames = ResolvedRemoteNames
  { branch :: Share.RemoteProjectBranch,
    from :: ResolvedRemoteNamesFrom
  }
  deriving stock (Generic)

data ResolvedRemoteNamesFrom
  = ResolvedRemoteNamesFrom'Branch
  | ResolvedRemoteNamesFrom'Project
  | ResolvedRemoteNamesFrom'ProjectAndBranch

-- Resolve remote names to an actual remote branch.
--
--   <project>/           ==>   abort if <project> doesn't have a user slug
--
--   /<branch>            ==>   abort if not in a project
--
--   <project>/<branch>   ==>   abort if <project> doesn't have a user slug
--
--   <thing>              ==>   if we're not in a project, then treat as if it was <thing>/
--
--                              otherwise, if <thing> doesn't have a user slug, treat it as /<thing>
--
--                              otherwise, hit the server, and if <thing>/ xor /<thing> was valid (e.g. cloning the
--                              "@runar/topic" project-or-branch, where the "@runar/topic" branch does exist in the
--                              project in question, and the "@runar/topic" project does not exist), we'll do that,
--                              otherwise abort
resolveRemoteNames ::
  Maybe (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch, Path) ->
  ProjectAndBranchNames ->
  Cli ResolvedRemoteNames
resolveRemoteNames maybeCurrentProjectBranch = \case
  ProjectAndBranchNames'Ambiguous remoteProjectName remoteBranchName ->
    case maybeCurrentProjectBranch of
      Nothing -> resolveP remoteProjectName
      Just (currentProjectBranch, _path) ->
        case projectNameUserSlug remoteProjectName of
          Nothing -> resolveB remoteBranchName
          Just _ -> do
            branchProjectKey <- Cli.runTransaction (projectBranchRemoteProjectKey currentProjectBranch)
            -- Fetching these in parallel would be an improvement
            maybeRemoteProject <- Share.getProjectByName remoteProjectName
            maybeRemoteBranch <-
              runMaybeT do
                remoteProjectId <-
                  case branchProjectKey of
                    RemoteProjectKey'Id remoteProjectId -> pure remoteProjectId
                    RemoteProjectKey'Name remoteBranchProjectName -> do
                      remoteBranchProject <- MaybeT (Share.getProjectByName remoteBranchProjectName)
                      pure (remoteBranchProject ^. #projectId)
                MaybeT do
                  Share.getProjectBranchByName (ProjectAndBranch remoteProjectId remoteBranchName) <&> \case
                    Share.GetProjectBranchResponseBranchNotFound -> Nothing
                    Share.GetProjectBranchResponseProjectNotFound -> Nothing
                    Share.GetProjectBranchResponseSuccess remoteBranch -> Just remoteBranch
            case (maybeRemoteProject, maybeRemoteBranch) of
              (Just remoteProject, Nothing) -> do
                let remoteProjectId = remoteProject ^. #projectId
                let remoteProjectName = remoteProject ^. #projectName
                let remoteBranchName = unsafeFrom @Text "main"
                remoteBranch <-
                  ProjectUtils.expectRemoteProjectBranchByName
                    (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
                pure
                  ResolvedRemoteNames
                    { branch = remoteBranch,
                      from = ResolvedRemoteNamesFrom'Project
                    }
              (Nothing, Just remoteBranch) ->
                pure
                  ResolvedRemoteNames
                    { branch = remoteBranch,
                      from = ResolvedRemoteNamesFrom'Branch
                    }
              -- Treat neither existing and both existing uniformly as "ambiguous input"
              -- Alternatively, if neither exist, we could instead say "although your input was ambiguous, disambuating
              -- wouldn't help, because we did enough work to know neither thing exists"
              _ -> do
                branchProjectName <-
                  case branchProjectKey of
                    RemoteProjectKey'Id remoteProjectId ->
                      Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
                    RemoteProjectKey'Name remoteProjectName -> pure remoteProjectName
                Cli.returnEarly $
                  Output.AmbiguousCloneRemote
                    remoteProjectName
                    (ProjectAndBranch branchProjectName remoteBranchName)
  ProjectAndBranchNames'Unambiguous (This p) -> resolveP p
  ProjectAndBranchNames'Unambiguous (That b) -> resolveB b
  ProjectAndBranchNames'Unambiguous (These p b) -> resolvePB p b
  where
    resolveB branchName = do
      (currentProjectBranch, _path) <- maybeCurrentProjectBranch & onNothing (Cli.returnEarly Output.NotOnProjectBranch)
      projectKey <- Cli.runTransaction (projectBranchRemoteProjectKey currentProjectBranch)
      branch <- expectB projectKey branchName
      pure ResolvedRemoteNames {branch, from = ResolvedRemoteNamesFrom'Branch}

    resolveP projectName = do
      assertProjectNameHasUserSlug projectName
      branch <- expectB (RemoteProjectKey'Name projectName) (unsafeFrom @Text "main")
      pure ResolvedRemoteNames {branch, from = ResolvedRemoteNamesFrom'Project}

    resolvePB projectName branchName = do
      assertProjectNameHasUserSlug projectName
      branch <- expectB (RemoteProjectKey'Name projectName) branchName
      pure ResolvedRemoteNames {branch, from = ResolvedRemoteNamesFrom'ProjectAndBranch}

    expectB remoteProjectKey remoteBranchName =
      case remoteProjectKey of
        RemoteProjectKey'Id remoteProjectId -> do
          remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
          ProjectUtils.expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
        RemoteProjectKey'Name remoteProjectName ->
          ProjectUtils.expectRemoteProjectBranchByNames (ProjectAndBranch remoteProjectName remoteBranchName)

-- Resolve the local names to an actual local project (which may not exist yet), aborting on nonsense
-- inputs:
--
--   <project>/           ==>   if we already know the remote branch name is <branch>, abort if <project>/<branch>
--                              already exists
--
--   /<branch>            ==>   abort if not in a project
--
--                              abort if <branch> already exists in this project
--
--   <project>/<branch>   ==>   abort if <project>/<branch> already exists
--
--   <thing>              ==>   if we're not in a project, then treat as if it was <thing>/
--
--                              otherwise, <thing> is ambiguous, as we don't know if the user wants to clone into
--                              <thing>/<branch> (where <branch> is determined by the name of the remote branch we
--                              are cloning), or /<thing>
--
-- The resolved remote names are used to fill in missing local names (i.e. a one-argument clone). For example, if
-- `clone @foo/bar` resulted in treating `@foo/bar` as a contributor branch of the current project, then it is as if
-- the user typed `clone /@foo/bar` instead, which is equivalent to the two-arg `clone /@foo/bar /@foo/bar`.
resolveLocalNames ::
  Maybe (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch, Path) ->
  ResolvedRemoteNames ->
  Maybe ProjectAndBranchNames ->
  Cli (ProjectAndBranch LocalProjectKey ProjectBranchName)
resolveLocalNames maybeCurrentProjectBranch resolvedRemoteNames maybeLocalNames =
  resolve case maybeLocalNames of
    Nothing ->
      ProjectAndBranchNames'Unambiguous case resolvedRemoteNames ^. #from of
        ResolvedRemoteNamesFrom'Branch -> That remoteBranchName
        ResolvedRemoteNamesFrom'Project -> This remoteProjectName
        ResolvedRemoteNamesFrom'ProjectAndBranch -> These remoteProjectName remoteBranchName
    Just localNames -> localNames
  where
    remoteBranchName = resolvedRemoteNames ^. #branch ^. #branchName
    remoteProjectName = resolvedRemoteNames ^. #branch ^. #projectName

    resolve names =
      case names of
        ProjectAndBranchNames'Ambiguous localProjectName localBranchName ->
          case maybeCurrentProjectBranch of
            Nothing -> resolveP localProjectName
            Just (ProjectAndBranch currentProject _, _path) -> do
              Cli.returnEarly $
                Output.AmbiguousCloneLocal
                  (ProjectAndBranch localProjectName remoteBranchName)
                  (ProjectAndBranch (currentProject ^. #name) localBranchName)
        ProjectAndBranchNames'Unambiguous (This localProjectName) -> resolveP localProjectName
        ProjectAndBranchNames'Unambiguous (That localBranchName) -> resolveB localBranchName
        ProjectAndBranchNames'Unambiguous (These localProjectName localBranchName) -> resolvePB localProjectName localBranchName

    resolveP localProjectName =
      go (LocalProjectKey'Name localProjectName) remoteBranchName

    resolveB localBranchName = do
      (ProjectAndBranch currentProject _, _path) <-
        maybeCurrentProjectBranch & onNothing (Cli.returnEarly Output.NotOnProjectBranch)
      go (LocalProjectKey'Project currentProject) localBranchName

    resolvePB localProjectName localBranchName =
      go (LocalProjectKey'Name localProjectName) localBranchName

    go project branch = do
      void (Cli.runEitherTransaction (assertLocalProjectBranchDoesntExist (ProjectAndBranch project branch)))
      pure (ProjectAndBranch project branch)

-- `cloneInto command local remote` clones `remote` into `local`, which is believed to not exist yet, but may (because
-- it takes some time to pull the remote).
cloneInto :: ProjectAndBranch LocalProjectKey ProjectBranchName -> Share.RemoteProjectBranch -> Cli ()
cloneInto localProjectBranch remoteProjectBranch = do
  let remoteProjectName = remoteProjectBranch ^. #projectName
  let remoteBranchName = remoteProjectBranch ^. #branchName
  let remoteProjectBranchNames = ProjectAndBranch remoteProjectName remoteBranchName

  -- Pull the remote branch's contents
  let remoteBranchHeadJwt = remoteProjectBranch ^. #branchHead
  (result, numDownloaded) <-
    Cli.with HandleInput.Pull.withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
      result <-
        Share.downloadEntities
          Share.hardCodedBaseUrl
          (Share.RepoInfo (into @Text remoteProjectBranchNames))
          remoteBranchHeadJwt
          downloadedCallback
      numDownloaded <- liftIO getNumDownloaded
      pure (result, numDownloaded)
  case result of
    Left err0 ->
      (Cli.returnEarly . Output.ShareError) case err0 of
        Share.SyncError err -> Output.ShareErrorDownloadEntities err
        Share.TransportError err -> Output.ShareErrorTransport err
    Right () -> Cli.respond (Output.DownloadedEntities numDownloaded)

  localProjectAndBranch <-
    Cli.runEitherTransaction do
      -- Repeat the check from before, because (although it's highly unlikely) we could have a name conflict after
      -- downloading the remote branch
      assertLocalProjectBranchDoesntExist localProjectBranch >>= \case
        Left err -> pure (Left err)
        Right maybeLocalProject -> do
          -- Create the local project (if necessary), and create the local branch
          (localProjectId, localProjectName) <-
            case maybeLocalProject of
              Left localProjectName -> do
                localProjectId <- Sqlite.unsafeIO (ProjectId <$> UUID.nextRandom)
                Queries.insertProject localProjectId localProjectName
                pure (localProjectId, localProjectName)
              Right localProject -> pure (localProject ^. #projectId, localProject ^. #name)
          localBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId = localProjectId,
                branchId = localBranchId,
                name = localProjectBranch ^. #branch,
                parentBranchId = Nothing
              }
          Queries.insertBranchRemoteMapping
            localProjectId
            localBranchId
            (remoteProjectBranch ^. #projectId)
            Share.hardCodedUri
            (remoteProjectBranch ^. #branchId)
          pure (Right (ProjectAndBranch (localProjectId, localProjectName) localBranchId))

  Cli.respond $
    Output.ClonedProjectBranch
      remoteProjectBranchNames
      ( ProjectAndBranch
          (localProjectAndBranch ^. #project . _2)
          (localProjectBranch ^. #branch)
      )

  -- Manipulate the root namespace and cd
  Cli.Env {codebase} <- ask
  let branchHead = hash32ToCausalHash (Share.API.hashJWTHash remoteBranchHeadJwt)
  theBranch <- liftIO (Codebase.expectBranchForHash codebase branchHead)
  let path = projectBranchPath (over #project fst localProjectAndBranch)
  Cli.stepAt
    ("clone " <> into @Text remoteProjectBranchNames)
    (Path.unabsolute path, const (Branch.head theBranch))
  Cli.cd path

-- Return the remote project id associated with the given project branch
projectBranchRemoteProjectKey ::
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Sqlite.Transaction RemoteProjectKey
projectBranchRemoteProjectKey (ProjectAndBranch project branch) = do
  Queries.loadRemoteProjectBranch (project ^. #projectId) Share.hardCodedUri (branch ^. #branchId) <&> \case
    Nothing -> RemoteProjectKey'Name (project ^. #name)
    Just (remoteProjectId, _) -> RemoteProjectKey'Id remoteProjectId

assertProjectNameHasUserSlug :: ProjectName -> Cli ()
assertProjectNameHasUserSlug projectName =
  void $
    projectNameUserSlug projectName
      & onNothing (Cli.returnEarly (Output.ProjectNameRequiresUserSlug projectName))

-- Assert that a local project+branch with this name doesn't already exist. If it does exist, we can't clone over it.
assertLocalProjectBranchDoesntExist ::
  ProjectAndBranch LocalProjectKey ProjectBranchName ->
  Sqlite.Transaction (Either Output.Output (Either ProjectName Sqlite.Project))
assertLocalProjectBranchDoesntExist = \case
  ProjectAndBranch (LocalProjectKey'Name projectName) branchName ->
    Queries.loadProjectByName projectName >>= \case
      Nothing -> pure (Right (Left projectName))
      Just project -> go project branchName
  ProjectAndBranch (LocalProjectKey'Project project) branchName -> go project branchName
  where
    go project branchName =
      Queries.projectBranchExistsByName (project ^. #projectId) branchName <&> \case
        False -> Right (Right project)
        True -> Left (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch (project ^. #name) branchName))

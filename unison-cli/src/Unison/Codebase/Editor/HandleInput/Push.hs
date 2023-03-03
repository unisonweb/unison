-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push
  ( handleGist,
    handlePushRemoteBranch,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens (to, (^.))
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set.NonEmpty as Set.NonEmpty
import Data.Text as Text
import Data.These (These (..))
import qualified Servant.Client
import qualified System.Console.Regions as Console.Regions
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Sqlite.Operations as Operations
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth, projectBranchPath)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Cli.UnisonConfigUtils as UnisonConfigUtils
import Unison.Codebase (PushGitBranchOpts (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import qualified Unison.Codebase.Editor.HandleInput.AuthLogin as AuthLogin
import Unison.Codebase.Editor.Input (GistInput (..), PushRemoteBranchInput (..), PushSource (..), PushSourceTarget (..), PushTarget (..))
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.Output.PushPull (PushPull (Push))
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    ShareUserHandle (..),
    WriteGitRemotePath (..),
    WriteRemotePath (..),
    WriteShareRemotePath (..),
    writeToReadGit,
  )
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.ShortCausalHash as SCH
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Type (GitPushBehavior (..))
import qualified Unison.Hash as Hash
import Unison.Hash32 (Hash32)
import qualified Unison.Hash32 as Hash32
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchName,
    ProjectName,
    prependUserSlugToProjectBranchName,
    prependUserSlugToProjectName,
    projectBranchNameUserSlug,
    projectNameUserSlug,
  )
import qualified Unison.Share.API.Projects as Share.API
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Share.Sync as Share
import qualified Unison.Share.Sync.Types as Share
import Unison.Share.Types (codeserverBaseURL)
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.Types as Share
import Witch (unsafeFrom)

-- | Handle a @gist@ command.
handleGist :: GistInput -> Cli ()
handleGist (GistInput repo) = do
  Cli.Env {codebase} <- ask
  sourceBranch <- Cli.getCurrentBranch
  let opts =
        PushGitBranchOpts
          { behavior = GitPushBehaviorGist,
            syncMode = SyncMode.ShortCircuit
          }
  result <-
    Cli.ioE (Codebase.pushGitBranch codebase repo opts (\_remoteRoot -> pure (Right sourceBranch))) \err ->
      Cli.returnEarly (Output.GitError err)
  _branch <- result & onLeft Cli.returnEarly
  schLength <- Cli.runTransaction Codebase.branchHashLength
  Cli.respond $
    GistCreated $
      ReadRemoteNamespaceGit
        ReadGitRemoteNamespace
          { repo = writeToReadGit repo,
            sch = Just (SCH.fromHash schLength (Branch.headHash sourceBranch)),
            path = Path.empty
          }

-- | Handle a @push@ command.
handlePushRemoteBranch :: PushRemoteBranchInput -> Cli ()
handlePushRemoteBranch PushRemoteBranchInput {sourceTarget, pushBehavior, syncMode} = do
  case sourceTarget of
    -- push <implicit> to <implicit>
    PushSourceTarget0 ->
      getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          remotePath <- UnisonConfigUtils.resolveConfiguredUrl Push Path.currentPath
          pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode
        Just localProjectAndBranch -> pushProjectBranchToProjectBranch localProjectAndBranch Nothing
    -- push <implicit> to .some.path
    PushSourceTarget1 (PathyTarget remotePath) -> do
      localPath <- Cli.getCurrentPath
      pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode
    -- push <implicit> to @some/project
    PushSourceTarget1 (ProjyTarget remoteProjectAndBranch0) ->
      getCurrentProjectBranch >>= \case
        Nothing -> do
          localPath <- Cli.getCurrentPath
          remoteProjectAndBranch <- branchNameSpecToNames remoteProjectAndBranch0
          pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
        Just localProjectAndBranch ->
          pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch0)
    -- push .some.path to .some.path
    PushSourceTarget2 (PathySource localPath0) (PathyTarget remotePath) -> do
      localPath <- Cli.resolvePath' localPath0
      pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode
    -- push .some.path to @some/project
    PushSourceTarget2 (PathySource localPath0) (ProjyTarget remoteProjectAndBranch0) -> do
      localPath <- Cli.resolvePath' localPath0
      remoteProjectAndBranch <- branchNameSpecToNames remoteProjectAndBranch0
      pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
    -- push @some/project to .some.path
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (PathyTarget remotePath) -> do
      localProjectAndBranch <- branchNameSpecToIds localProjectAndBranch0
      pushLooseCodeToLooseCode (projectBranchPath localProjectAndBranch) remotePath pushBehavior syncMode
    -- push @some/project to @some/project
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (ProjyTarget remoteProjectAndBranch) -> do
      localProjectAndBranch <- branchNameSpecToIds localProjectAndBranch0
      pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch)

-- Convert a "branch name spec" (project name, or branch name, or both) into local ids for the project and branch, using
-- the following defaults, if a name is missing:
--
--   - The project at the current path
--   - The branch named "main"
branchNameSpecToIds ::
  These ProjectName ProjectBranchName ->
  Cli (ProjectAndBranch ProjectId ProjectBranchId)
branchNameSpecToIds = \case
  This projectName -> branchNameSpecToIds (These projectName (unsafeFrom @Text "main"))
  That branchName -> do
    ProjectAndBranch projectId _branchId <-
      getCurrentProjectBranch & onNothingM do
        loggeth ["not on a project branch yo"]
        Cli.returnEarlyWithoutOutput
    branch <-
      Cli.runTransaction (Queries.loadProjectBranchByName projectId (into @Text branchName)) & onNothingM do
        loggeth ["no branch in project ", tShow projectId, " with name ", into @Text branchName]
        Cli.returnEarlyWithoutOutput
    pure (ProjectAndBranch projectId (branch ^. #branchId))
  These projectName branchName -> do
    maybeProjectAndBranch <-
      Cli.runTransaction do
        runMaybeT do
          project <- MaybeT (Queries.loadProjectByName (into @Text projectName))
          let projectId = project ^. #projectId
          branch <- MaybeT (Queries.loadProjectBranchByName projectId (into @Text branchName))
          pure (ProjectAndBranch projectId (branch ^. #branchId))
    maybeProjectAndBranch & onNothing do
      loggeth [into @Text (These projectName branchName), " not found!"]
      Cli.returnEarlyWithoutOutput

-- Convert a "branch name spec" (project name, or branch name, or both) into names for the project and branch, using the
-- following defaults, if a name is missing:
--
--   - The project at the current path
--   - The branch named "main"
branchNameSpecToNames ::
  These ProjectName ProjectBranchName ->
  Cli (ProjectAndBranch ProjectName ProjectBranchName)
branchNameSpecToNames = \case
  This projectName -> pure (ProjectAndBranch projectName (unsafeFrom @Text "main"))
  That branchName -> do
    ProjectAndBranch projectId _branchId <-
      getCurrentProjectBranch & onNothingM do
        loggeth ["not on a project branch"]
        Cli.returnEarlyWithoutOutput
    Cli.runTransaction do
      project <- Queries.expectProject projectId
      pure (ProjectAndBranch (unsafeFrom @Text (project ^. #name)) branchName)
  These projectName branchName -> pure (ProjectAndBranch projectName branchName)

-- Push a local namespace ("loose code") to a remote namespace ("loose code").
pushLooseCodeToLooseCode ::
  Path.Absolute ->
  WriteRemotePath ->
  PushBehavior ->
  SyncMode ->
  Cli ()
pushLooseCodeToLooseCode localPath remotePath pushBehavior syncMode = do
  case remotePath of
    WriteRemotePathGit gitRemotePath -> pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior syncMode
    WriteRemotePathShare shareRemotePath -> pushLooseCodeToShareLooseCode localPath shareRemotePath pushBehavior

-- Push a local namespace ("loose code") to a Git-hosted remote namespace ("loose code").
pushLooseCodeToGitLooseCode ::
  Path.Absolute ->
  WriteGitRemotePath ->
  PushBehavior ->
  SyncMode ->
  Cli ()
pushLooseCodeToGitLooseCode localPath gitRemotePath pushBehavior syncMode = do
  sourceBranch <- Cli.getBranchAt localPath
  let withRemoteRoot :: Branch IO -> Either Output (Branch IO)
      withRemoteRoot remoteRoot = do
        let -- We don't merge `sourceBranch` with `remoteBranch`, we just replace it. This push will be rejected if
            -- this rewinds time or misses any new updates in the remote branch that aren't in `sourceBranch`
            -- already.
            f remoteBranch = if shouldPushTo pushBehavior remoteBranch then Just sourceBranch else Nothing
        case Branch.modifyAtM (gitRemotePath ^. #path) f remoteRoot of
          Nothing -> Left (RefusedToPush pushBehavior (WriteRemotePathGit gitRemotePath))
          Just newRemoteRoot -> Right newRemoteRoot
  let opts =
        PushGitBranchOpts
          { behavior =
              case pushBehavior of
                PushBehavior.ForcePush -> GitPushBehaviorForce
                PushBehavior.RequireEmpty -> GitPushBehaviorFf
                PushBehavior.RequireNonEmpty -> GitPushBehaviorFf,
            syncMode
          }
  Cli.Env {codebase} <- ask
  let push =
        Codebase.pushGitBranch
          codebase
          (gitRemotePath ^. #repo)
          opts
          (\remoteRoot -> pure (withRemoteRoot remoteRoot))
  result <-
    liftIO push & onLeftM \err ->
      Cli.returnEarly (Output.GitError err)
  _branch <- result & onLeft Cli.returnEarly
  Cli.respond Success
  where
    -- Per `pushBehavior`, we are either:
    --
    --   (1) force-pushing, in which case the remote branch state doesn't matter
    --   (2) updating an empty branch, which fails if the branch isn't empty (`push.create`)
    --   (3) updating a non-empty branch, which fails if the branch is empty (`push`)
    shouldPushTo :: PushBehavior -> Branch m -> Bool
    shouldPushTo pushBehavior remoteBranch =
      case pushBehavior of
        PushBehavior.ForcePush -> True
        PushBehavior.RequireEmpty -> Branch.isEmpty0 (Branch.head remoteBranch)
        PushBehavior.RequireNonEmpty -> not (Branch.isEmpty0 (Branch.head remoteBranch))

-- Push a local namespace ("loose code") to a Share-hosted remote namespace ("loose code").
pushLooseCodeToShareLooseCode :: Path.Absolute -> WriteShareRemotePath -> PushBehavior -> Cli ()
pushLooseCodeToShareLooseCode localPath remote@WriteShareRemotePath {server, repo, path = remotePath} behavior = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  let sharePath = Share.Path (shareUserHandleToText repo Nel.:| pathToSegments remotePath)
  ensureAuthenticatedWithCodeserver codeserver

  localCausalHash <-
    Cli.runTransaction (Ops.loadCausalHashAtPath (pathToSegments (Path.unabsolute localPath))) & onNothingM do
      Cli.returnEarly (EmptyPush . Path.absoluteToPath' $ localPath)

  let checkAndSetPush :: Maybe Hash32 -> Cli ()
      checkAndSetPush remoteHash =
        when (Just (Hash32.fromHash (unCausalHash localCausalHash)) /= remoteHash) do
          let push =
                Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
                  Share.checkAndSetPush
                    baseURL
                    sharePath
                    remoteHash
                    localCausalHash
                    uploadedCallback
          push & onLeftM (pushError ShareErrorCheckAndSetPush)

  case behavior of
    PushBehavior.ForcePush -> do
      maybeHashJwt <-
        Share.getCausalHashByPath baseURL sharePath & onLeftM \err0 ->
          (Cli.returnEarly . Output.ShareError) case err0 of
            Share.SyncError err -> ShareErrorGetCausalHashByPath err
            Share.TransportError err -> ShareErrorTransport err
      checkAndSetPush (Share.hashJWTHash <$> maybeHashJwt)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireEmpty -> do
      checkAndSetPush Nothing
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireNonEmpty -> do
      let push :: Cli (Either (Share.SyncError Share.FastForwardPushError) ())
          push =
            Cli.with withEntitiesUploadedProgressCallback \uploadedCallback ->
              Share.fastForwardPush
                baseURL
                sharePath
                localCausalHash
                uploadedCallback
      push & onLeftM (pushError ShareErrorFastForwardPush)
      Cli.respond (ViewOnShare remote)
  where
    pathToSegments :: Path -> [Text]
    pathToSegments =
      coerce Path.toList

    pushError :: (a -> Output.ShareError) -> Share.SyncError a -> Cli b
    pushError f err0 = do
      Cli.returnEarly case err0 of
        Share.SyncError err -> Output.ShareError (f err)
        Share.TransportError err -> Output.ShareError (ShareErrorTransport err)

-- Info related to an upload
data UploadInfo = UploadInfo
  { -- The "repo name" to upload to. For a contributor branch like @arya/topic, for example, this will be the username
    -- "arya".
    repoName :: Share.RepoName,
    -- The action to call after a successful upload.
    afterUploadAction :: Cli ()
  }

-- UPLOADEO:
--   1. Call action that returns repo name and after-upload callback
--   2. Upload entities
--   3. Call after-upload callback
uploadeo :: Hash32 -> Cli UploadInfo -> Cli ()
uploadeo localBranchHead action = do
  UploadInfo {repoName, afterUploadAction} <- action
  oinkUpload repoName localBranchHead
  afterUploadAction

-- Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch :: Path.Absolute -> ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
pushLooseCodeToProjectBranch localPath remoteProjectAndBranch = do
  localBranchHead <- Cli.runEitherTransaction (loadCausalHashToPush localPath)
  uploadeo localBranchHead (oompaLoompa0 Nothing localBranchHead remoteProjectAndBranch)

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Maybe (These ProjectName ProjectBranchName) ->
  Cli ()
pushProjectBranchToProjectBranch localProjectAndBranchIds maybeRemoteProjectAndBranchNames = do
  -- Load local project and branch from database and get the causal hash to push
  (localProjectAndBranch, localBranchHead) <-
    Cli.runEitherTransaction do
      loadCausalHashToPush (projectBranchPath localProjectAndBranchIds) >>= \case
        Left output -> pure (Left output)
        Right hash -> do
          localProjectAndBranch <- expectProjectAndBranch localProjectAndBranchIds
          pure (Right (localProjectAndBranch, hash))

  uploadeo
    localBranchHead
    case maybeRemoteProjectAndBranchNames of
      Nothing -> bazinga0 localProjectAndBranch localBranchHead
      Just (This remoteProjectName) ->
        bazinga10 localProjectAndBranch localBranchHead (ProjectAndBranch (Just remoteProjectName) Nothing)
      Just (That remoteBranchName) -> bazinga5 localProjectAndBranch localBranchHead remoteBranchName
      Just (These remoteProjectName remoteBranchName) ->
        oompaLoompa0
          (Just localProjectAndBranch)
          localBranchHead
          (ProjectAndBranch remoteProjectName remoteBranchName)

-- Get the causal hash to push at the given path, or error if there's no history there.
loadCausalHashToPush :: Path.Absolute -> Sqlite.Transaction (Either Output Hash32)
loadCausalHashToPush path =
  Operations.loadCausalHashAtPath segments <&> \case
    -- If there is nothing to push, fail with some message
    Nothing -> Left (EmptyPush (Path.absoluteToPath' path))
    Just (CausalHash hash) -> Right (Hash32.fromHash hash)
  where
    segments = coerce @[NameSegment] @[Text] (Path.toList (Path.unabsolute path))

-- "push", remote mapping unknown
bazinga0 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> Cli UploadInfo
bazinga0 localProjectAndBranch localBranchHead =
  Cli.runTransaction (Queries.loadRemoteProjectBranchByLocalProjectBranch localProjectId localBranchId) >>= \case
    Nothing -> bazinga10 localProjectAndBranch localBranchHead (ProjectAndBranch Nothing Nothing)
    Just (remoteProjectId, Nothing) -> bazinga2 localProjectAndBranch localBranchHead remoteProjectId
    Just (remoteProjectId, Just remoteBranchId) ->
      oompaLoompa2 localProjectAndBranch localBranchHead (ProjectAndBranch remoteProjectId remoteBranchId)
  where
    localProjectId = localProjectAndBranch ^. #project . #projectId
    localBranchId = localProjectAndBranch ^. #branch . #branchId

-- "push" with remote mapping for project (from ancestor branch)
bazinga2 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> RemoteProjectId -> Cli UploadInfo
bazinga2 localProjectAndBranch localBranchHead remoteProjectId = do
  myUserHandle <- oinkGetLoggedInUser
  let localBranchName = unsafeFrom @Text (localProjectAndBranch ^. #branch . #name)
  -- Derive the remote branch name from the user's handle and the local branch name.
  --
  -- user "bob" has local branch "topic":        remoteBranchName = "@bob/topic"
  -- user "bob" has local branch "@runar/topic": remoteBranchName = "@runar/topic"
  let remoteBranchName =
        case projectBranchNameUserSlug localBranchName of
          Nothing -> prependUserSlugToProjectBranchName myUserHandle localBranchName
          Just _userSlug -> localBranchName
  oompaLoompa1 localProjectAndBranch localBranchHead (ProjectAndBranch remoteProjectId remoteBranchName)

-- "push /foo", remote mapping unknown
bazinga5 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> ProjectBranchName -> Cli UploadInfo
bazinga5 localProjectAndBranch localBranchHead remoteBranchName = do
  Cli.runTransaction (Queries.loadRemoteProjectBranchByLocalProjectBranch localProjectId localBranchId) >>= \case
    Nothing -> bazinga10 localProjectAndBranch localBranchHead (ProjectAndBranch Nothing (Just remoteBranchName))
    Just (remoteProjectId, _maybeRemoteBranchId) ->
      -- FIXME if maybeRemoteBranchId is Nothing, we do want to establish a remote mapping for this branch. otherwise,
      -- we don't. (so maybe we just create a new mapping if one doesn't already exist in our create-branch helper?)
      oompaLoompa1 localProjectAndBranch localBranchHead (ProjectAndBranch remoteProjectId remoteBranchName)
  where
    localProjectId = localProjectAndBranch ^. #project . #projectId
    localBranchId = localProjectAndBranch ^. #branch . #branchId

-- "push", "push foo", or "push /foo" with no remote mapping
bazinga10 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli UploadInfo
bazinga10 localProjectAndBranch localBranchHead remoteProjectAndBranchMaybes = do
  myUserHandle <- oinkGetLoggedInUser
  let localProjectName = unsafeFrom @Text (localProjectAndBranch ^. #project . #name)
  let localBranchName = unsafeFrom @Text (localProjectAndBranch ^. #branch . #name)
  let remoteProjectName =
        case remoteProjectAndBranchMaybes ^. #project of
          Nothing -> prependUserSlugToProjectName myUserHandle localProjectName
          Just remoteProjectName1 -> remoteProjectName1
  let remoteBranchName =
        case remoteProjectAndBranchMaybes ^. #branch of
          Nothing -> prependUserSlugToProjectBranchName myUserHandle localBranchName
          Just remoteBranchName1 -> remoteBranchName1
  let remoteProjectAndBranch = ProjectAndBranch remoteProjectName remoteBranchName
  oompaLoompa0 (Just localProjectAndBranch) localBranchHead remoteProjectAndBranch

-- we have the remote project and branch names, but we don't know whether either already exist
oompaLoompa0 ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli UploadInfo
oompaLoompa0 maybeLocalProjectAndBranch localBranchHead remoteProjectAndBranch = do
  repoName <- projectBranchRepoName remoteProjectAndBranch
  let remoteProjectName = remoteProjectAndBranch ^. #project
  let remoteBranchName = remoteProjectAndBranch ^. #branch
  let doCreateBranch :: Share.API.Project -> Cli ()
      doCreateBranch remoteProject = do
        oinkUpload repoName localBranchHead
        oompaLoompaCreateBranch
          maybeLocalProjectAndBranch
          localBranchHead
          (ProjectAndBranch (RemoteProjectId (remoteProject ^. #projectId)) remoteBranchName)
  Share.getProjectByName remoteProjectName >>= \case
    Share.API.GetProjectResponseNotFound {} -> do
      let afterUploadAction = do
            remoteProject <- oinkCreateRemoteProject remoteProjectName
            doCreateBranch remoteProject
      pure UploadInfo {repoName, afterUploadAction}
    Share.API.GetProjectResponseUnauthorized {} -> wundefined
    Share.API.GetProjectResponseSuccess remoteProject -> do
      let remoteProjectId = RemoteProjectId (remoteProject ^. #projectId)
      Share.getProjectBranchByName (ProjectAndBranch remoteProjectId remoteBranchName) >>= \case
        Share.API.GetProjectBranchResponseNotFound {} -> do
          let afterUploadAction = doCreateBranch remoteProject
          pure UploadInfo {repoName, afterUploadAction}
        Share.API.GetProjectBranchResponseUnauthorized {} -> wundefined
        Share.API.GetProjectBranchResponseSuccess remoteBranch -> do
          afterUploadAction <- oompaLoompaFastForward maybeLocalProjectAndBranch localBranchHead remoteBranch
          pure UploadInfo {repoName, afterUploadAction}

-- "push /foo" with a remote mapping for the project (either from this branch or one of our ancestors)
-- but we don't know whether the remote branch exists
oompaLoompa1 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli UploadInfo
oompaLoompa1 localProjectAndBranch localBranchHead remoteProjectAndBranch = do
  repoName <-
    case projectBranchNameUserSlug (remoteProjectAndBranch ^. #branch) of
      Nothing ->
        Share.getProjectById (remoteProjectAndBranch ^. #project) >>= \case
          Share.API.GetProjectResponseNotFound (Share.API.NotFound msg) -> do
            loggeth ["project deleted on share: " <> msg]
            Cli.returnEarlyWithoutOutput
          Share.API.GetProjectResponseUnauthorized (Share.API.Unauthorized msg) -> do
            loggeth ["unauthorized: " <> msg]
            Cli.returnEarlyWithoutOutput
          Share.API.GetProjectResponseSuccess remoteProject -> remoteProjectRepoName remoteProject
      Just userSlug -> pure (Share.RepoName userSlug)
  Share.getProjectBranchByName remoteProjectAndBranch >>= \case
    Share.API.GetProjectBranchResponseNotFound {} -> do
      -- FIXME check to see if the project exists here instead of assuming it does
      let afterUploadAction =
            oompaLoompaCreateBranch (Just localProjectAndBranch) localBranchHead remoteProjectAndBranch
      pure UploadInfo {repoName, afterUploadAction}
    Share.API.GetProjectBranchResponseUnauthorized {} -> wundefined
    Share.API.GetProjectBranchResponseSuccess remoteBranch -> do
      afterUploadAction <- oompaLoompaFastForward (Just localProjectAndBranch) localBranchHead remoteBranch
      pure UploadInfo {repoName, afterUploadAction}

-- "push" with remote mapping for branch
oompaLoompa2 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId RemoteProjectBranchId ->
  Cli UploadInfo
oompaLoompa2 localProjectAndBranch localBranchHead remoteProjectAndBranch = do
  Share.getProjectBranchById remoteProjectAndBranch >>= \case
    Share.API.GetProjectBranchResponseNotFound (Share.API.NotFound msg) -> do
      loggeth ["project or branch deleted on Share: " <> msg]
      Cli.returnEarlyWithoutOutput
    Share.API.GetProjectBranchResponseUnauthorized {} -> wundefined
    Share.API.GetProjectBranchResponseSuccess remoteBranch -> do
      repoName <- remoteProjectBranchRepoName remoteBranch
      afterUploadAction <- oompaLoompaFastForward (Just localProjectAndBranch) localBranchHead remoteBranch
      pure UploadInfo {repoName, afterUploadAction}

-- we know remote project exists but remote branch doesn't
oompaLoompaCreateBranch ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli ()
oompaLoompaCreateBranch
  maybeLocalProjectAndBranch
  localBranchHead
  (ProjectAndBranch remoteProjectId remoteBranchName) = do
    mergeTargetSuggestion <- runMaybeT do
      ProjectAndBranch localProject localBranch <- MaybeT (pure maybeLocalProjectAndBranch)
      (prj, bch) <-
        MaybeT $
          Cli.runTransaction
            ( Queries.loadDefaultMergeTargetForLocalProjectBranch
                (localProject ^. #projectId)
                (localBranch ^. #branchId)
            )
      pure
        ( Share.API.ProjectBranchIds
            (unRemoteProjectId prj)
            (unRemoteProjectBranchId bch)
        )
    oinkCreateRemoteBranch
      maybeLocalProjectAndBranch
      Share.API.CreateProjectBranchRequest
        { projectId = unRemoteProjectId remoteProjectId,
          branchName = into @Text remoteBranchName,
          branchCausalHash = localBranchHead,
          branchMergeTarget = mergeTargetSuggestion
        }

oompaLoompaFastForward ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  Share.API.ProjectBranch ->
  Cli (Cli ())
oompaLoompaFastForward maybeLocalProjectAndBranch localBranchHead remoteBranch = do
  Cli.runTransaction wouldBeFastForward >>= \case
    False -> do
      loggeth ["local head behind remote"]
      Cli.returnEarlyWithoutOutput
    True ->
      pure do
        let request =
              Share.API.SetProjectBranchHeadRequest
                { projectId = remoteBranch ^. #projectId,
                  branchId = remoteBranch ^. #branchId,
                  branchOldCausalHash = Just remoteBranchHead,
                  branchNewCausalHash = localBranchHead
                }
        Share.setProjectBranchHead request >>= \case
          Share.API.SetProjectBranchHeadResponseUnauthorized (Share.API.Unauthorized msg) -> do
            loggeth ["SetProjectBranchHeadResponseUnauthorized: " <> msg]
            Cli.returnEarlyWithoutOutput
          Share.API.SetProjectBranchHeadResponseNotFound (Share.API.NotFound msg) -> do
            loggeth ["SetProjectBranchHeadResponseNotFound: " <> msg]
          Share.API.SetProjectBranchHeadResponseMissingCausalHash _missingCausalHash -> do
            -- TODO: push the missing causal
            wundefined
          Share.API.SetProjectBranchHeadResponseExpectedCausalHashMismatch expected actual -> do
            -- TODO: Report that the remote branch causal has changed.
            wundefined
          Share.API.SetProjectBranchHeadResponseSuccess -> do
            loggeth ["SetProjectBranchHeadResponseSuccess"]
            case maybeLocalProjectAndBranch of
              Nothing -> pure ()
              Just (ProjectAndBranch localProject localBranch) -> do
                loggeth ["ensure branch remote mapping is set"]
                Cli.runTransaction do
                  Queries.ensureBranchRemoteMapping
                    (localProject ^. #projectId)
                    (localBranch ^. #branchId)
                    (remoteBranch ^. #projectId . to RemoteProjectId)
                    (Text.pack (Servant.Client.showBaseUrl Share.hardCodedBaseUrl))
                    (remoteBranch ^. #branchId . to RemoteProjectBranchId)
  where
    remoteBranchHead = remoteBranch ^. #branchHead

    wouldBeFastForward :: Sqlite.Transaction Bool
    wouldBeFastForward = do
      maybeHashIds <-
        runMaybeT $
          (,)
            <$> MaybeT (Queries.loadCausalHashIdByCausalHash (CausalHash (Hash32.toHash localBranchHead)))
            <*> MaybeT (Queries.loadCausalHashIdByCausalHash (CausalHash (Hash32.toHash remoteBranchHead)))
      case maybeHashIds of
        Nothing -> pure False
        Just (localBranchHead1, remoteBranchHead1) -> Queries.before remoteBranchHead1 localBranchHead1

expectProjectName :: Text -> Cli ProjectName
expectProjectName projectName =
  case tryInto projectName of
    -- This shouldn't happen often - Share gave us a project name that we don't consider valid?
    Left err -> do
      loggeth ["Invalid project name: ", tShow err]
      Cli.returnEarlyWithoutOutput
    Right x -> pure x

expectUserSlug :: ProjectName -> Cli Share.RepoName
expectUserSlug projectName =
  case projectNameUserSlug projectName of
    Nothing -> do
      loggeth
        [ "Expected project name: ",
          tShow projectName,
          " to contain a user slug.",
          "\n",
          tShow projectName
        ]
      Cli.returnEarlyWithoutOutput
    Just userSlug -> pure (Share.RepoName userSlug)

expectBranchName :: Text -> Cli ProjectBranchName
expectBranchName branchName = case tryInto branchName of
  Left err -> do
    loggeth
      [ "Expected text: ",
        tShow branchName,
        " to be a valid project branch name.",
        "\n",
        tShow err
      ]
    Cli.returnEarlyWithoutOutput
  Right x -> pure x

remoteProjectRepoName :: Share.API.Project -> Cli Share.RepoName
remoteProjectRepoName project =
  expectUserSlug <=< expectProjectName $ (project ^. #projectName)

remoteProjectBranchRepoName :: Share.API.ProjectBranch -> Cli Share.RepoName
remoteProjectBranchRepoName branch = do
  projectName <- expectProjectName (branch ^. #projectName)
  branchName <- expectBranchName (branch ^. #branchName)
  projectBranchRepoName (ProjectAndBranch projectName branchName)

-- A couple example repo names derived from the project/branch names:
--
--   "@unison/base" / "@arya/topic" => "arya", because the branch "@arya/topic" has a user component
--
--   "@unison/base" / "main"        => "unison", because the branch "main" doesn't have a user component
--
--   "something"    / "weird"       => oh no, we probably have to fail here, because even though we tried to design an
--                                     API that allows any ol' project and branch name, we don't really know *where* to
--                                     upload (i.e. the repo name of) a project that doesn't have a user component
projectBranchRepoName :: ProjectAndBranch ProjectName ProjectBranchName -> Cli Share.RepoName
projectBranchRepoName (ProjectAndBranch projectName branchName) =
  case projectBranchNameUserSlug branchName of
    Nothing ->
      case projectNameUserSlug projectName of
        Nothing -> do
          loggeth
            [ "Cannot determine repo name: neither project nor branch name have a user slug.\n",
              tShow (ProjectAndBranch projectName branchName)
            ]
          Cli.returnEarlyWithoutOutput
        Just userSlug -> pure (Share.RepoName userSlug)
    Just userSlug -> pure (Share.RepoName userSlug)

expectProjectAndBranch ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Sqlite.Transaction (ProjectAndBranch Queries.Project Queries.Branch)
expectProjectAndBranch (ProjectAndBranch projectId branchId) = do
  project <- Queries.expectProject projectId
  branch <- Queries.expectProjectBranch projectId branchId
  pure (ProjectAndBranch project branch)

oinkUpload :: Share.RepoName -> Hash32 -> Cli ()
oinkUpload repoName causalHash = do
  loggeth ["uploading entities"]
  Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
    let upload =
          Share.uploadEntities
            (codeserverBaseURL Codeserver.defaultCodeserver)
            repoName
            (Set.NonEmpty.singleton causalHash)
            uploadedCallback
    upload & onLeftM \err -> do
      loggeth ["upload entities error"]
      loggeth [tShow err]
      Cli.returnEarlyWithoutOutput

oinkCreateRemoteProject :: ProjectName -> Cli Share.API.Project
oinkCreateRemoteProject projectName = do
  let request = Share.API.CreateProjectRequest {projectName = into @Text projectName}
  loggeth ["Making create-project request for project"]
  loggeth [tShow request]
  Share.createProject request >>= \case
    Share.API.CreateProjectResponseUnauthorized (Share.API.Unauthorized msg) -> do
      loggeth ["Share says: unauthorized: " <> msg]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectResponseNotFound (Share.API.NotFound msg) -> do
      loggeth ["Share says: not-found: " <> msg]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectResponseSuccess remoteProject -> do
      loggeth ["Share says: success!"]
      loggeth [tShow remoteProject]
      pure remoteProject

oinkCreateRemoteBranch ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Share.API.CreateProjectBranchRequest ->
  Cli ()
oinkCreateRemoteBranch maybeLocalProjectAndBranch request = do
  loggeth ["creating remote branch"]
  loggeth [tShow request]
  remoteBranch <-
    Share.createProjectBranch request >>= \case
      Share.API.CreateProjectBranchResponseUnauthorized (Share.API.Unauthorized msg) -> do
        loggeth ["Share says: unauthorized: " <> msg]
        Cli.returnEarlyWithoutOutput
      Share.API.CreateProjectBranchResponseNotFound (Share.API.NotFound msg) -> do
        loggeth ["Share says: not-found: " <> msg]
        Cli.returnEarlyWithoutOutput
      Share.API.CreateProjectBranchResponseMissingCausalHash missingCausalHash -> do
        loggeth ["Share says: missing causal hash: " <> tShow missingCausalHash]
        wundefined
      Share.API.CreateProjectBranchResponseSuccess remoteBranch -> pure remoteBranch
  loggeth ["Share says: success!"]
  loggeth [tShow remoteBranch]
  let remoteProjectId = RemoteProjectId (remoteBranch ^. #projectId)
  let remoteBranchId = RemoteProjectBranchId (remoteBranch ^. #branchId)
  Cli.runTransaction do
    case maybeLocalProjectAndBranch of
      Nothing -> pure ()
      Just (ProjectAndBranch localProject localBranch) -> do
        -- If the local branch has no associated remote then we
        -- associate this newly created branch.
        Queries.ensureBranchRemoteMapping
          (localProject ^. #projectId)
          (localBranch ^. #branchId)
          remoteProjectId
          (Text.pack (Servant.Client.showBaseUrl Share.hardCodedBaseUrl))
          remoteBranchId

oinkGetLoggedInUser :: Cli Text
oinkGetLoggedInUser = do
  loggeth ["Getting current logged-in user on Share"]
  userInfo <- AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  let myUserHandle = userInfo ^. #handle
  loggeth ["Got current logged-in user on Share: ", myUserHandle]
  pure myUserHandle

-- Provide the given action a callback that displays to the terminal.
withEntitiesUploadedProgressCallback :: ((Int -> IO ()) -> IO a) -> IO a
withEntitiesUploadedProgressCallback action = do
  entitiesUploadedVar <- newTVarIO 0
  Console.Regions.displayConsoleRegions do
    Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
      Console.Regions.setConsoleRegion region do
        entitiesUploaded <- readTVar entitiesUploadedVar
        pure $
          "\n  Uploaded "
            <> tShow entitiesUploaded
            <> " entities...\n\n"
      result <- action (\n -> atomically (modifyTVar' entitiesUploadedVar (+ n)))
      entitiesUploaded <- readTVarIO entitiesUploadedVar
      Console.Regions.finishConsoleRegion region $
        "\n  Uploaded " <> tShow entitiesUploaded <> " entities."
      pure result

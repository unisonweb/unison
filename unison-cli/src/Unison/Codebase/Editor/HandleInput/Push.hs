-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push
  ( handleGist,
    handlePushRemoteBranch,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set.NonEmpty as Set.NonEmpty
import Data.Text as Text
import Data.These (These (..))
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

-- UPLOADEO:
--   1. Call action that returns repo name and after-upload callback
--   2. Upload entities
--   3. Call after-upload callback
uploadeo ::
  Hash32 ->
  -- An action that returns the "repo name" to upload to via the upload-entities API (for a contributor branch for
  -- example, this will be the username of the contributor), and an action to run after branch contents are uploaded.
  Cli (Share.RepoName, Cli ()) ->
  Cli ()
uploadeo localBranchCausalHash action = do
  (repoName, afterUpload) <- action
  oinkUpload repoName localBranchCausalHash
  afterUpload

-- Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch ::
  Path.Absolute ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli ()
pushLooseCodeToProjectBranch localPath remoteProjectAndBranch = do
  localBranchCausalHash <- Cli.runEitherTransaction (loadCausalHashToPush localPath)
  uploadeo localBranchCausalHash (bazinga7 Nothing localBranchCausalHash remoteProjectAndBranch)

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Maybe (These ProjectName ProjectBranchName) ->
  Cli ()
pushProjectBranchToProjectBranch localProjectAndBranchIds maybeRemoteProjectAndBranchNames = do
  -- Load local project and branch from database and get the causal hash to push
  (localProjectAndBranch, localBranchCausalHash) <-
    Cli.runEitherTransaction do
      loadCausalHashToPush (projectBranchPath localProjectAndBranchIds) >>= \case
        Left output -> pure (Left output)
        Right hash -> do
          localProjectAndBranch <- expectProjectAndBranch localProjectAndBranchIds
          pure (Right (localProjectAndBranch, hash))

  uploadeo
    localBranchCausalHash
    case maybeRemoteProjectAndBranchNames of
      Nothing -> bazinga0 localProjectAndBranch localBranchCausalHash
      Just (This remoteProjectName) ->
        bazinga10 localProjectAndBranch localBranchCausalHash (ProjectAndBranch (Just remoteProjectName) Nothing)
      Just (That remoteBranchName) -> bazinga5 localProjectAndBranch localBranchCausalHash remoteBranchName
      Just (These remoteProjectName remoteBranchName) ->
        bazinga7
          (Just localProjectAndBranch)
          localBranchCausalHash
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
bazinga0 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> Cli (Share.RepoName, Cli ())
bazinga0 localProjectAndBranch localBranchCausalHash =
  Cli.runTransaction (Queries.loadRemoteProjectBranchByLocalProjectBranch localProjectId localBranchId) >>= \case
    Nothing -> bazinga10 localProjectAndBranch localBranchCausalHash (ProjectAndBranch Nothing Nothing)
    Just (remoteProjectId, Nothing) -> bazinga2 localProjectAndBranch localBranchCausalHash remoteProjectId
    Just (remoteProjectId, Just remoteBranchId) ->
      bazinga3
        localProjectAndBranch
        localBranchCausalHash
        (ProjectAndBranch remoteProjectId remoteBranchId)
  where
    localProjectId = localProjectAndBranch ^. #project . #projectId
    localBranchId = localProjectAndBranch ^. #branch . #branchId

-- "push" with remote mapping for project (from ancestor branch)
bazinga2 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> RemoteProjectId -> Cli (Share.RepoName, Cli ())
bazinga2 localProjectAndBranch localBranchCausalHash remoteProjectId = do
  myUserHandle <- oinkGetLoggedInUser
  let localBranchName = unsafeFrom @Text (localProjectAndBranch ^. #branch . #name)
  let (remoteBranchUserSlug, remoteBranchName) = deriveRemoteBranchName myUserHandle localBranchName
  let remoteProjectAndBranch = ProjectAndBranch remoteProjectId remoteBranchName
  afterUpload <- oompaLoompa1 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch
  pure (Share.RepoName remoteBranchUserSlug, afterUpload)

-- "push" with remote mapping for branch
bazinga3 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId RemoteProjectBranchId ->
  Cli (Share.RepoName, Cli ())
bazinga3 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch = do
  remoteBranch <-
    Share.getProjectBranchById remoteProjectAndBranch >>= \case
      Share.API.GetProjectBranchResponseNotFound -> do
        loggeth ["GetProjectBranchResponseNotFound"]
        Cli.returnEarlyWithoutOutput
      Share.API.GetProjectBranchResponseSuccess remoteBranch -> pure remoteBranch
  repoName <- remoteProjectBranchRepoName remoteBranch
  afterUpload <- oompaLoompa2 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch
  pure (repoName, afterUpload)

-- "push /foo"
bazinga5 :: ProjectAndBranch Queries.Project Queries.Branch -> Hash32 -> ProjectBranchName -> Cli (Share.RepoName, Cli ())
bazinga5 localProjectAndBranch localBranchCausalHash remoteBranchName = do
  Cli.runTransaction (Queries.loadRemoteProjectBranchByLocalProjectBranch localProjectId localBranchId) >>= \case
    Nothing -> bazinga10 localProjectAndBranch localBranchCausalHash (ProjectAndBranch Nothing (Just remoteBranchName))
    Just (remoteProjectId, _maybeRemoteBranchId) ->
      -- FIXME if maybeRemoteBranchId is Nothing, we do want to establish a remote mapping for this branch. otherwise,
      -- we don't. (so maybe we just create a new mapping if one doesn't already exist in our create-branch helper?)
      bazinga8 localProjectAndBranch localBranchCausalHash (ProjectAndBranch remoteProjectId remoteBranchName)
  where
    localProjectId = localProjectAndBranch ^. #project . #projectId
    localBranchId = localProjectAndBranch ^. #branch . #branchId

-- "push /foo" with a remote mapping for the project (either from this branch or one of our ancestors)
bazinga8 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli (Share.RepoName, Cli ())
bazinga8 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch = do
  repoName <-
    case projectBranchNameUserSlug (remoteProjectAndBranch ^. #branch) of
      Nothing ->
        Share.getProjectById (remoteProjectAndBranch ^. #project) >>= \case
          Share.API.GetProjectResponseNotFound -> wundefined
          Share.API.GetProjectResponseSuccess remoteProject -> remoteProjectRepoName remoteProject
      Just userSlug -> pure (Share.RepoName userSlug)
  afterUpload <- oompaLoompa1 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch
  pure (repoName, afterUpload)

-- "push", "push foo", or "push /foo" with no remote mapping
bazinga10 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli (Share.RepoName, Cli ())
bazinga10 localProjectAndBranch localBranchCausalHash remoteProjectAndBranchMaybes = do
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
  repoName <- projectBranchRepoName remoteProjectAndBranch
  afterUpload <- oompaLoompa0 (Just localProjectAndBranch) localBranchCausalHash remoteProjectAndBranch
  pure (repoName, afterUpload)

bazinga7 ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli (Share.RepoName, Cli ())
bazinga7 maybeLocalProjectAndBranch localBranchCausalHash remoteProjectAndBranch = do
  repoName <- projectBranchRepoName remoteProjectAndBranch
  afterUpload <- oompaLoompa0 maybeLocalProjectAndBranch localBranchCausalHash remoteProjectAndBranch
  pure (repoName, afterUpload)

-- we have the remote project and branch names, but we don't know whether either already exist
oompaLoompa0 ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli (Cli ())
oompaLoompa0 maybeLocalProjectAndBranch localBranchCausalHash (ProjectAndBranch remoteProjectName remoteBranchName) = do
  let doCreateBranch :: Share.API.Project -> Cli ()
      doCreateBranch remoteProject = do
        oompaLoompaCreateBranch
          maybeLocalProjectAndBranch
          localBranchCausalHash
          (ProjectAndBranch (RemoteProjectId (remoteProject ^. #projectId)) remoteBranchName)
  Share.getProjectByName remoteProjectName >>= \case
    Share.API.GetProjectResponseNotFound ->
      pure do
        remoteProject <- oinkCreateRemoteProject remoteProjectName
        doCreateBranch remoteProject
    Share.API.GetProjectResponseSuccess remoteProject -> do
      let remoteProjectId = RemoteProjectId (remoteProject ^. #projectId)
      Share.getProjectBranchByName (ProjectAndBranch remoteProjectId remoteBranchName) >>= \case
        Share.API.GetProjectBranchResponseNotFound -> pure (doCreateBranch remoteProject)
        Share.API.GetProjectBranchResponseSuccess remoteBranch ->
          oompaLoompaFastForward maybeLocalProjectAndBranch localBranchCausalHash remoteBranch

-- we have the remote project id and remote branch name, but we don't know whether the remote branch exists
oompaLoompa1 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli (Cli ())
oompaLoompa1 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch =
  Share.getProjectBranchByName remoteProjectAndBranch >>= \case
    Share.API.GetProjectBranchResponseNotFound ->
      -- FIXME check to see if the project exists here instead of assuming it does
      pure (oompaLoompaCreateBranch (Just localProjectAndBranch) localBranchCausalHash remoteProjectAndBranch)
    Share.API.GetProjectBranchResponseSuccess remoteBranch ->
      oompaLoompaFastForward (Just localProjectAndBranch) localBranchCausalHash remoteBranch

-- we have the remote project id and remote branch id
oompaLoompa2 ::
  ProjectAndBranch Queries.Project Queries.Branch ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId RemoteProjectBranchId ->
  Cli (Cli ())
oompaLoompa2 localProjectAndBranch localBranchCausalHash remoteProjectAndBranch = do
  Share.getProjectBranchById remoteProjectAndBranch >>= \case
    Share.API.GetProjectBranchResponseNotFound ->
      -- remote branch or project deleted
      wundefined
    Share.API.GetProjectBranchResponseSuccess remoteBranch ->
      oompaLoompaFastForward
        (Just localProjectAndBranch)
        localBranchCausalHash
        remoteBranch

-- we know remote project exists but remote branch doesn't
oompaLoompaCreateBranch ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli ()
oompaLoompaCreateBranch
  maybeLocalProjectAndBranch
  localBranchCausalHash
  (ProjectAndBranch remoteProjectId remoteBranchName) = do
    _remoteBranch <-
      oinkCreateRemoteBranch
        Share.API.CreateProjectBranchRequest
          { projectId = unRemoteProjectId remoteProjectId,
            branchName = into @Text remoteBranchName,
            branchCausalHash = localBranchCausalHash,
            branchMergeTarget = wundefined
          }
    pure ()

oompaLoompaFastForward ::
  Maybe (ProjectAndBranch Queries.Project Queries.Branch) ->
  Hash32 ->
  Share.API.ProjectBranch ->
  Cli (Cli ())
oompaLoompaFastForward maybeLocalProjectAndBranch localBranchCausalHash remoteBranch = do
  -- TODO don't proceed with push if local head not ahead of remote head
  -- TODO otherwise make fast-forward request
  wundefined

-- Derive the remote branch user slug and remote branch name from the user's handle and the local branch name.
--
-- Example 1 - user "bob" has local branch "topic"
--   remoteBranchUserSlug = "bob"
--   remoteBranchName     = "@bob/topic"
--
-- Example 2 - user "bob" has local branch "@runar/topic"
--   remoteBranchUserSlug = "runar"
--   remoteBranchName     = "@runar/topic"
deriveRemoteBranchName :: Text -> ProjectBranchName -> (Text, ProjectBranchName)
deriveRemoteBranchName myUserHandle branchName =
  case projectBranchNameUserSlug branchName of
    Nothing -> (myUserHandle, prependUserSlugToProjectBranchName myUserHandle branchName)
    Just userSlug -> (userSlug, branchName)

remoteProjectRepoName :: Share.API.Project -> Cli Share.RepoName
remoteProjectRepoName project =
  case tryInto @ProjectName (project ^. #projectName) of
    -- This shouldn't happen often - Share gave us a project name that we don't consider valid?
    Left _ -> wundefined
    Right projectName ->
      case projectNameUserSlug projectName of
        Nothing -> wundefined
        Just userSlug -> pure (Share.RepoName userSlug)

remoteProjectBranchRepoName :: Share.API.ProjectBranch -> Cli Share.RepoName
remoteProjectBranchRepoName branch =
  case tryInto @ProjectName (branch ^. #projectName) of
    -- This shouldn't happen often - Share gave us a branch name that we don't consider valid?
    Left _ -> wundefined
    Right projectName ->
      case tryInto @ProjectBranchName (branch ^. #branchName) of
        -- This shouldn't happen often - Share gave us a project name that we don't consider valid?
        Left _ -> wundefined
        Right branchName -> projectBranchRepoName (ProjectAndBranch projectName branchName)

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
        -- eep, neither project nor branch name have a user slug, so it doesn't seem like we know what to use for the
        -- repo name. just bail.
        Nothing -> wundefined
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
    Share.API.CreateProjectResponseBadRequest -> do
      loggeth ["Share says: bad request"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectResponseUnauthorized -> do
      loggeth ["Share says: unauthorized"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectResponseSuccess remoteProject -> do
      loggeth ["Share says: success!"]
      loggeth [tShow remoteProject]
      loggeth ["TODO insert remote_project"]
      pure remoteProject

oinkCreateRemoteBranch :: Share.API.CreateProjectBranchRequest -> Cli Share.API.ProjectBranch
oinkCreateRemoteBranch request = do
  loggeth ["creating remote branch"]
  loggeth [tShow request]
  Share.createProjectBranch request >>= \case
    Share.API.CreateProjectBranchResponseBadRequest -> do
      loggeth ["Share says: bad request"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectBranchResponseUnauthorized -> do
      loggeth ["Share says: unauthorized"]
      Cli.returnEarlyWithoutOutput
    Share.API.CreateProjectBranchResponseSuccess remoteBranch -> do
      loggeth ["Share says: success!"]
      loggeth [tShow remoteBranch]
      loggeth ["TODO insert remote_project_branch"]
      loggeth ["TODO insert remote_project"]
      loggeth ["TODO insert project_branch_remote_mapping"]
      pure remoteBranch

oinkGetLoggedInUser :: Cli Text
oinkGetLoggedInUser = do
  loggeth ["Getting current logged-in user on Share"]
  AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  myUserHandle <- wundefined
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

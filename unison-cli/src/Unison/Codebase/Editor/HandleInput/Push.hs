-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Set.NonEmpty as Set.NonEmpty
import Data.Text as Text
import qualified System.Console.Regions as Console.Regions
import U.Codebase.HashTags (CausalHash (..))
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
          remoteProjectAndBranch <- resolveProjectAndBranchMaybeNamesToNames remoteProjectAndBranch0
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
      remoteProjectAndBranch <- resolveProjectAndBranchMaybeNamesToNames remoteProjectAndBranch0
      pushLooseCodeToProjectBranch localPath remoteProjectAndBranch
    -- push @some/project to .some.path
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (PathyTarget remotePath) -> do
      localProjectAndBranch <- resolveProjectAndBranchMaybeNamesToIds localProjectAndBranch0
      pushLooseCodeToLooseCode (projectBranchPath localProjectAndBranch) remotePath pushBehavior syncMode
    -- push @some/project to @some/project
    PushSourceTarget2 (ProjySource localProjectAndBranch0) (ProjyTarget remoteProjectAndBranch) -> do
      localProjectAndBranch <- resolveProjectAndBranchMaybeNamesToIds localProjectAndBranch0
      pushProjectBranchToProjectBranch localProjectAndBranch (Just remoteProjectAndBranch)

resolveProjectAndBranchMaybeNamesToIds ::
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli (ProjectAndBranch Queries.ProjectId Queries.BranchId)
resolveProjectAndBranchMaybeNamesToIds = \case
  ProjectAndBranch Nothing Nothing ->
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch yo"]
        Cli.returnEarlyWithoutOutput
      Just projectAndBranch -> pure projectAndBranch
  ProjectAndBranch Nothing (Just branchName) ->
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch yo"]
        Cli.returnEarlyWithoutOutput
      Just (ProjectAndBranch projectId _branchId) ->
        Cli.runTransaction (Queries.loadProjectBranchByName projectId (into @Text branchName)) >>= \case
          Nothing -> do
            loggeth ["no branch in project ", tShow projectId, " with name ", into @Text branchName]
            Cli.returnEarlyWithoutOutput
          Just branch -> pure (ProjectAndBranch projectId (branch ^. #branchId))
  ProjectAndBranch (Just projectName) maybeBranchName -> do
    let branchName = fromMaybe (unsafeFrom @Text "main") maybeBranchName
    maybeProjectAndBranch <-
      Cli.runTransaction do
        runMaybeT do
          project <- MaybeT (Queries.loadProjectByName (into @Text projectName))
          let projectId = project ^. #projectId
          branch <- MaybeT (Queries.loadProjectBranchByName projectId (into @Text branchName))
          pure (ProjectAndBranch projectId (branch ^. #branchId))
    case maybeProjectAndBranch of
      Nothing -> do
        loggeth [into @Text (ProjectAndBranch (Just projectName) (Just branchName)), " not found!"]
        Cli.returnEarlyWithoutOutput
      Just projectAndBranch -> pure projectAndBranch

resolveProjectAndBranchMaybeNamesToNames ::
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli (ProjectAndBranch ProjectName ProjectBranchName)
resolveProjectAndBranchMaybeNamesToNames = \case
  ProjectAndBranch Nothing Nothing ->
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch"]
        Cli.returnEarlyWithoutOutput
      Just (ProjectAndBranch projectId branchId) ->
        Cli.runTransaction do
          project <- Queries.expectProject projectId
          branch <- Queries.expectProjectBranch projectId branchId
          pure (ProjectAndBranch (unsafeFrom @Text (project ^. #name)) (unsafeFrom @Text (branch ^. #name)))
  ProjectAndBranch Nothing (Just branchName) -> do
    getCurrentProjectBranch >>= \case
      Nothing -> do
        loggeth ["not on a project branch"]
        Cli.returnEarlyWithoutOutput
      Just (ProjectAndBranch projectId _branchId) ->
        Cli.runTransaction do
          project <- Queries.expectProject projectId
          pure (ProjectAndBranch (unsafeFrom @Text (project ^. #name)) branchName)
  ProjectAndBranch (Just projectName) Nothing -> pure (ProjectAndBranch projectName (unsafeFrom @Text "main"))
  ProjectAndBranch (Just projectName) (Just branchName) -> pure (ProjectAndBranch projectName branchName)

-- | Push a local namespace ("loose code") to a remote namespace ("loose code").
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

-- | Push a local namespace ("loose code") to a Git-hosted remote namespace ("loose code").
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

-- | Push a local namespace ("loose code") to a Share-hosted remote namespace ("loose code").
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

-- | Push a local namespace ("loose code") to a remote project branch.
pushLooseCodeToProjectBranch ::
  Path.Absolute ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli ()
pushLooseCodeToProjectBranch localPath remoteProjectAndBranch = wundefined

-- | Push a local project branch to a remote project branch. If the remote project branch is left unspecified, we either
-- use a pre-existing mapping for the local branch, or else infer what remote branch to push to (possibly creating it).
pushProjectBranchToProjectBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  Maybe (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName)) ->
  Cli ()
pushProjectBranchToProjectBranch localProjectAndBranchIds = \case
  Nothing -> pushProjectBranchToImplicitProjectBranch localProjectAndBranchIds
  Just remoteProjectAndBranchNames ->
    pushProjectBranchToExplicitProjectBranch
      localProjectAndBranchIds
      remoteProjectAndBranchNames

pushProjectBranchToImplicitProjectBranch :: ProjectAndBranch Queries.ProjectId Queries.BranchId -> Cli ()
pushProjectBranchToImplicitProjectBranch localProjectAndBranchIds =
  Cli.runTransaction oinkResolveRemoteIds >>= \case
    Nothing -> pushProjectBranchToBrandNewProjectBranch localProjectAndBranchIds
    Just (ProjectAndBranch remoteProjectId maybeRemoteBranchId) ->
      case maybeRemoteBranchId of
        Nothing -> pushProjectBranchToNewProjectBranch localProjectAndBranchIds (ProjectAndBranch remoteProjectId ())
        Just remoteBranchId ->
          pushProjectBranchToExistingProjectBranch
            localProjectAndBranchIds
            (ProjectAndBranch remoteProjectId remoteBranchId)

-- "brand new" = new project and new branch
pushProjectBranchToBrandNewProjectBranch :: ProjectAndBranch Queries.ProjectId Queries.BranchId -> Cli ()
pushProjectBranchToBrandNewProjectBranch localProjectAndBranchIds = do
  loggeth ["We don't have a remote branch mapping for this branch or any ancestor"]

  -- Load local project and branch from database
  ProjectAndBranch localProject localBranch <-
    Cli.runTransaction (expectProjectAndBranch localProjectAndBranchIds)

  -- Get my user handle from Share
  myUserHandle <- oinkGetLoggedInUser

  -- Get the remote project user slug and remote project name.
  --
  -- Example 1 - the local project is called "foo" and the user is "bob":
  --   remoteProjectUserSlug = "bob"
  --   remoteProjectName     = "@bob/foo"
  --
  -- Example 2 - the local project is called "@foo/bar" and the user is "bob":
  --   remoteProjectUserSlug = "foo"
  --   remoteProjectName     = "@foo/bar"
  let (remoteProjectUserSlug, remoteProjectName) =
        let localProjectName = unsafeFrom @Text (localProject ^. #name)
         in case projectNameUserSlug localProjectName of
              Nothing -> (myUserHandle, prependUserSlugToProjectName myUserHandle localProjectName)
              Just userSlug -> (userSlug, localProjectName)

  -- Upload the branch to Share. We use the remote project user slug as the "Share repo" to upload to.
  localBranchCausalHash <- do
    let localPath = projectBranchPath localProjectAndBranchIds
    let localPathSegments = coerce @[NameSegment] @[Text] (Path.toList (Path.unabsolute localPath))
    Cli.runTransaction (Operations.loadCausalHashAtPath localPathSegments) & onNothingM do
      Cli.returnEarly (EmptyPush (Path.absoluteToPath' localPath))
  oinkUpload (Hash32.fromHash (unCausalHash localBranchCausalHash)) remoteProjectUserSlug

  -- The branch contents are uploaded, so create the remote project and remote branch.
  remoteProject <- oinkCreateRemoteProject (into @Text remoteProjectName)
  let localBranchName = unsafeFrom @Text (localBranch ^. #name)
  let remoteBranchName = prependUserSlugToProjectBranchName myUserHandle localBranchName
  remoteBranch <-
    oinkCreateRemoteBranch
      Share.API.CreateProjectBranchRequest
        { projectId = remoteProject ^. #projectId,
          branchName = into @Text remoteBranchName,
          branchCausalHash = Hash32.fromHash (unCausalHash localBranchCausalHash),
          branchMergeTarget = Nothing
        }

  pure ()

-- "new" = existing project, new branch
pushProjectBranchToNewProjectBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  ProjectAndBranch Text () ->
  Cli ()
pushProjectBranchToNewProjectBranch localProjectAndBranchIds (ProjectAndBranch remoteProjectId ()) = do
  loggeth
    [ "We don't have a remote branch mapping, but our ancestor maps to project: ",
      remoteProjectId
    ]
  myUserHandle <- oinkGetLoggedInUser
  wundefined

pushProjectBranchToExistingProjectBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  ProjectAndBranch Text Text ->
  Cli ()
pushProjectBranchToExistingProjectBranch localProjectAndBranchIds (ProjectAndBranch remoteProjectId remoteBranchId) = wundefined

pushProjectBranchToExplicitProjectBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) ->
  Cli ()
pushProjectBranchToExplicitProjectBranch = wundefined

expectProjectAndBranch ::
  ProjectAndBranch Queries.ProjectId Queries.BranchId ->
  Sqlite.Transaction (ProjectAndBranch Queries.Project Queries.Branch)
expectProjectAndBranch (ProjectAndBranch projectId branchId) = do
  project <- Queries.expectProject projectId
  branch <- Queries.expectProjectBranch projectId branchId
  pure (ProjectAndBranch project branch)

oinkUpload localBranchCausalHash remoteProjectUserSlug = do
  loggeth ["uploading entities"]
  Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
    let upload =
          Share.uploadEntities
            (codeserverBaseURL Codeserver.defaultCodeserver)
            (Share.RepoName remoteProjectUserSlug)
            (Set.NonEmpty.singleton localBranchCausalHash)
            uploadedCallback
    upload & onLeftM \err -> do
      loggeth ["upload entities error"]
      loggeth [tShow err]
      Cli.returnEarlyWithoutOutput

oinkCreateRemoteProject projectName = do
  let request = Share.API.CreateProjectRequest {projectName}
  loggeth ["Making create-project request for project"]
  loggeth [tShow request]
  response <-
    Share.createProject request & onLeftM \err -> do
      loggeth ["Creating a project failed"]
      loggeth [tShow err]
      Cli.returnEarlyWithoutOutput
  case response of
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

oinkCreateRemoteBranch request = do
  loggeth ["creating remote branch"]
  loggeth [tShow request]
  response <-
    Share.createProjectBranch request & onLeftM \err -> do
      loggeth ["Creating a branch failed"]
      loggeth [tShow err]
      Cli.returnEarlyWithoutOutput
  case response of
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

oinkResolveRemoteIds :: Sqlite.Transaction (Maybe (ProjectAndBranch Text (Maybe Text)))
oinkResolveRemoteIds = undefined

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

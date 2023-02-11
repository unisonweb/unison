-- | @push@ input handler
module Unison.Codebase.Editor.HandleInput.Push where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import Data.Text as Text
import qualified System.Console.Regions as Console.Regions
import U.Codebase.HashTags (CausalHash (..))
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.ProjectUtils (getCurrentProjectBranch, loggeth)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Cli.UnisonConfigUtils as UnisonConfigUtils
import Unison.Codebase (PushGitBranchOpts (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import qualified Unison.Codebase.Editor.HandleInput.AuthLogin as AuthLogin
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.Output.PushPull (PushPull (Push))
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    ShareUserHandle (..),
    WriteGitRemotePath (..),
    WriteGitRepo,
    WriteRemotePath (..),
    WriteShareRemotePath (..),
    writeToReadGit,
  )
import Unison.Codebase.Path (Path, Path' (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.ShortCausalHash as SCH
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Type (GitPushBehavior (..))
import qualified Unison.Hash as Hash
import Unison.Hash32 (Hash32)
import qualified Unison.Hash32 as Hash32
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch,
    ProjectBranchName,
    ProjectName,
    prependUserSlugToProjectBranchName,
    prependUserSlugToProjectName,
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
handleGist (GistInput repo) =
  doPushRemoteBranch (GistyPush repo) Path.currentPath SyncMode.ShortCircuit

-- | Handle a @push@ command.
handlePushRemoteBranch :: PushRemoteBranchInput -> Cli ()
handlePushRemoteBranch PushRemoteBranchInput {localPath, maybeRemoteRepo, pushBehavior, syncMode} =
  case (localPath, maybeRemoteRepo) of
    (Nothing, Nothing) ->
      getCurrentProjectBranch >>= \case
        Nothing -> do
          remotePath <- UnisonConfigUtils.resolveConfiguredUrl Push Path.currentPath
          doPushRemoteBranch (NormalPush remotePath pushBehavior) Path.currentPath SyncMode.ShortCircuit
        Just (projectId, branchId) -> projectPush projectId branchId Nothing
    (Nothing, Just (PathyTarget remotePath)) ->
      doPushRemoteBranch (NormalPush remotePath pushBehavior) Path.currentPath syncMode
    (Nothing, Just (ProjyTarget remoteProjectAndBranch)) -> wundefined
    (Just (PathySource localPath1), Just (PathyTarget remotePath)) ->
      doPushRemoteBranch (NormalPush remotePath pushBehavior) localPath1 syncMode
    (Just (PathySource localPath1), Just (ProjyTarget remoteProjectAndBranch)) -> wundefined
    (Just (ProjySource projectAndBranch), Just (PathyTarget remotePath)) ->
      doPushRemoteBranch (NormalPush remotePath pushBehavior) wundefined syncMode
    (Just (ProjySource projectAndBranch), Just (ProjyTarget remoteProjectAndBranch)) -> wundefined

-- | Either perform a "normal" push (updating a remote path), which takes a 'PushBehavior' (to control whether creating
-- a new namespace is allowed), or perform a "gisty" push, which doesn't update any paths (and also is currently only
-- uploaded for remote git repos, not remote Share repos).
data PushFlavor
  = NormalPush WriteRemotePath PushBehavior
  | GistyPush WriteGitRepo

-- Internal helper that implements pushing to a remote repo, which generalizes @gist@ and @push@.
doPushRemoteBranch ::
  -- | The repo to push to.
  PushFlavor ->
  -- | The local path to push. If relative, it's resolved relative to the current path (`cd`).
  Path' ->
  SyncMode.SyncMode ->
  Cli ()
doPushRemoteBranch pushFlavor localPath0 syncMode = do
  Cli.Env {codebase} <- ask
  localPath <- Cli.resolvePath' localPath0
  case pushFlavor of
    NormalPush (writeRemotePath@(WriteRemotePathGit WriteGitRemotePath {repo, path = remotePath})) pushBehavior -> do
      sourceBranch <- Cli.getBranchAt localPath
      let withRemoteRoot :: Branch IO -> Either Output (Branch IO)
          withRemoteRoot remoteRoot = do
            let -- We don't merge `sourceBranch` with `remoteBranch`, we just replace it. This push will be rejected if
                -- this rewinds time or misses any new updates in the remote branch that aren't in `sourceBranch`
                -- already.
                f remoteBranch = if shouldPushTo pushBehavior remoteBranch then Just sourceBranch else Nothing
            case Branch.modifyAtM remotePath f remoteRoot of
              Nothing -> Left (RefusedToPush pushBehavior writeRemotePath)
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
      result <-
        Cli.ioE (Codebase.pushGitBranch codebase repo opts (\remoteRoot -> pure (withRemoteRoot remoteRoot))) \err ->
          Cli.returnEarly (Output.GitError err)
      _branch <- result & onLeft Cli.returnEarly
      Cli.respond Success
    NormalPush (WriteRemotePathShare sharePath) pushBehavior -> handlePushToUnisonShare sharePath localPath pushBehavior
    GistyPush repo -> do
      sourceBranch <- Cli.getBranchAt localPath
      let opts =
            PushGitBranchOpts
              { behavior = GitPushBehaviorGist,
                syncMode
              }
      result <-
        Cli.ioE (Codebase.pushGitBranch codebase repo opts (\_remoteRoot -> pure (Right sourceBranch))) \err ->
          Cli.returnEarly (Output.GitError err)
      _branch <- result & onLeft Cli.returnEarly
      schLength <- Cli.runTransaction Codebase.branchHashLength
      Cli.respond $
        GistCreated
          ( ReadRemoteNamespaceGit
              ReadGitRemoteNamespace
                { repo = writeToReadGit repo,
                  sch = Just (SCH.fromHash schLength (Branch.headHash sourceBranch)),
                  path = Path.empty
                }
          )
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

handlePushToUnisonShare :: WriteShareRemotePath -> Path.Absolute -> PushBehavior -> Cli ()
handlePushToUnisonShare remote@WriteShareRemotePath {server, repo, path = remotePath} localPath behavior = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  let sharePath = Share.Path (shareUserHandleToText repo Nel.:| pathToSegments remotePath)
  ensureAuthenticatedWithCodeserver codeserver

  -- doesn't handle the case where a non-existent path is supplied
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

    pushError :: (a -> Output.ShareError) -> Share.SyncError a -> Cli b
    pushError f err0 = do
      Cli.returnEarly case err0 of
        Share.SyncError err -> Output.ShareError (f err)
        Share.TransportError err -> Output.ShareError (ShareErrorTransport err)

-- | Push a project branch.
projectPush ::
  Queries.ProjectId ->
  Queries.BranchId ->
  Maybe (ProjectAndBranch ProjectName (Maybe ProjectBranchName)) ->
  Cli ()
projectPush projectId branchId = \case
  Nothing -> do
    Cli.runTransaction oinkResolveRemoteIds >>= \case
      Nothing -> do
        loggeth ["We don't have a remote branch mapping for this branch or any ancestor"]
        loggeth ["Getting current logged-in user on Share"]
        myUserHandle <- oinkGetLoggedInUser
        loggeth ["Got current logged-in user on Share: ", myUserHandle]
        (project, branch) <-
          Cli.runTransaction do
            project <- Queries.expectProject projectId
            branch <- Queries.expectProjectBranch projectId branchId
            pure (project, branch)
        let localProjectName = unsafeFrom @Text (project ^. #name)
        let remoteProjectName = prependUserSlugToProjectName myUserHandle localProjectName
        response <- do
          let request = Share.API.CreateProjectRequest {projectName = into @Text remoteProjectName}
          loggeth ["Making create-project request for project"]
          loggeth [tShow request]
          Share.createProject request & onLeftM \err -> do
            loggeth ["Creating a project failed"]
            loggeth [tShow err]
            Cli.returnEarlyWithoutOutput
        remoteProject <-
          case response of
            Share.API.CreateProjectResponseBadRequest -> do
              loggeth ["Share says: bad request"]
              Cli.returnEarlyWithoutOutput
            Share.API.CreateProjectResponseUnauthorized -> do
              loggeth ["Share says: unauthorized"]
              Cli.returnEarlyWithoutOutput
            Share.API.CreateProjectResponseSuccess remoteProject -> pure remoteProject
        loggeth ["Share says: success!"]
        loggeth [tShow remoteProject]
        let localBranchName = unsafeFrom @Text (branch ^. #name)
        let remoteBranchName = prependUserSlugToProjectBranchName myUserHandle localBranchName
        loggeth ["Making create-branch request for branch", into @Text remoteProjectName]
        response <- do
          let request =
                Share.API.CreateProjectBranchRequest
                  { projectId = remoteProject ^. #projectId,
                    branchName = into @Text remoteBranchName,
                    branchCausalHash = wundefined,
                    branchMergeTarget = wundefined
                  }
          loggeth ["Making create-project request for project"]
          loggeth [tShow request]
          Share.createProjectBranch request & onLeftM \err -> do
            loggeth ["Creating a branch failed"]
            loggeth [tShow err]
            Cli.returnEarlyWithoutOutput
        remoteBranch <-
          case response of
            Share.API.CreateProjectBranchResponseBadRequest -> do
              loggeth ["Share says: bad request"]
              Cli.returnEarlyWithoutOutput
            Share.API.CreateProjectBranchResponseUnauthorized -> do
              loggeth ["Share says: unauthorized"]
              Cli.returnEarlyWithoutOutput
            Share.API.CreateProjectBranchResponseSuccess remoteBranch -> pure remoteBranch
        loggeth ["Share says: success!"]
        loggeth [tShow remoteBranch]
      Just projectAndBranch ->
        case projectAndBranch ^. #branch of
          Nothing -> do
            let ancestorRemoteProjectId = projectAndBranch ^. #project
            loggeth ["We don't have a remote branch mapping, but our ancestor maps to project: ", ancestorRemoteProjectId]
            loggeth ["Creating remote branch not implemented"]
            Cli.returnEarlyWithoutOutput
          Just remoteBranchId -> do
            let remoteProjectId = projectAndBranch ^. #project
            loggeth ["Found remote branch mapping: ", remoteProjectId, ":", remoteBranchId]
            loggeth ["Pushing to existing branch not implemented"]
            Cli.returnEarlyWithoutOutput
  Just projectAndBranch -> do
    let _projectName = projectAndBranch ^. #project
    let _branchName = fromMaybe (unsafeFrom @Text "main") (projectAndBranch ^. #branch)
    loggeth ["Specifying project/branch to push to not implemented"]
    Cli.returnEarlyWithoutOutput

oinkResolveRemoteIds :: Sqlite.Transaction (Maybe (ProjectAndBranch Text (Maybe Text)))
oinkResolveRemoteIds = undefined

oinkGetLoggedInUser :: Cli Text
oinkGetLoggedInUser = do
  AuthLogin.ensureAuthenticatedWithCodeserver Codeserver.defaultCodeserver
  wundefined

-- | @pull@ input handler
module Unison.Codebase.Editor.HandleInput.Pull
  ( doPullRemoteBranch,
    loadShareLooseCodeIntoMemory,
    loadPropagateDiffDefaultPatch,
    mergeBranchAndPropagateDefaultPatch,
    propagatePatch,
    downloadShareProjectBranch,
    withEntitiesDownloadedProgressCallback,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List.NonEmpty qualified as Nel
import Data.Text qualified as Text
import Data.These
import System.Console.Regions qualified as Console.Regions
import U.Codebase.Sqlite.Project qualified as Sqlite (Project)
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Cli.UnisonConfigUtils (resolveConfiguredUrl)
import Unison.Codebase (Preprocessing (..))
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.Output.PushPull qualified as PushPull
import Unison.Codebase.Editor.Propagate qualified as Propagate
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace (..), ReadShareLooseCode (..), ShareUserHandle (..), printReadRemoteNamespace)
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.SyncMode qualified as SyncMode
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName))
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchNameOrLatestRelease (..), ProjectName)
import Unison.Share.API.Hash (HashJWT)
import Unison.Share.API.Hash qualified as Share
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Sync qualified as Share
import Unison.Share.Sync.Types qualified as Share
import Unison.Share.Types (codeserverBaseURL)
import Unison.Sync.Common qualified as Common
import Unison.Sync.Types qualified as Share
import Witch (unsafeFrom)

doPullRemoteBranch :: PullSourceTarget -> SyncMode.SyncMode -> PullMode -> Verbosity.Verbosity -> Cli ()
doPullRemoteBranch unresolvedSourceAndTarget syncMode pullMode verbosity = do
  let includeSquashed = case pullMode of
        Input.PullWithHistory -> Share.NoSquashedHead
        Input.PullWithoutHistory -> Share.IncludeSquashedHead
  (source, target) <- resolveSourceAndTarget includeSquashed unresolvedSourceAndTarget
  remoteBranchObject <- loadRemoteNamespaceIntoMemory syncMode pullMode source
  when (Branch.isEmpty0 (Branch.head remoteBranchObject)) do
    Cli.respond (PulledEmptyBranch source)
  targetAbsolutePath <-
    case target of
      Left path -> Cli.resolvePath' path
      Right (ProjectAndBranch project branch) ->
        pure $ ProjectUtils.projectBranchPath (ProjectAndBranch (project ^. #projectId) (branch ^. #branchId))
  let description =
        Text.unwords
          [ Text.pack . InputPattern.patternName $
              case pullMode of
                PullWithoutHistory -> InputPatterns.pullWithoutHistory
                PullWithHistory -> InputPatterns.pull,
            printReadRemoteNamespace (\remoteBranch -> into @Text (ProjectAndBranch (remoteBranch ^. #projectName) (remoteBranch ^. #branchName))) source,
            case target of
              Left path -> Path.toText' path
              Right (ProjectAndBranch project branch) -> into @Text (ProjectAndBranch (project ^. #name) (branch ^. #name))
          ]
  case pullMode of
    Input.PullWithHistory -> do
      targetBranchObject <- Cli.getBranch0At targetAbsolutePath
      if Branch.isEmpty0 targetBranchObject
        then do
          void $ Cli.updateAtM description targetAbsolutePath (const $ pure remoteBranchObject)
          Cli.respond $ MergeOverEmpty target
        else do
          Cli.respond AboutToMerge
          mergeBranchAndPropagateDefaultPatch
            Branch.RegularMerge
            description
            (Just (PullAlreadyUpToDate source target))
            remoteBranchObject
            (if Verbosity.isSilent verbosity then Nothing else Just target)
            targetAbsolutePath
    Input.PullWithoutHistory -> do
      didUpdate <-
        Cli.updateAtM
          description
          targetAbsolutePath
          (\targetBranchObject -> pure $ remoteBranchObject `Branch.consBranchSnapshot` targetBranchObject)
      Cli.respond
        if didUpdate
          then PullSuccessful source target
          else PullAlreadyUpToDate source target

resolveSourceAndTarget ::
  Share.IncludeSquashedHead ->
  PullSourceTarget ->
  Cli
    ( ReadRemoteNamespace Share.RemoteProjectBranch,
      Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
    )
resolveSourceAndTarget includeSquashed = \case
  Input.PullSourceTarget0 -> liftA2 (,) (resolveImplicitSource includeSquashed) resolveImplicitTarget
  Input.PullSourceTarget1 source -> liftA2 (,) (resolveExplicitSource includeSquashed source) resolveImplicitTarget
  Input.PullSourceTarget2 source target ->
    liftA2 (,) (resolveExplicitSource includeSquashed source) (ProjectUtils.expectLooseCodeOrProjectBranch target)

resolveImplicitSource :: Share.IncludeSquashedHead -> Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveImplicitSource includeSquashed =
  ProjectUtils.getCurrentProjectBranch >>= \case
    Nothing -> RemoteRepo.writeNamespaceToRead <$> resolveConfiguredUrl PushPull.Pull Path.currentPath
    Just (localProjectAndBranch, _restPath) -> do
      (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName) <-
        Cli.runTransactionWithRollback \rollback -> do
          let localProjectId = localProjectAndBranch ^. #project . #projectId
          let localBranchId = localProjectAndBranch ^. #branch . #branchId
          Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId >>= \case
            Just (remoteProjectId, Just remoteBranchId) -> do
              remoteProjectName <- Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri
              remoteBranchName <-
                Queries.expectRemoteProjectBranchName
                  Share.hardCodedUri
                  remoteProjectId
                  remoteBranchId
              pure (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName)
            _ -> rollback (Output.NoAssociatedRemoteProjectBranch Share.hardCodedUri localProjectAndBranch)
      remoteBranch <-
        ProjectUtils.expectRemoteProjectBranchById includeSquashed $
          ProjectAndBranch
            (remoteProjectId, remoteProjectName)
            (remoteBranchId, remoteBranchName)
      pure (ReadShare'ProjectBranch remoteBranch)

resolveExplicitSource ::
  Share.IncludeSquashedHead ->
  ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease) ->
  Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveExplicitSource includeSquashed = \case
  ReadRemoteNamespaceGit namespace -> pure (ReadRemoteNamespaceGit namespace)
  ReadShare'LooseCode namespace -> pure (ReadShare'LooseCode namespace)
  ReadShare'ProjectBranch (This remoteProjectName) -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName remoteProjectName
    let remoteProjectId = remoteProject ^. #projectId
    let remoteBranchName = unsafeFrom @Text "main"
    remoteProjectBranch <-
      ProjectUtils.expectRemoteProjectBranchByName
        includeSquashed
        (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
    pure (ReadShare'ProjectBranch remoteProjectBranch)
  ReadShare'ProjectBranch (That branchNameOrLatestRelease) -> do
    (ProjectAndBranch localProject localBranch, _restPath) <- ProjectUtils.expectCurrentProjectBranch
    let localProjectId = localProject ^. #projectId
    let localBranchId = localBranch ^. #branchId
    Cli.runTransaction (Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId) >>= \case
      Just (remoteProjectId, _maybeProjectBranchId) -> do
        remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
        remoteBranchName <- resolveRemoteBranchName remoteProjectName branchNameOrLatestRelease
        remoteProjectBranch <-
          ProjectUtils.expectRemoteProjectBranchByName
            includeSquashed
            (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
        pure (ReadShare'ProjectBranch remoteProjectBranch)
      Nothing -> do
        Cli.returnEarly $
          Output.NoAssociatedRemoteProject
            Share.hardCodedUri
            (ProjectAndBranch (localProject ^. #name) (localBranch ^. #name))
  ReadShare'ProjectBranch (These projectName branchNameOrLatestRelease) -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    branchName <- resolveRemoteBranchName projectName branchNameOrLatestRelease
    remoteProjectBranch <-
      ProjectUtils.expectRemoteProjectBranchByName
        includeSquashed
        (ProjectAndBranch (remoteProjectId, projectName) branchName)
    pure (ReadShare'ProjectBranch remoteProjectBranch)
  where
    resolveRemoteBranchName :: ProjectName -> ProjectBranchNameOrLatestRelease -> Cli ProjectBranchName
    resolveRemoteBranchName projectName = \case
      ProjectBranchNameOrLatestRelease'Name branchName -> pure branchName
      ProjectBranchNameOrLatestRelease'LatestRelease -> do
        remoteProject <- ProjectUtils.expectRemoteProjectByName projectName
        case remoteProject ^. #latestRelease of
          Nothing -> Cli.returnEarly (Output.ProjectHasNoReleases projectName)
          Just semver -> pure (UnsafeProjectBranchName ("releases/" <> into @Text semver))

resolveImplicitTarget :: Cli (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
resolveImplicitTarget =
  ProjectUtils.getCurrentProjectBranch <&> \case
    Nothing -> Left Path.currentPath
    Just (projectAndBranch, _restPath) -> Right projectAndBranch

loadRemoteNamespaceIntoMemory ::
  SyncMode ->
  PullMode ->
  ReadRemoteNamespace Share.RemoteProjectBranch ->
  Cli (Branch IO)
loadRemoteNamespaceIntoMemory syncMode pullMode remoteNamespace = do
  Cli.Env {codebase} <- ask
  case remoteNamespace of
    ReadRemoteNamespaceGit repo -> do
      let preprocess = case pullMode of
            Input.PullWithHistory -> Unmodified
            Input.PullWithoutHistory -> Preprocessed $ pure . Branch.discardHistory
      Cli.ioE (Codebase.importRemoteBranch codebase repo syncMode preprocess) \err ->
        Cli.returnEarly (Output.GitError err)
    ReadShare'LooseCode repo -> loadShareLooseCodeIntoMemory repo
    ReadShare'ProjectBranch remoteBranch -> do
      projectBranchCausalHashJWT <- downloadShareProjectBranch (pullMode == Input.PullWithoutHistory) remoteBranch
      let causalHash = Common.hash32ToCausalHash (Share.hashJWTHash projectBranchCausalHashJWT)
      liftIO (Codebase.expectBranchForHash codebase causalHash)

-- | @downloadShareProjectBranch branch@ downloads the given branch.
downloadShareProjectBranch :: HasCallStack => Bool -> Share.RemoteProjectBranch -> Cli HashJWT
downloadShareProjectBranch useSquashedIfAvailable branch = do
  let remoteProjectBranchName = branch ^. #branchName
  let repoInfo = Share.RepoInfo (into @Text (ProjectAndBranch (branch ^. #projectName) remoteProjectBranchName))
  causalHashJwt <-
    if useSquashedIfAvailable
      then case (branch ^. #squashedBranchHead) of
        Nothing -> Cli.returnEarly (Output.ShareError ShareExpectedSquashedHead)
        Just squashedHead -> pure squashedHead
      else pure (branch ^. #branchHead)
  exists <- Cli.runTransaction (Queries.causalExistsByHash32 (Share.hashJWTHash causalHashJwt))
  when (not exists) do
    (result, numDownloaded) <-
      Cli.with withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
        result <- Share.downloadEntities Share.hardCodedBaseUrl repoInfo causalHashJwt downloadedCallback
        numDownloaded <- liftIO getNumDownloaded
        pure (result, numDownloaded)
    result & onLeft \err0 -> do
      (Cli.returnEarly . Output.ShareError) case err0 of
        Share.SyncError err -> Output.ShareErrorDownloadEntities err
        Share.TransportError err -> Output.ShareErrorTransport err
    Cli.respond (Output.DownloadedEntities numDownloaded)
  pure causalHashJwt

loadShareLooseCodeIntoMemory :: ReadShareLooseCode -> Cli (Branch IO)
loadShareLooseCodeIntoMemory rrn@(ReadShareLooseCode {server, repo, path}) = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  -- Auto-login to share if pulling from a non-public path
  when (not $ RemoteRepo.isPublic rrn) . void $ ensureAuthenticatedWithCodeserver codeserver
  let shareFlavoredPath = Share.Path (shareUserHandleToText repo Nel.:| coerce @[NameSegment] @[Text] (Path.toList path))
  Cli.Env {codebase} <- ask
  (causalHash, numDownloaded) <-
    Cli.with withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
      causalHash <-
        Share.pull baseURL shareFlavoredPath downloadedCallback & onLeftM \err0 ->
          (Cli.returnEarly . Output.ShareError) case err0 of
            Share.SyncError err -> Output.ShareErrorPull err
            Share.TransportError err -> Output.ShareErrorTransport err
      numDownloaded <- liftIO getNumDownloaded
      pure (causalHash, numDownloaded)
  Cli.respond (Output.DownloadedEntities numDownloaded)
  liftIO (Codebase.expectBranchForHash codebase causalHash)

-- Provide the given action a callback that display to the terminal.
withEntitiesDownloadedProgressCallback :: ((Int -> IO (), IO Int) -> IO a) -> IO a
withEntitiesDownloadedProgressCallback action = do
  entitiesDownloadedVar <- newTVarIO 0
  Console.Regions.displayConsoleRegions do
    Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
      Console.Regions.setConsoleRegion region do
        entitiesDownloaded <- readTVar entitiesDownloadedVar
        pure $
          "\n  Downloaded "
            <> tShow entitiesDownloaded
            <> " entities...\n\n"
      action ((\n -> atomically (modifyTVar' entitiesDownloadedVar (+ n))), readTVarIO entitiesDownloadedVar)

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch ::
  Branch.MergeMode ->
  Text ->
  Maybe Output ->
  Branch IO ->
  Maybe (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)) ->
  Path.Absolute ->
  Cli ()
mergeBranchAndPropagateDefaultPatch mode inputDescription unchangedMessage srcb maybeDest0 dest =
  ifM
    mergeBranch
    (loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest)
    (for_ unchangedMessage Cli.respond)
  where
    mergeBranch :: Cli Bool
    mergeBranch =
      Cli.time "mergeBranch" do
        Cli.Env {codebase} <- ask
        destb <- Cli.getBranchAt dest
        merged <- liftIO (Branch.merge'' (Codebase.lca codebase) mode srcb destb)
        b <- Cli.updateAtM inputDescription dest (const $ pure merged)
        for_ maybeDest0 \dest0 -> do
          (ppe, diff) <- diffHelper (Branch.head destb) (Branch.head merged)
          Cli.respondNumbered (ShowDiffAfterMerge dest0 dest ppe diff)
        pure b

loadPropagateDiffDefaultPatch ::
  Text ->
  Maybe (Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)) ->
  Path.Absolute ->
  Cli ()
loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest = do
  Cli.respond Output.AboutToPropagatePatch
  Cli.time "loadPropagateDiffDefaultPatch" do
    original <- Cli.getBranch0At dest
    patch <- liftIO $ Branch.getPatch Cli.defaultPatchNameSegment original
    patchDidChange <- propagatePatch inputDescription patch dest
    when patchDidChange do
      whenJust maybeDest0 \dest0 -> do
        Cli.respond Output.CalculatingDiff
        patched <- Cli.getBranchAt dest
        let patchPath = Path.Path' (Right (Path.Relative (Path.fromList [Cli.defaultPatchNameSegment])))
        (ppe, diff) <- diffHelper original (Branch.head patched)
        Cli.respondNumbered (ShowDiffAfterMergePropagate dest0 dest patchPath ppe diff)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch ::
  Text ->
  Patch ->
  Path.Absolute ->
  Cli Bool
propagatePatch inputDescription patch scopePath = do
  Cli.time "propagatePatch" do
    Cli.stepAt'
      (inputDescription <> " (applying patch)")
      (Path.unabsolute scopePath, Propagate.propagateAndApply patch)

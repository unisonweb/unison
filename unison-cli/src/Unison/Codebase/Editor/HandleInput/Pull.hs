-- | @pull@ input handler
module Unison.Codebase.Editor.HandleInput.Pull
  ( doPullRemoteBranch,
    loadShareLooseCodeIntoMemory,
    loadPropagateDiffDefaultPatch,
    mergeBranchAndPropagateDefaultPatch,
    propagatePatch,
    withEntitiesDownloadedProgressCallback,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens (snoc, (^.))
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import Data.These
import qualified System.Console.Regions as Console.Regions
import qualified U.Codebase.Sqlite.Project as Sqlite (Project)
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite (ProjectBranch)
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.ProjectUtils (loggeth)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Cli.Share.Projects as Share
import Unison.Cli.UnisonConfigUtils (resolveConfiguredUrl)
import Unison.Codebase (Preprocessing (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.Input
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Editor.Output.PushPull as PushPull
import qualified Unison.Codebase.Editor.Propagate as Propagate
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace (..), ReadShareLooseCode (..), ShareUserHandle (..))
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.Codebase.SyncMode as SyncMode
import qualified Unison.Codebase.Verbosity as Verbosity
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Share.API.Hash as Share
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Share.Sync as Share
import qualified Unison.Share.Sync.Types as Share
import Unison.Share.Types (codeserverBaseURL)
import qualified Unison.Sync.Common as Common
import qualified Unison.Sync.Types as Share

doPullRemoteBranch ::
  PullSourceTarget ->
  SyncMode.SyncMode ->
  PullMode ->
  Verbosity.Verbosity ->
  Text ->
  Cli ()
doPullRemoteBranch unresolvedSourceAndTarget syncMode pullMode verbosity description = do
  (source, target) <- resolveSourceAndTarget unresolvedSourceAndTarget
  remoteBranchObject <- loadRemoteNamespaceIntoMemory syncMode pullMode source
  when (Branch.isEmpty0 (Branch.head remoteBranchObject)) do
    Cli.respond (PulledEmptyBranch source)
  targetAbsolutePath <-
    case target of
      PullTargetLooseCode path -> Cli.resolvePath' path
      PullTargetProject (ProjectAndBranch project branch) ->
        pure $ ProjectUtils.projectBranchPath (ProjectAndBranch (project ^. #projectId) (branch ^. #branchId))
  let printDiffPath =
        if Verbosity.isSilent verbosity
          then Nothing
          else case target of
            PullTargetLooseCode path -> Just path
            PullTargetProject _ -> Nothing -- todo
  case pullMode of
    Input.PullWithHistory -> do
      targetBranchObject <- Cli.getBranch0At targetAbsolutePath
      if Branch.isEmpty0 targetBranchObject
        then do
          void $ Cli.updateAtM description targetAbsolutePath (const $ pure remoteBranchObject)
          Cli.respond $ MergeOverEmpty target
        else
          mergeBranchAndPropagateDefaultPatch
            Branch.RegularMerge
            description
            (Just (PullAlreadyUpToDate source target))
            remoteBranchObject
            printDiffPath
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
  PullSourceTarget ->
  Cli
    ( ReadRemoteNamespace Share.RemoteProjectBranch,
      PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
    )
resolveSourceAndTarget = \case
  Input.PullSourceTarget0 -> liftA2 (,) resolveImplicitSource resolveImplicitTarget
  Input.PullSourceTarget1 source -> liftA2 (,) (resolveExplicitSource source) resolveImplicitTarget
  Input.PullSourceTarget2 source target -> liftA2 (,) (resolveExplicitSource source) (resolveExplicitTarget target)

resolveImplicitSource :: Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveImplicitSource =
  ProjectUtils.getCurrentProjectBranch >>= \case
    Nothing -> RemoteRepo.writeNamespaceToRead <$> resolveConfiguredUrl PushPull.Pull Path.currentPath
    Just (ProjectAndBranch localProjectId localBranchId) -> do
      (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName) <-
        Cli.runEitherTransaction do
          Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId >>= \case
            Just (remoteProjectId, Just remoteBranchId) -> do
              remoteProjectName <- Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri
              remoteBranchName <- Queries.expectRemoteProjectBranchName Share.hardCodedUri remoteProjectId remoteBranchId
              pure (Right (remoteProjectId, remoteProjectName, remoteBranchId, remoteBranchName))
            _ -> pure (Left wundefined) -- no pull target
      remoteBranch <-
        ProjectUtils.expectRemoteProjectBranchById $
          ProjectAndBranch
            (remoteProjectId, remoteProjectName)
            (remoteBranchId, remoteBranchName)
      pure (ReadShare'ProjectBranch remoteBranch)

resolveExplicitSource ::
  ReadRemoteNamespace (These ProjectName ProjectBranchName) ->
  Cli (ReadRemoteNamespace Share.RemoteProjectBranch)
resolveExplicitSource = \case
  ReadRemoteNamespaceGit namespace -> pure (ReadRemoteNamespaceGit namespace)
  ReadShare'LooseCode namespace -> pure (ReadShare'LooseCode namespace)
  ReadShare'ProjectBranch projectAndBranchNames ->
    ReadShare'ProjectBranch <$> ProjectUtils.expectRemoteProjectBranchByTheseNames projectAndBranchNames

resolveImplicitTarget :: Cli (PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
resolveImplicitTarget =
  ProjectUtils.getCurrentProjectBranch >>= \case
    Nothing -> pure (PullTargetLooseCode Path.currentPath)
    Just (ProjectAndBranch projectId projectBranchId) ->
      Cli.runTransaction do
        project <- Queries.expectProject projectId
        branch <- Queries.expectProjectBranch projectId projectBranchId
        pure (PullTargetProject (ProjectAndBranch project branch))

resolveExplicitTarget ::
  PullTarget (These ProjectName ProjectBranchName) ->
  Cli (PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
resolveExplicitTarget = \case
  PullTargetLooseCode path -> pure (PullTargetLooseCode path)
  PullTargetProject projectAndBranchNames -> do
    projectAndBranch <- ProjectUtils.expectProjectAndBranchByTheseNames projectAndBranchNames
    pure (PullTargetProject projectAndBranch)

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
    ReadShare'ProjectBranch remoteBranch ->
      let repoInfo = Share.RepoInfo (into @Text (These (remoteBranch ^. #projectName) remoteProjectBranchName))
          causalHash = Common.hash32ToCausalHash . Share.hashJWTHash $ causalHashJwt
          causalHashJwt = remoteBranch ^. #branchHead
          remoteProjectBranchName = remoteBranch ^. #branchName
       in Cli.with withEntitiesDownloadedProgressCallback \downloadedCallback ->
            Share.downloadEntities Share.hardCodedBaseUrl repoInfo causalHashJwt downloadedCallback >>= \case
              Left err -> do
                loggeth ["Downloading failure: ", tShow err]
                Cli.returnEarlyWithoutOutput
              Right () -> liftIO (Codebase.expectBranchForHash codebase causalHash)

loadShareLooseCodeIntoMemory :: ReadShareLooseCode -> Cli (Branch IO)
loadShareLooseCodeIntoMemory rrn@(ReadShareLooseCode {server, repo, path}) = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  -- Auto-login to share if pulling from a non-public path
  when (not $ RemoteRepo.isPublic rrn) . void $ ensureAuthenticatedWithCodeserver codeserver
  let shareFlavoredPath = Share.Path (shareUserHandleToText repo Nel.:| coerce @[NameSegment] @[Text] (Path.toList path))
  Cli.Env {codebase} <- ask
  causalHash <-
    Cli.with withEntitiesDownloadedProgressCallback \downloadedCallback ->
      Share.pull baseURL shareFlavoredPath downloadedCallback & onLeftM \err0 ->
        (Cli.returnEarly . Output.ShareError) case err0 of
          Share.SyncError err -> Output.ShareErrorPull err
          Share.TransportError err -> Output.ShareErrorTransport err
  liftIO (Codebase.expectBranchForHash codebase causalHash)

-- Provide the given action a callback that display to the terminal.
withEntitiesDownloadedProgressCallback :: ((Int -> IO ()) -> IO a) -> IO a
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
      result <- action (\n -> atomically (modifyTVar' entitiesDownloadedVar (+ n)))
      entitiesDownloaded <- readTVarIO entitiesDownloadedVar
      Console.Regions.finishConsoleRegion region $
        "\n  Downloaded " <> tShow entitiesDownloaded <> " entities."
      pure result

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch ::
  Branch.MergeMode ->
  Text ->
  Maybe Output ->
  Branch IO ->
  Maybe Path.Path' ->
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
  Maybe Path.Path' ->
  Path.Absolute ->
  Cli ()
loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest = do
  Cli.time "loadPropagateDiffDefaultPatch" do
    original <- Cli.getBranch0At dest
    patch <- liftIO $ Branch.getPatch Cli.defaultPatchNameSegment original
    patchDidChange <- propagatePatch inputDescription patch dest
    when patchDidChange do
      whenJust maybeDest0 \dest0 -> do
        patched <- Cli.getBranchAt dest
        let patchPath = snoc dest0 Cli.defaultPatchNameSegment
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

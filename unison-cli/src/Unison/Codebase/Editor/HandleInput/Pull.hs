-- | @pull@ input handler
module Unison.Codebase.Editor.HandleInput.Pull
  ( doPullRemoteBranch,
    importRemoteShareBranch,
    loadPropagateDiffDefaultPatch,
    mergeBranchAndPropagateDefaultPatch,
    propagatePatch,
    withEntitiesDownloadedProgressCallback,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Lens (snoc, (&), (^.))
import Control.Monad.Reader (ask)
import qualified Data.List.NonEmpty as Nel
import Data.These (These)
import qualified System.Console.Regions as Console.Regions
import U.Codebase.Sqlite.DbId (RemoteProjectBranchId (..), RemoteProjectId (..))
import U.Codebase.Sqlite.Queries (RemoteProject (..), RemoteProjectBranch (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.ProjectUtils (expectBranchName, expectProjectName, getCurrentProjectBranch, loggeth, resolveNames, resolveRemoteNames)
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
import Unison.Codebase.Editor.Output.PushPull (PushPull (Pull))
import qualified Unison.Codebase.Editor.Propagate as Propagate
import Unison.Codebase.Editor.RemoteRepo
  ( ReadRemoteNamespace (..),
    ReadShareRemoteNamespace (..),
    ShareUserHandle (..),
    writePathToRead,
  )
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Path (Path' (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SyncMode as SyncMode
import qualified Unison.Codebase.Verbosity as Verbosity
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Share.Sync as Share
import qualified Unison.Share.Sync.Types as Share
import Unison.Share.Types (codeserverBaseURL)
import Unison.Sqlite (Transaction)
import qualified Unison.Sync.Types as Share

doPullRemoteBranch ::
  PullSourceTarget ->
  SyncMode.SyncMode ->
  PullMode ->
  Verbosity.Verbosity ->
  Text ->
  Cli ()
doPullRemoteBranch sourceTarget {- mayRepo target -} syncMode pullMode verbosity description = do
  Cli.Env {codebase} <- ask
  let preprocess = case pullMode of
        Input.PullWithHistory -> Unmodified
        Input.PullWithoutHistory -> Preprocessed $ pure . Branch.discardHistory
  ns :: ReadRemoteNamespace (ProjectAndBranch RemoteProjectId RemoteProjectBranchId) <-
    case sourceTarget of
      Input.PullSourceTarget0 ->
        getCurrentProjectBranch >>= \case
          Nothing -> wundefined
          Just (ProjectAndBranch projectId projectBranchId) ->
            let loadRemoteNames :: Transaction (Maybe (RemoteProjectId, RemoteProjectBranchId))
                loadRemoteNames = runMaybeT do
                  (remoteProjectId, mremoteBranchId) <- MaybeT (Queries.loadRemoteProjectBranch projectId projectBranchId)
                  remoteBranchId <- MaybeT (pure mremoteBranchId)
                  pure (remoteProjectId, remoteBranchId)
             in Cli.runTransaction loadRemoteNames >>= \case
                  Nothing -> do
                    loggeth ["No default pull target for this branch"]
                    Cli.returnEarlyWithoutOutput
                  Just (remoteProjectId, remoteProjectBranchId) -> do
                    pure (ReadRemoteProjectBranch (ProjectAndBranch remoteProjectId remoteProjectBranchId))
      Input.PullSourceTarget1 source -> case source of
        ReadRemoteProjectBranch projectAndBranchNames -> ReadRemoteProjectBranch <$> resolveRemoteNames projectAndBranchNames
        _ -> wundefined
      Input.PullSourceTarget2 source _target -> wundefined source
  remoteBranch <- case ns of
    ReadRemoteNamespaceGit repo ->
      Cli.ioE (Codebase.importRemoteBranch codebase repo syncMode preprocess) \err ->
        Cli.returnEarly (Output.GitError err)
    ReadRemoteNamespaceShare repo -> importRemoteShareBranch repo
    ReadRemoteProjectBranch (ProjectAndBranch remoteProjectId remoteProjectBranchId) -> wundefined
  let ns = wundefined
  when (Branch.isEmpty0 (Branch.head remoteBranch)) do
    Cli.respond (PulledEmptyBranch ns)
  target <- wundefined
  let unchangedMsg = PullAlreadyUpToDate ns target
  destAbs <-
    case target of
      PullTargetLooseCode path -> Cli.resolvePath' path
      PullTargetProject _ -> wundefined
  let printDiffPath =
        if Verbosity.isSilent verbosity
          then Nothing
          else case target of
            PullTargetLooseCode path -> Just path
            PullTargetProject _ -> wundefined
  case pullMode of
    Input.PullWithHistory -> do
      destBranch <- Cli.getBranch0At destAbs
      if Branch.isEmpty0 destBranch
        then do
          void $ Cli.updateAtM description destAbs (const $ pure remoteBranch)
          Cli.respond $ MergeOverEmpty target
        else
          mergeBranchAndPropagateDefaultPatch
            Branch.RegularMerge
            description
            (Just unchangedMsg)
            remoteBranch
            printDiffPath
            destAbs
    Input.PullWithoutHistory -> do
      didUpdate <-
        Cli.updateAtM
          description
          destAbs
          (\destBranch -> pure $ remoteBranch `Branch.consBranchSnapshot` destBranch)
      Cli.respond
        if didUpdate
          then PullSuccessful ns target
          else unchangedMsg

importRemoteShareBranch :: ReadShareRemoteNamespace -> Cli (Branch IO)
importRemoteShareBranch rrn@(ReadShareRemoteNamespace {server, repo, path}) = do
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

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
import Control.Lens (over, snoc, (^.))
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Except
import qualified Data.List.NonEmpty as Nel
import Data.These
import qualified System.Console.Regions as Console.Regions
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..), RemoteProjectBranchId (..), RemoteProjectId (..))
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
import Unison.Sqlite (Transaction)
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.Common as Common
import qualified Unison.Sync.Types as Share
import Witch (unsafeFrom)

data LoadInfoError
  = NoDefaultPullTarget
  | NoLocalProjectRows
  deriving stock (Show)

doPullRemoteBranch ::
  PullSourceTarget ->
  SyncMode.SyncMode ->
  PullMode ->
  Verbosity.Verbosity ->
  Text ->
  Cli ()
doPullRemoteBranch sourceAndTarget syncMode pullMode verbosity description = do
  Cli.Env {codebase} <- ask
  -- First, we resolve the source and target strings to identifiers and such
  (resolvedSource, resolvedTarget) <- resolveSourceAndTarget sourceAndTarget
  let preprocess = case pullMode of
        Input.PullWithHistory -> Unmodified
        Input.PullWithoutHistory -> Preprocessed $ pure . Branch.discardHistory
  remoteBranch <-
    case resolvedSource of
      ReadRemoteNamespaceGit repo ->
        Cli.ioE (Codebase.importRemoteBranch codebase repo syncMode preprocess) \err ->
          Cli.returnEarly (Output.GitError err)
      ReadShare'LooseCode repo -> importRemoteShareBranch repo
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
  let nsNamesOnly :: ReadRemoteNamespace (ProjectAndBranch ProjectName ProjectBranchName)
      nsNamesOnly =
        over
          #_ReadShare'ProjectBranch
          (\branch -> ProjectAndBranch (branch ^. #projectName) (branch ^. #branchName))
          resolvedSource
      targetNamesOnly :: PullTarget (ProjectAndBranch ProjectName ProjectBranchName)
      targetNamesOnly =
        over
          #_PullTargetProject
          (\(ProjectAndBranch (_, projectName) (_, branchName)) -> ProjectAndBranch projectName branchName)
          resolvedTarget
      targetIdsOnly :: PullTarget (ProjectAndBranch ProjectId ProjectBranchId)
      targetIdsOnly =
        over
          #_PullTargetProject
          (\(ProjectAndBranch (projectId, _) (branchId, _)) -> ProjectAndBranch projectId branchId)
          resolvedTarget
  when (Branch.isEmpty0 (Branch.head remoteBranch)) do
    Cli.respond (PulledEmptyBranch nsNamesOnly)
  let unchangedMsg = PullAlreadyUpToDate nsNamesOnly targetNamesOnly
  destAbs <-
    case targetIdsOnly of
      PullTargetLooseCode path -> Cli.resolvePath' path
      PullTargetProject pb -> pure $ ProjectUtils.projectBranchPath pb
  let printDiffPath =
        if Verbosity.isSilent verbosity
          then Nothing
          else case targetNamesOnly of
            PullTargetLooseCode path -> Just path
            PullTargetProject _ -> Nothing -- todo
  case pullMode of
    Input.PullWithHistory -> do
      destBranch <- Cli.getBranch0At destAbs
      if Branch.isEmpty0 destBranch
        then do
          void $ Cli.updateAtM description destAbs (const $ pure remoteBranch)
          Cli.respond $ MergeOverEmpty targetNamesOnly
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
          then PullSuccessful nsNamesOnly targetNamesOnly
          else unchangedMsg

type ResolvedSource =
  ReadRemoteNamespace Share.RemoteProjectBranch

type ResolvedTarget =
  PullTarget (ProjectAndBranch (ProjectId, ProjectName) (ProjectBranchId, ProjectBranchName))

resolveSourceAndTarget :: PullSourceTarget -> Cli (ResolvedSource, ResolvedTarget)
resolveSourceAndTarget = \case
  Input.PullSourceTarget0 -> resolveSourceAndTarget0
  Input.PullSourceTarget1 source -> resolveSourceAndTarget1 source
  Input.PullSourceTarget2 source _target -> wundefined source

-- Resolve source and target when neither are specified.
resolveSourceAndTarget0 :: Cli (ResolvedSource, ResolvedTarget)
resolveSourceAndTarget0 =
  ProjectUtils.getCurrentProjectBranch >>= \case
    Nothing -> do
      source <- RemoteRepo.writeNamespaceToRead <$> resolveConfiguredUrl PushPull.Pull Path.currentPath
      pure (source, PullTargetLooseCode Path.currentPath)
    Just (ProjectAndBranch projectId projectBranchId) ->
      let loadRemoteNames :: ExceptT LoadInfoError Transaction (RemoteProjectId, ProjectName, RemoteProjectBranchId, ProjectBranchName)
          loadRemoteNames = ExceptT $
            fmap (maybe (Left NoDefaultPullTarget) Right) $ runMaybeT do
              (remoteProjectId, mremoteBranchId) <- MaybeT (Queries.loadRemoteProjectBranch projectId Share.hardCodedUri projectBranchId)
              remoteBranchId <- MaybeT (pure mremoteBranchId)
              remoteProjectName <- lift $ Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri
              remoteProjectBranchName <- lift $ Queries.expectRemoteProjectBranchName Share.hardCodedUri remoteProjectId remoteBranchId
              pure (remoteProjectId, remoteProjectName, remoteBranchId, remoteProjectBranchName)
          loadLocalNames :: ExceptT LoadInfoError Transaction (ProjectName, ProjectBranchName)
          loadLocalNames = do
            ExceptT $
              Queries.loadProjectAndBranchNames projectId projectBranchId >>= \case
                Nothing -> pure (Left NoLocalProjectRows)
                Just x -> pure (Right x)
       in Cli.runTransaction (runExceptT ((,) <$> loadLocalNames <*> loadRemoteNames)) >>= \case
            Right
              ( (localProjectName, localProjectBranchName),
                (remoteProjectId, remoteProjectName, remoteProjectBranchId, remoteProjectBranchName)
                ) -> do
                branch <-
                  ProjectUtils.expectRemoteProjectBranchById $
                    ProjectAndBranch
                      (remoteProjectId, remoteProjectName)
                      (remoteProjectBranchId, remoteProjectBranchName)
                let pullTarget =
                      PullTargetProject $
                        ProjectAndBranch
                          (projectId, localProjectName)
                          (projectBranchId, localProjectBranchName)
                pure (ReadShare'ProjectBranch branch, pullTarget)
            Left err -> case err of
              NoDefaultPullTarget -> do
                loggeth ["No default pull target for this branch"]
                Cli.returnEarlyWithoutOutput
              NoLocalProjectRows -> do
                loggeth ["Corrupt DB: No projects rows for current project"]
                Cli.returnEarlyWithoutOutput

-- Resolve source and target when source is specified
resolveSourceAndTarget1 ::
  ReadRemoteNamespace (These ProjectName ProjectBranchName) ->
  Cli (ResolvedSource, ResolvedTarget)
resolveSourceAndTarget1 unresolvedSource = do
  source <-
    case unresolvedSource of
      ReadRemoteNamespaceGit namespace -> pure (ReadRemoteNamespaceGit namespace)
      ReadShare'LooseCode namespace -> pure (ReadShare'LooseCode namespace)
      ReadShare'ProjectBranch projectAndBranchNames ->
        ReadShare'ProjectBranch <$> resolveRemoteNames projectAndBranchNames

  target <-
    ProjectUtils.getCurrentProjectBranch >>= \case
      Nothing -> pure (PullTargetLooseCode Path.currentPath)
      Just (ProjectAndBranch projectId projectBranchId) ->
        Cli.runTransaction (Queries.loadProjectAndBranchNames projectId projectBranchId) >>= \case
          Nothing -> wundefined -- FIXME expect, not load
          Just (localProjectName, localProjectBranchName) ->
            pure $
              PullTargetProject
                ( ProjectAndBranch
                    (projectId, localProjectName)
                    (projectBranchId, localProjectBranchName)
                )

  pure (source, target)

importRemoteShareBranch :: ReadShareLooseCode -> Cli (Branch IO)
importRemoteShareBranch rrn@(ReadShareLooseCode {server, repo, path}) = do
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

resolveRemoteNames :: These ProjectName ProjectBranchName -> Cli Share.RemoteProjectBranch
resolveRemoteNames = \case
  This projectName -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    let remoteBranchName = unsafeFrom @Text "main"
    ProjectUtils.expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, projectName) remoteBranchName)
  That branchName -> do
    ProjectAndBranch localProjectId localBranchId <- ProjectUtils.expectCurrentProjectBranch
    Cli.runTransaction (Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId) >>= \case
      Just (remoteProjectId, _maybeProjectBranchId) -> do
        projectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
        ProjectUtils.expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, projectName) branchName)
      Nothing -> do
        loggeth ["no remote associated with this project"]
        Cli.returnEarlyWithoutOutput
  These projectName branchName -> do
    remoteProject <- ProjectUtils.expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    ProjectUtils.expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, projectName) branchName)

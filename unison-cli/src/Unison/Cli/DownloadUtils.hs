-- | Utility functions for downloading remote entities and storing them locally in SQLite.
--
-- These are shared by commands like `pull` and `clone`.
module Unison.Cli.DownloadUtils
  ( downloadProjectBranchFromShare,
    downloadLooseCodeFromShare,
    GitNamespaceHistoryTreatment (..),
    downloadLooseCodeFromGitRepo,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVar, readTVarIO)
import Data.List.NonEmpty (pattern (:|))
import System.Console.Regions qualified as Console.Regions
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Git qualified as Git
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadGitRemoteNamespace, ReadShareLooseCode, shareUserHandleToText)
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Type (GitError)
import Unison.Codebase.Type qualified as Codebase (viewRemoteBranch')
import Unison.Core.Project (ProjectAndBranch (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Share.API.Hash qualified as Share
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Sync qualified as Share
import Unison.Share.Sync.Types qualified as Share
import Unison.Share.Types (codeserverBaseURL)
import Unison.Symbol (Symbol)
import Unison.Sync.Common qualified as Sync.Common
import Unison.Sync.Types qualified as Share

-- | Download a project/branch from Share.
downloadProjectBranchFromShare ::
  HasCallStack =>
  Bool ->
  Share.RemoteProjectBranch ->
  Cli (Either Output.ShareError CausalHash)
downloadProjectBranchFromShare useSquashedIfAvailable branch =
  Cli.labelE \done -> do
    let remoteProjectBranchName = branch.branchName
    let repoInfo = Share.RepoInfo (into @Text (ProjectAndBranch branch.projectName remoteProjectBranchName))
    causalHashJwt <-
      if useSquashedIfAvailable
        then case branch.squashedBranchHead of
          Nothing -> done Output.ShareExpectedSquashedHead
          Just squashedHead -> pure squashedHead
        else pure branch.branchHead
    exists <- Cli.runTransaction (Queries.causalExistsByHash32 (Share.hashJWTHash causalHashJwt))
    when (not exists) do
      (result, numDownloaded) <-
        Cli.with withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
          result <- Share.downloadEntities Share.hardCodedBaseUrl repoInfo causalHashJwt downloadedCallback
          numDownloaded <- liftIO getNumDownloaded
          pure (result, numDownloaded)
      result & onLeft \err0 -> do
        done case err0 of
          Share.SyncError err -> Output.ShareErrorDownloadEntities err
          Share.TransportError err -> Output.ShareErrorTransport err
      Cli.respond (Output.DownloadedEntities numDownloaded)
    pure (Sync.Common.hash32ToCausalHash (Share.hashJWTHash causalHashJwt))

-- | Download loose code from Share.
downloadLooseCodeFromShare :: ReadShareLooseCode -> Cli (Either Output.ShareError CausalHash)
downloadLooseCodeFromShare namespace = do
  let codeserver = Codeserver.resolveCodeserver namespace.server
  let baseURL = codeserverBaseURL codeserver

  -- Auto-login to share if pulling from a non-public path
  when (not (RemoteRepo.isPublic namespace)) do
    _userInfo <- ensureAuthenticatedWithCodeserver codeserver
    pure ()

  let shareFlavoredPath =
        Share.Path $
          shareUserHandleToText namespace.repo
            :| map NameSegment.toUnescapedText (Path.toList namespace.path)

  Cli.labelE \done -> do
    (causalHash, numDownloaded) <-
      Cli.with withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
        causalHash <-
          Share.pull baseURL shareFlavoredPath downloadedCallback & onLeftM \err0 ->
            done case err0 of
              Share.SyncError err -> Output.ShareErrorPull err
              Share.TransportError err -> Output.ShareErrorTransport err
        numDownloaded <- liftIO getNumDownloaded
        pure (causalHash, numDownloaded)
    Cli.respond (Output.DownloadedEntities numDownloaded)
    pure causalHash

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

data GitNamespaceHistoryTreatment
  = -- | Don't touch the history
    GitNamespaceHistoryTreatment'LetAlone
  | -- | Throw away all history at all levels
    GitNamespaceHistoryTreatment'DiscardAllHistory

-- | Download loose code that's in a SQLite codebase in a Git repo.
downloadLooseCodeFromGitRepo ::
  MonadIO m =>
  Codebase IO Symbol Ann ->
  GitNamespaceHistoryTreatment ->
  ReadGitRemoteNamespace ->
  m (Either GitError CausalHash)
downloadLooseCodeFromGitRepo codebase historyTreatment namespace = liftIO do
  Codebase.viewRemoteBranch' codebase namespace Git.RequireExistingBranch \(branch0, cacheDir) -> do
    let branch =
          case historyTreatment of
            GitNamespaceHistoryTreatment'LetAlone -> branch0
            GitNamespaceHistoryTreatment'DiscardAllHistory -> Branch.discardHistory branch0

    Codebase.syncFromDirectory codebase cacheDir branch
    pure (Branch.headHash branch)

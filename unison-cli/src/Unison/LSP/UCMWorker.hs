module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names.Cache qualified as NamesCache
import Unison.Codebase.Path qualified as Path
import Unison.Debug qualified as Debug
import Unison.LSP.Completion
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.PrettyPrintEnvDecl
import Unison.Project.Util (projectContextFromPath, projectRootPathFromContext)
import Unison.Server.NameSearch (NameSearch)
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Sqlite qualified as Sqlite
import UnliftIO.STM

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker ::
  TVar PrettyPrintEnvDecl ->
  TVar NamesWithHistory ->
  TVar (NameSearch Sqlite.Transaction) ->
  STM (Branch IO) ->
  STM Path.Absolute ->
  Lsp ()
ucmWorker ppedVar parseNamesVar nameSearchCacheVar getLatestRoot getLatestPath = do
  Env {codebase, completionsVar} <- ask
  let loop :: (Branch IO, Path.Absolute) -> Lsp a
      loop (currentRoot, currentPath) = do
        Debug.debugM Debug.LSP "LSP path: " currentPath
        let projectContext = projectContextFromPath currentPath
        let projectRootPath = projectRootPathFromContext projectContext
        causalHash <- fmap Branch.headHash . liftIO $ Codebase.getBranchAtPath codebase projectRootPath
        NamesCache.BranchNames {branchNames = projectNames, branchPPED = pped} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
        let projectNamesWH = NamesWithHistory.fromCurrentNames projectNames
        hl <- liftIO $ Codebase.runTransaction codebase Codebase.hashLength
        atomically $ do
          writeTVar parseNamesVar projectNamesWH
          writeTVar ppedVar pped
          writeTVar nameSearchCacheVar (NameSearch.makeNameSearch hl projectNamesWH)
        -- Re-check everything with the new names and ppe
        VFS.markAllFilesDirty
        atomically do
          writeTVar completionsVar (namesToCompletionTree $ NamesWithHistory.currentNames projectNamesWH)
        latest <- atomically $ do
          latestRoot <- getLatestRoot
          latestPath <- getLatestPath
          guard $ (currentRoot /= latestRoot || currentPath /= latestPath)
          pure (latestRoot, latestPath)
        loop latest

  -- Bootstrap manually from codebase just in case we're in headless mode and don't get any
  -- updates from UCM
  rootBranch <- liftIO $ Codebase.getRootBranch codebase
  loop (rootBranch, Path.absoluteEmpty)

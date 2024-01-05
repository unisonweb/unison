module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Debug qualified as Debug
import Unison.LSP.Completion
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.Names (Names)
import Unison.PrettyPrintEnvDecl
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Server.NameSearch (NameSearch)
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Sqlite qualified as Sqlite
import UnliftIO.STM

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker ::
  TVar PrettyPrintEnvDecl ->
  TVar Names ->
  TVar (NameSearch Sqlite.Transaction) ->
  STM (Branch IO) ->
  STM Path.Absolute ->
  Lsp ()
ucmWorker ppedVar parseNamesVar nameSearchCacheVar getLatestRoot getLatestPath = do
  Env {codebase, completionsVar} <- ask
  let loop :: (Branch IO, Path.Absolute) -> Lsp a
      loop (currentRoot, currentPath) = do
        Debug.debugM Debug.LSP "LSP path: " currentPath
        let currentBranch0 = Branch.getAt0 (Path.unabsolute currentPath) (Branch.head currentRoot)
        let parseNames = Branch.toNames currentBranch0
        hl <- liftIO $ Codebase.runTransaction codebase Codebase.hashLength
        let pped = PPED.fromNamesDecl hl parseNames
        atomically $ do
          writeTVar parseNamesVar parseNames
          writeTVar ppedVar pped
          writeTVar nameSearchCacheVar (NameSearch.makeNameSearch hl parseNames)
        -- Re-check everything with the new names and ppe
        VFS.markAllFilesDirty
        atomically do
          writeTVar completionsVar (namesToCompletionTree parseNames)
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

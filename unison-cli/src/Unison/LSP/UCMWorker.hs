module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Path qualified as Path
import Unison.Debug qualified as Debug
import Unison.LSP.Completion
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.PrettyPrintEnvDecl
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Server.Backend qualified as Backend
import Unison.Server.NameSearch (NameSearch)
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Sqlite qualified as Sqlite
import UnliftIO.STM

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker ::
  TVar PrettyPrintEnvDecl ->
  TVar NamesWithHistory ->
  TVar Names ->
  TVar (NameSearch Sqlite.Transaction) ->
  STM (Branch IO) ->
  STM Path.Absolute ->
  Lsp ()
ucmWorker ppeVar parseNamesVar tdnrNamesVar nameSearchCacheVar getLatestRoot getLatestPath = do
  Env {codebase, completionsVar} <- ask
  let loop :: (Branch IO, Path.Absolute) -> Lsp a
      loop (currentRoot, currentPath) = do
        Debug.debugM Debug.LSP "LSP path: " currentPath
        let parseNames = Backend.getCurrentParseNames (Path.unabsolute currentPath) currentRoot
        let tdnrNames = Backend.tdnrNamesForBranch currentRoot (Path.unabsolute currentPath)
        hl <- liftIO $ Codebase.runTransaction codebase Codebase.hashLength
        let ppe = PPE.fromNamesDecl hl parseNames
        atomically $ do
          writeTVar parseNamesVar parseNames
          writeTVar tdnrNamesVar tdnrNames
          writeTVar ppeVar ppe
          writeTVar nameSearchCacheVar (NameSearch.makeNameSearch hl parseNames)
        -- Re-check everything with the new names and ppe
        VFS.markAllFilesDirty
        atomically do
          writeTVar completionsVar (namesToCompletionTree $ NamesWithHistory.currentNames parseNames)
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

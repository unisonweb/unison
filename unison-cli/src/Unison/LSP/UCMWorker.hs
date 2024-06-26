module Unison.LSP.UCMWorker where

import Control.Monad (guard)
import Control.Monad.State (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Functor (void)
import U.Codebase.HashTags
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Debug qualified as Debug
import Unison.LSP.Completion
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.Names (Names)
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Server.NameSearch (NameSearch)
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Sqlite qualified as Sqlite
import UnliftIO.STM

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker ::
  TMVar PrettyPrintEnvDecl ->
  TMVar Names ->
  TMVar (NameSearch Sqlite.Transaction) ->
  TMVar Path.Absolute ->
  STM CausalHash ->
  STM Path.Absolute ->
  Lsp ()
ucmWorker ppedVar currentNamesVar nameSearchCacheVar currentPathVar getLatestRoot getLatestPath = do
  Env {codebase, completionsVar} <- ask
  let loop :: (CausalHash, Path.Absolute) -> Lsp a
      loop (currentRoot, currentPath) = do
        Debug.debugM Debug.LSP "LSP path: " currentPath
        currentBranch0 <- fmap Branch.head . liftIO $ (Codebase.getBranchAtPath codebase currentPath)
        let currentNames = Branch.toNames currentBranch0
        hl <- liftIO $ Codebase.runTransaction codebase Codebase.hashLength
        let pped = PPED.makePPED (PPE.hqNamer hl currentNames) (PPE.suffixifyByHash currentNames)
        atomically $ do
          writeTMVar currentPathVar currentPath
          writeTMVar currentNamesVar currentNames
          writeTMVar ppedVar pped
          writeTMVar nameSearchCacheVar (NameSearch.makeNameSearch hl currentNames)
        -- Re-check everything with the new names and ppe
        VFS.markAllFilesDirty
        atomically do
          writeTMVar completionsVar (namesToCompletionTree currentNames)
        Debug.debugLogM Debug.LSP "LSP Initialized"
        latest <- atomically $ do
          latestRoot <- getLatestRoot
          latestPath <- getLatestPath
          guard $ (currentRoot /= latestRoot || currentPath /= latestPath)
          pure (latestRoot, latestPath)
        Debug.debugLogM Debug.LSP "LSP Change detected"
        loop latest
  (rootBranch, currentPath) <- atomically $ do
    rootBranch <- getLatestRoot
    currentPath <- getLatestPath
    pure (rootBranch, currentPath)
  loop (rootBranch, currentPath)
  where
    -- This is added in stm-2.5.1, remove this if we upgrade.
    writeTMVar :: TMVar a -> a -> STM ()
    writeTMVar var a =
      tryReadTMVar var >>= \case
        Nothing -> putTMVar var a
        Just _ -> void $ swapTMVar var a

module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Debug qualified as Debug
import Unison.LSP.Completion
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.Names (Names)
import Unison.Prelude
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
  TMVar ProjectPath ->
  STM ProjectPath ->
  Lsp ()
ucmWorker ppedVar currentNamesVar nameSearchCacheVar currentPathVar getLatestProjectPath = do
  Env {codebase, completionsVar} <- ask
  let loop :: ProjectPath -> Lsp a
      loop currentProjectPath = do
        currentBranch <- liftIO $ Codebase.expectProjectBranchRoot codebase (currentProjectPath ^. #branch)
        Debug.debugM Debug.LSP "LSP path: " currentProjectPath
        let currentBranch0 = Branch.head currentBranch
        let currentNames = Branch.toNames currentBranch0
        hl <- liftIO $ Codebase.runTransaction codebase Codebase.hashLength
        let pped = PPED.makePPED (PPE.hqNamer hl currentNames) (PPE.suffixifyByHash currentNames)
        atomically $ do
          writeTMVar currentPathVar currentProjectPath
          writeTMVar currentNamesVar currentNames
          writeTMVar ppedVar pped
          writeTMVar nameSearchCacheVar (NameSearch.makeNameSearch hl currentNames)
        -- Re-check everything with the new names and ppe
        VFS.markAllFilesDirty
        atomically do
          writeTMVar completionsVar (namesToCompletionTree currentNames)
        Debug.debugLogM Debug.LSP "LSP Initialized"
        latest <- atomically $ do
          latestPath <- getLatestProjectPath
          guard $ (currentProjectPath /= latestPath)
          pure latestPath
        Debug.debugLogM Debug.LSP "LSP Change detected"
        loop latest
  currentProjectPath <- atomically $ do
    currentProjectPath <- getLatestProjectPath
    pure currentProjectPath
  loop currentProjectPath
  where
    -- This is added in stm-2.5.1, remove this if we upgrade.
    writeTMVar :: TMVar a -> a -> STM ()
    writeTMVar var a =
      tryReadTMVar var >>= \case
        Nothing -> putTMVar var a
        Just _ -> void $ swapTMVar var a

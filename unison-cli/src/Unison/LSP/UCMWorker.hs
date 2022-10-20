module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Debug as Debug
import Unison.LSP.Completion
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.PrettyPrintEnvDecl
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import qualified Unison.Server.Backend as Backend
import UnliftIO.STM

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker ::
  TVar PrettyPrintEnvDecl ->
  TVar NamesWithHistory ->
  STM (Branch IO) ->
  STM Path.Absolute ->
  Lsp ()
ucmWorker ppeVar parseNamesVar getLatestRoot getLatestPath = do
  Env {codebase, completionsVar} <- ask
  let loop :: (Branch IO, Path.Absolute) -> Lsp a
      loop (currentRoot, currentPath) = do
        Debug.debugM Debug.LSP "LSP path: " currentPath
        let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) currentRoot
        hl <- liftIO $ Codebase.hashLength codebase
        let ppe = PPE.fromNamesDecl hl parseNames
        atomically $ do
          writeTVar parseNamesVar parseNames
          writeTVar ppeVar ppe
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

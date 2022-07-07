module Unison.LSP.UCMWorker where

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.List.NonEmpty as NE
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Path as Path
import Unison.LSP.Types
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import qualified Unison.Server.Backend as Backend

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker :: TQueue (Branch IO, Path.Absolute) -> Lsp ()
ucmWorker ucmStateChanges = do
  Env {ppeCache, parseNamesCache, codebase} <- ask
  let loop (currentBranch, currentPath) = do
        latestState@(latestRoot, latestPath) <- atomically $ do
          updates <- flushTQueue ucmStateChanges
          case updates of
            [] -> retry
            (u : us) -> pure $ NE.last (u NE.:| us)
          guard . not . null $ updates
          pure $ last updates
        if latestState /= (currentBranch, currentPath)
          then do
            let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute latestPath)) latestRoot
            atomically $ writeTVar parseNamesCache parseNames
            hl <- Codebase.hashLength codebase
            let ppe = PPE.fromNamesDecl hl parseNames
            atomically $ writeTVar ppeCache ppe
          else loop latestState

  -- Bootstrap manually from codebase just in case we're in headless mode and don't get any
  -- updates from UCM
  liftIO $ do
    rootBranch <- Codebase.getRootBranch codebase
    loop (rootBranch, Path.absoluteEmpty)

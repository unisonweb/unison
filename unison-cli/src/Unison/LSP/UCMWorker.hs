module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import U.Codebase.HashTags
import qualified Unison.Codebase as Codebase
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.PrettyPrintEnvDecl
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import UnliftIO.STM

-- | Watches for state changes in UCM and updates cached LSP state accordingly
ucmWorker ::
  TVar PrettyPrintEnvDecl ->
  TVar NamesWithHistory ->
  STM (BranchHash, Names) ->
  Lsp ()
ucmWorker ppeVar parseNamesVar getCurrentBranchNames = do
  Env {codebase} <- ask
  let loop :: Maybe (BranchHash, NamesWithHistory) -> Lsp a
      loop previous = do
        -- Re-check everything with the new names and ppe
        VFS.markAllFilesDirty
        latest@(_latestHash, latestNames) <- atomically $ do
          (hash, names) <- getCurrentBranchNames
          case previous of
            Nothing -> pure ()
            Just (currentHash, _) -> guard $ (currentHash /= hash)
          pure (hash, NamesWithHistory.fromCurrentNames names)
        hl <- liftIO $ Codebase.hashLength codebase
        let ppe = PPE.fromNamesDecl hl latestNames
        atomically $ do
          writeTVar parseNamesVar $! latestNames
          writeTVar ppeVar $! ppe
        loop (Just latest)
  loop Nothing

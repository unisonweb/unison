module Unison.LSP.UCMWorker where

import Control.Monad.Reader
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.LSP.Completion
import Unison.LSP.Types
import Unison.LSP.Util.Signal (Signal)
import Unison.LSP.Util.Signal qualified as Signal
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
  Signal PP.ProjectPathIds ->
  Lsp ()
ucmWorker ppedVar currentNamesVar nameSearchCacheVar currentPathVar changeSignal = do
  signalChanges <- Signal.subscribe changeSignal
  loop signalChanges Nothing
  where
    loop :: STM PP.ProjectPathIds -> Maybe (Branch.Branch IO) -> Lsp a
    loop signalChanges currentBranch = do
      Env {codebase, completionsVar} <- ask
      getChanges signalChanges currentBranch >>= \case
        (_newPP, Nothing) -> loop signalChanges currentBranch
        (newPP, Just newBranch) -> do
          let newBranch0 = Branch.head newBranch
          let newNames = Branch.toNames newBranch0
          hl <- liftIO $ Codebase.runTransaction codebase Codebase.hashLength
          let pped = PPED.makePPED (PPE.hqNamer hl newNames) (PPE.suffixifyByHash newNames)
          atomically $ do
            writeTMVar currentPathVar newPP
            writeTMVar currentNamesVar newNames
            writeTMVar ppedVar pped
            writeTMVar nameSearchCacheVar (NameSearch.makeNameSearch hl newNames)
          -- Re-check everything with the new names and ppe
          VFS.markAllFilesDirty
          atomically do
            writeTMVar completionsVar (namesToCompletionTree newNames)
          loop signalChanges (Just newBranch)
    -- Waits for a possible change, then checks if there's actually any difference to the branches we care about.
    -- If so, returns the new branch, otherwise Nothing.
    getChanges :: STM PP.ProjectPathIds -> Maybe (Branch.Branch IO) -> Lsp (ProjectPath, Maybe (Branch.Branch IO))
    getChanges signalChanges currentBranch = do
      Env {codebase} <- ask
      ppIds <- atomically signalChanges
      pp <- liftIO . Codebase.runTransaction codebase $ Codebase.resolveProjectPathIds ppIds
      atomically $ writeTMVar currentPathVar pp
      newBranch <- fmap (fromMaybe Branch.empty) . liftIO $ Codebase.getBranchAtProjectPath codebase pp
      pure $ (pp, if Just newBranch == currentBranch then Nothing else Just newBranch)
    -- This is added in stm-2.5.1, remove this if we upgrade.
    writeTMVar :: TMVar a -> a -> STM ()
    writeTMVar var a =
      tryReadTMVar var >>= \case
        Nothing -> putTMVar var a
        Just _ -> void $ swapTMVar var a

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Unison.LSP where

import Colog.Core (LogAction (LogAction))
import qualified Colog.Core as Colog
import Control.Monad.Reader
import Data.Aeson hiding (Options, defaultOptions)
import qualified Ki
import qualified Language.LSP.Logging as LSP
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.SMethodMap
import qualified Language.LSP.Types.SMethodMap as SMM
import Language.LSP.VFS
import qualified Network.Simple.TCP as TCP
import Network.Socket
import System.Environment (lookupEnv)
import Unison.Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import Unison.LSP.Orphans ()
import Unison.LSP.RequestHandlers
import Unison.LSP.Types
import Unison.LSP.UCMWorker (ucmWorker)
import qualified Unison.LSP.VFS as VFS
import Unison.Parser.Ann
import Unison.Prelude
import Unison.Symbol
import UnliftIO

getLspPort :: IO String
getLspPort = fromMaybe "5050" <$> lookupEnv "UNISON_LSP_PORT"

-- | Spawn an LSP server on the configured port.
spawnLsp :: Codebase IO Symbol Ann -> Runtime Symbol -> TQueue (Branch IO, Path.Absolute) -> IO ()
spawnLsp codebase runtime ucmStateChanges = do
  lspPort <- getLspPort
  putStrLn $ "  Language server listening at 127.0.0.1:" <> lspPort
  putStrLn $ "  You can view LSP setup instructions at https://github.com/unisonweb/unison/blob/trunk/docs/ability-typechecking.markdown"
  TCP.serve (TCP.Host "127.0.0.1") "5050" $ \(sock, _sockaddr) -> do
    sockHandle <- socketToHandle sock ReadWriteMode
    Ki.scoped \scope -> do
      -- currently we have an independent VFS for each LSP client since each client might have
      -- different un-saved state for the same file.
      initVFS $ \vfs -> do
        vfsVar <- newMVar vfs
        void $ runServerWithHandles lspServerLogger lspClientLogger sockHandle sockHandle (serverDefinition vfsVar codebase runtime scope ucmStateChanges)
  where
    -- Where to send logs that occur before a client connects
    lspServerLogger = Colog.filterBySeverity Colog.Error Colog.getSeverity $ Colog.cmap (fmap tShow) (LogAction print)
    -- Where to send logs that occur after a client connects
    lspClientLogger = Colog.cmap (fmap tShow) LSP.defaultClientLogger

serverDefinition ::
  MVar VFS ->
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Ki.Scope ->
  TQueue (Branch IO, Path.Absolute) ->
  ServerDefinition Config
serverDefinition vfsVar codebase runtime scope ucmStateChanges =
  ServerDefinition
    { defaultConfig = lspDefaultConfig,
      onConfigurationChange = lspOnConfigurationChange,
      doInitialize = lspDoInitialize vfsVar codebase runtime scope ucmStateChanges,
      staticHandlers = lspStaticHandlers,
      interpretHandler = lspInterpretHandler,
      options = lspOptions
    }

-- | Detect user LSP configuration changes.
lspOnConfigurationChange :: Config -> Value -> Either Text Config
lspOnConfigurationChange _ _ = pure Config

lspDefaultConfig :: Config
lspDefaultConfig = Config

-- | Initialize any context needed by the LSP server
lspDoInitialize ::
  MVar VFS ->
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Ki.Scope ->
  TQueue (Branch IO, Path.Absolute) ->
  LanguageContextEnv Config ->
  Message 'Initialize ->
  IO (Either ResponseError Env)
lspDoInitialize vfsVar codebase runtime scope ucmStateChanges context _initMsg = do
  checkedFilesVar <- newTVarIO mempty
  dirtyFilesVar <- newTVarIO mempty
  ppeCache <- newTVarIO mempty
  parseNamesCache <- newTVarIO mempty
  let env = Env {..}
  let lspToIO = flip runReaderT context . unLspT . flip runReaderT env . runLspM
  Ki.fork scope (lspToIO VFS.fileCheckingWorker)
  Ki.fork scope (lspToIO $ ucmWorker ucmStateChanges)
  pure $ Right $ env

-- | LSP request handlers that don't register/unregister dynamically
lspStaticHandlers :: Handlers Lsp
lspStaticHandlers =
  Handlers
    { reqHandlers = lspRequestHandlers,
      notHandlers = lspNotificationHandlers
    }

-- | LSP request handlers
lspRequestHandlers :: SMethodMap (ClientMessageHandler Lsp 'Request)
lspRequestHandlers =
  mempty
    & SMM.insert STextDocumentHover (ClientMessageHandler hoverHandler)

-- & SMM.insert STextDocumentCompletion (ClientMessageHandler completionHandler)
-- & SMM.insert SCodeLensResolve (ClientMessageHandler codeLensResolveHandler)

-- | LSP notification handlers
lspNotificationHandlers :: SMethodMap (ClientMessageHandler Lsp 'Notification)
lspNotificationHandlers =
  mempty
    & SMM.insert STextDocumentDidOpen (ClientMessageHandler VFS.lspOpenFile)
    & SMM.insert STextDocumentDidClose (ClientMessageHandler VFS.lspCloseFile)
    & SMM.insert STextDocumentDidChange (ClientMessageHandler VFS.lspChangeFile)

-- | A natural transformation into IO, required by the LSP lib.
lspInterpretHandler :: Env -> Lsp <~> IO
lspInterpretHandler env@(Env {context}) =
  Iso toIO fromIO
  where
    toIO (Lsp m) = flip runReaderT context . unLspT . flip runReaderT env $ m
    fromIO m = liftIO m

lspOptions :: Options
lspOptions = defaultOptions {textDocumentSync = Just $ textDocSyncOptions}
  where
    textDocSyncOptions =
      TextDocumentSyncOptions
        { -- Clients should send file open/close messages so the VFS can handle them
          _openClose = Just True,
          -- Clients should send file change messages so the VFS can handle them
          _change = Just TdSyncIncremental,
          -- Clients should tell us when files are saved
          _willSave = Just False,
          -- If we implement a pre-save hook we can enable this.
          _willSaveWaitUntil = Just False,
          -- If we implement a save hook we can enable this.
          _save = Just (InL False)
        }

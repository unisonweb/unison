{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Unison.LSP
  ( spawnLsp,
    LspFormattingConfig (..),
  )
where

import Colog.Core (LogAction (LogAction))
import Colog.Core qualified as Colog
import Compat (onWindows)
import Control.Monad.Reader
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Char (toLower)
import GHC.IO.Exception (ioe_errno)
import Ki qualified
import Language.LSP.Logging qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Utils.SMethodMap
import Language.LSP.Protocol.Utils.SMethodMap qualified as SMM
import Language.LSP.Server
import Language.LSP.VFS
import Network.Simple.TCP qualified as TCP
import System.Environment (lookupEnv)
import System.IO (hPutStrLn)
import Unison.Codebase
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime (Runtime)
import Unison.Debug qualified as Debug
import Unison.LSP.CancelRequest (cancelRequestHandler)
import Unison.LSP.CodeAction (codeActionHandler)
import Unison.LSP.CodeLens (codeLensHandler)
import Unison.LSP.Commands (executeCommandHandler, supportedCommands)
import Unison.LSP.Completion (completionHandler, completionItemResolveHandler)
import Unison.LSP.Configuration qualified as Config
import Unison.LSP.FileAnalysis qualified as Analysis
import Unison.LSP.FoldingRange (foldingRangeRequest)
import Unison.LSP.Formatting (formatDocRequest, formatRangeRequest)
import Unison.LSP.HandlerUtils qualified as Handlers
import Unison.LSP.Hover (hoverHandler)
import Unison.LSP.NotificationHandlers qualified as Notifications
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import Unison.LSP.UCMWorker (ucmWorker)
import Unison.LSP.Util.Signal (Signal)
import Unison.LSP.VFS qualified as VFS
import Unison.Parser.Ann
import Unison.Prelude
import Unison.Symbol
import UnliftIO
import UnliftIO.Foreign (Errno (..), eADDRINUSE)

data LspFormattingConfig = LspFormatEnabled | LspFormatDisabled
  deriving (Show, Eq)

getLspPort :: IO String
getLspPort = fromMaybe "5757" <$> lookupEnv "UNISON_LSP_PORT"

-- | Spawn an LSP server on the configured port.
spawnLsp ::
  LspFormattingConfig ->
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Signal PP.ProjectPathIds ->
  IO ()
spawnLsp lspFormattingConfig codebase runtime signal =
  ifEnabled . TCP.withSocketsDo $ do
    lspPort <- getLspPort
    UnliftIO.handleIO (handleFailure lspPort) $ do
      TCP.serve (TCP.Host "127.0.0.1") lspPort $ \(sock, _sockaddr) -> do
        Ki.scoped \scope -> do
          -- If the socket is closed, reading/writing will throw an exception,
          -- but since the socket is closed, this connection will be shutting down
          -- immediately anyways, so we just ignore it.
          let clientInput = handleAny (\_ -> pure "") do
                -- The server will be in the process of shutting down if the socket is closed,
                -- so just return empty input in the meantime.
                fromMaybe "" <$> TCP.recv sock defaultChunkSize
          let clientOutput output = handleAny (\_ -> pure ()) do
                TCP.sendLazy sock output

          -- currently we have an independent VFS for each LSP client since each client might have
          -- different un-saved state for the same file.
          do
            vfsVar <- newMVar emptyVFS
            void $ runServerWith lspServerLogger lspClientLogger clientInput clientOutput (serverDefinition lspFormattingConfig vfsVar codebase runtime scope signal)
  where
    handleFailure :: String -> IOException -> IO ()
    handleFailure lspPort ioerr =
      case Errno <$> ioe_errno ioerr of
        Just errNo
          | errNo == eADDRINUSE -> do
              putStrLn $ "Note: Port " <> lspPort <> " is already bound by another process or another UCM. The LSP server will not be started."
        _ -> do
          Debug.debugM Debug.LSP "LSP Exception" ioerr
          Debug.debugM Debug.LSP "LSP Errno" (ioe_errno ioerr)
          putStrLn "LSP server failed to start."
    -- Where to send logs that occur before a client connects
    lspServerLogger = Colog.filterBySeverity Colog.Error Colog.getSeverity $ Colog.cmap (fmap tShow) (LogAction print)
    -- Where to send logs that occur after a client connects
    lspClientLogger = Colog.cmap (fmap tShow) LSP.defaultClientLogger
    ifEnabled :: IO () -> IO ()
    ifEnabled runServer = do
      -- Default LSP to disabled on Windows unless explicitly enabled
      lookupEnv "UNISON_LSP_ENABLED" >>= \case
        Just (fmap toLower -> "false") -> pure ()
        Just (fmap toLower -> "true") -> runServer
        Just x -> hPutStrLn stderr $ "Invalid value for UNISON_LSP_ENABLED, expected 'true' or 'false' but found: " <> x
        Nothing -> when (not onWindows) runServer

serverDefinition ::
  LspFormattingConfig ->
  MVar VFS ->
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Ki.Scope ->
  Signal PP.ProjectPathIds ->
  ServerDefinition Config
serverDefinition lspFormattingConfig vfsVar codebase runtime scope signal =
  ServerDefinition
    { defaultConfig = defaultLSPConfig,
      configSection = "unison",
      parseConfig = Config.parseConfig,
      onConfigChange = Config.updateConfig,
      doInitialize = lspDoInitialize vfsVar codebase runtime scope signal,
      staticHandlers = lspStaticHandlers lspFormattingConfig,
      interpretHandler = lspInterpretHandler,
      options = lspOptions
    }

-- | Initialize any context needed by the LSP server
lspDoInitialize ::
  MVar VFS ->
  Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Ki.Scope ->
  Signal PP.ProjectPathIds ->
  LanguageContextEnv Config ->
  Msg.TMessage 'Msg.Method_Initialize ->
  IO (Either Msg.ResponseError Env)
lspDoInitialize vfsVar codebase runtime scope signal lspContext _initMsg = do
  checkedFilesVar <- newTVarIO mempty
  dirtyFilesVar <- newTVarIO mempty
  ppedCacheVar <- newEmptyTMVarIO
  currentNamesCacheVar <- newEmptyTMVarIO
  currentPathCacheVar <- newEmptyTMVarIO
  cancellationMapVar <- newTVarIO mempty
  completionsVar <- newEmptyTMVarIO
  nameSearchCacheVar <- newEmptyTMVarIO
  let env =
        Env
          { ppedCache = atomically $ readTMVar ppedCacheVar,
            currentNamesCache = atomically $ readTMVar currentNamesCacheVar,
            currentProjectPathCache = atomically $ readTMVar currentPathCacheVar,
            nameSearchCache = atomically $ readTMVar nameSearchCacheVar,
            ..
          }
  let lspToIO = flip runReaderT lspContext . unLspT . flip runReaderT env . runLspM
  Ki.fork scope (lspToIO Analysis.fileAnalysisWorker)
  Ki.fork scope (lspToIO $ ucmWorker ppedCacheVar currentNamesCacheVar nameSearchCacheVar currentPathCacheVar signal)
  pure $ Right $ env

-- | LSP request handlers that don't register/unregister dynamically
lspStaticHandlers :: LspFormattingConfig -> ClientCapabilities -> Handlers Lsp
lspStaticHandlers lspFormattingConfig _capabilities =
  Handlers
    { reqHandlers = lspRequestHandlers lspFormattingConfig,
      notHandlers = lspNotificationHandlers
    }

-- | LSP request handlers
lspRequestHandlers :: LspFormattingConfig -> SMethodMap (ClientMessageHandler Lsp 'Msg.Request)
lspRequestHandlers lspFormattingConfig =
  mempty
    & SMM.insert Msg.SMethod_TextDocumentHover (mkHandler hoverHandler)
    & SMM.insert Msg.SMethod_TextDocumentCodeAction (mkHandler codeActionHandler)
    & SMM.insert Msg.SMethod_TextDocumentCodeLens (mkHandler codeLensHandler)
    & SMM.insert Msg.SMethod_WorkspaceExecuteCommand (mkHandler executeCommandHandler)
    & SMM.insert Msg.SMethod_TextDocumentFoldingRange (mkHandler foldingRangeRequest)
    & SMM.insert Msg.SMethod_TextDocumentCompletion (mkHandler completionHandler)
    & SMM.insert Msg.SMethod_CompletionItemResolve (mkHandler completionItemResolveHandler)
    & addFormattingHandlers
  where
    addFormattingHandlers handlers =
      case lspFormattingConfig of
        LspFormatEnabled ->
          handlers
            & SMM.insert Msg.SMethod_TextDocumentFormatting (mkHandler formatDocRequest)
            & SMM.insert Msg.SMethod_TextDocumentRangeFormatting (mkHandler formatRangeRequest)
        LspFormatDisabled -> handlers
    defaultTimeout = 10_000 -- 10s
    mkHandler ::
      forall m.
      (Show (Msg.TRequestMessage m), Show (Msg.TResponseMessage m), Show (Msg.MessageResult m)) =>
      ( ( Msg.TRequestMessage m ->
          (Either Msg.ResponseError (Msg.MessageResult m) -> Lsp ()) ->
          Lsp ()
        ) ->
        ClientMessageHandler Lsp 'Msg.Request m
      )
    mkHandler h =
      h
        & Handlers.withCancellation (Just defaultTimeout)
        & Handlers.withDebugging
        & ClientMessageHandler

-- | LSP notification handlers
lspNotificationHandlers :: SMethodMap (ClientMessageHandler Lsp 'Msg.Notification)
lspNotificationHandlers =
  mempty
    & SMM.insert Msg.SMethod_TextDocumentDidOpen (ClientMessageHandler VFS.lspOpenFile)
    & SMM.insert Msg.SMethod_TextDocumentDidClose (ClientMessageHandler VFS.lspCloseFile)
    & SMM.insert Msg.SMethod_TextDocumentDidChange (ClientMessageHandler VFS.lspChangeFile)
    & SMM.insert Msg.SMethod_Initialized (ClientMessageHandler Notifications.initializedHandler)
    & SMM.insert Msg.SMethod_CancelRequest (ClientMessageHandler $ Notifications.withDebugging cancelRequestHandler)
    & SMM.insert Msg.SMethod_WorkspaceDidChangeConfiguration (ClientMessageHandler Config.workspaceConfigurationChanged)
    & SMM.insert Msg.SMethod_SetTrace (ClientMessageHandler Notifications.setTraceHandler)

-- | A natural transformation into IO, required by the LSP lib.
lspInterpretHandler :: Env -> Lsp <~> IO
lspInterpretHandler env@(Env {lspContext}) =
  Iso toIO fromIO
  where
    toIO :: forall a. Lsp a -> IO a
    toIO (Lsp m) = flip runReaderT lspContext . unLspT . flip runReaderT env $ m
    fromIO m = liftIO m

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just $ textDocSyncOptions,
      optExecuteCommandCommands = Just supportedCommands
    }
  where
    textDocSyncOptions =
      TextDocumentSyncOptions
        { -- Clients should send file open/close messages so the VFS can handle them
          _openClose = Just True,
          -- Clients should send file change messages so the VFS can handle them
          _change = Just TextDocumentSyncKind_Incremental,
          -- Clients should tell us when files are saved
          _willSave = Just False,
          -- If we implement a pre-save hook we can enable this.
          _willSaveWaitUntil = Just False,
          -- If we implement a save hook we can enable this.
          _save = Just (InL False)
        }

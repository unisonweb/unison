{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of unison LSP
--
-- Goals:
--
-- * Format on save
-- * Hover type-signature/definition
-- * Autocomplete
-- * Snippets for case-statements & handlers
--
-- Stretch goals:
-- * Jump to definition
-- * Directives/commands, e.g. :edit <>
-- * codelens for "add"
module Unison.LSP where

import Colog.Core (LogAction (LogAction))
import Control.Monad.Reader
import Data.Aeson hiding (Options, defaultOptions)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.SMethodMap
import qualified Language.LSP.Types.SMethodMap as SMM
import Language.LSP.VFS
import qualified Network.Simple.TCP as TCP
import Network.Socket
import Unison.Codebase
import Unison.Codebase.Runtime (Runtime)
import Unison.LSP.RequestHandlers
import Unison.LSP.Types
import Unison.LSP.VFS
import Unison.Parser.Ann
import Unison.Prelude
import Unison.Symbol
import UnliftIO

spawnLsp :: Codebase IO Symbol Ann -> Runtime Symbol -> IO ()
spawnLsp codebase runtime = do
  putStrLn "Booting up LSP"
  TCP.serve (TCP.Host "127.0.0.1") "5050" $ \(sock, _sockaddr) -> do
    sockHandle <- socketToHandle sock ReadWriteMode
    putStrLn "LSP Client connected."
    initVFS $ \vfs -> do
      vfsVar <- newMVar vfs
      void $ runServerWithHandles (LogAction print) (LogAction $ liftIO . print) sockHandle sockHandle (serverDefinition (liftIO . print) vfsVar codebase runtime)

serverDefinition :: (forall a. Show a => a -> Lsp ()) -> MVar VFS -> Codebase IO Symbol Ann -> Runtime Symbol -> ServerDefinition Config
serverDefinition logger vfsVar codebase runtime =
  ServerDefinition
    { defaultConfig = lspDefaultConfig,
      onConfigurationChange = lspOnConfigurationChange,
      doInitialize = lspDoInitialize vfsVar codebase runtime,
      staticHandlers = lspStaticHandlers logger,
      interpretHandler = lspInterpretHandler,
      options = lspOptions
    }

lspOnConfigurationChange :: Config -> Value -> Either Text Config
lspOnConfigurationChange _ _ = pure Config

lspDefaultConfig :: Config
lspDefaultConfig = Config

lspDoInitialize :: MVar VFS -> Codebase IO Symbol Ann -> Runtime Symbol -> LanguageContextEnv Config -> Message 'Initialize -> IO (Either ResponseError Env)
lspDoInitialize vfsVar codebase runtime context _ = pure $ Right $ Env {..}

lspStaticHandlers :: (forall a. Show a => a -> Lsp ()) -> Handlers Lsp
lspStaticHandlers logger =
  Handlers
    { reqHandlers = lspReqHandlers,
      notHandlers = lspNotHandlers logger
    }

lspReqHandlers :: SMethodMap (ClientMessageHandler Lsp 'Request)
lspReqHandlers =
  mempty
    & SMM.insert STextDocumentHover (ClientMessageHandler hoverHandler)
    & SMM.insert STextDocumentCompletion (ClientMessageHandler completionHandler)
    & SMM.insert SCodeLensResolve (ClientMessageHandler codeLensResolveHandler)

lspNotHandlers :: (forall a. Show a => a -> Lsp ()) -> SMethodMap (ClientMessageHandler Lsp 'Notification)
lspNotHandlers logger =
  mempty
    & SMM.insert STextDocumentDidOpen (ClientMessageHandler $ usingVFS . openVFS (LogAction $ lift . logger))
    & SMM.insert STextDocumentDidClose (ClientMessageHandler $ usingVFS . closeVFS (LogAction $ lift . logger))
    & SMM.insert STextDocumentDidChange (ClientMessageHandler $ usingVFS . changeFromClientVFS (LogAction $ lift . logger))

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
        { _openClose = Just True,
          _change = Just TdSyncIncremental, -- TdSyncFull
          _willSave = Just True,
          _willSaveWaitUntil = Just False,
          _save = Just (InL False)
        }

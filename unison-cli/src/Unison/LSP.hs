{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Unison.LSP where

import Colog.Core (LogAction (LogAction))
import Control.Monad.Reader
import Data.Aeson hiding (Options, defaultOptions)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.SMethodMap
import qualified Network.Simple.TCP as TCP
import Network.Socket
import System.IO (IOMode (ReadWriteMode))
import Unison.Prelude

newtype Lsp a = Lsp {runLspM :: ReaderT Env (LspM Config) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

data Env = Env
  { context :: LanguageContextEnv Config
  }

spawnLsp :: IO ()
spawnLsp = do
  putStrLn "Booting up LSP"
  TCP.serve (TCP.Host "127.0.0.1") "5050" $ \(sock, _sockaddr) -> do
    sockHandle <- socketToHandle sock ReadWriteMode
    putStrLn "LSP Client connected."
    void $ runServerWithHandles (LogAction print) (LogAction $ liftIO . print) sockHandle sockHandle serverDefinition

serverDefinition :: ServerDefinition Config
serverDefinition =
  ServerDefinition
    { defaultConfig = lspDefaultConfig,
      onConfigurationChange = lspOnConfigurationChange,
      doInitialize = lspDoInitialize,
      staticHandlers = lspStaticHandlers,
      interpretHandler = lspInterpretHandler,
      options = lspOptions
    }

data Config = Config

lspOnConfigurationChange :: Config -> Value -> Either Text Config
lspOnConfigurationChange _ _ = pure Config

lspDefaultConfig :: Config
lspDefaultConfig = Config

lspDoInitialize :: LanguageContextEnv Config -> Message 'Initialize -> IO (Either ResponseError Env)
lspDoInitialize ctx _ = pure $ Right $ Env ctx

lspStaticHandlers :: Handlers m
lspStaticHandlers =
  Handlers
    { reqHandlers = lspReqHandlers,
      notHandlers = lspNotHandlers
    }

lspReqHandlers :: SMethodMap v
lspReqHandlers = mempty

lspNotHandlers :: SMethodMap v
lspNotHandlers = mempty

lspInterpretHandler :: Env -> Lsp <~> IO
lspInterpretHandler env@(Env ctx) =
  Iso toIO fromIO
  where
    toIO (Lsp m) = flip runReaderT ctx . unLspT . flip runReaderT env $ m
    fromIO m = liftIO m

lspOptions :: Options
lspOptions = defaultOptions

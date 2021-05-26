{-# LANGUAGE TemplateHaskell #-}

module Unison.Server.AppState where

import Control.Concurrent (MVar, readMVar)
import Control.Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Servant.Server (Handler)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Parser (Ann)
import Unison.Prelude
import Unison.Server.Backend (Backend, BackendState (..))
import Unison.Server.Errors (errFromEither, backendError)

data AppState v =
  AppState {
    _authHandler :: Handler ()
  , _codebase :: Codebase IO v Ann
  , _rootBranch :: MVar (Branch IO)
  }

makeLenses ''AppState

type AppM v = ReaderT (AppState v) Handler

tryAuth :: AppM v ()
tryAuth = lift =<< view authHandler

getRootBranch :: AppM v (Branch IO)
getRootBranch = do
  mvar <- view rootBranch
  liftIO $ readMVar mvar

provideAppState
  :: Handler ()
  -> Codebase IO v Ann
  -> MVar (Branch IO)
  -> AppM v a
  -> Handler a
provideAppState authHandler codebase rootVar app =
  runReaderT app $ AppState authHandler codebase rootVar

doBackend :: Backend IO v a -> AppM v a
doBackend b = do
  root <- getRootBranch
  cb   <- view codebase
  let backendState = BackendState root cb
  ea <- liftIO $ runReaderT (runExceptT b) backendState
  errFromEither backendError ea

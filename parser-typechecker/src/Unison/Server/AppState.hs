{-# LANGUAGE TemplateHaskell #-}

module Unison.Server.AppState where

import Control.Concurrent (MVar, readMVar)
import Control.Lens
import Control.Monad.Reader (ReaderT)
import Servant.Server (Handler)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Parser (Ann)
import Unison.Prelude

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


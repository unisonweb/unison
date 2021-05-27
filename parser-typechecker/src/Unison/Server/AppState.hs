{-# LANGUAGE TemplateHaskell #-}

module Unison.Server.AppState where

import Control.Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Servant.Server (Handler)
import U.Util.Cache (Cache, semispaceCache)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Path (Path)
import Unison.Names2 (Names0)
import Unison.Parser (Ann)
import Unison.Prelude
import Unison.Server.Backend (Backend, BackendState (..))
import Unison.Server.Errors (backendError, errFromEither)

data AppState v =
  AppState {
    _authHandler :: Handler ()
  , _codebase :: Codebase IO v Ann
  , _branchCache :: Cache Branch.Hash (Branch IO)
  , _ppeCache :: Cache (Path, Branch.Hash, Int) (Branch IO)
  , _basicNamesCache :: Cache (Path, Branch.Hash) (Names0, Names0)
  }

makeLenses ''AppState

type AppM v = ReaderT (AppState v) Handler

tryAuth :: AppM v ()
tryAuth = lift =<< view authHandler

branchCacheSize, ppeCacheSize, namesCacheSize :: Word
branchCacheSize = 10
ppeCacheSize = 10
namesCacheSize = 10

provideAppState
  :: Handler ()
  -> Codebase IO v Ann
  -> AppM v a
  -> Handler a
provideAppState authHandler codebase app = do
  branches <- semispaceCache branchCacheSize
  ppes <- semispaceCache ppeCacheSize
  names <- semispaceCache namesCacheSize
  runReaderT app $ AppState authHandler codebase branches ppes names

doBackend :: Backend IO v a -> AppM v a
doBackend b = do
  cb <- view codebase
  bc <- view branchCache
  pc <- view ppeCache
  nc <- view basicNamesCache
  let backendState = BackendState cb bc pc nc
  ea <- liftIO $ runReaderT (runExceptT b) backendState
  errFromEither backendError ea

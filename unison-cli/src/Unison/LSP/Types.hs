module Unison.LSP.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Language.LSP.Server
import Language.LSP.VFS
import Unison.Codebase
import Unison.Codebase.Runtime (Runtime)
import Unison.Parser.Ann
import qualified Unison.Server.Backend as Backend
import Unison.Symbol
import UnliftIO

newtype Lsp a = Lsp {runLspM :: ReaderT Env (LspM Config) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env)

data Env = Env
  { context :: LanguageContextEnv Config,
    codebase :: Codebase IO Symbol Ann,
    vfsVar :: MVar VFS,
    runtime :: Runtime Symbol
  }

data Config = Config

lspBackend :: Backend.Backend IO a -> Lsp (Either Backend.BackendError a)
lspBackend = liftIO . runExceptT . flip runReaderT (Backend.BackendEnv False) . Backend.runBackend

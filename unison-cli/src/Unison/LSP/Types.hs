module Unison.LSP.Types where

import Colog.Core
import Control.Monad.Except
import Control.Monad.Reader
import qualified Language.LSP.Logging as LSP
import Language.LSP.Server
import Language.LSP.VFS
import Unison.Codebase
import Unison.Codebase.Runtime (Runtime)
import Unison.Parser.Ann
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.Symbol
import UnliftIO

newtype Lsp a = Lsp {runLspM :: ReaderT Env (LspM Config) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env, MonadLsp Config)

logInfo :: Text -> Lsp ()
logInfo msg = do
  let LogAction log = LSP.defaultClientLogger
  log (WithSeverity msg Info)

logError :: Text -> Lsp ()
logError msg = do
  let LogAction log = LSP.defaultClientLogger
  log (WithSeverity msg Error)

data Env = Env
  { context :: LanguageContextEnv Config,
    codebase :: Codebase IO Symbol Ann,
    vfsVar :: MVar VFS,
    runtime :: Runtime Symbol
  }

data Config = Config

lspBackend :: Backend.Backend IO a -> Lsp (Either Backend.BackendError a)
lspBackend = liftIO . runExceptT . flip runReaderT (Backend.BackendEnv False) . Backend.runBackend

module Unison.LSP.Types where

import Control.Monad.Reader
import Language.LSP.Server
import Language.LSP.VFS
import UnliftIO

newtype Lsp a = Lsp {runLspM :: ReaderT Env (LspM Config) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env)

data Env = Env
  { context :: LanguageContextEnv Config,
    vfs :: MVar VFS
  }

data Config = Config

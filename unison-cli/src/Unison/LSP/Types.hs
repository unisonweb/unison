module Unison.LSP.Types where

import Colog.Core
import Control.Monad.Except
import Control.Monad.Reader
import qualified Ki
import qualified Language.LSP.Logging as LSP
import Language.LSP.Server
import Language.LSP.Types (TextDocumentIdentifier)
import Language.LSP.VFS
import Unison.Codebase
import Unison.Codebase.Editor.Command (LexedSource)
import Unison.Codebase.Runtime (Runtime)
import Unison.LSP.Orphans ()
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.Parser.Ann
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Result (Note)
import qualified Unison.Server.Backend as Backend
import Unison.Symbol
import qualified Unison.UnisonFile as UF
import UnliftIO

-- | A custom LSP monad wrapper so we can provide our own environment.
newtype Lsp a = Lsp {runLspM :: ReaderT Env (LspM Config) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env, MonadLsp Config)

-- | Log an info message to the client's LSP log.
logInfo :: Text -> Lsp ()
logInfo msg = do
  let LogAction log = LSP.defaultClientLogger
  log (WithSeverity msg Info)

-- | Log an error message to the client's LSP log, this will be shown to the user in most LSP
-- implementations.
logError :: Text -> Lsp ()
logError msg = do
  let LogAction log = LSP.defaultClientLogger
  log (WithSeverity msg Error)

data Env = Env
  { context :: LanguageContextEnv Config,
    codebase :: Codebase IO Symbol Ann,
    parseNamesCache :: TVar NamesWithHistory,
    ppeCache :: TVar PrettyPrintEnvDecl,
    vfsVar :: MVar VFS,
    runtime :: Runtime Symbol,
    -- The information we have for each file, which may or may not have a valid parse or
    -- typecheck.
    checkedFilesVar :: TVar (Map TextDocumentIdentifier FileInfo),
    dirtyFilesVar :: TVar (Set TextDocumentIdentifier),
    scope :: Ki.Scope
  }

data FileInfo = FileInfo
  { lexedSource :: LexedSource,
    parsedFile :: Maybe (UF.UnisonFile Symbol Ann),
    typecheckedFile :: Maybe (UF.TypecheckedUnisonFile Symbol Ann),
    notes :: Seq (Note Symbol Ann)
  }
  deriving (Show)

data Config = Config

-- | Lift a backend computation into the Lsp monad.
lspBackend :: Backend.Backend IO a -> Lsp (Either Backend.BackendError a)
lspBackend = liftIO . runExceptT . flip runReaderT (Backend.BackendEnv False) . Backend.runBackend

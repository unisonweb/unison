-- | The main CLI monad.
--
-- TODO export list, docs
module Unison.Monad.Cli where

import Control.Monad.Reader (MonadReader (..), ReaderT (ReaderT))
import Control.Monad.Trans.Cont
import qualified Data.Configurator.Types as Configurator
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.Output (Output, NumberedOutput, NumberedArgs)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Parser as Parser
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified UnliftIO

data ReturnType a
  = Success a
  | HaltStep
  | HaltRepl

data Bailing
  = HaltingStep
  | HaltingRepl
  deriving stock (Show)
  deriving anyclass (Exception)

newtype Cli r a = Cli {unCli :: (a -> Env -> IO (ReturnType r)) -> Env -> IO (ReturnType r)}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env
    )
    via ContT (ReturnType r) (ReaderT Env IO)

data Env = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO Symbol Ann,
    config :: Configurator.Config,
    credentialManager :: CredentialManager,
    -- | Generate a unique name.
    generateUniqueName :: IO Parser.UniqueName,
    -- | How to load source code.
    loadSource :: SourceName -> IO LoadSourceResult,
    -- | What to do with output for the user.
    notify :: Output -> IO (),
    -- | What to do with numbered output for the user.
    notifyNumbered :: NumberedOutput -> IO NumberedArgs,
    runtime :: Runtime Symbol,
    sandboxedRuntime :: Runtime Symbol,
    serverBaseUrl :: Maybe Server.BaseUrl,
    ucmVersion :: UCMVersion
  }

type SourceName = Text

data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

-- | 'withCliToIO' generalized to accept other monads that we can turn
-- into 'Cli' (e.g. Action)
withCliToIO' :: forall r a. ((forall m x. (m x -> Cli x x) -> m x -> IO x) -> IO a) -> Cli r a
withCliToIO' run = Cli \k env -> do
  ea <- try $
    run $ \toCli someMonad ->
      let Cli ma = toCli someMonad
       in ma (\a _ -> pure (Success a)) env >>= \case
            HaltStep -> UnliftIO.throwIO HaltingStep
            HaltRepl -> UnliftIO.throwIO HaltingRepl
            Success a -> pure a
  case ea of
    Left HaltingStep -> pure HaltStep
    Left HaltingRepl -> pure HaltRepl
    Right a -> k a env

-- | Provide a way to run 'Cli' to IO. Note that this also delimits
-- the scope of and 'succeedWith' or 'with' calls.
withCliToIO :: ((forall x. Cli x x -> IO x) -> IO a) -> Cli r a
withCliToIO k = withCliToIO' \k' -> k (k' id)

short :: ReturnType r -> Cli r a
short r = Cli \_k _env -> pure r

-- | Short-circuit success. Returns 'r' to the nearest enclosing
-- 'scopeWith'
succeedWith :: r -> Cli r a
succeedWith = short . Success

-- | Short-circuit success
abortStep :: Cli r a
abortStep = short HaltStep

-- | Halt the repl
haltRepl :: Cli r a
haltRepl = short HaltRepl

-- | Wrap a continuation with 'Cli'. Provides a nicer syntax to
-- resource acquiring functions.
--
-- @
-- resource <- with (bracket acquire close)
-- @
--
-- Delimit the scope of acquired resources with 'scopeWith'.
with :: (forall x. (a -> IO x) -> IO x) -> Cli r a
with resourceK = Cli \k env -> resourceK (\resource -> k resource env)

-- | Delimit the scope of 'with' calls
scopeWith :: Cli x x -> Cli r x
scopeWith (Cli ma) = Cli \k env -> do
  ma (\x _ -> pure (Success x)) env >>= \case
    Success x -> k x env
    HaltStep -> pure HaltStep
    HaltRepl -> pure HaltRepl

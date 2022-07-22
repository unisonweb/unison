-- | The main CLI monad.
--
-- TODO export list, docs
module Unison.Monad.Cli where

import Control.Monad.Reader (MonadReader (..), ReaderT (ReaderT))
import Control.Monad.Trans.Cont
import qualified Data.Configurator.Types as Configurator
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output (NumberedArgs, NumberedOutput, Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Parser as Parser
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.UnisonFile as UF
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
    loopStateRef :: IORef LoopState,
    -- | What to do with output for the user.
    notify :: Output -> IO (),
    -- | What to do with numbered output for the user.
    notifyNumbered :: NumberedOutput -> IO NumberedArgs,
    runtime :: Runtime Symbol,
    sandboxedRuntime :: Runtime Symbol,
    serverBaseUrl :: Maybe Server.BaseUrl,
    ucmVersion :: UCMVersion
  }

data LoopState = LoopState
  { _root :: Branch IO,
    _lastSavedRoot :: Branch IO,
    -- the current position in the namespace
    _currentPathStack :: NonEmpty Path.Absolute,
    -- TBD
    -- , _activeEdits :: Set Branch.EditGuid

    -- The file name last modified, and whether to skip the next file
    -- change event for that path (we skip file changes if the file has
    -- just been modified programmatically)
    _latestFile :: Maybe (FilePath, Bool),
    _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile Symbol Ann),
    -- The previous user input. Used to request confirmation of
    -- questionable user commands.
    _lastInput :: Maybe Input,
    -- A 1-indexed list of strings that can be referenced by index at the
    -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
    -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
    _numberedArgs :: NumberedArgs
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

-- | Provide a way to run 'Cli' to IO. Note that provided run-in-IO function also delimits
-- the scope of 'succeedWith' and 'with' calls.
withCliToIO :: ((forall x. Cli x x -> IO x) -> IO a) -> Cli r a
withCliToIO k = withCliToIO' \k' -> k (k' id)

short :: ReturnType r -> Cli r a
short r = Cli \_k _env -> pure r

-- | Short-circuit success. Returns 'r' to the nearest enclosing
-- 'scopeWith'
succeedWith :: r -> Cli r a
succeedWith = short . Success

-- | Short-circuit the processing of this input.
returnEarly :: Cli r a
returnEarly = short HaltStep

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

respondNumbered :: NumberedOutput -> Cli r ()
respondNumbered output = do
  Env {loopStateRef, notifyNumbered} <- ask
  args <- liftIO (notifyNumbered output)
  unless (null args) do
    liftIO do
      atomicModifyIORef' loopStateRef \loopState ->
        let !numberedArgs = toList args
         in (loopState {_numberedArgs = numberedArgs}, ())

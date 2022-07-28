-- | The main CLI monad.
--
-- TODO export list, docs
module Unison.Monad.Cli where

import Control.Monad.Reader (MonadReader (..), ReaderT (ReaderT))
import Control.Monad.Trans.Cont
import qualified Data.Configurator.Types as Configurator
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output (NumberedArgs, NumberedOutput, Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Debug as Debug
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

-- | Run a @Cli@ action down to @IO@.
runCli :: Env -> Cli a a -> IO (ReturnType a)
runCli env (Cli action) =
  action (\x _ -> pure (Success x)) env

getLoopState :: Cli r LoopState
getLoopState = do
  Env {loopStateRef} <- ask
  liftIO (readIORef loopStateRef)

putLoopState :: LoopState -> Cli r ()
putLoopState newSt = do
  Env {loopStateRef} <- ask
  liftIO (writeIORef loopStateRef newSt)

modifyLoopState :: (LoopState -> LoopState) -> Cli r ()
modifyLoopState f = do
  Env {loopStateRef} <- ask
  liftIO (modifyIORef' loopStateRef f)

type SourceName = Text

data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

-- | Lift an action of type @IO (Either e a)@, given a continuation for @e@.
ioE :: IO (Either e a) -> (e -> Cli r a) -> Cli r a
ioE action errK =
  Cli \k env ->
    action >>= \case
      Left err -> unCli (errK err) k env
      Right value -> k value env

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
returnEarly :: Output -> Cli r a
returnEarly x = do
  respond x
  returnEarlyWithoutOutput

-- | Variant of 'returnEarly' that doesn't take a final output message.
returnEarlyWithoutOutput :: Cli r a
returnEarlyWithoutOutput =
  short HaltStep

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

-- | A variant of 'with' for the variant of bracketing function that may return a Left rather than call the provided
-- continuation.
withE :: (forall x. (a -> IO x) -> IO (Either e x)) -> (e -> Cli r a) -> Cli r a
withE resourceK errK =
  Cli \k env ->
    resourceK (\resource -> k resource env) >>= \case
      Left err -> unCli (errK err) k env
      Right val -> pure val

-- | Delimit the scope of 'with' calls
scopeWith :: Cli x x -> Cli r x
scopeWith (Cli ma) = Cli \k env -> do
  ma (\x _ -> pure (Success x)) env >>= \case
    Success x -> k x env
    HaltStep -> pure HaltStep
    HaltRepl -> pure HaltRepl

-- | Time an action.
time :: String -> Cli r ()
time label =
  if Debug.shouldDebug Debug.Timing
    then Cli \k env -> do
      systemStart <- getSystemTime
      cpuPicoStart <- getCPUTime
      r <- k () env
      cpuPicoEnd <- getCPUTime
      systemEnd <- getSystemTime
      let systemDiff =
            diffTimeToNanos
              (diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart))
      let cpuDiff = picosToNanos (cpuPicoEnd - cpuPicoStart)
      printf "%s: %s (cpu), %s (system)\n" label (renderNanos cpuDiff) (renderNanos systemDiff)
      pure r
    else pure ()
  where
    diffTimeToNanos :: DiffTime -> Double
    diffTimeToNanos =
      picosToNanos . diffTimeToPicoseconds

    picosToNanos :: Integer -> Double
    picosToNanos =
      (/ 1_000) . realToFrac

    -- Render nanoseconds, trying to fit into 4 characters.
    renderNanos :: Double -> String
    renderNanos ns
      | ns < 0.5 = "0 ns"
      | ns < 995 = printf "%.0f ns" ns
      | ns < 9_950 = printf "%.2f µs" us
      | ns < 99_500 = printf "%.1f µs" us
      | ns < 995_000 = printf "%.0f µs" us
      | ns < 9_950_000 = printf "%.2f ms" ms
      | ns < 99_500_000 = printf "%.1f ms" ms
      | ns < 995_000_000 = printf "%.0f ms" ms
      | ns < 9_950_000_000 = printf "%.2f s" s
      | ns < 99_500_000_000 = printf "%.1f s" s
      | otherwise = printf "%.0f s" s
      where
        us = ns / 1_000
        ms = ns / 1_000_000
        s = ns / 1_000_000_000

respond :: Output -> Cli r ()
respond output = do
  Env {notify} <- ask
  liftIO (notify output)

respondNumbered :: NumberedOutput -> Cli r ()
respondNumbered output = do
  Env {loopStateRef, notifyNumbered} <- ask
  args <- liftIO (notifyNumbered output)
  unless (null args) do
    liftIO do
      atomicModifyIORef' loopStateRef \loopState ->
        let !numberedArgs = toList args
         in (loopState {_numberedArgs = numberedArgs}, ())

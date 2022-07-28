{-# LANGUAGE DataKinds #-}

-- | The main CLI monad.
--
-- TODO export list, docs
module Unison.Cli.Monad
  ( -- * Cli monad
    Cli,
    ReturnType (..),
    runCli,

    -- * Envronment
    Env (..),

    -- * Mutable state
    LoopState (..),
    loopState0,
    getLoopState,
    putLoopState,
    modifyLoopState,

    -- * Lifting IO actions
    ioE,

    -- * Acquiring resources
    with,
    withE,

    -- * Scoping actions
    newBlock,

    -- * Short-circuiting
    returnWith,
    returnEarly,
    returnEarlyWithoutOutput,
    haltRepl,

    -- * Communicating output to the user
    respond,
    respondNumbered,

    -- * Debug-timing actions
    time,

    -- * Misc types
    LoadSourceResult (..),
  )
where

import Control.Lens (lens, set)
import Control.Monad.Reader (MonadReader (..), ReaderT (ReaderT))
import Control.Monad.Trans.Cont
import qualified Data.Configurator.Types as Configurator
import Data.Generics.Labels ()
import Data.IORef
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import GHC.OverloadedLabels (IsLabel (..))
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

data ReturnType a
  = Success a
  | HaltStep
  | HaltRepl

-- | The main command-line app monad.
--
-- * It is a reader monad: get the 'Env' environment with 'ask'.
--
-- * It is a state monad (kind of): get the 'LoopState' with 'getLoopState', and put it with 'putLoopState'. The state
-- is stored in an @IORef@, so state changes are visible across threads, and persist even when an exception is raised.
--
-- * It is a short-circuiting monad: like the IO monad (due to exceptions), a @Cli@ computation can short-circuit with
-- success or failure.
--
-- * It is a resource monad: resources can be acquired with straight-line syntax, and will be released at the end of
-- do-block.
--
-- * It is an IO monad: you can do IO things, but throwing synchronous exceptions is discouraged. Use the built-in
-- short-circuiting mechanism instead.
newtype Cli r a = Cli {unCli :: (a -> Env -> IO (ReturnType r)) -> Env -> IO (ReturnType r)}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env
    )
    via ContT (ReturnType r) (ReaderT Env IO)

-- | The command-line app monad environment.
data Env = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO Symbol Ann,
    config :: Configurator.Config,
    credentialManager :: CredentialManager,
    -- | Generate a unique name.
    generateUniqueName :: IO Parser.UniqueName,
    -- | How to load source code.
    loadSource :: Text -> IO LoadSourceResult,
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
  deriving stock (Generic)

-- | The command-line app monad mutable state.
--
-- There's an additional pseudo @"currentPath"@ field lens, for convenience.
data LoopState = LoopState
  { root :: Branch IO,
    lastSavedRoot :: Branch IO,
    -- the current position in the namespace
    currentPathStack :: List.NonEmpty Path.Absolute,
    -- TBD
    -- , _activeEdits :: Set Branch.EditGuid

    -- The file name last modified, and whether to skip the next file
    -- change event for that path (we skip file changes if the file has
    -- just been modified programmatically)
    latestFile :: Maybe (FilePath, Bool),
    latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile Symbol Ann),
    -- The previous user input. Used to request confirmation of
    -- questionable user commands.
    lastInput :: Maybe Input,
    -- A 1-indexed list of strings that can be referenced by index at the
    -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
    -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
    numberedArgs :: NumberedArgs
  }
  deriving stock (Generic)

instance
  {-# OVERLAPS #-}
  Functor f =>
  IsLabel "currentPath" ((Path.Absolute -> f Path.Absolute) -> (LoopState -> f LoopState))
  where
  fromLabel :: (Path.Absolute -> f Path.Absolute) -> (LoopState -> f LoopState)
  fromLabel =
    lens
      (\LoopState {currentPathStack} -> List.NonEmpty.head currentPathStack)
      ( \loopState@LoopState {currentPathStack = _ List.NonEmpty.:| paths} path ->
          loopState {currentPathStack = path List.NonEmpty.:| paths}
      )

-- | Create an initial loop state given a root branch and the current path.
loopState0 :: Branch IO -> Path.Absolute -> LoopState
loopState0 b p =
  LoopState
    { root = b,
      lastSavedRoot = b,
      currentPathStack = pure p,
      latestFile = Nothing,
      latestTypecheckedFile = Nothing,
      lastInput = Nothing,
      numberedArgs = []
    }

-- | Run a @Cli@ action down to @IO@.
runCli :: Env -> Cli a a -> IO (ReturnType a)
runCli env (Cli action) =
  action (\x _ -> pure (Success x)) env

-- | Get the loop state.
getLoopState :: Cli r LoopState
getLoopState = do
  Env {loopStateRef} <- ask
  liftIO (readIORef loopStateRef)

-- | Put the loop state.
putLoopState :: LoopState -> Cli r ()
putLoopState newSt = do
  Env {loopStateRef} <- ask
  liftIO (writeIORef loopStateRef newSt)

-- | Modify the loop state.
modifyLoopState :: (LoopState -> LoopState) -> Cli r ()
modifyLoopState f = do
  Env {loopStateRef} <- ask
  liftIO (modifyIORef' loopStateRef f)

-- | The result of calling 'loadSource'.
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

short :: ReturnType r -> Cli r a
short r = Cli \_k _env -> pure r

-- | Short-circuit with a value. Returns @r@ to the nearest enclosing
-- 'newBlock' (or 'runCli' if there is no enclosing 'newBlock').
returnWith :: r -> Cli r a
returnWith = short . Success

-- | Short-circuit the processing of the current input.
returnEarly :: Output -> Cli r a
returnEarly x = do
  respond x
  returnEarlyWithoutOutput

-- | Variant of 'returnEarly' that doesn't take a final output message.
returnEarlyWithoutOutput :: Cli r a
returnEarlyWithoutOutput =
  short HaltStep

-- | Stop processing inputs from the user.
haltRepl :: Cli r a
haltRepl = short HaltRepl

-- | Wrap a continuation with 'Cli'.
--
-- Useful for resource acquisition:
--
-- @
-- resource <- with (bracket acquire close)
-- @
--
-- Delimit the scope of acquired resources with 'newBlock'.
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

-- | Run the given action in a new block, which delimits the scope of any 'succeedWith', 'with'/'withE', and 'time'
-- calls contained within.
newBlock :: Cli x x -> Cli r x
newBlock (Cli ma) = Cli \k env -> do
  ma (\x _ -> pure (Success x)) env >>= \case
    Success x -> k x env
    HaltStep -> pure HaltStep
    HaltRepl -> pure HaltRepl

-- | Time an action.
--
-- Use 'newBlock' to scope what is timed. The following example times the execution of @foo2 >> foo3@
--
-- @
-- foo0
-- r <- newBlock do
--   foo1
--   time "barbaz"
--   foo2
--   foo3
-- foo4
-- @
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
         in (set #numberedArgs numberedArgs loopState, ())

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}

-- | The main CLI monad.
module Unison.Cli.Monad
  ( -- * Cli monad
    Cli,
    ReturnType (..),
    runCli,

    -- * Envronment
    Env (..),

    -- * Immutable state
    LoopState (..),
    loopState0,

    -- * Lifting IO actions
    ioE,

    -- * Acquiring resources
    with,
    with_,
    withE,

    -- * Short-circuiting
    label,
    returnEarly,
    returnEarlyWithoutOutput,
    haltRepl,

    -- * Communicating output to the user
    respond,
    respondNumbered,

    -- * Debug-timing actions
    time,

    -- * Running transactions
    runTransaction,
    runEitherTransaction,

    -- * Misc types
    LoadSourceResult (..),
  )
where

import Control.Exception (throwIO)
import Control.Lens (lens, (.=))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState)
import qualified Control.Monad.State.Strict as State
import qualified Data.Configurator.Types as Configurator
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Unique (Unique, newUnique)
import GHC.OverloadedLabels (IsLabel (..))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import U.Codebase.HashTags (CausalHash)
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output (NumberedArgs, NumberedOutput, Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Debug as Debug
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.CodebaseServer as Server
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Parser as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import UnliftIO.STM
import Unsafe.Coerce (unsafeCoerce)

-- | The main command-line app monad.
--
-- * It is a reader monad of 'Env'.
--
-- * It is a state monad of 'LoopState'.
--
-- * It is a short-circuiting monad: a @Cli@ computation can short-circuit with success or failure in a delimited scope.
--
-- * It is a resource monad: resources can be acquired in callback-style.
--
-- * It is an IO monad: you can do IO things, but throwing synchronous exceptions is discouraged. Use the built-in
-- short-circuiting mechanism instead.
newtype Cli a = Cli
  { unCli ::
      forall r.
      Env ->
      (a -> LoopState -> IO (ReturnType r, LoopState)) ->
      LoopState ->
      IO (ReturnType r, LoopState)
  }
  deriving stock (Functor)

instance Applicative Cli where
  pure x = Cli \_ k -> k x
  (<*>) = ap

instance Monad Cli where
  return = pure
  Cli mx >>= f =
    Cli \env k ->
      mx env \a -> unCli (f a) env k

instance MonadIO Cli where
  liftIO mx =
    Cli \_ k s -> do
      x <- mx
      k x s

instance MonadReader Env Cli where
  ask = Cli \env k -> k env
  local f m = Cli \env -> unCli m (f env)

instance MonadState LoopState Cli where
  get = Cli \_ k s -> k s s
  put s = Cli \_ k _ -> k () s

-- | What a Cli action returns: a value, an instruction to continue processing input, or an instruction to stop
-- processing input.
data ReturnType a
  = Success a
  | Continue
  | HaltRepl
  deriving stock (Eq, Show)

-- | The command-line app monad environment.
--
-- Get the environment with 'ask'.
data Env = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO Symbol Ann,
    config :: Configurator.Config,
    credentialManager :: CredentialManager,
    -- | Generate a unique name.
    generateUniqueName :: IO Parser.UniqueName,
    -- | How to load source code.
    loadSource :: Text -> IO LoadSourceResult,
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
  { root :: TMVar (Branch IO),
    lastSavedRootHash :: CausalHash,
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
    numberedArgs :: NumberedArgs,
    -- The result of the last run, along with a unison file that
    -- captures the state of dependencies when the last run occurred
    lastRunResult :: Maybe (Term Symbol Ann, Type Symbol Ann, UF.TypecheckedUnisonFile Symbol Ann)
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
loopState0 :: CausalHash -> TMVar (Branch IO) -> Path.Absolute -> LoopState
loopState0 lastSavedRootHash b p = do
  LoopState
    { root = b,
      lastSavedRootHash = lastSavedRootHash,
      currentPathStack = pure p,
      latestFile = Nothing,
      latestTypecheckedFile = Nothing,
      lastInput = Nothing,
      numberedArgs = [],
      lastRunResult = Nothing
    }

-- | Run a @Cli@ action down to @IO@.
runCli :: Env -> LoopState -> Cli a -> IO (ReturnType a, LoopState)
runCli env s0 (Cli action) =
  action env (\x s1 -> pure (Success x, s1)) s0

feed :: (a -> LoopState -> IO (ReturnType b, LoopState)) -> (ReturnType a, LoopState) -> IO (ReturnType b, LoopState)
feed k = \case
  (Success x, s) -> k x s
  (Continue, s) -> pure (Continue, s)
  (HaltRepl, s) -> pure (HaltRepl, s)

-- | The result of calling 'loadSource'.
data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

-- | Lift an action of type @IO (Either e a)@, given a continuation for @e@.
ioE :: IO (Either e a) -> (e -> Cli a) -> Cli a
ioE action errK =
  liftIO action >>= \case
    Left err -> errK err
    Right value -> pure value

short :: (forall r. ReturnType r) -> Cli a
short r = Cli \_env _k s -> pure (r, s)

-- | Short-circuit the processing of the current input.
returnEarly :: Output -> Cli a
returnEarly x = do
  respond x
  returnEarlyWithoutOutput

-- | Variant of 'returnEarly' that doesn't take a final output message.
returnEarlyWithoutOutput :: Cli a
returnEarlyWithoutOutput =
  short Continue

-- | Stop processing inputs from the user.
haltRepl :: Cli a
haltRepl = short HaltRepl

-- | Wrap a continuation with 'Cli'.
--
-- Useful for resource acquisition:
--
-- @
-- with (bracket create destroy) \\resource ->
--   ...
-- @
with :: (forall x. (a -> IO x) -> IO x) -> (a -> Cli b) -> Cli b
with resourceK action =
  Cli \env k s ->
    resourceK (runCli env s . action) >>= feed k

-- | A variant of 'with' for actions that don't acquire a resource (like 'Control.Exception.bracket_').
with_ :: (forall x. IO x -> IO x) -> Cli a -> Cli a
with_ resourceK action =
  Cli \env k s ->
    resourceK (runCli env s action) >>= feed k

-- | A variant of 'with' for the variant of bracketing function that may return a Left rather than call the provided
-- continuation.
withE :: (forall x. (a -> IO x) -> IO (Either e x)) -> (Either e a -> Cli b) -> Cli b
withE resourceK action =
  Cli \env k s ->
    resourceK (\a -> runCli env s (action (Right a))) >>= \case
      Left err -> runCli env s (action (Left err)) >>= feed k
      Right result -> feed k result

data X
  = forall a. X !Unique !LoopState a
  deriving anyclass (Exception)

instance Show X where
  show _ = "<internal exception type>"

-- | Create a label that can be jumped to.
--
-- @
-- x \<- label \\j0 -\> do
--   ...
--   label \\j1 -> do
--     ...
--     j0 someValue
--     ... -- We don't get here
--   ... -- We don't get here
-- -- x is bound to someValue
-- @
label :: forall a. ((forall void. a -> Cli void) -> Cli a) -> Cli a
label f =
  Cli \env k s0 -> do
    n <- newUnique
    let bail :: forall void. a -> Cli void
        bail a = do
          s1 <- State.get
          liftIO (throwIO (X n s1 a))
    try (runCli env s0 (f bail)) >>= \case
      Left err@(X m s1 a)
        | n == m -> k (unsafeCoerce a) s1
        | otherwise -> throwIO err
      Right a -> feed k a

-- | Time an action.
time :: String -> Cli a -> Cli a
time label action =
  if Debug.shouldDebug Debug.Timing
    then Cli \env k s -> do
      systemStart <- getSystemTime
      cpuPicoStart <- getCPUTime
      a <- unCli action env (\a loopState -> pure (Success a, loopState)) s
      cpuPicoEnd <- getCPUTime
      systemEnd <- getSystemTime
      let systemDiff =
            diffTimeToNanos
              (diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart))
      let cpuDiff = picosToNanos (cpuPicoEnd - cpuPicoStart)
      printf "%s: %s (cpu), %s (system)\n" label (renderNanos cpuDiff) (renderNanos systemDiff)
      feed k a
    else action
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

respond :: Output -> Cli ()
respond output = do
  Env {notify} <- ask
  liftIO (notify output)

respondNumbered :: NumberedOutput -> Cli ()
respondNumbered output = do
  Env {notifyNumbered} <- ask
  args <- liftIO (notifyNumbered output)
  unless (null args) do
    #numberedArgs .= args

runTransaction :: Sqlite.Transaction a -> Cli a
runTransaction action = do
  Env {codebase} <- ask
  liftIO (Codebase.runTransaction codebase action)

-- | Return early if a transaction returns Left.
runEitherTransaction :: Sqlite.Transaction (Either Output a) -> Cli a
runEitherTransaction action =
  runTransaction action & onLeftM returnEarly

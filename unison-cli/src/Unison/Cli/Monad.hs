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

    -- * Misc types
    LoadSourceResult (..),
    Label,
    R,
  )
where

import Control.Lens (lens, (.=))
import Control.Monad.Reader (MonadReader (..), ReaderT (ReaderT))
import Control.Monad.State.Strict (MonadState, StateT (StateT))
import Control.Monad.Trans.Cont
import qualified Data.Configurator.Types as Configurator
import Data.Generics.Labels ()
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
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Parser as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import UnliftIO.STM

-- | The main command-line app monad.
--
-- * It is a reader monad of 'Env'.
--
-- * It is a state monad of 'LoopState'.
--
-- * It is a short-circuiting monad: a @Cli@ computation can short-circuit with success or failure.
--
-- * It is a resource monad: resources can be acquired with straight-line syntax, and will be released at the end of
-- do-block.
--
-- * It is an IO monad: you can do IO things, but throwing synchronous exceptions is discouraged. Use the built-in
-- short-circuiting mechanism instead.
--
-- The @r@ type variable is boilerplate. It starts out as @r@, and grows an new 'R' layer for each scoped action (e.g.
-- 'with' / 'label'). For this reason, whenever you find yourself writing down an explicit type that has to carefully
-- mention how many 'R' layers are inferred, you may prefer to use an @_@ (with @PartialTypeSignatures@) instead, as the
-- precise type is not very meaningful useful to a reader.
newtype Cli r a = Cli
  { unCli ::
      Env ->
      (a -> LoopState -> IO (ReturnType r, LoopState)) ->
      LoopState ->
      IO (ReturnType r, LoopState)
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadState LoopState
    )
    via ReaderT Env (ContT (ReturnType r) (StateT LoopState IO))

-- | What a Cli action returns: a value, an instruction to continue processing input, or an instruction to stop
-- processing input.
data ReturnType a
  = Success a
  | Continue
  | HaltRepl

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
    lastSavedRoot :: TMVar (Branch IO),
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
loopState0 :: TMVar (Branch IO) -> Path.Absolute -> LoopState
loopState0 b p = do
  LoopState
    { root = b,
      lastSavedRoot = b,
      currentPathStack = pure p,
      latestFile = Nothing,
      latestTypecheckedFile = Nothing,
      lastInput = Nothing,
      numberedArgs = [],
      lastRunResult = Nothing
    }

-- | Run a @Cli@ action down to @IO@.
runCli :: Env -> LoopState -> Cli a a -> IO (ReturnType a, LoopState)
runCli = runCli' id

runCli' :: (a -> b) -> Env -> LoopState -> Cli b a -> IO (ReturnType b, LoopState)
runCli' f env s0 (Cli action) =
  action env (\x s1 -> pure (Success (f x), s1)) s0

feed :: (a -> LoopState -> IO (ReturnType b, LoopState)) -> (ReturnType a, LoopState) -> IO (ReturnType b, LoopState)
feed k = \case
  (Success x, s) -> k x s
  (Continue, s) -> pure (Continue, s)
  (HaltRepl, s) -> pure (HaltRepl, s)

feedE :: (a -> LoopState -> IO (ReturnType r, LoopState)) -> (ReturnType (R r a), LoopState) -> IO (ReturnType r, LoopState)
feedE k = feed \era s -> case era of
  GoL r -> pure (Success r, s)
  GoR a -> k a s

-- | The result of calling 'loadSource'.
data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

-- | Lift an action of type @IO (Either e a)@, given a continuation for @e@.
ioE :: IO (Either e a) -> (e -> Cli r a) -> Cli r a
ioE action errK =
  liftIO action >>= \case
    Left err -> errK err
    Right value -> pure value

short :: ReturnType r -> Cli r a
short r = Cli \_env _k s -> pure (r, s)

-- | Short-circuit the processing of the current input.
returnEarly :: Output -> Cli r a
returnEarly x = do
  respond x
  returnEarlyWithoutOutput

-- | Variant of 'returnEarly' that doesn't take a final output message.
returnEarlyWithoutOutput :: Cli r a
returnEarlyWithoutOutput =
  short Continue

-- | Stop processing inputs from the user.
haltRepl :: Cli r a
haltRepl = short HaltRepl

-- | Wrap a continuation with 'Cli'.
--
-- Useful for resource acquisition:
--
-- @
-- with (bracket create destroy) \\resource ->
--   ...
-- @
with :: (forall x. (a -> IO x) -> IO x) -> (a -> Cli (R r b) b) -> Cli r b
with resourceK action =
  Cli \env k s ->
    resourceK (runCli' GoR env s . action) >>= feedE k

-- | A variant of 'with' for actions that don't acquire a resource (like 'Control.Exception.bracket_').
with_ :: (forall x. IO x -> IO x) -> Cli (R r a) a -> Cli r a
with_ resourceK action =
  Cli \env k s ->
    resourceK (runCli' GoR env s action) >>= feedE k

-- | A variant of 'with' for the variant of bracketing function that may return a Left rather than call the provided
-- continuation.
withE :: (forall x. (a -> IO x) -> IO (Either e x)) -> (Either e a -> Cli (R r b) b) -> Cli r b
withE resourceK action =
  Cli \env k s ->
    resourceK (\a -> runCli' GoR env s (action (Right a))) >>= \case
      Left err -> runCli' GoR env s (action (Left err)) >>= feedE k
      Right result -> feedE k result

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
label :: forall r a. ((forall t b. Label (R r a) t => a -> Cli t b) -> Cli (R r a) a) -> Cli r a
label f = Cli \env k s0 -> do
  res <-
    unCli
      ( f
          ( \a -> Cli \_ _ s1 ->
              pure (Success (wrap @(R r a) (GoR a)), s1)
          )
      )
      env
      (\a s -> pure (Success (GoR a), s))
      s0
  feedE k res

-- | Time an action.
time :: String -> Cli r a -> Cli r a
time label action =
  if Debug.shouldDebug Debug.Timing
    then Cli \env k s -> do
      systemStart <- getSystemTime
      cpuPicoStart <- getCPUTime
      r <- unCli action env k s
      cpuPicoEnd <- getCPUTime
      systemEnd <- getSystemTime
      let systemDiff =
            diffTimeToNanos
              (diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart))
      let cpuDiff = picosToNanos (cpuPicoEnd - cpuPicoStart)
      printf "%s: %s (cpu), %s (system)\n" label (renderNanos cpuDiff) (renderNanos systemDiff)
      pure r
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

respond :: Output -> Cli r ()
respond output = do
  Env {notify} <- ask
  liftIO (notify output)

respondNumbered :: NumberedOutput -> Cli r ()
respondNumbered output = do
  Env {notifyNumbered} <- ask
  args <- liftIO (notifyNumbered output)
  unless (null args) do
    #numberedArgs .= args

class Label s t where
  wrap :: s -> t
  -- this default is only provided so that the haddocks don't show
  -- 'wrap'
  default wrap :: s ~ t => s -> t
  wrap = id

instance Label (R t x) (R t x)

instance {-# OVERLAPPABLE #-} Label s t => Label s (R t x) where
  wrap = GoL . wrap

data R a b
  = GoL a
  | GoR b

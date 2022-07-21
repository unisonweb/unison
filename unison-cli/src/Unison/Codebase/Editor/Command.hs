{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Editor.Command
  ( Command (..),
    Env (..),
    LoopState (..),
    AmbientAbilities,
    LexedSource,
    Source,
    SourceName,
    TypecheckingResult,
    LoadSourceResult (..),
    UseCache,
    EvalResult,
    commandName,
    lookupEvalResult,
    abort,
    quit,
    with,
    reset,
    root,
    numberedArgs,
    currentPathStack,
    lastInput,
    lastSavedRoot,
    latestFile,
    latestTypecheckedFile,
    currentPath,
    loopState0,
    respond,
    respondNumbered,
    askCodebase,
    askRuntime,
    askSandboxedRuntime,
    askServerBaseUrl,
    getConfig,
    InputDescription,
    Action (..),
    eval,
  )
where

import Control.Lens (Getter, makeLenses, to, view, (.=), _5)
-- TODO: Don't import backend, but move dependencies to own modules

import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..))
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import Unison.Result (Note, Result)
import qualified Unison.Server.CodebaseServer as Server
import Unison.Server.QueryResult (QueryResult)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free
import qualified Unison.WatchKind as WK

type AmbientAbilities v = [Type v Ann]

type SourceName = Text

type Source = Text

type LexedSource = (Text, [L.Token L.Lexeme])

data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

type TypecheckingResult v =
  Result
    (Seq (Note v Ann))
    (Either Names (UF.TypecheckedUnisonFile v Ann))

data Command a where
  AskEnv :: Command Env
  LocalEnv :: (Env -> Env) -> Free Command a -> Command a
  GetLoopState :: Command LoopState
  PutLoopState :: LoopState -> Command ()
  Eval :: IO a -> Command a
  HQNameQuery ::
    Maybe Path ->
    Branch IO ->
    [HQ.HashQualified Name] ->
    Command QueryResult
  NotifyNumbered :: NumberedOutput -> Command NumberedArgs
  LoadSource :: SourceName -> Command LoadSourceResult
  Typecheck ::
    AmbientAbilities Symbol ->
    NamesWithHistory ->
    SourceName ->
    LexedSource ->
    Command (TypecheckingResult Symbol)
  TypecheckFile ::
    UF.UnisonFile Symbol Ann ->
    [Type Symbol Ann] ->
    Command (TypecheckingResult Symbol)
  -- Evaluate all watched expressions in a UnisonFile and return
  -- their results, keyed by the name of the watch variable. The tuple returned
  -- has the form:
  --   (hash, (ann, sourceTerm, evaluatedTerm, isCacheHit))
  --
  -- where
  --   `hash` is the hash of the original watch expression definition
  --   `ann` gives the location of the watch expression
  --   `sourceTerm` is a closed term (no free vars) for the watch expression
  --   `evaluatedTerm` is the result of evaluating that `sourceTerm`
  --   `isCacheHit` is True if the result was computed by just looking up
  --   in a cache
  --
  -- It's expected that the user of this action might add the
  -- `(hash, evaluatedTerm)` mapping to a cache to make future evaluations
  -- of the same watches instantaneous.

  Evaluate ::
    Bool -> -- sandboxed
    PPE.PrettyPrintEnv ->
    UF.TypecheckedUnisonFile Symbol Ann ->
    Command (Either Runtime.Error (EvalResult Symbol))
  -- Evaluate a single closed definition
  Evaluate1 :: Bool -> PPE.PrettyPrintEnv -> UseCache -> Term Symbol Ann -> Command (Either Runtime.Error (Term Symbol Ann))
  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch IO -> Command ()
  -- IsDerivedTerm :: H.Hash -> Command m i v Bool
  -- IsDerivedType :: H.Hash -> Command m i v Bool

  WithRunInIO :: ((forall x. Action x -> IO x) -> IO a) -> Command a
  Abort :: Command a
  Quit :: Command a

instance MonadIO (Free Command) where
  liftIO io = Free.eval $ Eval io

type UseCache = Bool

type EvalResult v =
  ( [(v, Term v ())],
    Map v (Ann, WK.WatchKind, Reference.Id, Term v (), Term v (), Runtime.IsCacheHit)
  )

lookupEvalResult :: Ord v => v -> EvalResult v -> Maybe (Term v ())
lookupEvalResult v (_, m) = view _5 <$> Map.lookup v m

commandName :: Command a -> String
commandName = \case
  AskEnv -> "AskEnv"
  LocalEnv {} -> "LocalEnv"
  GetLoopState -> "GetLoopState"
  PutLoopState {} -> "PutLoopState"
  Abort -> "Abort"
  Quit -> "Quit"
  WithRunInIO {} -> "WithRunInIO"
  Eval {} -> "Eval"
  NotifyNumbered {} -> "NotifyNumbered"
  LoadSource {} -> "LoadSource"
  Typecheck {} -> "Typecheck"
  TypecheckFile {} -> "TypecheckFile"
  Evaluate {} -> "Evaluate"
  Evaluate1 {} -> "Evaluate1"
  SyncLocalRootBranch {} -> "SyncLocalRootBranch"
  HQNameQuery {} -> "HQNameQuery"

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
    _latestFile :: Maybe (FilePath, SkipNextUpdate),
    _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile Symbol Ann),
    -- The previous user input. Used to request confirmation of
    -- questionable user commands.
    _lastInput :: Maybe Input,
    -- A 1-indexed list of strings that can be referenced by index at the
    -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
    -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
    _numberedArgs :: NumberedArgs
  }

type SkipNextUpdate = Bool

data Env = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO Symbol Ann,
    config :: Configurator.Config,
    credentialManager :: CredentialManager,
    -- | What to do with output for the user.
    notify :: Output -> IO (),
    runtime :: Runtime Symbol,
    sandboxedRuntime :: Runtime Symbol,
    serverBaseUrl :: Maybe Server.BaseUrl,
    ucmVersion :: UCMVersion
  }

newtype Action a = Action {unAction :: Free Command a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )

instance MonadReader Env Action where
  ask = Action (Free.eval AskEnv)
  local a (Action b) = Action (Free.eval (LocalEnv a b))

instance MonadState LoopState Action where
  get = Action (Free.eval GetLoopState)
  put st = Action (Free.eval (PutLoopState st))

instance MonadUnliftIO Action where
  withRunInIO k = Action (Free.eval (WithRunInIO k))

abort :: Action a
abort = Action (Free.eval Abort)

quit :: Action a
quit = Action (Free.eval Quit)

eval :: Command a -> Action a
eval x = Action (Free.eval x)

-- | Acquire a resource for the duration of an action.
with :: (forall b. (a -> IO b) -> IO b) -> Action a
with _ = undefined

-- | Delimit the scope of inner 'with' actions.
reset :: Action a -> Action a
reset _ = undefined

makeLenses ''LoopState

-- replacing the old read/write scalar Lens with "peek" Getter for the NonEmpty
currentPath :: Getter LoopState Path.Absolute
currentPath = currentPathStack . to Nel.head

loopState0 :: Branch IO -> Path.Absolute -> LoopState
loopState0 b p =
  LoopState
    { _root = b,
      _lastSavedRoot = b,
      _currentPathStack = (pure p),
      _latestFile = Nothing,
      _latestTypecheckedFile = Nothing,
      _lastInput = Nothing,
      _numberedArgs = []
    }

respond :: Output -> Action ()
respond output = do
  Env {notify} <- ask
  liftIO (notify output)

respondNumbered :: NumberedOutput -> Action ()
respondNumbered output = do
  args <- Action (Free.eval $ NotifyNumbered output)
  unless (null args) $
    numberedArgs .= toList args

-- | Get the codebase out of the environment.
askCodebase :: Action (Codebase IO Symbol Ann)
askCodebase =
  asks codebase

-- | Get the runtime out of the environment.
askRuntime :: Action (Runtime Symbol)
askRuntime =
  asks runtime

-- | Get the sandboxed runtime out of the environment.
askSandboxedRuntime :: Action (Runtime Symbol)
askSandboxedRuntime =
  asks sandboxedRuntime

-- | Get the server base url out of the environment.
askServerBaseUrl :: Action (Maybe Server.BaseUrl)
askServerBaseUrl =
  asks serverBaseUrl

-- | Lookup a config value by key.
getConfig :: Configurator.Configured a => Text -> Action (Maybe a)
getConfig key = do
  cfg <- asks config
  liftIO (Configurator.lookup cfg key)

type InputDescription = Text

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
    InputDescription,
    Action (..),
    Action',
    eval,
  )
where

import Control.Lens (Getter, makeLenses, to, view, (.=), _5)
-- TODO: Don't import backend, but move dependencies to own modules

import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..))
import Data.Configurator.Types (Configured)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Event, Input)
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
import Unison.Server.QueryResult (QueryResult)
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

data
  Command
    i -- Input type
    v -- Type of variables in the codebase
    a -- Result of running the command
  where
  -- Escape hatch.
  AskEnv :: Command i v (Env v)
  LocalEnv :: (Env v -> Env v) -> Free (Command i v) a -> Command i v a
  GetLoopState :: Command i v (LoopState v)
  PutLoopState :: LoopState v -> Command i v ()
  Eval :: IO a -> Command i v a
  UI :: Command i v ()
  API :: Command i v ()
  HQNameQuery ::
    Maybe Path ->
    Branch IO ->
    [HQ.HashQualified Name] ->
    Command i v QueryResult
  ConfigLookup :: Configured a => Text -> Command i v (Maybe a)
  -- Presents some output to the user
  Notify :: Output v -> Command i v ()
  NotifyNumbered :: NumberedOutput v -> Command i v NumberedArgs
  LoadSource :: SourceName -> Command i v LoadSourceResult
  Typecheck ::
    AmbientAbilities v ->
    NamesWithHistory ->
    SourceName ->
    LexedSource ->
    Command i v (TypecheckingResult v)
  TypecheckFile ::
    UF.UnisonFile v Ann ->
    [Type v Ann] ->
    Command i v (TypecheckingResult v)
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
    UF.TypecheckedUnisonFile v Ann ->
    Command i v (Either Runtime.Error (EvalResult v))
  -- Evaluate a single closed definition
  Evaluate1 :: Bool -> PPE.PrettyPrintEnv -> UseCache -> Term v Ann -> Command i v (Either Runtime.Error (Term v Ann))
  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch IO -> Command i v ()
  -- IsDerivedTerm :: H.Hash -> Command m i v Bool
  -- IsDerivedType :: H.Hash -> Command m i v Bool

  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> [String] -> Command i v (Runtime.WatchResults v Ann)
  WithRunInIO :: ((forall x. Action i v x -> IO x) -> IO a) -> Command i v a
  Abort :: Command i v a
  Quit :: Command i v a

instance MonadIO (Free (Command i v)) where
  liftIO io = Free.eval $ Eval io

type UseCache = Bool

type EvalResult v =
  ( [(v, Term v ())],
    Map v (Ann, WK.WatchKind, Reference.Id, Term v (), Term v (), Runtime.IsCacheHit)
  )

lookupEvalResult :: Ord v => v -> EvalResult v -> Maybe (Term v ())
lookupEvalResult v (_, m) = view _5 <$> Map.lookup v m

commandName :: Command i v a -> String
commandName = \case
  AskEnv -> "AskEnv"
  LocalEnv {} -> "LocalEnv"
  GetLoopState -> "GetLoopState"
  PutLoopState {} -> "PutLoopState"
  Abort -> "Abort"
  Quit -> "Quit"
  WithRunInIO {} -> "WithRunInIO"
  Eval {} -> "Eval"
  API -> "API"
  UI -> "UI"
  ConfigLookup {} -> "ConfigLookup"
  Notify {} -> "Notify"
  NotifyNumbered {} -> "NotifyNumbered"
  LoadSource {} -> "LoadSource"
  Typecheck {} -> "Typecheck"
  TypecheckFile {} -> "TypecheckFile"
  Evaluate {} -> "Evaluate"
  Evaluate1 {} -> "Evaluate1"
  SyncLocalRootBranch {} -> "SyncLocalRootBranch"
  Execute {} -> "Execute"
  HQNameQuery {} -> "HQNameQuery"

data LoopState v = LoopState
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
    _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann),
    -- The previous user input. Used to request confirmation of
    -- questionable user commands.
    _lastInput :: Maybe Input,
    -- A 1-indexed list of strings that can be referenced by index at the
    -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
    -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
    _numberedArgs :: NumberedArgs
  }

type SkipNextUpdate = Bool

data Env v = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO v Ann,
    credentialManager :: CredentialManager,
    runtime :: Runtime v,
    sandboxedRuntime :: Runtime v,
    ucmVersion :: UCMVersion
  }

newtype Action i v a = Action {unAction :: Free (Command i v) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )

instance MonadReader (Env v) (Action i v) where
  ask = Action (Free.eval AskEnv)
  local a (Action b) = Action (Free.eval (LocalEnv a b))

instance MonadState (LoopState v) (Action i v) where
  get = Action (Free.eval GetLoopState)
  put st = Action (Free.eval (PutLoopState st))

instance MonadUnliftIO (Action i v) where
  withRunInIO k = Action (Free.eval (WithRunInIO k))

abort :: Action i v r
abort = Action (Free.eval Abort)

quit :: Action i v r
quit = Action (Free.eval Quit)

eval :: Command i v a -> Action i v a
eval x = Action (Free.eval x)

makeLenses ''LoopState

-- replacing the old read/write scalar Lens with "peek" Getter for the NonEmpty
currentPath :: Getter (LoopState v) Path.Absolute
currentPath = currentPathStack . to Nel.head

loopState0 :: Branch IO -> Path.Absolute -> LoopState v
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

respond :: Output v -> Action i v ()
respond output = Action (Free.eval $ Notify output)

respondNumbered :: NumberedOutput v -> Action i v ()
respondNumbered output = do
  args <- Action (Free.eval $ NotifyNumbered output)
  unless (null args) $
    numberedArgs .= toList args

-- | Get the codebase out of the environment.
askCodebase :: Action i v (Codebase IO v Ann)
askCodebase =
  asks codebase

-- | Get the runtime out of the environment.
askRuntime :: Action i v (Runtime v)
askRuntime =
  asks runtime

-- | Get the sandboxed runtime out of the environment.
askSandboxedRuntime :: Action i v (Runtime v)
askSandboxedRuntime =
  asks sandboxedRuntime

type InputDescription = Text

type Action' v = Action (Either Event Input) v

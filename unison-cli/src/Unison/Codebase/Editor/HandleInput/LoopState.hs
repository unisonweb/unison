{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Codebase.Editor.HandleInput.LoopState where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Configurator ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free

type F i v = Free (Command i v)

data Env v = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO v Ann,
    credentialManager :: CredentialManager,
    runtime :: Runtime v
  }

newtype Action i v a = Action {unAction :: MaybeT (ReaderT (Env v) (StateT (LoopState v) (F i v))) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadState (LoopState v), MonadReader (Env v))
  -- We should likely remove this MonadFail instance since it's really hard to debug,
  -- but it's currently in use.
  deriving newtype (MonadFail)

resetAndUnlift :: forall i v r. ((forall x. F i v x -> IO x) -> IO r) -> Action i v r
resetAndUnlift k = do
  RunInIO ru <- eval ResetAndUnlift
  eval (Eval (k ru))

abort :: Action i v r
abort = eval Abort

runAction :: Env v -> LoopState v -> Action i v a -> (F i v (Maybe a, LoopState v))
runAction env state (Action m) =
  m
    & runMaybeT
    & flip runReaderT env
    & flip runStateT state

liftF :: F i v a -> Action i v a
liftF = Action . lift . lift . lift

-- | A typeclass representing monads which can evaluate 'Command's.
class Monad m => MonadCommand m v i | m -> v i where
  eval :: Command v i a -> m a

instance MonadCommand (Free (Command i v)) i v where
  eval = Free.eval

instance MonadCommand m i v => MonadCommand (StateT s m) i v where
  eval = lift . eval

instance MonadCommand m i v => MonadCommand (MaybeT m) i v where
  eval = lift . eval

instance MonadCommand m i v => MonadCommand (ExceptT e m) i v where
  eval = lift . eval

instance MonadCommand m i v => MonadCommand (ReaderT r m) i v where
  eval = lift . eval

instance MonadCommand (Action i v) i v where
  eval = Action . eval

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

type Action' v = Action (Either Event Input) v

type SkipNextUpdate = Bool

type InputDescription = Text

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

respond :: MonadCommand m i v => Output v -> m ()
respond output = eval $ Notify output

respondNumbered :: NumberedOutput v -> Action i v ()
respondNumbered output = do
  args <- eval $ NotifyNumbered output
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

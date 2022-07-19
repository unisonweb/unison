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
import Unison.Codebase.Branch
  ( Branch (..),
  )
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

type F m i v = Free (Command m i v)

data Env m v = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase m v Ann,
    credentialManager :: CredentialManager,
    runtime :: Runtime v
  }

newtype Action m i v a = Action {unAction :: MaybeT (ReaderT (Env m v) (StateT (LoopState m v) (F m i v))) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadState (LoopState m v), MonadReader (Env m v))
  -- We should likely remove this MonadFail instance since it's really hard to debug,
  -- but it's currently in use.
  deriving newtype (MonadFail)

runAction :: Env m v -> LoopState m v -> Action m i v a -> (F m i v (Maybe a, LoopState m v))
runAction env state (Action m) =
  m
    & runMaybeT
    & flip runReaderT env
    & flip runStateT state

liftF :: F m i v a -> Action m i v a
liftF = Action . lift . lift . lift

-- | A typeclass representing monads which can evaluate 'Command's.
class Monad n => MonadCommand n m v i | n -> m v i where
  eval :: Command m v i a -> n a

instance MonadCommand (Free (Command m i v)) m i v where
  eval = Free.eval

instance MonadCommand n m i v => MonadCommand (StateT s n) m i v where
  eval = lift . eval

instance MonadCommand n m i v => MonadCommand (MaybeT n) m i v where
  eval = lift . eval

instance MonadCommand n m i v => MonadCommand (ExceptT e n) m i v where
  eval = lift . eval

instance MonadCommand n m i v => MonadCommand (ReaderT r n) m i v where
  eval = lift . eval

instance MonadCommand (Action m i v) m i v where
  eval = Action . eval

data LoopState m v = LoopState
  { _root :: Branch m,
    _lastSavedRoot :: Branch m,
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

type Action' m v = Action m (Either Event Input) v

type SkipNextUpdate = Bool

type InputDescription = Text

makeLenses ''LoopState

-- replacing the old read/write scalar Lens with "peek" Getter for the NonEmpty
currentPath :: Getter (LoopState m v) Path.Absolute
currentPath = currentPathStack . to Nel.head

loopState0 :: Branch m -> Path.Absolute -> LoopState m v
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

respond :: MonadCommand n m i v => Output v -> n ()
respond output = eval $ Notify output

respondNumbered :: NumberedOutput v -> Action m i v ()
respondNumbered output = do
  args <- eval $ NotifyNumbered output
  unless (null args) $
    numberedArgs .= toList args

-- | Get the codebase out of the environment.
askCodebase :: Action m i v (Codebase m v Ann)
askCodebase =
  asks codebase

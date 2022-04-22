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
import qualified Network.HTTP.Client as HTTP
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Codebase.Branch
  ( Branch (..),
    Branch0,
  )
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Path as Path
import Unison.Names (Names)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free
import UnliftIO

type F m i v = Free (Command m i v)

data Env = Env
  { authHTTPClient :: HTTP.Manager,
    credentialManager :: CredentialManager
  }

newtype Action m i v a = Action {unAction :: MaybeT (ReaderT Env (StateT (LoopState m v) (F m i v))) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadState (LoopState m v), MonadReader Env)
  -- We should likely remove this MonadFail instance since it's really hard to debug,
  -- but it's currently in use.
  deriving newtype (MonadFail)

runAction :: Env -> LoopState m v -> Action m i v a -> (F m i v (Maybe a, LoopState m v))
runAction env state (Action m) =
  m
    & runMaybeT
    & flip runReaderT env
    & flip runStateT state

liftF :: F m i v a -> Action m i v a
liftF = Action . lift . lift . lift

actionLiftM :: m a -> Action m i v a
actionLiftM = Action . eval . Eval

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
  { _root :: MVar (Branch m),
    _lastSavedRoot :: MVar (Branch m),
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

loadRoot :: MonadIO m => Action m i v (Branch m)
loadRoot = do
  rootVar <- use root
  liftIO . readMVar $ rootVar

loadRoot0 :: MonadIO m => Action m i v (Branch0 m)
loadRoot0 = do
  Branch.head <$> loadRoot

loadLastSavedRoot :: MonadIO m => Action m i v (Branch m)
loadLastSavedRoot = do
  rootVar <- use lastSavedRoot
  actionLiftM . readMVar $ rootVar

loadCurrentBranch :: MonadIO m => Action m i v (Branch m)
loadCurrentBranch = do
  path <- use currentPath
  rootBranch <- loadRoot
  pure $ Branch.getAt' (Path.unabsolute path) rootBranch

loadCurrentBranch0 :: MonadIO m => Action m i v (Branch0 m)
loadCurrentBranch0 = do
  Branch.head <$> loadCurrentBranch

-- replacing the old read/write scalar Lens with "peek" Getter for the NonEmpty
currentPath :: Getter (LoopState m v) Path.Absolute
currentPath = currentPathStack . to Nel.head

loopState0 :: MVar (Branch m) -> Path.Absolute -> LoopState m v
loopState0 b p = LoopState b b (pure p) Nothing Nothing Nothing []

getRootNames :: MonadIO m => Action' m v Names
getRootNames = do
  Branch.toNames <$> loadRoot0

respond :: MonadCommand n m i v => Output v -> n ()
respond output = eval $ Notify output

respondNumbered :: NumberedOutput v -> Action m i v ()
respondNumbered output = do
  args <- eval $ NotifyNumbered output
  unless (null args) $
    numberedArgs .= toList args

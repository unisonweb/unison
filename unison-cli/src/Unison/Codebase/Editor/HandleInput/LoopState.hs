{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Codebase.Editor.HandleInput.LoopState where

import Control.Lens
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState, StateT)
import Data.Configurator ()
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import Unison.Codebase.Branch
  ( Branch (..),
  )
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import qualified Unison.Codebase.Path as Path
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free

type UCMVersion = Text

data Env = Env
  { ucmVersion :: Text
  }

type F m i v = Free (Command m i v)

-- type Action m i v = MaybeT (ReaderT Env (StateT (LoopState m v) (F m i v)))
newtype Action m i v a = Action {runAction :: MaybeT (ReaderT Env (StateT (LoopState m v) (F m i v))) a}
  deriving newtype (Functor, Alternative, Applicative, Monad, MonadIO, MonadState (LoopState m v), MonadReader Env)
  -- This instance is used, but questionable, we should revisit at some point.
  deriving newtype (MonadFail)

liftF :: F m i v a -> Action m i v a
liftF m = Action (lift . lift . lift $ m)

-- | A typeclass representing monads which can evaluate 'Command's.
class Monad n => MonadCommand n m i v | n -> m i v where
  eval :: Command m i v a -> n a

instance MonadCommand (Action m i v) m i v where
  eval m = Action (eval m)

instance MonadCommand (Free (Command m i v)) m i v where
  eval = Free.eval

instance MonadCommand n m i v => MonadCommand (StateT s n) m i v where
  eval = lift . eval

instance MonadCommand n m i v => MonadCommand (MaybeT n) m i v where
  eval = lift . eval

instance MonadCommand n m i v => MonadCommand (ExceptT e n) m i v where
  eval = lift . eval

instance MonadCommand n m i v => MonadCommand (ReaderT e n) m i v where
  eval = lift . eval

type NumberedArgs = [String]

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
loopState0 b p = LoopState b b (pure p) Nothing Nothing Nothing []

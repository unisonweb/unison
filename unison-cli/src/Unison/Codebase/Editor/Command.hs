{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Editor.Command
  ( Env (..),
    AmbientAbilities,
    LexedSource,
    Source,
    SourceName,
    TypecheckingResult,
    UseCache,
    lookupEvalResult,
    root,
    numberedArgs,
    currentPathStack,
    lastInput,
    lastSavedRoot,
    latestFile,
    latestTypecheckedFile,
    currentPath,
    loopState0,
    getConfig,
    InputDescription,
  )
where

import Control.Lens (Getter, makeLenses, to, view, _5)
import Control.Monad.Reader (MonadReader (..), asks)
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Lexer as L
import Unison.Monad.Cli hiding (respondNumbered, runCli, with)
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Reference
import Unison.Result (Note, Result)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import qualified Unison.WatchKind as WK

type AmbientAbilities v = [Type v Ann]

type Source = Text

type LexedSource = (Text, [L.Token L.Lexeme])

type TypecheckingResult v =
  Result
    (Seq (Note v Ann))
    (Either Names (UF.TypecheckedUnisonFile v Ann))

type UseCache = Bool

type EvalResult v =
  ( [(v, Term v ())],
    Map v (Ann, WK.WatchKind, Reference.Id, Term v (), Term v (), Runtime.IsCacheHit)
  )

lookupEvalResult :: Ord v => v -> EvalResult v -> Maybe (Term v ())
lookupEvalResult v (_, m) = view _5 <$> Map.lookup v m

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

-- | Lookup a config value by key.
getConfig :: forall a m. (MonadReader Env m, Configurator.Configured a, MonadIO m) => Text -> m (Maybe a)
getConfig key = do
  cfg <- asks config
  liftIO (Configurator.lookup cfg key)

type InputDescription = Text

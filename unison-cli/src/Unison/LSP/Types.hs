{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.LSP.Types where

import Colog.Core hiding (Lens')
import Control.Comonad.Cofree (Cofree)
import qualified Control.Comonad.Cofree as Cofree
import Control.Lens hiding (List, (:<))
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Ki
import qualified Language.LSP.Logging as LSP
import Language.LSP.Server
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import Language.LSP.Types.Lens
import Language.LSP.VFS
import Unison.Codebase
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import Unison.LSP.Orphans ()
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.Parser.Ann
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Result (Note)
import qualified Unison.Server.Backend as Backend
import Unison.Symbol
import qualified Unison.Syntax.Lexer as Lexer
import qualified Unison.UnisonFile as UF
import UnliftIO

-- | A custom LSP monad wrapper so we can provide our own environment.
newtype Lsp a = Lsp {runLspM :: ReaderT Env (LspM Config) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader Env, MonadLsp Config)

-- | Log an info message to the client's LSP log.
logInfo :: Text -> Lsp ()
logInfo msg = do
  let LogAction log = LSP.defaultClientLogger
  log (WithSeverity msg Info)

-- | Log an error message to the client's LSP log, this will be shown to the user in most LSP
-- implementations.
logError :: Text -> Lsp ()
logError msg = do
  let LogAction log = LSP.defaultClientLogger
  log (WithSeverity msg Error)

-- | Environment for the Lsp monad.
data Env = Env
  { -- contains handlers for talking to the client.
    lspContext :: LanguageContextEnv Config,
    codebase :: Codebase IO Symbol Ann,
    parseNamesCache :: IO NamesWithHistory,
    ppeCache :: IO PrettyPrintEnvDecl,
    currentPathCache :: IO Path.Absolute,
    vfsVar :: MVar VFS,
    runtime :: Runtime Symbol,
    -- The information we have for each file, which may or may not have a valid parse or
    -- typecheck.
    checkedFilesVar :: TVar (Map Uri FileAnalysis),
    dirtyFilesVar :: TVar (Set Uri),
    -- A map  of request IDs to an action which kills that request.
    cancellationMapVar :: TVar (Map SomeLspId (IO ())),
    -- A lazily computed map of all valid completion suffixes from the current path.
    completionsVar :: TVar CompletionTree,
    scope :: Ki.Scope
  }

-- | A suffix tree over path segments of name completions.
-- see 'namesToCompletionTree' for more on how this is built and the invariants it should have.
newtype CompletionTree = CompletionTree
  { unCompletionTree :: Cofree (Map NameSegment) (Set (Name, LabeledDependency))
  }
  deriving (Show)

instance Semigroup CompletionTree where
  CompletionTree (a Cofree.:< subtreeA) <> CompletionTree (b Cofree.:< subtreeB) =
    CompletionTree (a <> b Cofree.:< Map.unionWith (\a b -> unCompletionTree $ CompletionTree a <> CompletionTree b) subtreeA subtreeB)

instance Monoid CompletionTree where
  mempty = CompletionTree $ mempty Cofree.:< mempty

-- | A monotonically increasing file version tracked by the lsp client.
type FileVersion = Int32

type LexedSource = (Text, [Lexer.Token Lexer.Lexeme])

data FileAnalysis = FileAnalysis
  { fileUri :: Uri,
    fileVersion :: FileVersion,
    lexedSource :: LexedSource,
    parsedFile :: Maybe (UF.UnisonFile Symbol Ann),
    typecheckedFile :: Maybe (UF.TypecheckedUnisonFile Symbol Ann),
    notes :: Seq (Note Symbol Ann),
    diagnostics :: IntervalMap Position [Diagnostic],
    codeActions :: IntervalMap Position [CodeAction]
  }
  deriving (Show)

getCurrentPath :: Lsp Path.Absolute
getCurrentPath = asks currentPathCache >>= liftIO

getCompletions :: Lsp CompletionTree
getCompletions = asks completionsVar >>= readTVarIO

globalPPE :: Lsp PrettyPrintEnvDecl
globalPPE = asks ppeCache >>= liftIO

getParseNames :: Lsp NamesWithHistory
getParseNames = asks parseNamesCache >>= liftIO

data Config = Config
  { -- 'Nothing' will load ALL available completions, which is slower, but may provide a better
    -- solution for some users.
    --
    -- 'Just n' will only fetch the first 'n' completions and will prompt the client to ask for
    -- more completions after more typing.
    maxCompletions :: Maybe Int
  }
  deriving stock (Show)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" \obj -> do
    maxCompletions <- obj Aeson..:! "maxCompletions" Aeson..!= maxCompletions defaultLSPConfig
    let invalidKeys = Set.fromList (map Aeson.Key.toText (Aeson.KeyMap.keys obj)) `Set.difference` validKeys
    when (not . null $ invalidKeys) do
      fail . Text.unpack $
        "Unrecognized configuration key(s): "
          <> Text.intercalate ", " (Set.toList invalidKeys)
          <> ".\nThe default configuration is:\n"
          <> Text.pack defaultConfigExample
    pure Config {..}
    where
      validKeys = Set.fromList ["maxCompletions"]
      defaultConfigExample =
        BSC.unpack $ Aeson.encode defaultLSPConfig

instance Aeson.ToJSON Config where
  toJSON (Config maxCompletions) =
    Aeson.object
      [ "maxCompletions" Aeson..= maxCompletions
      ]

defaultLSPConfig :: Config
defaultLSPConfig = Config {..}
  where
    maxCompletions = Just 100

-- | Lift a backend computation into the Lsp monad.
lspBackend :: Backend.Backend IO a -> Lsp (Either Backend.BackendError a)
lspBackend = liftIO . runExceptT . flip runReaderT (Backend.BackendEnv False) . Backend.runBackend

sendNotification :: forall (m :: Method 'FromServer 'Notification). (Message m ~ NotificationMessage m) => NotificationMessage m -> Lsp ()
sendNotification notif = do
  sendServerMessage <- asks (resSendMessage . lspContext)
  liftIO $ sendServerMessage $ FromServerMess (notif ^. method) (notif)

data RangedCodeAction = RangedCodeAction
  { -- All the ranges the code action applies
    _codeActionRanges :: [Range],
    _codeAction :: CodeAction
  }
  deriving stock (Eq, Show)

instance HasCodeAction RangedCodeAction CodeAction where
  codeAction = lens _codeAction (\rca ca -> rca {_codeAction = ca})

rangedCodeAction :: Text -> [Diagnostic] -> [Range] -> RangedCodeAction
rangedCodeAction title diags ranges =
  RangedCodeAction ranges $
    CodeAction
      { _title = title,
        _kind = Nothing,
        _diagnostics = Just . List $ diags,
        _isPreferred = Nothing,
        _disabled = Nothing,
        _edit = Nothing,
        _command = Nothing,
        _xdata = Nothing
      }

-- | Provided ranges must not intersect.
includeEdits :: Uri -> Text -> [Range] -> RangedCodeAction -> RangedCodeAction
includeEdits uri replacement ranges rca =
  let edits = do
        r <- ranges
        pure $ TextEdit r replacement
      workspaceEdit =
        WorkspaceEdit
          { _changes = Just $ HM.singleton uri (List edits),
            _documentChanges = Nothing,
            _changeAnnotations = Nothing
          }
   in rca & codeAction . edit ?~ workspaceEdit

getConfig :: Lsp Config
getConfig = LSP.getConfig

setConfig :: Config -> Lsp ()
setConfig = LSP.setConfig

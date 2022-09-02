{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.LSP.Types where

import Colog.Core hiding (Lens')
import Control.Lens hiding (List)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Ki
import qualified Language.LSP.Logging as LSP
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens
import Language.LSP.VFS
import Unison.Codebase
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import Unison.LSP.Orphans ()
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
    scope :: Ki.Scope
  }

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
    codeActions :: IntervalMap Position [CodeAction],
    testing :: Text
  }
  deriving (Show)

getCurrentPath :: Lsp Path.Absolute
getCurrentPath = asks currentPathCache >>= liftIO

globalPPE :: Lsp PrettyPrintEnvDecl
globalPPE = asks ppeCache >>= liftIO

getParseNames :: Lsp NamesWithHistory
getParseNames = asks parseNamesCache >>= liftIO

data Config = Config

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

-- -- | The existentials in the lsp-types ids are annoying to work with, so we use this instead.
-- type RequestId = Either Int32 Text

-- -- | The built-in "LspId m" type is annoying to work with due to existentials.
-- -- This type and classes simplify things.
-- class HasReqId p where
--   reqId :: Lens' p RequestId

-- -- instance {-# OVERLAPPING #-} HasReqId CancelParams where
-- --   reqId = lens getter setter
-- --     where
-- --       getter (CancelParams (IdInt n)) = Left (fromIntegral n)
-- --       getter (CancelParams (IdString s)) = Right s
-- --       setter _ (Left n) = CancelParams (IdInt (fromIntegral n))
-- --       setter _ (Right s) = CancelParams (IdString s)

-- instance {-# OVERLAPPABLE #-} HasId p (LspId m) => HasReqId p where
--   reqId = lens getter setter
--     where
--       getter p =
--         case p ^. LSP.id of
--           IdInt n -> Left n
--           IdString txt -> Right txt
--       setter p (Left n) = p & LSP.id .~ IdInt n
--       setter p (Right txt) = p & LSP.id .~ IdString txt

-- lspIdToReqId :: LspId m ->

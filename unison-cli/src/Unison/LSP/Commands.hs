{-# LANGUAGE DataKinds #-}

module Unison.LSP.Commands where

import Control.Lens hiding (List)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Editor.HandleCommand as Commands
import Unison.CommandLine.OutputMessages (watchPrinter)
import qualified Unison.Debug as Debug
import Unison.LSP.FileAnalysis (getEvaluatedFile, getFileAnalysis)
import Unison.LSP.Types
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.UnisonFile as UF
import Unison.Util.Monoid (intercalateMap)

data UCommand
  = RefreshWatches Uri
  deriving (Show, Eq, Ord)

renderCommand :: UCommand -> Command
renderCommand = \case
  RefreshWatches uri ->
    Command {_title = "Refresh", _command = "refresh-watches", _arguments = Just . List $ [toJSON uri]}

parseCommand :: Text -> [Value] -> Maybe UCommand
parseCommand cmd args = case (cmd, fromJSON <$> args) of
  ("refresh-watches", [Success uri]) -> Just $ RefreshWatches uri
  _ -> do
    Debug.debugM Debug.LSP "Unrecognized Command" (cmd, args)
    Nothing

executeCommandHandler :: RequestMessage 'WorkspaceExecuteCommand -> (Either ResponseError (ResponseResult 'WorkspaceExecuteCommand) -> Lsp ()) -> Lsp ()
executeCommandHandler m respond =
  respond =<< runExceptT do
    let cmd = m ^. params . command
    let args = m ^. params . arguments
    cmd <- case parseCommand cmd (maybe [] toList args) of
      Nothing -> throwError (ResponseError InvalidParams ("Invalid Command: " <> cmd <> " " <> intercalateMap " " tShow args) Nothing)
      Just c -> pure c
    lift $ runCommand cmd
    pure Null

runCommand :: UCommand -> Lsp ()
runCommand = \case
  RefreshWatches uri -> refreshWatches uri

refreshWatches :: Uri -> Lsp ()
refreshWatches fileUri = void . runMaybeT $ do
  Env {codebase, runtime, lexedSource} <- ask
  ppe <- PPE.suffixifiedPPE <$> globalPPE
  ef <- MaybeT $ getEvaluatedFile fileUri
  let (src, _lexed) = lexedSource

-- watchPrinter src ppe

-- FileAnalysis {typecheckedFile} <- MaybeT $ getFileAnalysis fileUri
-- for_ (UF.watchComponents tf) \(wk, watches) ->
--   for_ watches \(_v, trm, _typ) -> do
--     (liftIO $ Commands.eval1 codebase runtime ppe True trm) >>= \case
--       Left pr -> do
--         -- TODO handle error?
--         pure ()
--       Right te -> do
--         watchPrinter ppe (ABT.annotation trm) wk trm
-- _

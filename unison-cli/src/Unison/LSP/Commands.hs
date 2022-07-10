{-# LANGUAGE DataKinds #-}

module Unison.LSP.Commands where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson as Aeson
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.Codebase.Editor.HandleCommand as Commands
import qualified Unison.Debug as Debug
import Unison.LSP.Types
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE
import Unison.Util.Monoid (intercalateMap)

data UCommand
  = RefreshWatches
  deriving (Show, Eq, Ord)

renderCommand :: UCommand -> Command
renderCommand = \case
  RefreshWatches ->
    Command {_title = "Refresh", _command = "refresh-watches", _arguments = Nothing}

parseCommand :: Text -> [Value] -> Maybe UCommand
parseCommand cmd args = case cmd of
  "refresh-watches" -> Just RefreshWatches
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
  RefreshWatches -> refreshWatches

refreshWatches = do
  Env {codebase, runtime} <- ask
  ppe <- PPE.suffixifiedPPE <$> globalPPE
  liftIO $ Commands.eval1 codebase runtime ppe True trm

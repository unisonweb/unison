{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.Commands where

import Control.Lens hiding (List)
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Language.LSP.Server (sendRequest)
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.Debug as Debug
import Unison.LSP.Types
import Unison.Prelude

supportedCommands :: [Text]
supportedCommands = ["replaceText"]

replaceText ::
  --  | The text displayed to the user for this command if used in a CodeLens
  Text ->
  TextReplacement ->
  Command
replaceText title tr = Command title "replaceText" (Just (List [Aeson.toJSON tr]))

data TextReplacement = TextReplacement
  { range :: Range,
    -- Used in things like the editor's undo buffer
    description :: Text,
    replacementText :: Text,
    fileUri :: Uri
  }

instance Aeson.ToJSON TextReplacement where
  toJSON (TextReplacement range description replacementText fileUri) =
    Aeson.object
      [ "range" Aeson..= range,
        "description" Aeson..= description,
        "replacementText" Aeson..= replacementText,
        "fileUri" Aeson..= fileUri
      ]

instance Aeson.FromJSON TextReplacement where
  parseJSON = Aeson.withObject "TextReplacement" $ \o ->
    TextReplacement <$> o Aeson..: "range"
      <*> o Aeson..: "description"
      <*> o Aeson..: "replacementText"
      <*> o Aeson..: "fileUri"

-- | Computes code actions for a document.
executeCommandHandler :: RequestMessage 'WorkspaceExecuteCommand -> (Either ResponseError Aeson.Value -> Lsp ()) -> Lsp ()
executeCommandHandler m respond =
  respond =<< runExceptT do
    let cmd = m ^. params . command
    let args = m ^. params . arguments
    let invalidCmdErr = throwError $ ResponseError InvalidParams "Invalid command" Nothing
    case cmd of
      "replaceText" -> case args of
        Just (List [Aeson.fromJSON -> Aeson.Success (TextReplacement range description replacementText fileUri)]) -> do
          let params =
                ApplyWorkspaceEditParams
                  (Just description)
                  (WorkspaceEdit (Just ((HM.singleton fileUri (List [TextEdit range replacementText])))) Nothing Nothing)
          lift
            ( sendRequest SWorkspaceApplyEdit params $ \case
                Left err -> Debug.debugM Debug.LSP "Error applying workspace edit" err
                Right _ -> pure ()
            )
        -- let edit = WorkspaceEdit (Just $ HM.singleton fileUri (List [TextEdit range replacementText])) Nothing
        -- lift $ sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
        -- pure $ CodeLens range Nothing Nothing
        _ -> invalidCmdErr
      _ -> invalidCmdErr
    pure Aeson.Null

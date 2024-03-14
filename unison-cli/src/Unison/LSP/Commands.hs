{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Unison.LSP.Commands where

import Control.Lens hiding (List)
import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Server (sendRequest)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Debug qualified as Debug
import Unison.LSP.Types
import Unison.LSP.Types qualified as Lsp
import Unison.Prelude
import Unison.Symbol
import Unison.Syntax.Name qualified as Name
import Unison.Var qualified as Var

data UnisonLspCommand
  = ReplaceText
  | Add
  | Update
  deriving (Eq, Show, Ord, Enum, Bounded)

commandName :: UnisonLspCommand -> Text
commandName = \case
  ReplaceText -> "unison.replaceText"
  Add -> "unison.add"
  Update -> "unison.update"

fromCommandName :: Text -> Maybe UnisonLspCommand
fromCommandName = \case
  "unison.replaceText" -> Just ReplaceText
  "unison.add" -> Just Add
  "unison.update" -> Just Update
  _ -> Nothing

supportedCommands :: [Text]
supportedCommands =
  commandName
    <$> [ minBound :: UnisonLspCommand
          .. maxBound :: UnisonLspCommand
        ]

replaceText ::
  --  | The text displayed to the user for this command if used in a CodeLens
  Text ->
  TextReplacement ->
  Command
replaceText title tr = Command title "replaceText" (Just [Aeson.toJSON tr])

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
    TextReplacement
      <$> o
        Aeson..: "range"
      <*> o
        Aeson..: "description"
      <*> o
        Aeson..: "replacementText"
      <*> o
        Aeson..: "fileUri"

data AddOrUpdateArgs = AddOrUpdateParams {symbol :: Symbol}

instance Aeson.ToJSON AddOrUpdateArgs where
  toJSON (AddOrUpdateParams sym) =
    Aeson.object
      [ "symbol" Aeson..= Text.pack (Var.nameStr sym)
      ]

instance Aeson.FromJSON AddOrUpdateArgs where
  parseJSON = Aeson.withObject "AddOrUpdateArgs" $ \o -> do
    sym <- o Aeson..: "symbol"
    pure $ AddOrUpdateParams (Var.named sym)

addCommand :: Symbol -> Command
addCommand sym =
  let title = "Add Definition"
      command = commandName Add
   in Command title command (Just [Aeson.toJSON $ AddOrUpdateParams sym])

updateCommand :: Symbol -> Command
updateCommand sym =
  let title = "Update Definition"
      command = commandName Update
   in Command title command (Just [Aeson.toJSON $ AddOrUpdateParams sym])

-- | Computes code actions for a document.
executeCommandHandler :: Msg.TRequestMessage 'Msg.Method_WorkspaceExecuteCommand -> (Either Msg.ResponseError (Aeson.Value |? Null) -> Lsp ()) -> Lsp ()
executeCommandHandler m respond =
  respond =<< runExceptT do
    let cmd = m ^. params . command
    let args = m ^. params . arguments
    let invalidCmdErr = throwError $ Msg.ResponseError (InR ErrorCodes_InvalidParams) "Invalid command" Nothing
    case fromCommandName cmd of
      Just ReplaceText -> case args of
        Just [Aeson.fromJSON -> Aeson.Success (TextReplacement range description replacementText fileUri)] -> do
          let params =
                ApplyWorkspaceEditParams
                  (Just description)
                  (WorkspaceEdit (Just ((Map.singleton fileUri [TextEdit range replacementText]))) Nothing Nothing)
          void . lift $ do
            sendRequest Msg.SMethod_WorkspaceApplyEdit params $ \case
              Left err -> Debug.debugM Debug.LSP "Error applying workspace edit" err
              Right _ -> pure ()
          pure $ Right (InR Null)
        _ -> invalidCmdErr
      Just Add -> case args of
        Just [Aeson.fromJSON -> Aeson.Success (AddOrUpdateParams sym)] -> do
          case Name.parseVar sym of
            Just name -> do
              lift $ Lsp.sendUCMInput (Right $ Input.AddI (Set.singleton name))
              let params = ShowMessageParams MessageType_Warning "Warning! Hi"
              lift $ sendNotification Msg.SMethod_WindowShowMessage params
              pure $ Right (InR Null)
            Nothing -> invalidCmdErr
        _ -> invalidCmdErr
      Just Update -> case args of
        Just [Aeson.fromJSON -> Aeson.Success (AddOrUpdateParams sym)] -> do
          case Name.parseVar sym of
            Just _ -> do
              lift $ Lsp.sendUCMInput (Right $ Input.Update2I)
              let params = ShowMessageParams MessageType_Warning "Warning! Hi"
              lift $ sendNotification Msg.SMethod_WindowShowMessage params
              pure $ Right (InR Null)
            Nothing -> invalidCmdErr
        _ -> invalidCmdErr
      Nothing -> invalidCmdErr
    pure $ InL Aeson.Null

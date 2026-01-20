{-# LANGUAGE DataKinds #-}

module Unison.LSP.EditDefinition
  ( editDefinitionHandler,
  )
where

import Control.Lens hiding (List)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Unison.Debug qualified as Debug
import Unison.LSP.FileAnalysis (ppedForFile)
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LSP.Util.Wrappers (editDefinitionByFQN)
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.HashQualified qualified as SyntaxHQ

data EditDefinitionParams = EditDefinitionParams
  { textDocument :: LSP.TextDocumentIdentifier,
    position :: Maybe LSP.Position,
    fqn :: Maybe Text
  }
  deriving (Show, Eq)

instance Aeson.FromJSON EditDefinitionParams where
  parseJSON = Aeson.withObject "EditDefinitionParams" $ \v -> do
    textDocument <- v Aeson..: "textDocument"
    position <- v Aeson..:? "position"
    fqn <- v Aeson..:? "fqn"
    pure EditDefinitionParams {textDocument, position, fqn}

data EditDefinitionResponse
  = EditDefinitionError Text
  | EditDefinitionSuccess Bool {- Whether the definition was newly added -}
  deriving (Show, Eq)

instance Aeson.ToJSON EditDefinitionResponse where
  toJSON = \case
    EditDefinitionSuccess newlyAdded ->
      Aeson.object
        [ "newlyAdded" Aeson..= newlyAdded
        ]
    EditDefinitionError err ->
      Aeson.object
        [ "error" Aeson..= err
        ]

-- | Handler for the 'unison/editDefinition' custom LSP request.
-- This resolves the symbol at the given position to its FQN and adds it to the current file.
editDefinitionHandler ::
  Msg.TRequestMessage ('Msg.Method_CustomMethod "unison/editDefinition") ->
  (Either Msg.ResponseError Aeson.Value -> Lsp ()) ->
  Lsp ()
editDefinitionHandler m respond = do
  result <- runExceptT $ do
    let paramsJSON = m ^. LSP.params
    Debug.debugM Debug.Temp "editDefinitionHandler: Received request, params:" paramsJSON
    EditDefinitionParams {textDocument, position, fqn} <- case Aeson.fromJSON paramsJSON of
      Aeson.Error err -> throwError $ "Invalid parameters: " <> Text.pack err
      Aeson.Success p -> pure p

    let fileURI = textDocument._uri
    -- Get the FQN either directly or by resolving the symbol at the position
    fqnText <- case (fqn, position) of
      (Just directFqn, _) ->
        -- Use the provided FQN directly
        pure directFqn
      (Nothing, Just pos) -> do
        Debug.debugM Debug.Temp "editDefinitionHandler: Resolving FQN at position: " pos
        -- Get the symbol reference at the position
        ref <- orFail "Error: Can only edit top-level definitions." . runMaybeT $ LSPQ.refAtPosition fileURI pos
        Debug.debugM Debug.Temp "editDefinitionHandler: Found reference: " ref

        -- Get the FQN for the reference
        pped <- lift $ ppedForFile fileURI
        let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
        let fqnName = case ref of
              LD.TypeReference typeRef -> PPE.typeName unsuffixifiedPPE typeRef
              LD.TermReferent termRef -> PPE.termName unsuffixifiedPPE termRef
        Debug.debugM Debug.Temp "editDefinitionHandler: Resolved FQN: " fqnName
        pure $ SyntaxHQ.toText fqnName
      (Nothing, Nothing) ->
        throwError "Either 'position' or 'fqn' must be provided"

    -- Call the editDefinitionByFQN utility
    Debug.debugM Debug.Temp "editDefinitionHandler: Editing definition for FQN: " fqnText
    editDefinitionByFQN fileURI fqnText
  Debug.debugM Debug.Temp "editDefinitionHandler: Got result" result

  -- Send the response
  case result of
    (Left errMsg) -> respond (Right $ Aeson.toJSON $ EditDefinitionError errMsg)
    Right isNewDefinition ->
      respond (Right $ Aeson.toJSON $ EditDefinitionSuccess isNewDefinition)
  where
    orFail :: Text -> Lsp (Maybe a) -> ExceptT Text Lsp a
    orFail err action = do
      ma <- lift action
      case ma of
        Just a -> pure a
        Nothing -> throwError err

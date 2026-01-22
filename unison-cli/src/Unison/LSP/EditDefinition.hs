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
        -- Get the symbol reference at the position
        ref <- orFail "Error: Definition not found in Codebase" . runMaybeT $ LSPQ.refAtPosition fileURI pos

        -- Get the FQN for the reference
        pped <- lift $ ppedForFile fileURI
        let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
        let fqnName = case ref of
              LD.TypeReference typeRef -> PPE.typeName unsuffixifiedPPE typeRef
              LD.TermReferent termRef -> PPE.termName unsuffixifiedPPE termRef
        pure $ SyntaxHQ.toText fqnName
      (Nothing, Nothing) ->
        throwError "Either 'position' or 'fqn' must be provided"

    -- Call the editDefinitionByFQN utility
    editDefinitionByFQN fileURI fqnText

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

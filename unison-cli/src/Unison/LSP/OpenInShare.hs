{-# LANGUAGE DataKinds #-}

module Unison.LSP.OpenInShare
  ( openInShareHandler,
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
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.URI (escapeURIString, isUnreserved)
import U.Codebase.Sqlite.Project
import Unison.Codebase.ProjectPath
import Unison.Core.Project (ProjectName (..))
import Unison.LSP.FileAnalysis (ppedForFile)
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.HashQualified qualified as SyntaxHQ
import UnliftIO qualified
import Web.Browser qualified as Web

data OpenInShareParams = OpenInShareParams
  { textDocument :: LSP.Uri,
    position :: LSP.Position
  }
  deriving (Show, Eq)

data DefinitionInfo
  = DefinitionInfo
  { definitionFQN :: Text,
    definitionBranchRef :: Text,
    definitionProjectRef :: Text,
    definitionKind :: Text
  }
  deriving (Show, Eq)

instance Aeson.FromJSON DefinitionInfo where
  parseJSON = Aeson.withObject "DefinitionInfo" $ \v -> do
    definitionFQN <- v Aeson..: "fqn"
    definitionBranchRef <- v Aeson..: "branchRef"
    definitionProjectRef <- v Aeson..: "projectRef"
    definitionKind <- v Aeson..: "kind"
    pure DefinitionInfo {definitionFQN, definitionBranchRef, definitionProjectRef, definitionKind}

data DefinitionSearchResponse = DefinitionSearchResponse
  { definitions :: [DefinitionInfo]
  }
  deriving (Show, Eq)

instance Aeson.FromJSON DefinitionSearchResponse where
  parseJSON = Aeson.withObject "DefinitionSearchResponse" $ \v -> do
    definitions <- v Aeson..: "definitions"
    pure DefinitionSearchResponse {definitions}

instance Aeson.FromJSON OpenInShareParams where
  parseJSON = Aeson.withObject "OpenInShareParams" $ \v -> do
    textDocument <- v Aeson..: "textDocument"
    position <- v Aeson..: "position"
    pure OpenInShareParams {textDocument, position}

-- | Handler for the 'unison/openInShare' custom LSP request.
-- This resolves the symbol at the given position to its FQN and makes an HTTP POST
-- to the local Share service.
openInShareHandler ::
  Msg.TRequestMessage ('Msg.Method_CustomMethod "unison/openInShare") ->
  (Either Msg.ResponseError (Msg.MessageResult ('Msg.Method_CustomMethod "unison/openInShare")) -> Lsp ()) ->
  Lsp ()
openInShareHandler m respond = do
  result <- runExceptT $ do
    pp <- lift getCurrentProjectPath
    let (UnsafeProjectName projectNameText) = pp.project.name
    -- Extract parameters from the request
    let params = m ^. LSP.params
    OpenInShareParams {textDocument, position} <- case Aeson.fromJSON params of
      Aeson.Error err -> throwError $ Msg.ResponseError (InR ErrorCodes_InvalidParams) (Text.pack err) Nothing
      Aeson.Success p -> pure p

    -- Get the symbol reference at the position
    maybeRef <- lift . runMaybeT $ LSPQ.refAtPosition textDocument position
    ref <- case maybeRef of
      Nothing -> throwError $ Msg.ResponseError (InR ErrorCodes_InvalidParams) "No symbol found at position" Nothing
      Just r -> pure r

    -- Get the FQN for the reference
    pped <- lift $ ppedForFile textDocument
    let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
    let fqn = case ref of
          LD.TypeReference typeRef -> PPE.typeName unsuffixifiedPPE typeRef
          LD.TermReferent termRef -> PPE.termName unsuffixifiedPPE termRef
    let fqnText = SyntaxHQ.toText fqn
    let encodedProjectName = Text.pack $ escapeURIString isUnreserved (Text.unpack projectNameText)

    let (projectFilter, query) =
          case Text.splitOn "." fqnText of
            ("lib" : _ : rest) -> ("&project-filter=" <> encodedProjectName, Text.intercalate "." rest)
            _ -> ("", fqnText)

    -- Build the request URL
    let shareUrl =
          "https://api.unison-lang.org/search-definitions"
            <> "?query="
            <> Text.unpack query
            <> Text.unpack projectFilter

    -- Make the HTTP POST request
    httpResponse <- liftIO $ UnliftIO.tryAny $ do
      manager <- HTTP.getGlobalManager
      request <- HTTP.parseRequest shareUrl
      let postRequest = request {HTTP.method = "POST"}
      response <- HTTP.httpLbs postRequest manager
      pure response

    -- Handle HTTP errors
    case httpResponse of
      Left err ->
        throwError $
          Msg.ResponseError
            (InR ErrorCodes_InternalError)
            ("Failed to connect to Share service: " <> Text.pack (show err))
            Nothing
      Right response
        | HTTP.statusIsSuccessful (HTTP.responseStatus response) ->
            case Aeson.decode (HTTP.responseBody response) of
              Just (DefinitionSearchResponse {definitions}) ->
                case definitions of
                  (DefinitionInfo {definitionProjectRef, definitionBranchRef, definitionKind} : _) -> do
                    -- E.g. https://share.unison-lang.org/@unison/base/code/releases/7.12.0/latest/terms/data/List/map
                    let shareUrl =
                          Text.unpack $
                            "https://share.unison-lang.org/@"
                              <> definitionProjectRef
                              <> "/code/branches/"
                              <> definitionBranchRef
                              <> "/latest"
                              <> "/"
                              <> definitionKind
                              <> "s/data/"
                              <> Text.replace "." "/" fqnText
                    void . liftIO $ Web.openBrowser shareUrl
                  [] ->
                    throwError $
                      Msg.ResponseError
                        (InR ErrorCodes_InternalError)
                        "No definitions found in Share service response"
                        Nothing
              Nothing ->
                throwError $
                  Msg.ResponseError
                    (InR ErrorCodes_InternalError)
                    "Failed to parse Share service response"
                    Nothing
        | otherwise ->
            throwError $
              Msg.ResponseError
                (InR ErrorCodes_InternalError)
                ("Share service returned error: " <> Text.pack (show response))
                Nothing

  -- Send the response
  case result of
    Left err -> respond (Left err)
    Right _ -> respond (Right Aeson.Null)

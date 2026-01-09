{-# LANGUAGE DataKinds #-}

module Unison.LSP.OpenInShare
  ( openInShareHandler,
  )
where

import Control.Lens hiding (List)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Generics.Product (field)
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.URI (escapeURIString, isUnreserved)
import Unison.LSP.FileAnalysis (ppedForFile)
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.HashQualified qualified as SyntaxHQ
import UnliftIO qualified

data OpenInShareParams = OpenInShareParams
  { textDocument :: LSP.Uri,
    position :: LSP.Position
  }
  deriving (Show, Eq)

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
    let encodedFqn = Text.pack $ escapeURIString isUnreserved (Text.unpack fqnText)
    let fileUriText = LSP.uriToText textDocument
    let encodedUri = Text.pack $ escapeURIString isUnreserved (Text.unpack fileUri)
    let encodedLoc =
          Text.pack $
            escapeURIString
              isUnreserved
              ( show (position ^. LSP.line)
                  <> ":"
                  <> show (position ^. LSP.character)
              )

    -- Build the request URL
    let shareUrl =
          "http://localhost:5424/open-on-share"
            <> "?fqn="
            <> Text.unpack encodedFqn
            <> "&fileURI="
            <> Text.unpack encodedUri
            <> "&loc="
            <> Text.unpack encodedLoc

    -- Make the HTTP POST request
    httpResult <- liftIO $ UnliftIO.tryAny $ do
      manager <- HTTP.getGlobalManager
      request <- HTTP.parseRequest shareUrl
      let postRequest = request {HTTP.method = "POST"}
      response <- HTTP.httpLbs postRequest manager
      pure $ HTTP.responseStatus response

    -- Handle HTTP errors
    case httpResult of
      Left err ->
        throwError $
          Msg.ResponseError
            (InR ErrorCodes_InternalError)
            ("Failed to connect to Share service: " <> Text.pack (show err))
            Nothing
      Right status
        | HTTP.statusIsSuccessful status -> pure ()
        | otherwise ->
            throwError $
              Msg.ResponseError
                (InR ErrorCodes_InternalError)
                ("Share service returned error: " <> Text.pack (show status))
                Nothing

  -- Send the response
  case result of
    Left err -> respond (Left err)
    Right _ -> respond (Right Aeson.Null)

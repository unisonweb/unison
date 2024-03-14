module Unison.LSP.Diagnostics
  ( reportDiagnostics,
    mkDiagnostic,
    DiagnosticSeverity (..),
  )
where

import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.LSP.Types
import Unison.Prelude

reportDiagnostics ::
  (Foldable f) =>
  Uri ->
  Maybe FileVersion ->
  -- | Note, it's important to still send an empty list of diagnostics if there aren't any
  -- because it clears existing diagnostics in the editor
  f Diagnostic ->
  Lsp ()
reportDiagnostics docUri fileVersion diags = do
  let params = PublishDiagnosticsParams {_uri = docUri, _version = fromIntegral <$> fileVersion, _diagnostics = toList $ diags}
  sendNotification Msg.SMethod_TextDocumentPublishDiagnostics params

mkDiagnostic :: Uri -> Range -> DiagnosticSeverity -> Text -> [(Text, Range)] -> Diagnostic
mkDiagnostic uri r severity msg references =
  Diagnostic
    { _range = r,
      _severity = Just severity,
      _code = Nothing, -- We could eventually pass error codes here
      _source = Just "unison",
      _message = msg,
      _tags = Nothing,
      _relatedInformation =
        case references of
          [] -> Nothing
          refs ->
            Just $
              refs <&> \(msg, range) ->
                DiagnosticRelatedInformation (Location uri range) msg,
      -- Could put links to the website in here with more info about specific errors.
      _codeDescription = Nothing,
      _data_ = Nothing
    }

module Unison.LSP.Diagnostics where

import Language.LSP.Types
import Unison.LSP.Types
import qualified Unison.Names.ResolutionResult as Names
import Unison.Prelude
import Unison.Symbol (Symbol)

reportDiagnostics ::
  Foldable f =>
  Uri ->
  Maybe FileVersion ->
  -- | Note, it's important to still send an empty list of diagnostics if there aren't any
  -- because it clears existing diagnostics in the editor
  f Diagnostic ->
  Lsp ()
reportDiagnostics docUri fileVersion diags = do
  let jsonRPC = "" -- TODO: what's this for?
  let params = PublishDiagnosticsParams {_uri = docUri, _version = fromIntegral <$> fileVersion, _diagnostics = List . toList $ diags}
  sendNotification (NotificationMessage jsonRPC STextDocumentPublishDiagnostics params)

data UnisonDiagnostic = UnisonDiagnostic Range UnisonDiagnosticInfo

data UnisonDiagnosticInfo
  = TypeError Text
  | NameResolutionFailure (Names.ResolutionFailure Symbol Range)

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
            Just . List $
              refs <&> \(msg, range) ->
                DiagnosticRelatedInformation (Location uri range) msg
    }

getSeverity :: UnisonDiagnosticInfo -> DiagnosticSeverity
getSeverity = \case
  TypeError {} -> DsError
  NameResolutionFailure {} -> DsError

diagnosticMessage :: UnisonDiagnosticInfo -> Text
diagnosticMessage = \case
  TypeError {} -> "Type Error!!"
  NameResolutionFailure rf -> tShow rf

toLSPDiagnostic :: UnisonDiagnostic -> Diagnostic
toLSPDiagnostic (UnisonDiagnostic r i) =
  Diagnostic
    { _range = r,
      _severity = Just . getSeverity $ i,
      _code = Nothing, -- We could eventually pass error codes here
      _source = Just "unison",
      _message = diagnosticMessage $ i,
      _tags = Nothing,
      _relatedInformation = Nothing
    }

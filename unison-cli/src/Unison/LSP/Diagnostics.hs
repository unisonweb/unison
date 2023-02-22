module Unison.LSP.Diagnostics where

import Language.LSP.Types
import Unison.LSP.Types
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import Unison.Prelude
import qualified Unison.Syntax.Lexer as Lex
import qualified Unison.Util.Range as Range

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.Ann start end -> Just $ Range (uToLspPos start) (uToLspPos end)

uToLspPos :: Lex.Pos -> Position
uToLspPos uPos =
  Position
    { _line = fromIntegral $ Lex.line uPos - 1, -- 1 indexed vs 0 indexed
      _character = fromIntegral $ Lex.column uPos - 1 -- 1 indexed vs 0 indexed
    }

uToLspRange :: Range.Range -> Range
uToLspRange (Range.Range start end) = Range (uToLspPos start) (uToLspPos end)

reportDiagnostics ::
  (Foldable f) =>
  Uri ->
  Maybe FileVersion ->
  -- | Note, it's important to still send an empty list of diagnostics if there aren't any
  -- because it clears existing diagnostics in the editor
  f Diagnostic ->
  Lsp ()
reportDiagnostics docUri fileVersion diags = do
  let jsonRPC = "2.0"
  let params = PublishDiagnosticsParams {_uri = docUri, _version = fromIntegral <$> fileVersion, _diagnostics = List . toList $ diags}
  sendNotification (NotificationMessage jsonRPC STextDocumentPublishDiagnostics params)

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

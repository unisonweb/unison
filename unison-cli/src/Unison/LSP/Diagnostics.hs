module Unison.LSP.Diagnostics where

import Control.Lens hiding (List)
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (to)
import Unison.LSP.FileInfo (annToRange)
import Unison.LSP.Types
import qualified Unison.Names.ResolutionResult as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Result (Note)
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)

noteDiagnostics :: Foldable f => f (Note Symbol Ann) -> [UnisonDiagnostic]
noteDiagnostics notes = do
  flip foldMap notes \case
    Result.NameResolutionFailures failures ->
      failures
        & fmap (traverse annToRange)
        & catMaybes
        <&> ( \case
                f@(Names.TypeResolutionFailure _sym range _re) -> UnisonDiagnostic range (NameResolutionFailure f)
                f@(Names.TermResolutionFailure _sym range _re) -> UnisonDiagnostic range (NameResolutionFailure f)
            )
    _ -> []

-- TODO
-- Result.Parsing {} -> _
-- Result.UnknownSymbol v loc -> _
-- Result.TypeError {} -> _
-- Result.TypeInfo {} -> []
-- Result.CompilerBug {} -> _

reportDiagnostics :: VersionedTextDocumentIdentifier -> [UnisonDiagnostic] -> Lsp ()
reportDiagnostics _uri [] = pure ()
reportDiagnostics docId diags = do
  let jsonRPC = "" -- TODO: what's this for?
  let params = PublishDiagnosticsParams {_uri = docId ^. uri, _version = docId ^? version . _Just . to fromIntegral, _diagnostics = List (toLSPDiagnostic <$> diags)}
  sendNotification (NotificationMessage jsonRPC STextDocumentPublishDiagnostics params)

data UnisonDiagnostic = UnisonDiagnostic Range UnisonDiagnosticInfo

data UnisonDiagnosticInfo
  = TypeError Text
  | NameResolutionFailure (Names.ResolutionFailure Symbol Range)

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

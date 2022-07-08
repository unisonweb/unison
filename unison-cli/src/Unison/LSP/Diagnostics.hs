module Unison.LSP.Diagnostics where

import qualified Data.Text as Text
import Language.LSP.Types
import Unison.LSP.Types
import qualified Unison.LSP.Types as LSP
import qualified Unison.Lexer as Lex
import qualified Unison.Names.ResolutionResult as Names
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.PrintError as PrintError
import Unison.Result (Note)
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Typechecker.Context as Typechecker
import qualified Unison.Util.Pretty as Pretty

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.Ann start end -> Just $ Range (toLspPos start) (toLspPos end)
  where
    toLspPos :: Lex.Pos -> Position
    toLspPos uPos =
      Position
        { _line = fromIntegral $ Lex.line uPos - 1, -- 1 indexed vs 0 indexed
          _character = fromIntegral $ Lex.column uPos - 1 -- 1 indexed vs 0 indexed
        }

infoDiagnostics :: FileAnalysis -> Lsp [Diagnostic]
infoDiagnostics FileAnalysis {lexedSource = (srcText, _lexed), notes} = do
  ppe <- LSP.globalPPE
  pure $ noteDiagnostics (PPE.suffixifiedPPE ppe) (Text.unpack srcText) notes

noteDiagnostics :: Foldable f => PrettyPrintEnv -> String -> f (Note Symbol Ann) -> [Diagnostic]
noteDiagnostics ppe src notes = do
  flip foldMap notes \note -> case note of
    Result.TypeError {} ->
      let msg = Text.pack $ Pretty.toPlain 80 $ PrintError.printNoteWithSource ppe src note
          mayRange = noteRange note >>= annToRange
       in maybe [] (\range -> [mkDiagnostic range DsError msg]) mayRange
    -- TODO
    -- Result.Parsing {} -> _
    -- Result.UnknownSymbol v loc -> _
    -- Result.TypeInfo {} -> []
    -- Result.CompilerBug {} -> _
    _ -> []

noteRange :: Note Symbol Ann -> Maybe Ann
noteRange = \case
  Result.Parsing {} -> todoAnnotation
  Result.NameResolutionFailures {} -> todoAnnotation
  Result.UnknownSymbol _sym loc -> Just loc
  Result.TypeError (Typechecker.ErrorNote {cause}) -> case cause of
    Typechecker.TypeMismatch _ctx -> todoAnnotation
    Typechecker.IllFormedType _ctx -> todoAnnotation
    Typechecker.UnknownSymbol loc _v -> Just loc
    Typechecker.UnknownTerm loc _sym _suggestions _typ -> Just loc
    Typechecker.AbilityCheckFailure {} -> todoAnnotation
    Typechecker.EffectConstructorWrongArgCount {} -> todoAnnotation
    Typechecker.MalformedEffectBind {} -> todoAnnotation
    Typechecker.PatternArityMismatch loc _ _ -> Just loc
    Typechecker.DuplicateDefinitions {} -> todoAnnotation
    Typechecker.UnguardedLetRecCycle {} -> todoAnnotation
    Typechecker.ConcatPatternWithoutConstantLength loc _ -> Just loc
    Typechecker.HandlerOfUnexpectedType loc _ -> Just loc
    Typechecker.DataEffectMismatch {} -> todoAnnotation
  Result.TypeInfo {} -> todoAnnotation
  Result.CompilerBug {} -> todoAnnotation
  where
    -- This error needs a specific annotation to occur at.
    todoAnnotation = Nothing

reportDiagnostics ::
  Uri ->
  Maybe FileVersion ->
  -- | Note, it's important to still send an empty list of diagnostics if there aren't any
  -- because it clears existing diagnostics in the editor
  [Diagnostic] ->
  Lsp ()
reportDiagnostics docUri fileVersion diags = do
  let jsonRPC = "" -- TODO: what's this for?
  let params = PublishDiagnosticsParams {_uri = docUri, _version = fromIntegral <$> fileVersion, _diagnostics = List diags}
  sendNotification (NotificationMessage jsonRPC STextDocumentPublishDiagnostics params)

data UnisonDiagnostic = UnisonDiagnostic Range UnisonDiagnosticInfo

data UnisonDiagnosticInfo
  = TypeError Text
  | NameResolutionFailure (Names.ResolutionFailure Symbol Range)

mkDiagnostic :: Range -> DiagnosticSeverity -> Text -> Diagnostic
mkDiagnostic r severity msg =
  Diagnostic
    { _range = r,
      _severity = Just severity,
      _code = Nothing, -- We could eventually pass error codes here
      _source = Just "unison",
      _message = msg,
      _tags = Nothing,
      _relatedInformation = Nothing
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

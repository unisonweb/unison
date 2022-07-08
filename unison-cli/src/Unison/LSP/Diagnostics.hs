module Unison.LSP.Diagnostics where

import Data.Bifunctor (second)
import qualified Data.Text as Text
import Language.LSP.Types
import qualified Unison.ABT as ABT
import qualified Unison.Debug as Debug
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
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Typechecker.TypeError as TypeError
import qualified Unison.Util.Pretty as Pretty
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

infoDiagnostics :: FileAnalysis -> Lsp [Diagnostic]
infoDiagnostics FileAnalysis {fileUri, lexedSource = (srcText, _lexed), notes} = do
  ppe <- LSP.globalPPE
  pure $ noteDiagnostics fileUri (PPE.suffixifiedPPE ppe) (Text.unpack srcText) notes

noteDiagnostics :: Foldable f => Uri -> PrettyPrintEnv -> String -> f (Note Symbol Ann) -> [Diagnostic]
noteDiagnostics fileUri ppe src notes = do
  flip foldMap notes \note -> case note of
    Result.TypeError {} -> noteDiagnostic note
    Result.NameResolutionFailures {} -> noteDiagnostic note
    Result.Parsing err -> do
      (errMsg, ranges) <- PrintError.renderParseErrors src err
      let txtMsg = Text.pack $ Pretty.toPlain 80 errMsg
      range <- ranges
      pure $ mkDiagnostic fileUri (uToLspRange range) DsError txtMsg []
    Result.UnknownSymbol {} -> noteDiagnostic note
    Result.TypeInfo {} -> []
    Result.CompilerBug {} -> noteDiagnostic note
  where
    noteDiagnostic note =
      let msg = Text.pack $ Pretty.toPlain 80 $ PrintError.printNoteWithSource ppe src note
          ranges = noteRanges note
       in do
            (range, references) <- ranges
            pure $ mkDiagnostic fileUri range DsError msg references

-- | Returns a list of ranges where this note should be marked in the document,
-- as well as a list of 'related' ranges the note might refer to, and their relevance.
--
-- E.g. a name conflict note might mark each conflicted name, and contain references to the
-- other conflicted name locations.
noteRanges :: Note Symbol Ann -> [(Range, [(Text, Range)])]
noteRanges = \case
  Result.UnknownSymbol _sym loc -> singleRange loc
  -- TODO: This should have an error extractor
  Result.TypeError (Context.ErrorNote {cause = Context.PatternArityMismatch loc _ _}) -> singleRange loc
  Result.TypeError errNote -> do
    let typeErr = TypeError.typeErrorFromNote errNote
    case typeErr of
      TypeError.Mismatch {mismatchSite} -> singleRange $ ABT.annotation mismatchSite
      TypeError.BooleanMismatch {mismatchSite} -> singleRange $ ABT.annotation mismatchSite
      TypeError.ExistentialMismatch {mismatchSite} -> singleRange $ ABT.annotation mismatchSite
      TypeError.FunctionApplication {f} -> singleRange $ ABT.annotation f
      TypeError.NotFunctionApplication {f} -> singleRange $ ABT.annotation f
      TypeError.AbilityCheckFailure {abilityCheckFailureSite} -> singleRange abilityCheckFailureSite
      TypeError.UnguardedLetRecCycle {cycleLocs} -> do
        let ranges :: [Range]
            ranges = cycleLocs >>= aToR
        (range, cycleRanges) <- withNeighbours ranges
        pure (range, ("cycle",) <$> cycleRanges)
      TypeError.UnknownType {typeSite} -> singleRange typeSite
      TypeError.UnknownTerm {termSite} -> singleRange termSite
      TypeError.DuplicateDefinitions {defns} -> do
        (_v, locs) <- toList defns
        (r, rs) <- withNeighbours (locs >>= aToR)
        pure (r, ("duplicate definition",) <$> rs)
      TypeError.Other e -> do
        Debug.debugM Debug.LSP "No Diagnostic configured for type error: " e
        empty
  Result.TypeInfo {} -> []
  Result.CompilerBug e -> do
    Debug.debugM Debug.LSP "No Diagnostic configured for compiler error: " e
    empty
  Result.Parsing {} ->
    -- Parse notes are handled manually in noteDiagnostics
    todoAnnotation
  Result.NameResolutionFailures {} -> todoAnnotation
  where
    todoAnnotation = []
    singleRange :: Ann -> [(Range, [a])]
    singleRange ann = do
      r <- aToR ann
      pure (r, [])

    aToR :: Ann -> [Range]
    aToR = maybeToList . annToRange
    -- >>> withNeighbours [1, 2, 3, 4]
    -- [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
    withNeighbours :: [a] -> [(a, [a])]
    withNeighbours [] = []
    withNeighbours (a : as) = (a, as) : (second (a :) <$> withNeighbours as)

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

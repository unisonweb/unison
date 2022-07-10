{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.FileAnalysis where

import Control.Lens
import Control.Monad.Reader
  ( asks,
  )
import qualified Crypto.Random as Random
import Data.Bifunctor (second)
import Data.Foldable
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Data.IntervalMap.Lazy as IM
import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.LSP.Types
  ( Diagnostic,
    DiagnosticSeverity (DsError),
    Position,
    Range,
    TextDocumentIdentifier (TextDocumentIdentifier),
    Uri (getUri),
  )
import Language.LSP.Types.Lens (HasRange (range), HasUri (uri))
import qualified Unison.ABT as ABT
import Unison.Codebase.Editor.HandleCommand (typecheckCommand)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Debug as Debug
import Unison.LSP.Conversions
import Unison.LSP.Diagnostics
  ( mkDiagnostic,
    reportDiagnostics,
  )
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import qualified Unison.LSP.Types as LSP
import qualified Unison.LSP.VFS as VFS
import qualified Unison.Lexer as L
import Unison.Parser.Ann (Ann)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.PrintError as PrintError
import Unison.Result (Note)
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Typechecker.TypeError as TypeError
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as Pretty
import UnliftIO (atomically, modifyTVar', readTVar, readTVarIO, writeTVar)
import Witherable (forMaybe)

-- | Lex, parse, and typecheck a file.
checkFile :: HasUri d Uri => d -> Lsp (Maybe FileAnalysis)
checkFile doc = runMaybeT $ do
  let fileUri = doc ^. uri
  (fileVersion, contents) <- MaybeT (VFS.getFileContents doc)
  parseNames <- lift getParseNames
  let sourceName = getUri $ doc ^. uri
  let lexedSource@(srcText, _) = (contents, L.lexer (Text.unpack sourceName) (Text.unpack contents))
  let ambientAbilities = []
  cb <- asks codebase
  drg <- liftIO Random.getSystemDRG
  r <- (liftIO $ typecheckCommand cb ambientAbilities parseNames sourceName lexedSource drg)
  let Result.Result notes mayResult = r
  let (parsedFile, typecheckedFile) = case mayResult of
        Nothing -> (Nothing, Nothing)
        Just (Left uf) -> (Just uf, Nothing)
        Just (Right tf) -> (Just $ UF.discardTypes tf, Just tf)
  (diagnostics, codeActions) <- lift $ analyseFile fileUri srcText notes
  let diagnosticRanges =
        diagnostics
          & fmap (\d -> (d ^. range, d))
          & toRangeMap
  let codeActionRanges =
        codeActions
          & foldMap (\(RangedCodeAction {_codeActionRanges, _codeAction}) -> (,_codeAction) <$> _codeActionRanges)
          & toRangeMap
  let fileAnalysis = FileAnalysis {diagnostics = diagnosticRanges, codeActions = codeActionRanges, testing = "hi!", ..}
  pure $ fileAnalysis

fileAnalysisWorker :: Lsp ()
fileAnalysisWorker = forever do
  dirtyFilesV <- asks dirtyFilesVar
  checkedFilesV <- asks checkedFilesVar
  -- We may want to debounce this if it typechecks too eagerly,
  -- but typechecking is pretty fast right now when scratch files are small
  dirtyFileIDs <- atomically $ do
    dirty <- readTVar dirtyFilesV
    writeTVar dirtyFilesV mempty
    guard $ not $ null dirty
    pure dirty
  freshlyCheckedFiles <-
    Map.fromList <$> forMaybe (toList dirtyFileIDs) \docUri -> runMaybeT do
      fileInfo <- MaybeT (checkFile $ TextDocumentIdentifier docUri)
      pure (docUri, fileInfo)
  Debug.debugM Debug.LSP "Freshly Typechecked " freshlyCheckedFiles
  -- Overwrite any files we successfully checked
  atomically $ modifyTVar' checkedFilesV (Map.union freshlyCheckedFiles)
  for freshlyCheckedFiles \(FileAnalysis {fileUri, fileVersion, diagnostics}) -> do
    reportDiagnostics fileUri (Just fileVersion) $ fold diagnostics

analyseFile :: Foldable f => Uri -> Text -> f (Note Symbol Ann) -> Lsp ([Diagnostic], [RangedCodeAction])
analyseFile fileUri srcText notes = do
  ppe <- LSP.globalPPE
  pure $ analyseNotes fileUri (PPE.suffixifiedPPE ppe) (Text.unpack srcText) notes

analyseNotes :: Foldable f => Uri -> PrettyPrintEnv -> String -> f (Note Symbol Ann) -> ([Diagnostic], [RangedCodeAction])
analyseNotes fileUri ppe src notes = do
  flip foldMap notes \note -> case note of
    -- Result.TypeError (Context.ErrorNote {cause = Context.PatternArityMismatch loc _ _}) ->
    --   ([], singleRange loc)
    Result.TypeError errNote@(Context.ErrorNote {cause}) ->
      let typeErr = TypeError.typeErrorFromNote errNote
          ranges = case typeErr of
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
          diags = noteDiagnostic note ranges
          codeActions = case cause of
            Context.UnknownTerm _ _ suggestions _ -> do
              Context.Suggestion {suggestionName, suggestionType = _, suggestionMatch = _} <- suggestions
              let ranges = (diags ^.. folded . range)
              let rca = rangedCodeAction ("Use " <> suggestionName) diags ranges
              pure $ includeEdits fileUri suggestionName ranges rca
            _ -> []
       in (diags, codeActions)
    Result.NameResolutionFailures {} ->
      -- TODO: diagnostics/code actions for resolution failures
      (noteDiagnostic note todoAnnotation, [])
    Result.Parsing err ->
      let diags = do
            (errMsg, ranges) <- PrintError.renderParseErrors src err
            let txtMsg = Text.pack $ Pretty.toPlain 80 errMsg
            range <- ranges
            pure $ mkDiagnostic fileUri (uToLspRange range) DsError txtMsg []
       in -- TODO: Some parsing errors likely have reasonable code actions
          (diags, [])
    Result.UnknownSymbol _ loc ->
      (noteDiagnostic note (singleRange loc), [])
    Result.TypeInfo {} ->
      -- No relevant diagnostics from type info.
      ([], [])
    Result.CompilerBug cbug ->
      let ranges = case cbug of
            Result.TopLevelComponentNotFound _ trm -> singleRange $ ABT.annotation trm
            Result.ResolvedNameNotFound _ loc _ -> singleRange loc
            Result.TypecheckerBug tcbug -> case tcbug of
              Context.UnknownDecl _un _ref decls -> decls & foldMap \decl -> singleRange $ DD.annotation decl
              Context.UnknownConstructor _un _gcr decl -> singleRange $ DD.annotation decl
              Context.UndeclaredTermVariable _sym _con -> todoAnnotation
              Context.RetractFailure _el _con -> todoAnnotation
              Context.EmptyLetRec trm -> singleRange $ ABT.annotation trm
              Context.PatternMatchFailure -> todoAnnotation
              Context.EffectConstructorHadMultipleEffects typ -> singleRange $ ABT.annotation typ
              Context.FreeVarsInTypeAnnotation _set -> todoAnnotation
              Context.UnannotatedReference _ref -> todoAnnotation
              Context.MalformedPattern pat -> singleRange $ Pattern.loc pat
              Context.UnknownTermReference _ref -> todoAnnotation
              Context.UnknownExistentialVariable _sym _con -> todoAnnotation
              Context.IllegalContextExtension _con _el _s -> todoAnnotation
              Context.OtherBug _s -> todoAnnotation
       in (noteDiagnostic note ranges, [])
  where
    -- Diagnostics with this return value haven't been properly configured yet.
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
    -- Builds diagnostics for a note, one diagnostic per range.
    noteDiagnostic ::
      Note Symbol Ann ->
      -- All ranges affected by this note, each range may have references to 'related'
      -- ranges.
      -- E.g. a name conflict note might mark each conflicted name, and contain references to the
      -- other conflicted name locations.
      [(Range, [(Text, Range)])] ->
      [Diagnostic]
    noteDiagnostic note ranges =
      let msg = Text.pack $ Pretty.toPlain 80 $ PrintError.printNoteWithSource ppe src note
       in do
            (range, references) <- ranges
            pure $ mkDiagnostic fileUri range DsError msg references

toRangeMap :: (Foldable f) => f (Range, a) -> IntervalMap Position [a]
toRangeMap vs =
  IM.fromListWith (<>) (toList vs <&> \(r, a) -> (rangeToInterval r, [a]))

getFileAnalysis :: Uri -> Lsp (Maybe FileAnalysis)
getFileAnalysis uri = do
  checkedFilesV <- asks checkedFilesVar
  checkedFiles <- readTVarIO checkedFilesV
  pure $ Map.lookup uri checkedFiles

{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.FileAnalysis where

import Control.Lens
import Control.Monad.Reader
import qualified Crypto.Random as Random
import Data.Align (alignWith)
import Data.Bifunctor (second)
import Data.Foldable
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Data.IntervalMap.Lazy as IM
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.These
import Language.LSP.Types
  ( Diagnostic,
    DiagnosticSeverity (DsError),
    Position,
    Range,
    TextDocumentIdentifier (TextDocumentIdentifier),
    Uri (getUri),
  )
import Language.LSP.Types.Lens (HasCodeAction (codeAction), HasIsPreferred (isPreferred), HasRange (range), HasUri (uri))
import qualified Unison.ABT as ABT
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.HandleInput (typecheckHelper)
import qualified Unison.Codebase.Path as Path
import qualified Unison.DataDeclaration as DD
import qualified Unison.Debug as Debug
import qualified Unison.HashQualified' as HQ'
import Unison.LSP.Conversions
import Unison.LSP.Diagnostics
  ( mkDiagnostic,
    reportDiagnostics,
  )
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import qualified Unison.LSP.Types as LSP
import qualified Unison.LSP.VFS as VFS
import Unison.Names (Names)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPED
import qualified Unison.PrintError as PrintError
import Unison.Result (Note)
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Syntax.TypePrinter as TypePrinter
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Typechecker.TypeError as TypeError
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Var as Var
import UnliftIO.STM

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
  let generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG
  r <- (liftIO $ typecheckHelper cb generateUniqueName ambientAbilities parseNames sourceName lexedSource)
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
  let fileAnalysis = FileAnalysis {diagnostics = diagnosticRanges, codeActions = codeActionRanges, ..}
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
  atomically $ do
    checkedFiles <- readTVar checkedFilesV
    let zipper = \case
          These mvar new -> tryTakeTMVar mvar *> putTMVar mvar new *> pure mvar
          This mvar -> pure mvar
          That new -> newTMVar new
    newCheckedFiles <- sequenceA $ alignWith zipper checkedFiles freshlyCheckedFiles
    writeTVar checkedFilesV newCheckedFiles
  for freshlyCheckedFiles \(FileAnalysis {fileUri, fileVersion, diagnostics}) -> do
    reportDiagnostics fileUri (Just fileVersion) $ fold diagnostics

analyseFile :: Foldable f => Uri -> Text -> f (Note Symbol Ann) -> Lsp ([Diagnostic], [RangedCodeAction])
analyseFile fileUri srcText notes = do
  ppe <- LSP.globalPPE
  analyseNotes fileUri (PPE.suffixifiedPPE ppe) (Text.unpack srcText) notes

analyseNotes :: Foldable f => Uri -> PrettyPrintEnv -> String -> f (Note Symbol Ann) -> Lsp ([Diagnostic], [RangedCodeAction])
analyseNotes fileUri ppe src notes = do
  currentPath <- getCurrentPath
  flip foldMapM notes \note -> case note of
    Result.TypeError errNote@(Context.ErrorNote {cause}) -> do
      let typeErr = TypeError.typeErrorFromNote errNote
          ranges = case typeErr of
            TypeError.Mismatch {mismatchSite} -> singleRange $ ABT.annotation mismatchSite
            TypeError.BooleanMismatch {mismatchSite} -> singleRange $ ABT.annotation mismatchSite
            TypeError.ExistentialMismatch {mismatchSite} -> singleRange $ ABT.annotation mismatchSite
            TypeError.FunctionApplication {f} -> singleRange $ ABT.annotation f
            TypeError.NotFunctionApplication {f} -> singleRange $ ABT.annotation f
            TypeError.AbilityCheckFailure {abilityCheckFailureSite} -> singleRange abilityCheckFailureSite
            TypeError.AbilityEqFailure {abilityCheckFailureSite} -> singleRange abilityCheckFailureSite
            TypeError.AbilityEqFailureFromAp {expectedSite, mismatchSite} -> do
              let locs = [ABT.annotation expectedSite, ABT.annotation mismatchSite]
              (r, rs) <- withNeighbours (locs >>= aToR)
              pure (r, ("mismatch",) <$> rs)
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
          diags = noteDiagnostic currentPath note ranges
      -- Sort on match accuracy first, then name.
      codeActions <- case cause of
        Context.UnknownTerm _ v suggestions typ -> do
          typeHoleActions <- typeHoleReplacementCodeActions diags v typ
          pure $
            nameResolutionCodeActions diags suggestions
              <> typeHoleActions
        _ -> pure []
      pure (diags, codeActions)
    Result.NameResolutionFailures {} -> do
      -- TODO: diagnostics/code actions for resolution failures
      pure (noteDiagnostic currentPath note todoAnnotation, [])
    Result.Parsing err -> do
      let diags = do
            (errMsg, ranges) <- PrintError.renderParseErrors src err
            let txtMsg = Text.pack $ Pretty.toPlain 80 errMsg
            range <- ranges
            pure $ mkDiagnostic fileUri (uToLspRange range) DsError txtMsg []
      -- TODO: Some parsing errors likely have reasonable code actions
      pure (diags, [])
    Result.UnknownSymbol _ loc ->
      pure (noteDiagnostic currentPath note (singleRange loc), [])
    Result.TypeInfo {} ->
      -- No relevant diagnostics from type info.
      pure ([], [])
    Result.CompilerBug cbug -> do
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
      pure (noteDiagnostic currentPath note ranges, [])
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
      Path.Absolute ->
      Note Symbol Ann ->
      -- All ranges affected by this note, each range may have references to 'related'
      -- ranges.
      -- E.g. a name conflict note might mark each conflicted name, and contain references to the
      -- other conflicted name locations.
      [(Range, [(Text, Range)])] ->
      [Diagnostic]
    noteDiagnostic currentPath note ranges =
      let msg = Text.pack $ Pretty.toPlain 80 $ PrintError.printNoteWithSource ppe src currentPath note
       in do
            (range, references) <- ranges
            pure $ mkDiagnostic fileUri range DsError msg references
    -- Suggest name replacements or qualifications when there's ambiguity
    nameResolutionCodeActions :: [Diagnostic] -> [Context.Suggestion Symbol Ann] -> [RangedCodeAction]
    nameResolutionCodeActions diags suggestions = do
      Context.Suggestion {suggestionName, suggestionType, suggestionMatch} <- sortOn nameResolutionSuggestionPriority suggestions
      let prettyType = TypePrinter.prettyStr Nothing ppe suggestionType
      let ranges = (diags ^.. folded . range)
      let rca = rangedCodeAction ("Use " <> suggestionName <> " : " <> Text.pack prettyType) diags ranges
      pure $
        rca
          & includeEdits fileUri suggestionName ranges
          & codeAction . isPreferred ?~ (suggestionMatch == Context.Exact)

    nameResolutionSuggestionPriority (Context.Suggestion {suggestionMatch, suggestionName}) = case suggestionMatch of
      Context.Exact -> (0 :: Int, suggestionName)
      Context.WrongType -> (1, suggestionName)
      Context.WrongName -> (2, suggestionName)

    -- typeHoleReplacementCodeActions :: Symbol -> _ -> Lsp [a]
    typeHoleReplacementCodeActions diags v typ
      | not (isUserBlank v) = pure []
      | otherwise = do
          Env {codebase} <- ask
          ppe <- PPE.suffixifiedPPE <$> globalPPE
          let cleanedTyp = Context.generalizeAndUnTypeVar typ -- TODO: is this right?
          refs <- liftIO $ Codebase.termsOfType codebase cleanedTyp
          forMaybe (toList refs) $ \ref -> runMaybeT $ do
            hqNameSuggestion <- MaybeT . pure $ PPE.terms ppe ref
            typ <- MaybeT . liftIO $ Codebase.getTypeOfReferent codebase ref
            let prettyType = TypePrinter.prettyStr Nothing ppe typ
            let txtName = HQ'.toText hqNameSuggestion
            let ranges = (diags ^.. folded . range)
            let rca = rangedCodeAction ("Use " <> txtName <> " : " <> Text.pack prettyType) diags ranges
            pure $ includeEdits fileUri txtName ranges rca
    isUserBlank :: Symbol -> Bool
    isUserBlank v = case Var.typeOf v of
      Var.User name -> Text.isPrefixOf "_" name
      _ -> False

toRangeMap :: (Foldable f) => f (Range, a) -> IntervalMap Position [a]
toRangeMap vs =
  IM.fromListWith (<>) (toList vs <&> \(r, a) -> (rangeToInterval r, [a]))

getFileAnalysis :: Uri -> MaybeT Lsp FileAnalysis
getFileAnalysis uri = do
  checkedFilesV <- asks checkedFilesVar
  -- Try to get the file analysis, if there's a var, then read it, waiting if necessary
  -- If there's no var, add one and wait for it to be filled (all Uris should be analyzed
  -- eventually unless theres some bug in the VFS).
  tmvar <- atomically do
    checkedFiles <- readTVar checkedFilesV
    case Map.lookup uri checkedFiles of
      Nothing -> do
        mvar <- newEmptyTMVar
        Debug.debugM Debug.LSP "File analysis requested but none available, waiting for analysis for" uri
        writeTVar checkedFilesV $ Map.insert uri mvar checkedFiles
        pure mvar
      Just mvar -> pure mvar
  atomically (readTMVar tmvar)

-- | Build a Names from a file if it's parseable.
getFileNames :: Uri -> MaybeT Lsp Names
getFileNames fileUri = do
  FileAnalysis {typecheckedFile = tf} <- getFileAnalysis fileUri
  hoistMaybe (UF.typecheckedToNames <$> tf)

-- | Build a PPE which uses names from a given file, falls back to the global PPE.
--
-- Note that currently we can only properly generate names if the file typechecks.
-- Otherwise this will return Nothing
-- TODO memoize per file
ppedForFile :: Uri -> MaybeT Lsp PPED.PrettyPrintEnvDecl
ppedForFile fileUri = do
  fileNames <- getFileNames fileUri
  hl <- asks codebase >>= liftIO . Codebase.hashLength
  let filePPE = PPED.fromNamesDecl hl (NamesWithHistory.fromCurrentNames fileNames)
  gPPED <- lift globalPPE
  pure (filePPE `PPED.addFallback` gPPED)

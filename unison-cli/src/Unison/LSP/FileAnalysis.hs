{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.FileAnalysis where

import Control.Lens
import Control.Monad.Reader
import Crypto.Random qualified as Random
import Data.Align (alignWith)
import Data.Foldable
import Data.IntervalMap.Lazy (IntervalMap)
import Data.IntervalMap.Lazy qualified as IM
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These
import Language.LSP.Types
  ( Diagnostic,
    Position,
    Range,
    TextDocumentIdentifier (TextDocumentIdentifier),
    Uri (getUri),
  )
import Language.LSP.Types.Lens (HasCodeAction (codeAction), HasIsPreferred (isPreferred), HasRange (range), HasUri (uri))
import Language.LSP.Types.Lens qualified as LSPTypes
import Unison.ABT qualified as ABT
import Unison.Cli.TypeCheck (typecheckHelper)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.LSP.Conversions
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.Diagnostics
  ( DiagnosticSeverity (..),
    mkDiagnostic,
    reportDiagnostics,
  )
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import Unison.LSP.Types qualified as LSP
import Unison.LSP.VFS qualified as VFS
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.PrintError qualified as PrintError
import Unison.Result (Note)
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Symbol qualified as Symbol
import Unison.Syntax.HashQualified' qualified as HQ' (toText)
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.TypeError qualified as TypeError
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as R1
import Unison.Var qualified as Var
import Unison.WatchKind (pattern TestWatch)
import UnliftIO.STM

-- | Lex, parse, and typecheck a file.
checkFile :: (HasUri d Uri) => d -> Lsp (Maybe FileAnalysis)
checkFile doc = runMaybeT $ do
  let fileUri = doc ^. uri
  (fileVersion, contents) <- VFS.getFileContents fileUri
  parseNames <- lift getParseNames
  let sourceName = getUri $ doc ^. uri
  let lexedSource@(srcText, tokens) = (contents, L.lexer (Text.unpack sourceName) (Text.unpack contents))
  let ambientAbilities = []
  cb <- asks codebase
  let generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG
  r <- (liftIO $ typecheckHelper cb generateUniqueName ambientAbilities parseNames sourceName lexedSource)
  let Result.Result notes mayResult = r
  let (parsedFile, typecheckedFile) = case mayResult of
        Nothing -> (Nothing, Nothing)
        Just (Left uf) -> (Just uf, Nothing)
        Just (Right tf) -> (Just $ UF.discardTypes tf, Just tf)
  (errDiagnostics, codeActions) <- lift $ analyseFile fileUri srcText notes
  let codeActionRanges =
        codeActions
          & foldMap (\(RangedCodeAction {_codeActionRanges, _codeAction}) -> (,_codeAction) <$> _codeActionRanges)
          & toRangeMap
  let fileSummary = mkFileSummary parsedFile typecheckedFile
  let tokenMap = getTokenMap tokens
  conflictWarningDiagnostics <-
    fold <$> for fileSummary \fs ->
      lift $ computeConflictWarningDiagnostics fileUri fs
  let diagnosticRanges =
        (errDiagnostics <> conflictWarningDiagnostics)
          & fmap (\d -> (d ^. range, d))
          & toRangeMap
  let fileAnalysis = FileAnalysis {diagnostics = diagnosticRanges, codeActions = codeActionRanges, fileSummary, ..}
  pure $ fileAnalysis

-- | If a symbol is a 'User' symbol, return (Just sym), otherwise return Nothing.
assertUserSym :: Symbol -> Maybe Symbol
assertUserSym sym = case sym of
  Symbol.Symbol _ (Var.User {}) -> Just sym
  _ -> Nothing

-- | Summarize the information available to us from the current state of the file.
-- See 'FileSummary' for more information.
mkFileSummary :: Maybe (UF.UnisonFile Symbol Ann) -> Maybe (UF.TypecheckedUnisonFile Symbol Ann) -> Maybe FileSummary
mkFileSummary parsed typechecked = case (parsed, typechecked) of
  (Nothing, Nothing) -> Nothing
  (_, Just tf@(UF.TypecheckedUnisonFileId {dataDeclarationsId', effectDeclarationsId', hashTermsId})) ->
    let (trms, testWatches, exprWatches) =
          hashTermsId & ifoldMap \sym (ann, ref, wk, trm, typ) ->
            case wk of
              Nothing -> (Map.singleton sym (ann, Just ref, trm, getUserTypeAnnotation sym <|> Just typ), mempty, mempty)
              Just TestWatch -> (mempty, [(ann, assertUserSym sym, Just ref, trm, getUserTypeAnnotation sym <|> Just typ)], mempty)
              Just _ -> (mempty, mempty, [(ann, assertUserSym sym, Just ref, trm, getUserTypeAnnotation sym <|> Just typ)])
     in Just $
          FileSummary
            { dataDeclsBySymbol = dataDeclarationsId',
              dataDeclsByReference = declsRefMap dataDeclarationsId',
              effectDeclsBySymbol = effectDeclarationsId',
              effectDeclsByReference = declsRefMap effectDeclarationsId',
              termsBySymbol = trms,
              termsByReference = termsRefMap trms,
              testWatchSummary = testWatches,
              exprWatchSummary = exprWatches,
              fileNames = UF.typecheckedToNames tf
            }
  (Just uf@(UF.UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches}), _) ->
    let trms =
          terms & foldMap \(sym, ann, trm) ->
            (Map.singleton sym (ann, Nothing, trm, Nothing))
        (testWatches, exprWatches) =
          watches & ifoldMap \wk tms ->
            tms & foldMap \(v, ann, trm) ->
              case wk of
                TestWatch -> ([(ann, assertUserSym v, Nothing, trm, Nothing)], mempty)
                _ -> (mempty, [(ann, assertUserSym v, Nothing, trm, Nothing)])
     in Just $
          FileSummary
            { dataDeclsBySymbol = dataDeclarationsId,
              dataDeclsByReference = declsRefMap dataDeclarationsId,
              effectDeclsBySymbol = effectDeclarationsId,
              effectDeclsByReference = declsRefMap effectDeclarationsId,
              termsBySymbol = trms,
              termsByReference = termsRefMap trms,
              testWatchSummary = testWatches,
              exprWatchSummary = exprWatches,
              fileNames = UF.toNames uf
            }
  where
    declsRefMap :: (Ord v, Ord r) => Map v (r, a) -> Map r (Map v a)
    declsRefMap m =
      m
        & Map.toList
        & fmap (\(v, (r, a)) -> (r, Map.singleton v a))
        & Map.fromListWith (<>)
    termsRefMap :: (Ord v, Ord r) => Map v (ann, r, a, b) -> Map r (Map v (ann, a, b))
    termsRefMap m =
      m
        & Map.toList
        & fmap (\(v, (ann, r, a, b)) -> (r, Map.singleton v (ann, a, b)))
        & Map.fromListWith (<>)
    -- Gets the user provided type annotation for a term if there is one.
    -- This type sig will have Ann's within the file if it exists.
    getUserTypeAnnotation :: Symbol -> Maybe (Type Symbol Ann)
    getUserTypeAnnotation v = do
      UF.UnisonFileId {terms, watches} <- parsed
      trm <- (terms <> fold watches) ^? folded . filteredBy (_1 . only v) . _3
      typ <- Term.getTypeAnnotation trm
      pure typ

-- | Get the location of user defined definitions within the file
getFileDefLocations :: Uri -> MaybeT Lsp (Map Symbol (Set Ann))
getFileDefLocations uri = do
  fileDefLocations <$> getFileSummary uri

-- | Compute the location of user defined definitions within the file
fileDefLocations :: FileSummary -> Map Symbol (Set Ann)
fileDefLocations FileSummary {dataDeclsBySymbol, effectDeclsBySymbol, testWatchSummary, exprWatchSummary, termsBySymbol} =
  fold
    [ dataDeclsBySymbol <&> \(_, decl) ->
        decl
          & DD.annotation
          & Set.singleton,
      effectDeclsBySymbol <&> \(_, decl) ->
        decl
          & DD.toDataDecl
          & DD.annotation
          & Set.singleton,
      (testWatchSummary <> exprWatchSummary)
        & foldMap \(ann, maySym, _id, _trm, _typ) ->
          case maySym of
            Nothing -> mempty
            Just sym -> Map.singleton sym (Set.singleton ann),
      termsBySymbol <&> \(ann, _id, _trm, _typ) -> Set.singleton ann
    ]

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
  Debug.debugM Debug.LSP "Freshly Typechecked " (Map.toList freshlyCheckedFiles)
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

analyseFile :: (Foldable f) => Uri -> Text -> f (Note Symbol Ann) -> Lsp ([Diagnostic], [RangedCodeAction])
analyseFile fileUri srcText notes = do
  pped <- PPED.suffixifiedPPE <$> LSP.globalPPED
  (noteDiags, noteActions) <- analyseNotes fileUri pped (Text.unpack srcText) notes
  pure (noteDiags, noteActions)

computeConflictWarningDiagnostics :: Uri -> FileSummary -> Lsp [Diagnostic]
computeConflictWarningDiagnostics fileUri fileSummary@FileSummary {fileNames} = do
  let defLocations = fileDefLocations fileSummary
  conflictedNames <- Names.conflicts . Names.currentNames <$> getParseNames
  let locationForName :: Name -> Set Ann
      locationForName name = fold $ Map.lookup (Name.toVar name) defLocations
  let conflictedTermLocations =
        let fileTerms = R1.toMultimap (Names.terms fileNames)
            conflictedTerms = R1.toMultimap (Names.terms conflictedNames)
         in Map.intersectionWithKey (\name _ _ -> locationForName name) fileTerms conflictedTerms
  let conflictedTypeLocations =
        let fileTypes = R1.toMultimap (Names.types fileNames)
            conflictedTypes = R1.toMultimap (Names.types conflictedNames)
         in Map.intersectionWithKey (\name _ _ -> locationForName name) fileTypes conflictedTypes
  let toDiagnostics annMap =
        annMap
          & Map.toList
          & foldMap \(name, locs) ->
            (mapMaybe Cv.annToRange . Set.toList $ locs)
              <&> \range ->
                let msg = ("`" <> Name.toText name <> "` is conflicted in your codebase")
                    newRangeEnd =
                      range ^. LSPTypes.start
                        & LSPTypes.character +~ fromIntegral (Text.length (Name.toText name))
                    newRange = range & LSPTypes.end .~ newRangeEnd
                 in mkDiagnostic
                      fileUri
                      newRange
                      DsWarning
                      msg
                      mempty
  pure $ toDiagnostics conflictedTermLocations <> toDiagnostics conflictedTypeLocations

getTokenMap :: [L.Token L.Lexeme] -> IM.IntervalMap Position L.Lexeme
getTokenMap tokens =
  tokens
    & mapMaybe
      ( \token ->
          IM.singleton <$> (annToInterval $ Parser.ann token) <*> pure (L.payload token)
      )
    & fold

analyseNotes :: (Foldable f) => Uri -> PrettyPrintEnv -> String -> f (Note Symbol Ann) -> Lsp ([Diagnostic], [RangedCodeAction])
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
            TypeError.RedundantPattern loc -> singleRange loc
            TypeError.UncoveredPatterns loc _pats -> singleRange loc
            -- These type errors don't have custom type error conversions, but some
            -- still have valid diagnostics.
            TypeError.Other e@(Context.ErrorNote {cause}) -> case cause of
              Context.PatternArityMismatch loc _typ _numArgs -> singleRange loc
              Context.HandlerOfUnexpectedType loc _typ -> singleRange loc
              Context.TypeMismatch {} -> shouldHaveBeenHandled e
              Context.IllFormedType {} -> shouldHaveBeenHandled e
              Context.UnknownSymbol loc _ -> singleRange loc
              Context.UnknownTerm loc _ _ _ -> singleRange loc
              Context.AbilityCheckFailure {} -> shouldHaveBeenHandled e
              Context.AbilityEqFailure {} -> shouldHaveBeenHandled e
              Context.EffectConstructorWrongArgCount {} -> shouldHaveBeenHandled e
              Context.MalformedEffectBind {} -> shouldHaveBeenHandled e
              Context.DuplicateDefinitions {} -> shouldHaveBeenHandled e
              Context.UnguardedLetRecCycle {} -> shouldHaveBeenHandled e
              Context.ConcatPatternWithoutConstantLength loc _ -> singleRange loc
              Context.DataEffectMismatch _ _ decl -> singleRange $ DD.annotation decl
              Context.UncoveredPatterns loc _ -> singleRange loc
              Context.RedundantPattern loc -> singleRange loc
              Context.InaccessiblePattern loc -> singleRange loc
          shouldHaveBeenHandled e = do
            Debug.debugM Debug.LSP "This diagnostic should have been handled by a previous case but was not" e
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
          ppe <- PPED.suffixifiedPPE <$> globalPPED
          let cleanedTyp = Context.generalizeAndUnTypeVar typ -- TODO: is this right?
          refs <- liftIO . Codebase.runTransaction codebase $ Codebase.termsOfType codebase cleanedTyp
          forMaybe (toList refs) $ \ref -> runMaybeT $ do
            hqNameSuggestion <- MaybeT . pure $ PPE.terms ppe ref
            typ <- MaybeT . liftIO . Codebase.runTransaction codebase $ Codebase.getTypeOfReferent codebase ref
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

getFileSummary :: Uri -> MaybeT Lsp FileSummary
getFileSummary uri = do
  FileAnalysis {fileSummary} <- getFileAnalysis uri
  MaybeT . pure $ fileSummary

-- TODO memoize per file
ppedForFile :: Uri -> Lsp PPED.PrettyPrintEnvDecl
ppedForFile fileUri = do
  runMaybeT (getFileAnalysis fileUri) >>= \case
    Just (FileAnalysis {typecheckedFile = tf, parsedFile = uf}) ->
      ppedForFileHelper uf tf
    _ -> ppedForFileHelper Nothing Nothing

ppedForFileHelper :: Maybe (UF.UnisonFile Symbol a) -> Maybe (UF.TypecheckedUnisonFile Symbol a) -> Lsp PPED.PrettyPrintEnvDecl
ppedForFileHelper uf tf = do
  codebasePPED <- globalPPED
  hashLen <- asks codebase >>= \codebase -> liftIO (Codebase.runTransaction codebase Codebase.hashLength)
  pure $ case (uf, tf) of
    (Nothing, Nothing) -> codebasePPED
    (_, Just tf) ->
      let fileNames = UF.typecheckedToNames tf
          filePPED = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames fileNames)
       in filePPED `PPED.addFallback` codebasePPED
    (Just uf, _) ->
      let fileNames = UF.toNames uf
          filePPED = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames fileNames)
       in filePPED `PPED.addFallback` codebasePPED

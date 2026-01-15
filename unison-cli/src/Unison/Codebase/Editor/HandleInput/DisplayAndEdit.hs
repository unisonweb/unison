module Unison.Codebase.Editor.HandleInput.DisplayAndEdit
  ( displayI,
    doDisplay,
  )
where

import Control.Error.Util qualified as ErrorUtil
import Control.Lens
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Load (EvalMode (Sandboxed), evalUnisonFile)
import Unison.Codebase.Editor.HandleInput.RuntimeUtils qualified as RuntimeUtils
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.CommandLine.DisplayValues qualified as DisplayValues
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo, empty)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime.Decompile qualified as Decompile
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Pretty qualified as P
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as WK
import UnliftIO.Directory qualified as Directory

displayI ::
  OutputLocation ->
  HQ.HashQualified Name ->
  Cli ()
displayI outputLoc hq = do
  let useRoot = any Name.isAbsolute hq
  (names, pped) <-
    if useRoot
      then do
        root <- Cli.getCurrentProjectRoot
        let root0 = Branch.head root
        let names = Names.makeAbsolute $ Branch.toNames root0
        let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
        pure (names, pped)
      else do
        names <- Cli.currentNames
        let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
        pure (names, pped)
  let suffixifiedPPE = PPE.suffixifiedPPE pped
  let bias = maybeToList $ HQ.toName hq
  latestTypecheckedFile <- Cli.getLatestTypecheckedFile
  case addWatch (Text.unpack (HQ.toText hq)) latestTypecheckedFile of
    Nothing -> do
      let results = Names.lookupHQTerm Names.IncludeSuffixes hq names
      ref <-
        Set.asSingleton results & onNothing do
          Cli.returnEarly
            if Set.null results
              then SearchTermsNotFound [hq]
              else TermAmbiguous suffixifiedPPE hq results
      let tm = Term.fromReferent External ref
      tm <- RuntimeUtils.evalUnisonTerm RuntimeUtils.displayDecompileErrors RuntimeUtils.displayResult Sandboxed (PPE.biasTo bias $ suffixifiedPPE) True tm
      doDisplay outputLoc names (Term.unannotate tm)
    Just (toDisplay, unisonFile) -> do
      let namesWithDefinitionsFromFile = UF.addNamesFromTypeCheckedUnisonFile unisonFile names
      let filePPED = PPED.makePPED (PPE.hqNamer 10 namesWithDefinitionsFromFile) (suffixify namesWithDefinitionsFromFile)

      let suffixifiedFilePPE = PPE.biasTo bias $ PPE.suffixifiedPPE filePPED
      (_, watches) <-
        evalUnisonFile Sandboxed suffixifiedFilePPE unisonFile [] & onLeftM \err ->
          Cli.returnEarly (Output.EvaluationFailure id err)
      (_, _, _, _, tm, _) <-
        Map.lookup toDisplay watches & onNothing (error $ "Evaluation dropped a watch expression: " <> Text.unpack (HQ.toText hq))
      let ns = UF.addNamesFromTypeCheckedUnisonFile unisonFile names
      doDisplay outputLoc ns tm
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

doDisplay :: OutputLocation -> Names -> Term Symbol () -> Cli ()
doDisplay outputLoc names tm = do
  loopState <- State.get
  latestTypecheckedFile <- Cli.getLatestTypecheckedFile
  mayFP <- case outputLoc of
    ConsoleLocation -> pure Nothing
    FileLocation path _ -> Just <$> Directory.canonicalizePath path
    LatestFileLocation _ -> traverse Directory.canonicalizePath $ fmap fst (loopState ^. #latestFile) <|> Just "scratch.u"

  let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
  rendered <- renderDefinition RuntimeUtils.displayDecompileErrors RuntimeUtils.displayResult latestTypecheckedFile pped tm
  whenJust mayFP \fp -> do
    liftIO $ prependFile fp (P.toPlain 80 $ rendered)

  Cli.respond $ DisplayRendered mayFP rendered
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

    prependFile :: FilePath -> Text -> IO ()
    prependFile filePath txt = do
      exists <- Directory.doesFileExist filePath
      if exists
        then do
          existing <- readUtf8 filePath
          writeUtf8 filePath (txt <> "\n\n" <> existing)
        else do
          writeUtf8 filePath txt

renderDefinition :: (MonadReader Cli.Env m, MonadIO m) => ([Decompile.DecompError] -> m ()) -> (P.Pretty P.ColorText -> m ()) -> (Maybe (TypecheckedUnisonFile Symbol a)) -> PrettyPrintEnvDecl -> Term Symbol () -> m DisplayValues.Pretty
renderDefinition renderDecompileError renderResult mayLatestTypecheckedFile pped tm = do
  let suffixifiedPPE = PPE.suffixifiedPPE pped
  Cli.Env {codebase} <- ask
  let (tms, typs) = case mayLatestTypecheckedFile of
        Just latestTypecheckedFile -> do
          UF.indexByReference latestTypecheckedFile
        Nothing -> (Map.empty, Map.empty)
  let useCache = True
      evalTerm tm =
        fmap ErrorUtil.hush . fmap (fmap Term.unannotate) $
          RuntimeUtils.evalUnisonTermE renderDecompileError renderResult Sandboxed suffixifiedPPE useCache (Term.amap (const External) tm)
      loadTerm (Reference.DerivedId r) = case Map.lookup r tms of
        Nothing -> fmap (fmap Term.unannotate) $ liftIO $ Codebase.runTransaction codebase (Codebase.getTerm codebase r)
        Just (_, tm, _) -> pure (Just $ Term.unannotate tm)
      loadTerm _ = pure Nothing
      loadDecl (Reference.DerivedId r) = case Map.lookup r typs of
        Nothing -> fmap (fmap $ DD.amap (const ())) $ liftIO $ Codebase.runTransaction codebase $ Codebase.getTypeDeclaration codebase r
        Just decl -> pure (Just $ DD.amap (const ()) decl)
      loadDecl _ = pure Nothing
      loadTypeOfTerm' (Referent.Ref (Reference.DerivedId r))
        | Just (_, _, ty) <- Map.lookup r tms = pure $ Just (void ty)
      loadTypeOfTerm' r = fmap (fmap void) . liftIO . Codebase.runTransaction codebase . Codebase.getTypeOfReferent codebase $ r
  DisplayValues.displayTerm pped loadTerm loadTypeOfTerm' evalTerm loadDecl tm

-- Adds a watch expression of the given name to the file, if
-- it would resolve to a TLD in the file. Returns the freshened
-- variable name and the new typechecked file.
--
-- Otherwise, returns `Nothing`.
addWatch ::
  (Var v) =>
  String ->
  Maybe (TypecheckedUnisonFile v Ann) ->
  Maybe (v, TypecheckedUnisonFile v Ann)
addWatch _watchName Nothing = Nothing
addWatch watchName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == watchName) . view _1) components
  case mainComponent of
    [(v, ann, tm, ty)] ->
      Just $
        let v2 = Var.freshIn (Set.fromList [v]) v
            a = ABT.annotation tm
         in ( v2,
              UF.typecheckedUnisonFile
                (UF.dataDeclarationsId' uf)
                (UF.effectDeclarationsId' uf)
                (UF.topLevelComponents' uf)
                (UF.watchComponents uf <> [(WK.RegularWatch, [(v2, ann, Term.var a v, ty)])])
            )
    _ -> addWatch watchName Nothing

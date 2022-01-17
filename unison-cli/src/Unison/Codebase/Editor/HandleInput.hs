{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Editor.HandleInput
  ( loop,
    parseSearchType,
  )
where

-- TODO: Don't import backend

import qualified Control.Error.Util as ErrorUtil
import Control.Lens
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Bifunctor (first, second)
import Data.Configurator ()
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.Extra (nubOrd)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (uncurry3)
import qualified Text.Megaparsec as P
import U.Util.Timing (unsafeTime)
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as Builtin
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Builtin.Terms as Builtin
import Unison.Codebase (PushGitBranchOpts (..), Preprocessing (..))
import Unison.Codebase.Branch (Branch (..), Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchDiff as BranchDiff
import qualified Unison.Codebase.BranchUtil as BranchUtil
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.AuthorInfo (AuthorInfo (..))
import Unison.Codebase.Editor.Command as Command
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Editor.HandleInput.LoopState (Action, Action')
import qualified Unison.Codebase.Editor.HandleInput.LoopState as LoopState
import qualified Unison.Codebase.Editor.HandleInput.NamespaceDependencies as NamespaceDependencies
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import qualified Unison.Codebase.Editor.Output.DumpNamespace as Output.DN
import qualified Unison.Codebase.Editor.Propagate as Propagate
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteRemotePath, WriteRepo, printNamespace, writePathToRead, writeToRead)
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.SlurpResult (SlurpResult (..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import qualified Unison.Codebase.Editor.TodoOutput as TO
import qualified Unison.Codebase.Editor.UriParser as UriParser
import qualified Unison.Codebase.MainTerm as MainTerm
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path, Path' (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.Codebase.SqliteCodebase.GitError (GitSqliteCodebaseError(NoDatabaseFile))
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.TermEdit (TermEdit (..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TermEdit.Typing as TermEdit
import Unison.Codebase.Type (GitError(GitSqliteCodebaseError))
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Unison.Codebase.Verbosity as Verbosity
import qualified Unison.CommandLine.DisplayValues as DisplayValues
import qualified Unison.CommandLine.FuzzySelect as Fuzzy
import qualified Unison.CommandLine.InputPattern as InputPattern
import qualified Unison.CommandLine.InputPatterns as InputPatterns
import Unison.ConstructorReference (GConstructorReference(..))
import qualified Unison.DataDeclaration as DD
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import qualified Unison.Lexer as L
import Unison.Name (Name)
import Unison.Position (Position(..))
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import Unison.Names (Names (Names))
import qualified Unison.Names as Names
import Unison.NamesWithHistory (NamesWithHistory (..))
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Names as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import Unison.Reference (Reference (..))
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Result (pattern Result)
import qualified Unison.Result as Result
import Unison.Runtime.IOSource (isTest)
import qualified Unison.Runtime.IOSource as DD
import qualified Unison.Runtime.IOSource as IOSource
import Unison.Server.Backend (ShallowListEntry (..), TermEntry (..), TypeEntry (..))
import qualified Unison.Server.Backend as Backend
import Unison.Server.QueryResult
import Unison.Server.SearchResult (SearchResult)
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.ShortHash as SH
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Type.Names as Type
import qualified Unison.Typechecker as Typechecker
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.Util.Find as Find
import Unison.Util.List (uniqueBy)
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Star3 as Star3
import Unison.Util.TransitiveClosure (transitiveClosure)
import Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.WatchKind as WK
import Unison.Codebase.Editor.HandleInput.LoopState (eval, MonadCommand(..))
import Unison.Util.Free (Free)
import UnliftIO (MonadUnliftIO)
import qualified Data.Set.NonEmpty as NESet
import Data.Set.NonEmpty (NESet)
import Unison.Symbol (Symbol)
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Editor.Slurp as Slurp
import qualified Unison.Codebase.Editor.SlurpResult as SlurpResult

defaultPatchNameSegment :: NameSegment
defaultPatchNameSegment = "patch"

prettyPrintEnvDecl :: MonadCommand n m i v => NamesWithHistory -> n PPE.PrettyPrintEnvDecl
prettyPrintEnvDecl ns = eval CodebaseHashLength <&> (`PPE.fromNamesDecl` ns)

-- | Get a pretty print env decl for the current names at the current path.
currentPrettyPrintEnvDecl :: Action' m v PPE.PrettyPrintEnvDecl
currentPrettyPrintEnvDecl = do
  root' <- use LoopState.root
  currentPath' <- Path.unabsolute <$> use LoopState.currentPath
  prettyPrintEnvDecl (Backend.getCurrentPrettyNames (Backend.AllNames currentPath') root')

loop :: forall m. MonadUnliftIO m => Action m (Either Event Input) Symbol ()
loop = do
  uf <- use LoopState.latestTypecheckedFile
  root' <- use LoopState.root
  currentPath' <- use LoopState.currentPath
  latestFile' <- use LoopState.latestFile
  currentBranch' <- getAt currentPath'
  e <- eval Input
  hqLength <- eval CodebaseHashLength
  sbhLength <- eval BranchHashLength
  let currentPath'' = Path.unabsolute currentPath'
      hqNameQuery q = eval $ HQNameQuery (Just currentPath'') root' q
      sbh = SBH.fromHash sbhLength
      root0 = Branch.head root'
      currentBranch0 = Branch.head currentBranch'
      defaultPatchPath :: PatchPath
      defaultPatchPath = (Path' $ Left currentPath', defaultPatchNameSegment)
      resolveSplit' :: (Path', a) -> (Path, a)
      resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      resolveToAbsolute :: Path' -> Path.Absolute
      resolveToAbsolute = Path.resolve currentPath'
      getAtSplit :: Path.Split -> Maybe (Branch m)
      getAtSplit p = BranchUtil.getBranch p root0
      getAtSplit' :: Path.Split' -> Maybe (Branch m)
      getAtSplit' = getAtSplit . resolveSplit'
      getPatchAtSplit' :: Path.Split' -> Action' m v (Maybe Patch)
      getPatchAtSplit' s = do
        let (p, seg) = Path.toAbsoluteSplit currentPath' s
        b <- getAt p
        eval . Eval $ Branch.getMaybePatch seg (Branch.head b)
      getHQ'TermsIncludingHistorical p =
        getTermsIncludingHistorical (resolveSplit' p) root0

      getHQ'Terms :: Path.HQSplit' -> Set Referent
      getHQ'Terms p = BranchUtil.getTerm (resolveSplit' p) root0
      getHQ'Types :: Path.HQSplit' -> Set Reference
      getHQ'Types p = BranchUtil.getType (resolveSplit' p) root0

      basicPrettyPrintNames :: Names
      basicPrettyPrintNames =
        Backend.basicPrettyPrintNames root' (Backend.AllNames $ Path.unabsolute currentPath')

      resolveHHQS'Types :: HashOrHQSplit' -> Action' m v (Set Reference)
      resolveHHQS'Types =
        either
          (eval . TypeReferencesByShortHash)
          (pure . getHQ'Types)
      -- Term Refs and Cons
      resolveHHQS'Referents =
        either
          (eval . TermReferentsByShortHash)
          (pure . getHQ'Terms)
      getTypes :: Path.Split' -> Set Reference
      getTypes = getHQ'Types . fmap HQ'.NameOnly
      getTerms :: Path.Split' -> Set Referent
      getTerms = getHQ'Terms . fmap HQ'.NameOnly
      getPatchAt :: Path.Split' -> Action' m v Patch
      getPatchAt patchPath' = do
        let (p, seg) = Path.toAbsoluteSplit currentPath' patchPath'
        b <- getAt p
        eval . Eval $ Branch.getPatch seg (Branch.head b)
      withFile ambient sourceName lexed@(text, tokens) k = do
        let getHQ = \case
              L.WordyId s (Just sh) ->
                Just (HQ.HashQualified (Name.unsafeFromString s) sh)
              L.SymbolyId s (Just sh) ->
                Just (HQ.HashQualified (Name.unsafeFromString s) sh)
              L.Hash sh -> Just (HQ.HashOnly sh)
              _ -> Nothing
            hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
        let parseNames = Backend.getCurrentParseNames (Backend.AllNames currentPath'') root'
        LoopState.latestFile .= Just (Text.unpack sourceName, False)
        LoopState.latestTypecheckedFile .= Nothing
        Result notes r <- eval $ Typecheck ambient parseNames sourceName lexed
        case r of
          -- Parsing failed
          Nothing ->
            respond $
              ParseErrors text [err | Result.Parsing err <- toList notes]
          Just (Left errNames) -> do
            ns <- makeShadowedPrintNamesFromHQ hqs errNames
            ppe <- suffixifiedPPE ns
            let tes = [err | Result.TypeError err <- toList notes]
                cbs =
                  [ bug
                    | Result.CompilerBug (Result.TypecheckerBug bug) <-
                        toList notes
                  ]
            when (not $ null tes) . respond $ TypeErrors text ppe tes
            when (not $ null cbs) . respond $ CompilerBugs text ppe cbs
          Just (Right uf) -> k uf
      loadUnisonFile sourceName text = do
        let lexed = L.lexer (Text.unpack sourceName) (Text.unpack text)
        withFile [] sourceName (text, lexed) $ \unisonFile -> do
          currentNames <- currentPathNames
          let sr = Slurp.slurpFile unisonFile mempty Nothing currentNames currentPath'
          names <- displayNames unisonFile
          pped <- prettyPrintEnvDecl names
          let ppe = PPE.suffixifiedPPE pped
          respond $ Typechecked sourceName ppe sr unisonFile
          unlessError' EvaluationFailure do
            (bindings, e) <- ExceptT . eval . Evaluate ppe $ unisonFile
            lift do
              let e' = Map.map go e
                  go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
              unless (null e') $
                respond $ Evaluated text ppe bindings e'
              LoopState.latestTypecheckedFile .= Just unisonFile

  case e of
    Left (IncomingRootBranch hashes) ->
      eval . Notify $
        WarnIncomingRootBranch
          (SBH.fromHash sbhLength $ Branch.headHash root')
          (Set.map (SBH.fromHash sbhLength) hashes)
    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      if maybe False snd latestFile'
        then modifying LoopState.latestFile (fmap (const False) <$>)
        else loadUnisonFile sourceName text
    Right input ->
      let branchNotFound = respond . BranchNotFound
          branchNotFound' = respond . BranchNotFound . Path.unsplit'
          patchNotFound :: Path.Split' -> Action' m v ()
          patchNotFound s = respond $ PatchNotFound s
          patchExists :: Path.Split' -> Action' m v ()
          patchExists s = respond $ PatchAlreadyExists s
          typeNotFound = respond . TypeNotFound
          typeNotFound' = respond . TypeNotFound'
          termNotFound = respond . TermNotFound
          termNotFound' = respond . TermNotFound'
          nameConflicted src tms tys = respond (DeleteNameAmbiguous hqLength src tms tys)
          typeConflicted src = nameConflicted src Set.empty
          termConflicted src tms = nameConflicted src tms Set.empty
          hashConflicted src = respond . HashAmbiguous src
          typeReferences :: [SearchResult] -> [Reference]
          typeReferences rs =
            [r | SR.Tp (SR.TypeResult _ r _) <- rs]
          termReferences :: [SearchResult] -> [Reference]
          termReferences rs =
            [r | SR.Tm (SR.TermResult _ (Referent.Ref r) _) <- rs]
          termResults rs = [r | SR.Tm r <- rs]
          typeResults rs = [r | SR.Tp r <- rs]
          doRemoveReplacement ::
               HQ.HashQualified Name
            -> Maybe PatchPath
            -> Bool
            -> Action' m Symbol ()
          doRemoveReplacement from patchPath isTerm = do
            let patchPath' = fromMaybe defaultPatchPath patchPath
            patch <- getPatchAt patchPath'
            QueryResult misses' hits <- hqNameQuery [from]
            let tpRefs = Set.fromList $ typeReferences hits
                tmRefs = Set.fromList $ termReferences hits
                misses =
                  Set.difference
                    (Set.fromList misses')
                    if isTerm
                      then Set.fromList $ SR.termName <$> termResults hits
                      else Set.fromList $ SR.typeName <$> typeResults hits
                go :: Reference -> Action m (Either Event Input) Symbol ()
                go fr = do
                  let termPatch =
                        over Patch.termEdits (R.deleteDom fr) patch
                      typePatch =
                        over Patch.typeEdits (R.deleteDom fr) patch
                      (patchPath'', patchName) = resolveSplit' patchPath'
                  -- Save the modified patch
                  stepAtM Branch.CompressHistory
                    inputDescription
                    ( patchPath'',
                      Branch.modifyPatches
                        patchName
                        (const (if isTerm then termPatch else typePatch))
                    )
                  -- Say something
                  success
            unless (Set.null misses) $
              respond $ SearchTermsNotFound (Set.toList misses)
            traverse_ go (if isTerm then tmRefs else tpRefs)
          typeExists dest = respond . TypeAlreadyExists dest
          termExists dest = respond . TermAlreadyExists dest
          inputDescription :: LoopState.InputDescription
          inputDescription = case input of
            ForkLocalBranchI src dest -> "fork " <> hp' src <> " " <> p' dest
            MergeLocalBranchI src dest mode -> case mode of
              Branch.RegularMerge -> "merge " <> p' src <> " " <> p' dest
              Branch.SquashMerge -> "merge.squash " <> p' src <> " " <> p' dest
            ResetRootI src -> "reset-root " <> hp' src
            AliasTermI src dest -> "alias.term " <> hhqs' src <> " " <> ps' dest
            AliasTypeI src dest -> "alias.type " <> hhqs' src <> " " <> ps' dest
            AliasManyI srcs dest ->
              "alias.many " <> intercalateMap " " hqs srcs <> " " <> p' dest
            MoveTermI src dest -> "move.term " <> hqs' src <> " " <> ps' dest
            MoveTypeI src dest -> "move.type " <> hqs' src <> " " <> ps' dest
            MoveBranchI src dest -> "move.namespace " <> ops' src <> " " <> ps' dest
            MovePatchI src dest -> "move.patch " <> ps' src <> " " <> ps' dest
            CopyPatchI src dest -> "copy.patch " <> ps' src <> " " <> ps' dest
            DeleteI thing -> "delete " <> hqs' thing
            DeleteTermI def -> "delete.term " <> hqs' def
            DeleteTypeI def -> "delete.type " <> hqs' def
            DeleteBranchI Try opath -> "delete.namespace " <> ops' opath
            DeleteBranchI Force opath -> "delete.namespace.force " <> ops' opath
            DeletePatchI path -> "delete.patch " <> ps' path
            ReplaceI src target p ->
              "replace " <> HQ.toText src <> " "
                <> HQ.toText target
                <> " "
                <> opatch p
            ResolveTermNameI path -> "resolve.termName " <> hqs' path
            ResolveTypeNameI path -> "resolve.typeName " <> hqs' path
            AddI _selection -> "add"
            UpdateI p _selection -> "update " <> opatch p
            PropagatePatchI p scope -> "patch " <> ps' p <> " " <> p' scope
            UndoI {} -> "undo"
            ApiI -> "api"
            UiI -> "ui"
            DocsToHtmlI path dir -> "docs.to-html " <> Path.toText' path <> " " <> Text.pack dir
            ExecuteI s args -> "execute " <> (Text.unwords . fmap Text.pack $ (s : args))
            IOTestI hq -> "io.test " <> HQ.toText hq
            LinkI md defs ->
              "link " <> HQ.toText md <> " " <> intercalateMap " " hqs' defs
            UnlinkI md defs ->
              "unlink " <> HQ.toText md <> " " <> intercalateMap " " hqs' defs
            UpdateBuiltinsI -> "builtins.update"
            MergeBuiltinsI -> "builtins.merge"
            MergeIOBuiltinsI -> "builtins.mergeio"
            MakeStandaloneI out nm ->
              "compile.output " <> Text.pack out <> " " <> HQ.toText nm
            PullRemoteBranchI orepo dest _syncMode pullMode _ ->
              (Text.pack . InputPattern.patternName $
                case pullMode of
                  PullWithoutHistory -> InputPatterns.pullWithoutHistory
                  PullWithHistory -> InputPatterns.pull
              )
                <> " "
                -- todo: show the actual config-loaded namespace
                <> maybe
                  "(remote namespace from .unisonConfig)"
                  (uncurry3 printNamespace)
                  orepo
                <> " "
                <> p' dest
            CreateMessage {} -> wat
            LoadI {} -> wat
            PreviewAddI {} -> wat
            PreviewUpdateI {} -> wat
            CreateAuthorI (NameSegment id) name -> "create.author " <> id <> " " <> name
            CreatePullRequestI {} -> wat
            LoadPullRequestI base head dest ->
              "pr.load "
                <> uncurry3 printNamespace base
                <> " "
                <> uncurry3 printNamespace head
                <> " "
                <> p' dest
            PushRemoteBranchI {} -> wat
            PreviewMergeLocalBranchI {} -> wat
            DiffNamespaceI {} -> wat
            SwitchBranchI {} -> wat
            UpI {} -> wat
            PopBranchI {} -> wat
            NamesI {} -> wat
            TodoI {} -> wat
            ListEditsI {} -> wat
            ListDependenciesI {} -> wat
            ListDependentsI {} -> wat
            NamespaceDependenciesI {} -> wat
            HistoryI {} -> wat
            TestI {} -> wat
            LinksI {} -> wat
            SearchByNameI {} -> wat
            FindShallowI {} -> wat
            FindPatchI {} -> wat
            ShowDefinitionI {} -> wat
            DisplayI {} -> wat
            DocsI {} -> wat
            ShowDefinitionByPrefixI {} -> wat
            ShowReflogI {} -> wat
            DebugNumberedArgsI {} -> wat
            DebugTypecheckedUnisonFileI {} -> wat
            DebugDumpNamespacesI {} -> wat
            DebugDumpNamespaceSimpleI {} -> wat
            DebugClearWatchI {} -> wat
            QuitI {} -> wat
            DeprecateTermI {} -> undefined
            DeprecateTypeI {} -> undefined
            GistI {} -> wat
            RemoveTermReplacementI src p ->
              "delete.term-replacement" <> HQ.toText src <> " " <> opatch p
            RemoveTypeReplacementI src p ->
              "delete.type-replacement" <> HQ.toText src <> " " <> opatch p
            where
              hp' = either (Text.pack . show) p'
              p' = Text.pack . show . resolveToAbsolute
              ops' = maybe "." ps'
              opatch = ps' . fromMaybe defaultPatchPath
              wat = error $ show input ++ " is not expected to alter the branch"
              hhqs' (Left sh) = SH.toText sh
              hhqs' (Right x) = hqs' x
              hqs' (p, hq) =
                Monoid.unlessM (Path.isRoot' p) (p' p) <> "." <> Text.pack (show hq)
              hqs (p, hq) = hqs' (Path' . Right . Path.Relative $ p, hq)
              ps' = p' . Path.unsplit'
          stepAt = Unison.Codebase.Editor.HandleInput.stepAt inputDescription
          stepManyAt = Unison.Codebase.Editor.HandleInput.stepManyAt inputDescription
          updateRoot = flip Unison.Codebase.Editor.HandleInput.updateRoot inputDescription
          syncRoot = use LoopState.root >>= updateRoot
          updateAtM ::
            Path.Absolute
            -> (Branch m -> Action m i v1 (Branch m))
            -> Action m i v1 Bool
          updateAtM = Unison.Codebase.Editor.HandleInput.updateAtM inputDescription
          unlessGitError = unlessError' Output.GitError
          importRemoteBranch ns mode preprocess =
            ExceptT . eval $ ImportRemoteBranch ns mode preprocess
          loadSearchResults = eval . LoadSearchResults
          saveAndApplyPatch patchPath'' patchName patch' = do
            stepAtM Branch.CompressHistory
              (inputDescription <> " (1/2)")
              ( patchPath'',
                Branch.modifyPatches patchName (const patch')
              )
            -- Apply the modified patch to the current path
            -- since we might be able to propagate further.
            void $ propagatePatch inputDescription patch' currentPath'
            -- Say something
            success
          previewResponse sourceName sr uf = do
            names <- displayNames uf
            ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl names
            respond $ Typechecked (Text.pack sourceName) ppe sr uf

          delete ::
            (Path.HQSplit' -> Set Referent) -> -- compute matching terms
            (Path.HQSplit' -> Set Reference) -> -- compute matching types
            Path.HQSplit' ->
            Action' m v ()
          delete getHQ'Terms getHQ'Types hq = do
            let matchingTerms = toList (getHQ'Terms hq)
            let matchingTypes = toList (getHQ'Types hq)
            case (matchingTerms, matchingTypes) of
              ([], []) -> respond (NameNotFound hq)
              (Set.fromList -> tms, Set.fromList -> tys) -> goMany tms tys
            where
              resolvedPath = resolveSplit' (HQ'.toName <$> hq)
              goMany tms tys = do
                let rootNames = Branch.toNames root0
                    name = Path.toName (Path.unsplit resolvedPath)
                    toRel :: Ord ref => Set ref -> R.Relation Name ref
                    toRel = R.fromList . fmap (name,) . toList
                    -- these names are relative to the root
                    toDelete = Names (toRel tms) (toRel tys)
                endangerments <-
                  getEndangeredDependents (eval . GetDependents) toDelete rootNames
                if null endangerments
                  then do
                    let makeDeleteTermNames = fmap (BranchUtil.makeDeleteTermName resolvedPath) . toList $ tms
                    let makeDeleteTypeNames = fmap (BranchUtil.makeDeleteTypeName resolvedPath) . toList $ tys
                    stepManyAt Branch.CompressHistory (makeDeleteTermNames ++ makeDeleteTypeNames)
                    root'' <- use LoopState.root
                    diffHelper (Branch.head root') (Branch.head root'')
                      >>= respondNumbered . uncurry ShowDiffAfterDeleteDefinitions
                  else do
                    ppeDecl <- currentPrettyPrintEnvDecl
                    respondNumbered $ CantDeleteDefinitions ppeDecl endangerments
       in case input of
            ApiI -> eval API
            CreateMessage pretty ->
              respond $ PrintMessage pretty
            ShowReflogI -> do
              entries <- convertEntries Nothing [] <$> eval LoadReflog
              LoopState.numberedArgs .= fmap (('#' :) . SBH.toString . Output.hash) entries
              respond $ ShowReflog entries
              where
                -- reverses & formats entries, adds synthetic entries when there is a
                -- discontinuity in the reflog.
                convertEntries ::
                  Maybe Branch.Hash ->
                  [Output.ReflogEntry] ->
                  [Reflog.Entry Branch.Hash] ->
                  [Output.ReflogEntry]
                convertEntries _ acc [] = acc
                convertEntries Nothing acc entries@(Reflog.Entry old _ _ : _) =
                  convertEntries
                    (Just old)
                    (Output.ReflogEntry (SBH.fromHash sbhLength old) "(initial reflogged namespace)" : acc)
                    entries
                convertEntries (Just lastHash) acc entries@(Reflog.Entry old new reason : rest) =
                  if lastHash /= old
                    then
                      convertEntries
                        (Just old)
                        (Output.ReflogEntry (SBH.fromHash sbhLength old) "(external change)" : acc)
                        entries
                    else
                      convertEntries
                        (Just new)
                        (Output.ReflogEntry (SBH.fromHash sbhLength new) reason : acc)
                        rest
            ResetRootI src0 ->
              case src0 of
                Left hash -> unlessError do
                  newRoot <- resolveShortBranchHash hash
                  lift do
                    updateRoot newRoot
                    success
                Right path' -> do
                  newRoot <- getAt $ resolveToAbsolute path'
                  if Branch.isEmpty newRoot
                    then respond $ BranchNotFound path'
                    else do
                      updateRoot newRoot
                      success
            ForkLocalBranchI src0 dest0 -> do
              let tryUpdateDest srcb dest0 = do
                    let dest = resolveToAbsolute dest0
                    -- if dest isn't empty: leave dest unchanged, and complain.
                    destb <- getAt dest
                    if Branch.isEmpty0 (Branch.head destb)
                      then do
                        ok <- updateAtM dest (const $ pure srcb)
                        if ok then success else respond $ BranchEmpty src0
                      else respond $ BranchAlreadyExists dest0
              case src0 of
                Left hash -> unlessError do
                  srcb <- resolveShortBranchHash hash
                  lift $ tryUpdateDest srcb dest0
                Right path' -> do
                  srcb <- getAt $ resolveToAbsolute path'
                  if Branch.isEmpty srcb
                    then respond $ BranchNotFound path'
                    else tryUpdateDest srcb dest0
            MergeLocalBranchI src0 dest0 mergeMode -> do
              let [src, dest] = resolveToAbsolute <$> [src0, dest0]
              srcb <- getAt src
              if Branch.isEmpty srcb
                then branchNotFound src0
                else do
                  let err = Just $ MergeAlreadyUpToDate src0 dest0
                  mergeBranchAndPropagateDefaultPatch mergeMode inputDescription err srcb (Just dest0) dest
            PreviewMergeLocalBranchI src0 dest0 -> do
              let [src, dest] = resolveToAbsolute <$> [src0, dest0]
              srcb <- getAt src
              if Branch.isEmpty srcb
                then branchNotFound src0
                else do
                  destb <- getAt dest
                  merged <- eval $ Merge Branch.RegularMerge srcb destb
                  if merged == destb
                    then respond (PreviewMergeAlreadyUpToDate src0 dest0)
                    else
                      diffHelper (Branch.head destb) (Branch.head merged)
                        >>= respondNumbered . uncurry (ShowDiffAfterMergePreview dest0 dest)
            DiffNamespaceI before after -> unlessError do
              let (absBefore, absAfter) = (resolveToAbsolute <$> before, resolveToAbsolute <$> after)
              beforeBranch0 <- Branch.head <$> branchForBranchId absBefore
              afterBranch0 <- Branch.head <$> branchForBranchId absAfter
              lift $ case (Branch.isEmpty0 beforeBranch0, Branch.isEmpty0 afterBranch0) of
                (True, True) -> respond . NamespaceEmpty $ (absBefore Nel.:| [absAfter])
                (True, False) -> respond . NamespaceEmpty $ (absBefore Nel.:| [])
                (False, True) -> respond . NamespaceEmpty $ (absAfter Nel.:| [])
                _ -> do
                  (ppe, outputDiff) <- diffHelper beforeBranch0 afterBranch0
                  respondNumbered $
                    ShowDiffNamespace
                      (resolveToAbsolute <$> before)
                      (resolveToAbsolute <$> after)
                      ppe
                      outputDiff
            CreatePullRequestI baseRepo headRepo -> do
              result <- join @(Either GitError) <$> viewRemoteBranch baseRepo \baseBranch -> do
                 viewRemoteBranch headRepo \headBranch -> do
                   merged <- eval $ Merge Branch.RegularMerge baseBranch headBranch
                   (ppe, diff) <- diffHelperCmd root' currentPath' (Branch.head baseBranch) (Branch.head merged)
                   pure $ ShowDiffAfterCreatePR baseRepo headRepo ppe diff
              case result of
                Left gitErr -> respond (Output.GitError gitErr)
                Right diff -> respondNumbered diff
            LoadPullRequestI baseRepo headRepo dest0 -> do
              let desta = resolveToAbsolute dest0
              let dest = Path.unabsolute desta
              destb <- getAt desta
              if Branch.isEmpty0 (Branch.head destb)
                then unlessGitError do
                  baseb <- importRemoteBranch baseRepo SyncMode.ShortCircuit Unmodified
                  headb <- importRemoteBranch headRepo SyncMode.ShortCircuit Unmodified
                  lift $ do
                    mergedb <- eval $ Merge Branch.RegularMerge baseb headb
                    squashedb <- eval $ Merge Branch.SquashMerge headb baseb
                    stepManyAt Branch.AllowRewritingHistory
                      [ BranchUtil.makeSetBranch (dest, "base") baseb,
                        BranchUtil.makeSetBranch (dest, "head") headb,
                        BranchUtil.makeSetBranch (dest, "merged") mergedb,
                        BranchUtil.makeSetBranch (dest, "squashed") squashedb
                      ]
                    let base = snoc dest0 "base"
                        head = snoc dest0 "head"
                        merged = snoc dest0 "merged"
                        squashed = snoc dest0 "squashed"
                    respond $ LoadPullRequest baseRepo headRepo base head merged squashed
                    loadPropagateDiffDefaultPatch
                      inputDescription
                      (Just merged)
                      (snoc desta "merged")
                else respond . BranchNotEmpty . Path.Path' . Left $ currentPath'

            -- move the LoopState.root to a sub-branch
            MoveBranchI Nothing dest -> do
              b <- use LoopState.root
              -- Overwrite history at destination.
              stepManyAt Branch.AllowRewritingHistory
                [ (Path.empty, const Branch.empty0),
                  BranchUtil.makeSetBranch (resolveSplit' dest) b
                ]
              success
            MoveBranchI (Just src) dest -> unlessError $ do
              srcBranch <- case getAtSplit' src of
                Just existingSrc | not (Branch.isEmpty0 (Branch.head existingSrc)) -> do
                  pure existingSrc
                _ -> throwError $ BranchNotFound (Path.unsplit' src)
              case getAtSplit' dest of
                Just existingDest
                  | not (Branch.isEmpty0 (Branch.head existingDest)) -> do
                    -- Branch exists and isn't empty, print an error
                    throwError (BranchAlreadyExists (Path.unsplit' dest))
                _ -> pure ()
              -- allow rewriting history to ensure we move the branch's history too.
              lift $ stepManyAt Branch.AllowRewritingHistory
                [ BranchUtil.makeDeleteBranch (resolveSplit' src),
                  BranchUtil.makeSetBranch (resolveSplit' dest) srcBranch
                ]
              lift $ success -- could give rando stats about new defns
            MovePatchI src dest -> do
              psrc <- getPatchAtSplit' src
              pdest <- getPatchAtSplit' dest
              case (psrc, pdest) of
                (Nothing, _) -> patchNotFound src
                (_, Just _) -> patchExists dest
                (Just p, Nothing) -> do
                  stepManyAt
                    Branch.CompressHistory
                    [ BranchUtil.makeDeletePatch (resolveSplit' src),
                      BranchUtil.makeReplacePatch (resolveSplit' dest) p
                    ]
                  success
            CopyPatchI src dest -> do
              psrc <- getPatchAtSplit' src
              pdest <- getPatchAtSplit' dest
              case (psrc, pdest) of
                (Nothing, _) -> patchNotFound src
                (_, Just _) -> patchExists dest
                (Just p, Nothing) -> do
                  stepAt
                    Branch.CompressHistory
                    (BranchUtil.makeReplacePatch (resolveSplit' dest) p)
                  success
            DeletePatchI src -> do
              psrc <- getPatchAtSplit' src
              case psrc of
                Nothing -> patchNotFound src
                Just _ -> do
                  stepAt
                    Branch.CompressHistory
                    (BranchUtil.makeDeletePatch (resolveSplit' src))
                  success
            DeleteBranchI insistence Nothing -> do
              hasConfirmed <- confirmedCommand input
              if (hasConfirmed || insistence == Force)
                then do stepAt
                          Branch.CompressHistory  -- Wipe out all definitions, but keep root branch history.
                          (Path.empty, const Branch.empty0)
                        respond DeletedEverything
                else respond DeleteEverythingConfirmation
            DeleteBranchI insistence (Just p) -> do
              case getAtSplit' p of
                Nothing -> branchNotFound' p
                Just (Branch.head -> b0) -> do
                  endangerments <- computeEndangerments b0
                  if null endangerments
                     then doDelete b0
                     else case insistence of
                       Force -> do
                         ppeDecl <- currentPrettyPrintEnvDecl
                         doDelete b0
                         respondNumbered $ DeletedDespiteDependents ppeDecl endangerments
                       Try -> do
                         ppeDecl <- currentPrettyPrintEnvDecl
                         respondNumbered $ CantDeleteNamespace ppeDecl endangerments
              where
                doDelete b0 = do
                      stepAt Branch.CompressHistory $ BranchUtil.makeDeleteBranch (resolveSplit' p)
                      -- Looks similar to the 'toDelete' above... investigate me! ;)
                      diffHelper b0 Branch.empty0
                        >>= respondNumbered
                          . uncurry
                            ( ShowDiffAfterDeleteBranch $
                                resolveToAbsolute (Path.unsplit' p)
                            )
                computeEndangerments :: Branch0 m1 -> Action' m v (Map LabeledDependency (NESet LabeledDependency))
                computeEndangerments b0 = do
                  let rootNames = Branch.toNames root0
                      toDelete =
                        Names.prefix0
                          (Path.toName . Path.unsplit . resolveSplit' $ p) -- resolveSplit' incorporates currentPath
                          (Branch.toNames b0)
                  getEndangeredDependents (eval . GetDependents) toDelete rootNames
            SwitchBranchI maybePath' -> do
              mpath' <- case maybePath' of
                Nothing ->
                  fuzzySelectNamespace Absolute root0 >>= \case
                    -- Shouldn't be possible to get multiple paths here, we can just take
                    -- the first.
                    Just (p : _) -> pure $ Just p
                    _ -> respond (HelpMessage InputPatterns.cd) $> Nothing
                Just p -> pure $ Just p
              case mpath' of
                Nothing -> pure ()
                Just path' -> do
                  let path = resolveToAbsolute path'
                  LoopState.currentPathStack %= Nel.cons path
                  branch' <- getAt path
                  when (Branch.isEmpty0 $ Branch.head branch') (respond $ CreatedNewBranch path)
            UpI ->
              use LoopState.currentPath >>= \p -> case Path.unsnoc (Path.unabsolute p) of
                Nothing -> pure ()
                Just (path, _) -> LoopState.currentPathStack %= Nel.cons (Path.Absolute path)
            PopBranchI ->
              use (LoopState.currentPathStack . to Nel.uncons) >>= \case
                (_, Nothing) -> respond StartOfCurrentPathHistory
                (_, Just t) -> LoopState.currentPathStack .= t
            HistoryI resultsCap diffCap from -> case from of
              Left hash -> unlessError do
                b <- resolveShortBranchHash hash
                lift $ doHistory 0 b []
              Right path' -> do
                let path = resolveToAbsolute path'
                branch' <- getAt path
                if Branch.isEmpty branch'
                  then respond $ CreatedNewBranch path
                  else doHistory 0 branch' []
              where
                doHistory !n b acc =
                  if maybe False (n >=) resultsCap
                    then respond $ History diffCap acc (PageEnd (sbh $ Branch.headHash b) n)
                    else case Branch._history b of
                      Causal.One {} ->
                        respond $ History diffCap acc (EndOfLog . sbh $ Branch.headHash b)
                      Causal.Merge {Causal.tails} ->
                        respond $ History diffCap acc (MergeTail (sbh $ Branch.headHash b) . map sbh $ Map.keys tails)
                      Causal.Cons {Causal.tail} -> do
                        b' <- fmap Branch.Branch . eval . Eval $ snd tail
                        let elem = (sbh $ Branch.headHash b, Branch.namesDiff b' b)
                        doHistory (n + 1) b' (elem : acc)
            UndoI -> do
              prev <- eval . Eval $ Branch.uncons root'
              case prev of
                Nothing ->
                  respond . CantUndo $
                    if Branch.isOne root'
                      then CantUndoPastStart
                      else CantUndoPastMerge
                Just (_, prev) -> do
                  updateRoot prev
                  diffHelper (Branch.head prev) (Branch.head root')
                    >>= respondNumbered . uncurry Output.ShowDiffAfterUndo
            UiI -> eval UI
            DocsToHtmlI namespacePath' sourceDirectory -> do
              let absPath = Path.unabsolute $ resolveToAbsolute namespacePath'
              eval (DocsToHtml root' absPath sourceDirectory)
            AliasTermI src dest -> do
              referents <- resolveHHQS'Referents src
              case (toList referents, toList (getTerms dest)) of
                ([r], []) -> do
                  stepAt Branch.CompressHistory (BranchUtil.makeAddTermName (resolveSplit' dest) r (oldMD r))
                  success
                ([_], rs@(_ : _)) -> termExists dest (Set.fromList rs)
                ([], _) -> either termNotFound' termNotFound src
                (rs, _) ->
                  either hashConflicted termConflicted src (Set.fromList rs)
              where
                oldMD r =
                  either
                    (const mempty)
                    ( \src ->
                        let p = resolveSplit' src
                         in BranchUtil.getTermMetadataAt p r root0
                    )
                    src
            AliasTypeI src dest -> do
              refs <- resolveHHQS'Types src
              case (toList refs, toList (getTypes dest)) of
                ([r], []) -> do
                  stepAt Branch.CompressHistory (BranchUtil.makeAddTypeName (resolveSplit' dest) r (oldMD r))
                  success
                ([_], rs@(_ : _)) -> typeExists dest (Set.fromList rs)
                ([], _) -> either typeNotFound' typeNotFound src
                (rs, _) ->
                  either
                    (\src -> hashConflicted src . Set.map Referent.Ref)
                    typeConflicted
                    src
                    (Set.fromList rs)
              where
                oldMD r =
                  either
                    (const mempty)
                    ( \src ->
                        let p = resolveSplit' src
                         in BranchUtil.getTypeMetadataAt p r root0
                    )
                    src

            -- this implementation will happily produce name conflicts,
            -- but will surface them in a normal diff at the end of the operation.
            AliasManyI srcs dest' -> do
              let destAbs = resolveToAbsolute dest'
              old <- getAt destAbs
              let (unknown, actions) = foldl' go mempty srcs
              stepManyAt Branch.CompressHistory actions
              new <- getAt destAbs
              diffHelper (Branch.head old) (Branch.head new)
                >>= respondNumbered . uncurry (ShowDiffAfterModifyBranch dest' destAbs)
              unless (null unknown) $
                respond . SearchTermsNotFound . fmap fixupOutput $ unknown
              where
                -- a list of missing sources (if any) and the actions that do the work
                go ::
                  ([Path.HQSplit], [(Path, Branch0 m -> Branch0 m)]) ->
                  Path.HQSplit ->
                  ([Path.HQSplit], [(Path, Branch0 m -> Branch0 m)])
                go (missingSrcs, actions) hqsrc =
                  let src :: Path.Split
                      src = second HQ'.toName hqsrc
                      proposedDest :: Path.Split
                      proposedDest = second HQ'.toName hqProposedDest
                      hqProposedDest :: Path.HQSplit
                      hqProposedDest =
                        first Path.unabsolute $
                          Path.resolve (resolveToAbsolute dest') hqsrc
                      -- `Nothing` if src doesn't exist
                      doType :: Maybe [(Path, Branch0 m -> Branch0 m)]
                      doType = case ( BranchUtil.getType hqsrc currentBranch0,
                                      BranchUtil.getType hqProposedDest root0
                                    ) of
                        (null -> True, _) -> Nothing -- missing src
                        (rsrcs, existing) ->
                          -- happy path
                          Just . map addAlias . toList $ Set.difference rsrcs existing
                          where
                            addAlias r = BranchUtil.makeAddTypeName proposedDest r (oldMD r)
                            oldMD r = BranchUtil.getTypeMetadataAt src r currentBranch0
                      doTerm :: Maybe [(Path, Branch0 m -> Branch0 m)]
                      doTerm = case ( BranchUtil.getTerm hqsrc currentBranch0,
                                      BranchUtil.getTerm hqProposedDest root0
                                    ) of
                        (null -> True, _) -> Nothing -- missing src
                        (rsrcs, existing) ->
                          Just . map addAlias . toList $ Set.difference rsrcs existing
                          where
                            addAlias r = BranchUtil.makeAddTermName proposedDest r (oldMD r)
                            oldMD r = BranchUtil.getTermMetadataAt src r currentBranch0
                   in case (doType, doTerm) of
                        (Nothing, Nothing) -> (missingSrcs :> hqsrc, actions)
                        (Just as, Nothing) -> (missingSrcs, actions ++ as)
                        (Nothing, Just as) -> (missingSrcs, actions ++ as)
                        (Just as1, Just as2) -> (missingSrcs, actions ++ as1 ++ as2)

                fixupOutput :: Path.HQSplit -> HQ.HashQualified Name
                fixupOutput = fmap Path.toName . HQ'.toHQ . Path.unsplitHQ
            NamesI thing -> do
              ns0 <- basicParseNames
              let ns = NamesWithHistory ns0 mempty
                  terms = NamesWithHistory.lookupHQTerm thing ns
                  types = NamesWithHistory.lookupHQType thing ns
                  printNames = NamesWithHistory basicPrettyPrintNames mempty
                  terms' :: Set (Referent, Set (HQ'.HashQualified Name))
                  terms' = Set.map go terms
                    where
                      go r = (r, NamesWithHistory.termName hqLength r printNames)
                  types' :: Set (Reference, Set (HQ'.HashQualified Name))
                  types' = Set.map go types
                    where
                      go r = (r, NamesWithHistory.typeName hqLength r printNames)
              respond $ ListNames hqLength (toList types') (toList terms')
            LinkI mdValue srcs -> do
              manageLinks False srcs [mdValue] Metadata.insert
              syncRoot
            UnlinkI mdValue srcs -> do
              manageLinks False srcs [mdValue] Metadata.delete
              syncRoot

            -- > links List.map (.Docs .English)
            -- > links List.map -- give me all the
            -- > links Optional License
            LinksI src mdTypeStr -> unlessError do
              (ppe, out) <- getLinks (show input) src (Right mdTypeStr)
              lift do
                LoopState.numberedArgs .= fmap (HQ.toString . view _1) out
                respond $ ListOfLinks ppe out
            DocsI srcs -> do
              srcs' <- case srcs of
                [] ->
                  fuzzySelectDefinition Absolute root0 >>= \case
                    Nothing -> do
                      respond (HelpMessage InputPatterns.docs)
                      pure []
                    Just defs -> do
                      -- HQ names should always parse as a valid split, so we just discard any
                      -- that don't to satisfy the type-checker.
                      pure . mapMaybe (eitherToMaybe . Path.parseHQSplit' . HQ.toString) $ defs
                xs -> pure xs
              for_ srcs' (docsI (show input) basicPrettyPrintNames)
            CreateAuthorI authorNameSegment authorFullName -> do
              initialBranch <- getAt currentPath'
              AuthorInfo
                guid@(guidRef, _, _)
                author@(authorRef, _, _)
                copyrightHolder@(copyrightHolderRef, _, _) <-
                eval $ CreateAuthorInfo authorFullName
              -- add the new definitions to the codebase and to the namespace
              traverse_ (eval . uncurry3 PutTerm) [guid, author, copyrightHolder]
              stepManyAt Branch.CompressHistory
                [ BranchUtil.makeAddTermName (resolveSplit' authorPath) (d authorRef) mempty,
                  BranchUtil.makeAddTermName (resolveSplit' copyrightHolderPath) (d copyrightHolderRef) mempty,
                  BranchUtil.makeAddTermName (resolveSplit' guidPath) (d guidRef) mempty
                ]
              finalBranch <- getAt currentPath'
              -- print some output
              diffHelper (Branch.head initialBranch) (Branch.head finalBranch)
                >>= respondNumbered
                  . uncurry
                    ( ShowDiffAfterCreateAuthor
                        authorNameSegment
                        (Path.unsplit' base)
                        currentPath'
                    )
              where
                d :: Reference.Id -> Referent
                d = Referent.Ref . Reference.DerivedId
                base :: Path.Split' = (Path.relativeEmpty', "metadata")
                authorPath = base |> "authors" |> authorNameSegment
                copyrightHolderPath = base |> "copyrightHolders" |> authorNameSegment
                guidPath = authorPath |> "guid"
            MoveTermI src dest ->
              case (toList (getHQ'Terms src), toList (getTerms dest)) of
                ([r], []) -> do
                  stepManyAt Branch.CompressHistory
                    [ BranchUtil.makeDeleteTermName p r,
                      BranchUtil.makeAddTermName (resolveSplit' dest) r (mdSrc r)
                    ]
                  success
                ([_], rs) -> termExists dest (Set.fromList rs)
                ([], _) -> termNotFound src
                (rs, _) -> termConflicted src (Set.fromList rs)
              where
                p = resolveSplit' (HQ'.toName <$> src)
                mdSrc r = BranchUtil.getTermMetadataAt p r root0
            MoveTypeI src dest ->
              case (toList (getHQ'Types src), toList (getTypes dest)) of
                ([r], []) -> do
                  stepManyAt Branch.CompressHistory
                    [ BranchUtil.makeDeleteTypeName p r,
                      BranchUtil.makeAddTypeName (resolveSplit' dest) r (mdSrc r)
                    ]
                  success
                ([_], rs) -> typeExists dest (Set.fromList rs)
                ([], _) -> typeNotFound src
                (rs, _) -> typeConflicted src (Set.fromList rs)
              where
                p = resolveSplit' (HQ'.toName <$> src)
                mdSrc r = BranchUtil.getTypeMetadataAt p r root0
            DeleteI hq -> delete getHQ'Terms getHQ'Types hq
            DeleteTypeI hq -> delete (const Set.empty) getHQ'Types hq
            DeleteTermI hq -> delete getHQ'Terms (const Set.empty) hq
            DisplayI outputLoc names' -> do
              names <- case names' of
                [] -> fuzzySelectDefinition Absolute root0 >>= \case
                        Nothing -> respond (HelpMessage InputPatterns.display) $> []
                        Just defs -> pure defs
                ns -> pure ns
              traverse_ (displayI basicPrettyPrintNames outputLoc) names
            ShowDefinitionI outputLoc query -> handleShowDefinition outputLoc query
            FindPatchI -> do
              let patches =
                    [ Path.toName $ Path.snoc p seg
                      | (p, b) <- Branch.toList0 currentBranch0,
                        (seg, _) <- Map.toList (Branch._edits b)
                    ]
              respond $ ListOfPatches $ Set.fromList patches
              LoopState.numberedArgs .= fmap Name.toString patches
            FindShallowI pathArg -> do
              let pathArgAbs = resolveToAbsolute pathArg
                  ppe =
                    Backend.basicSuffixifiedNames
                      sbhLength
                      root'
                      (Backend.AllNames $ Path.fromPath' pathArg)
              res <- eval $ FindShallow pathArgAbs
              case res of
                Left e -> handleBackendError e
                Right entries -> do
                  -- caching the result as an absolute path, for easier jumping around
                  LoopState.numberedArgs .= fmap entryToHQString entries
                  respond $ ListShallow ppe entries
                  where
                    entryToHQString :: ShallowListEntry v Ann -> String
                    entryToHQString e =
                      fixup $ case e of
                        ShallowTypeEntry (TypeEntry _ hq _) -> HQ'.toString hq
                        ShallowTermEntry (TermEntry _ hq _ _) -> HQ'.toString hq
                        ShallowBranchEntry ns _ _ -> NameSegment.toString ns
                        ShallowPatchEntry ns -> NameSegment.toString ns
                      where
                        fixup s = case pathArgStr of
                          "" -> s
                          p | last p == '.' -> p ++ s
                          p -> p ++ "." ++ s
                        pathArgStr = show pathArg
            SearchByNameI isVerbose _showAll ws -> do
              let prettyPrintNames = basicPrettyPrintNames
              unlessError do
                results <- case ws of
                  -- no query, list everything
                  [] -> pure . listBranch $ Branch.head currentBranch'
                  -- type query
                  ":" : ws ->
                    ExceptT (parseSearchType (show input) (unwords ws)) >>= \typ ->
                      ExceptT $ do
                        let named = Branch.deepReferents root0
                        matches <-
                          fmap (filter (`Set.member` named) . toList) $
                            eval $ GetTermsOfType typ
                        matches <-
                          if null matches
                            then do
                              respond NoExactTypeMatches
                              fmap (filter (`Set.member` named) . toList) $
                                eval $ GetTermsMentioningType typ
                            else pure matches
                        let results =
                              -- in verbose mode, aliases are shown, so we collapse all
                              -- aliases to a single search result; in non-verbose mode,
                              -- a separate result may be shown for each alias
                              (if isVerbose then uniqueBy SR.toReferent else id) $
                                searchResultsFor prettyPrintNames matches []
                        pure . pure $ results

                  -- name query
                  (map HQ.unsafeFromString -> qs) -> do
                    let ns = basicPrettyPrintNames
                    let srs = searchBranchScored ns fuzzyNameDistance qs
                    pure $ uniqueBy SR.toReferent srs
                lift do
                  LoopState.numberedArgs .= fmap searchResultToHQString results
                  results' <- loadSearchResults results
                  ppe <-
                    suffixifiedPPE
                      =<< makePrintNamesFromLabeled'
                        (foldMap SR'.labeledDependencies results')
                  respond $ ListOfDefinitions ppe isVerbose results'
            ResolveTypeNameI hq ->
              zeroOneOrMore (getHQ'Types hq) (typeNotFound hq) go (typeConflicted hq)
              where
                conflicted = getHQ'Types (fmap HQ'.toNameOnly hq)
                makeDelete =
                  BranchUtil.makeDeleteTypeName (resolveSplit' (HQ'.toName <$> hq))
                go r = stepManyAt Branch.CompressHistory . fmap makeDelete . toList . Set.delete r $ conflicted
            ResolveTermNameI hq -> do
              refs <- getHQ'TermsIncludingHistorical hq
              zeroOneOrMore refs (termNotFound hq) go (termConflicted hq)
              where
                conflicted = getHQ'Terms (fmap HQ'.toNameOnly hq)
                makeDelete =
                  BranchUtil.makeDeleteTermName (resolveSplit' (HQ'.toName <$> hq))
                go r = stepManyAt Branch.CompressHistory . fmap makeDelete . toList . Set.delete r $ conflicted
            ReplaceI from to patchPath -> do
              let patchPath' = fromMaybe defaultPatchPath patchPath
              patch <- getPatchAt patchPath'
              QueryResult fromMisses' fromHits <- hqNameQuery [from]
              QueryResult toMisses' toHits <- hqNameQuery [to]
              let termsFromRefs = termReferences fromHits
                  termsToRefs = termReferences toHits
                  typesFromRefs = typeReferences fromHits
                  typesToRefs = typeReferences toHits
                  --- Here are all the kinds of misses
                  --- [X] [X]
                  --- [Type] [Term]
                  --- [Term] [Type]
                  --- [Type] [X]
                  --- [Term] [X]
                  --- [X] [Type]
                  --- [X] [Term]
                  -- Type hits are term misses
                  termFromMisses =
                    fromMisses'
                      <> (SR.typeName <$> typeResults fromHits)
                  termToMisses =
                    toMisses'
                      <> (SR.typeName <$> typeResults toHits)
                  -- Term hits are type misses
                  typeFromMisses =
                    fromMisses'
                      <> (SR.termName <$> termResults fromHits)
                  typeToMisses =
                    toMisses'
                      <> (SR.termName <$> termResults toHits)

                  termMisses = termFromMisses <> termToMisses
                  typeMisses = typeFromMisses <> typeToMisses

                  replaceTerms ::
                    Reference ->
                    Reference ->
                    Action m (Either Event Input) Symbol ()
                  replaceTerms fr tr = do
                    mft <- eval $ LoadTypeOfTerm fr
                    mtt <- eval $ LoadTypeOfTerm tr
                    let termNotFound =
                          respond . TermNotFound'
                            . SH.take hqLength
                            . Reference.toShortHash
                    case (mft, mtt) of
                      (Nothing, _) -> termNotFound fr
                      (_, Nothing) -> termNotFound tr
                      (Just ft, Just tt) -> do
                        let patch' =
                              -- The modified patch
                              over
                                Patch.termEdits
                                ( R.insert fr (Replace tr (TermEdit.typing tt ft))
                                    . R.deleteDom fr
                                )
                                patch
                            (patchPath'', patchName) = resolveSplit' patchPath'
                        saveAndApplyPatch patchPath'' patchName patch'

                  replaceTypes ::
                    Reference ->
                    Reference ->
                    Action m (Either Event Input) Symbol ()
                  replaceTypes fr tr = do
                    let patch' =
                          -- The modified patch
                          over
                            Patch.typeEdits
                            (R.insert fr (TypeEdit.Replace tr) . R.deleteDom fr)
                            patch
                        (patchPath'', patchName) = resolveSplit' patchPath'
                    saveAndApplyPatch patchPath'' patchName patch'

                  ambiguous t rs =
                    let rs' = Set.map Referent.Ref $ Set.fromList rs
                     in case t of
                          HQ.HashOnly h ->
                            hashConflicted h rs'
                          (Path.parseHQSplit' . HQ.toString -> Right n) ->
                            termConflicted n rs'
                          _ -> respond . BadName $ HQ.toString t

                  mismatch typeName termName = respond $ TypeTermMismatch typeName termName

              case (termsFromRefs, termsToRefs, typesFromRefs, typesToRefs) of
                ([], [], [], []) -> respond $ SearchTermsNotFound termMisses
                ([_], [], [], [_]) -> mismatch to from
                ([], [_], [_], []) -> mismatch from to
                ([_], [], _, _) -> respond $ SearchTermsNotFound termMisses
                ([], [_], _, _) -> respond $ SearchTermsNotFound termMisses
                (_, _, [_], []) -> respond $ SearchTermsNotFound typeMisses
                (_, _, [], [_]) -> respond $ SearchTermsNotFound typeMisses
                ([fr], [tr], [], []) -> replaceTerms fr tr
                ([], [], [fr], [tr]) -> replaceTypes fr tr
                (froms, [_], [], []) -> ambiguous from froms
                ([], [], froms, [_]) -> ambiguous from froms
                ([_], tos, [], []) -> ambiguous to tos
                ([], [], [_], tos) -> ambiguous to tos
                (_, _, _, _) -> error "unpossible"
            LoadI maybePath ->
              case maybePath <|> (fst <$> latestFile') of
                Nothing -> respond NoUnisonFile
                Just path -> do
                  res <- eval . LoadSource . Text.pack $ path
                  case res of
                    InvalidSourceNameError -> respond $ InvalidSourceName path
                    LoadError -> respond $ SourceLoadFailed path
                    LoadSuccess contents -> loadUnisonFile (Text.pack path) contents
            AddI names -> do
              let vars = Set.map Name.toVar names
              case uf of
                Nothing -> respond NoUnisonFile
                Just uf -> do
                  currentNames <- currentPathNames
                  let sr = Slurp.slurpFile uf vars (Just Slurp.AddOp) currentNames currentPath'
                  let adds = SlurpResult.adds sr
                  stepAtNoSync Branch.CompressHistory (Path.unabsolute currentPath', doSlurpAdds adds uf)
                  eval . AddDefsToCodebase . filterBySlurpResult sr $ uf
                  ppe <- prettyPrintEnvDecl =<< displayNames uf
                  respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
                  addDefaultMetadata adds
                  syncRoot
            PreviewAddI names -> case (latestFile', uf) of
              (Just (sourceName, _), Just uf) -> do
                let vars = Set.map Name.toVar names
                currentNames <- currentPathNames
                let sr = Slurp.slurpFile uf vars (Just Slurp.AddOp) currentNames currentPath'
                previewResponse sourceName sr uf
              _ -> respond NoUnisonFile
            UpdateI maybePatchPath names -> handleUpdate input maybePatchPath names
            PreviewUpdateI names -> case (latestFile', uf) of
              (Just (sourceName, _), Just uf) -> do
                let vars = Set.map Name.toVar names
                currentNames <- currentPathNames
                let sr = Slurp.slurpFile uf vars (Just Slurp.UpdateOp) currentNames currentPath'
                previewResponse sourceName sr uf
              _ -> respond NoUnisonFile
            TodoI patchPath branchPath' -> do
              patch <- getPatchAt (fromMaybe defaultPatchPath patchPath)
              doShowTodoOutput patch $ resolveToAbsolute branchPath'
            TestI showOk showFail -> do
              let testTerms =
                    Map.keys . R4.d1 . uncurry R4.selectD34 isTest
                      . Branch.deepTermMetadata
                      $ currentBranch0
                  testRefs = Set.fromList [r | Referent.Ref r <- toList testTerms]
                  oks results =
                    [ (r, msg)
                      | (r, Term.List' ts) <- Map.toList results,
                        Term.App' (Term.Constructor' (ConstructorReference ref cid)) (Term.Text' msg) <- toList ts,
                        cid == DD.okConstructorId && ref == DD.testResultRef
                    ]
                  fails results =
                    [ (r, msg)
                      | (r, Term.List' ts) <- Map.toList results,
                        Term.App' (Term.Constructor' (ConstructorReference ref cid)) (Term.Text' msg) <- toList ts,
                        cid == DD.failConstructorId && ref == DD.testResultRef
                    ]
              cachedTests <- fmap Map.fromList . eval $ LoadWatches WK.TestWatch testRefs
              let stats = Output.CachedTests (Set.size testRefs) (Map.size cachedTests)
              names <-
                makePrintNamesFromLabeled' $
                  LD.referents testTerms
                    <> LD.referents [DD.okConstructorReferent, DD.failConstructorReferent]
              ppe <- fqnPPE names
              respond $
                TestResults
                  stats
                  ppe
                  showOk
                  showFail
                  (oks cachedTests)
                  (fails cachedTests)
              let toCompute = Set.difference testRefs (Map.keysSet cachedTests)
              unless (Set.null toCompute) $ do
                let total = Set.size toCompute
                computedTests <- fmap join . for (toList toCompute `zip` [1 ..]) $ \(r, n) ->
                  case r of
                    Reference.DerivedId rid -> do
                      tm <- eval $ LoadTerm rid
                      case tm of
                        Nothing -> [] <$ respond (TermNotFound' . SH.take hqLength . Reference.toShortHash $ Reference.DerivedId rid)
                        Just tm -> do
                          respond $ TestIncrementalOutputStart ppe (n, total) r tm
                          --                          v don't cache; test cache populated below
                          tm' <- eval $ Evaluate1 ppe False tm
                          case tm' of
                            Left e -> respond (EvaluationFailure e) $> []
                            Right tm' -> do
                              -- After evaluation, cache the result of the test
                              eval $ PutWatch WK.TestWatch rid tm'
                              respond $ TestIncrementalOutputEnd ppe (n, total) r tm'
                              pure [(r, tm')]
                    r -> error $ "unpossible, tests can't be builtins: " <> show r

                let m = Map.fromList computedTests
                respond $ TestResults Output.NewlyComputed ppe showOk showFail (oks m) (fails m)

            PropagatePatchI patchPath scopePath -> do
              patch <- getPatchAt patchPath
              updated <- propagatePatch inputDescription patch (resolveToAbsolute scopePath)
              unless updated (respond $ NothingToPatch patchPath scopePath)
            ExecuteI main args ->
              addRunMain main uf >>= \case
                NoTermWithThatName -> do
                  ppe <- suffixifiedPPE (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
                  mainType <- eval RuntimeMain
                  respond $ NoMainFunction main ppe [mainType]
                TermHasBadType ty -> do
                  ppe <- suffixifiedPPE (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
                  mainType <- eval RuntimeMain
                  respond $ BadMainFunction main ty ppe [mainType]
                RunMainSuccess unisonFile -> do
                  ppe <- executePPE unisonFile
                  e <- eval $ Execute ppe unisonFile args

                  case e of
                    Left e -> respond $ EvaluationFailure e
                    Right _ -> pure () -- TODO
            MakeStandaloneI output main -> do
              mainType <- eval RuntimeMain
              parseNames <-
                flip NamesWithHistory.NamesWithHistory mempty <$> basicPrettyPrintNamesA
              ppe <- suffixifiedPPE parseNames
              let resolved = toList $ NamesWithHistory.lookupHQTerm main parseNames
                  smain = HQ.toString main
              filtered <-
                catMaybes
                  <$> traverse (\r -> fmap (r,) <$> loadTypeOfTerm r) resolved
              case filtered of
                [(Referent.Ref ref, ty)]
                  | Typechecker.isSubtype ty mainType ->
                    eval (MakeStandalone ppe ref output) >>= \case
                      Just err -> respond $ EvaluationFailure err
                      Nothing -> pure ()
                  | otherwise ->
                    respond $ BadMainFunction smain ty ppe [mainType]
                _ -> respond $ NoMainFunction smain ppe [mainType]
            IOTestI main -> do
              -- todo - allow this to run tests from scratch file, using addRunMain
              testType <- eval RuntimeTest
              parseNames <- (`NamesWithHistory.NamesWithHistory` mempty) <$> basicPrettyPrintNamesA
              ppe <- suffixifiedPPE parseNames
              -- use suffixed names for resolving the argument to display
              let oks results =
                    [ (r, msg)
                      | (r, Term.List' ts) <- results,
                        Term.App' (Term.Constructor' (ConstructorReference ref cid)) (Term.Text' msg) <- toList ts,
                        cid == DD.okConstructorId && ref == DD.testResultRef
                    ]
                  fails results =
                    [ (r, msg)
                      | (r, Term.List' ts) <- results,
                        Term.App' (Term.Constructor' (ConstructorReference ref cid)) (Term.Text' msg) <- toList ts,
                        cid == DD.failConstructorId && ref == DD.testResultRef
                    ]

                  results = NamesWithHistory.lookupHQTerm main parseNames
               in case toList results of
                    [Referent.Ref ref] -> do
                      typ <- loadTypeOfTerm (Referent.Ref ref)
                      case typ of
                        Just typ | Typechecker.isSubtype typ testType -> do
                          let a = ABT.annotation tm
                              tm = DD.forceTerm a a (Term.ref a ref)
                           in do
                                --                          v Don't cache IO tests
                                tm' <- eval $ Evaluate1 ppe False tm
                                case tm' of
                                  Left e -> respond (EvaluationFailure e)
                                  Right tm' ->
                                    respond $ TestResults Output.NewlyComputed ppe True True (oks [(ref, tm')]) (fails [(ref, tm')])
                        _ -> respond $ NoMainFunction (HQ.toString main) ppe [testType]
                    _ -> respond $ NoMainFunction (HQ.toString main) ppe [testType]

            -- UpdateBuiltinsI -> do
            --   stepAt updateBuiltins
            --   checkTodo

            MergeBuiltinsI -> do
              -- these were added once, but maybe they've changed and need to be
              -- added again.
              let uf =
                    UF.typecheckedUnisonFile
                      (Map.fromList Builtin.builtinDataDecls)
                      (Map.fromList Builtin.builtinEffectDecls)
                      [Builtin.builtinTermsSrc Intrinsic]
                      mempty
              eval $ AddDefsToCodebase uf
              -- add the names; note, there are more names than definitions
              -- due to builtin terms; so we don't just reuse `uf` above.
              let srcb = BranchUtil.fromNames Builtin.names0
              _ <- updateAtM (currentPath' `snoc` "builtin") $ \destb ->
                eval $ Merge Branch.RegularMerge srcb destb
              success
            MergeIOBuiltinsI -> do
              -- these were added once, but maybe they've changed and need to be
              -- added again.
              let uf =
                    UF.typecheckedUnisonFile
                      (Map.fromList Builtin.builtinDataDecls)
                      (Map.fromList Builtin.builtinEffectDecls)
                      [Builtin.builtinTermsSrc Intrinsic]
                      mempty
              eval $ AddDefsToCodebase uf
              -- these have not necessarily been added yet
              eval $ AddDefsToCodebase IOSource.typecheckedFile'

              -- add the names; note, there are more names than definitions
              -- due to builtin terms; so we don't just reuse `uf` above.
              let names0 =
                    Builtin.names0
                      <> UF.typecheckedToNames IOSource.typecheckedFile'
              let srcb = BranchUtil.fromNames names0
              _ <- updateAtM (currentPath' `snoc` "builtin") $ \destb ->
                eval $ Merge Branch.RegularMerge srcb destb
              success
            ListEditsI maybePath -> do
              let (p, seg) =
                    maybe
                      (Path.toAbsoluteSplit currentPath' defaultPatchPath)
                      (Path.toAbsoluteSplit currentPath')
                      maybePath
              patch <- eval . Eval . Branch.getPatch seg . Branch.head =<< getAt p
              ppe <-
                suffixifiedPPE
                  =<< makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
              respond $ ListEdits patch ppe
            PullRemoteBranchI mayRepo path syncMode pullMode verbosity -> unlessError do
              let preprocess = case pullMode of
                    Input.PullWithHistory -> Unmodified
                    Input.PullWithoutHistory -> Preprocessed $ pure . Branch.discardHistory
              ns <- maybe (writePathToRead <$> resolveConfiguredGitUrl Pull path) pure mayRepo
              lift $ unlessGitError do
                remoteBranch <- importRemoteBranch ns syncMode preprocess
                let unchangedMsg = PullAlreadyUpToDate ns path
                let destAbs = resolveToAbsolute path
                let printDiffPath = if Verbosity.isSilent verbosity then Nothing else Just path
                lift $ case pullMode of
                  Input.PullWithHistory -> do
                    destBranch <- getAt destAbs
                    if Branch.isEmpty0 (Branch.head destBranch)
                       then do
                         void $ updateAtM destAbs (const $ pure remoteBranch)
                         respond $ MergeOverEmpty path
                       else mergeBranchAndPropagateDefaultPatch
                              Branch.RegularMerge
                              inputDescription
                              (Just unchangedMsg)
                              remoteBranch
                              printDiffPath
                              destAbs
                  Input.PullWithoutHistory -> do
                    didUpdate <- updateAtM
                                   destAbs
                                   (\destBranch -> pure $ remoteBranch `Branch.consBranchSnapshot` destBranch )
                    if didUpdate
                       then respond $ PullSuccessful ns path
                       else respond unchangedMsg

            PushRemoteBranchI mayRepo path pushBehavior syncMode -> handlePushRemoteBranch mayRepo path pushBehavior syncMode
            ListDependentsI hq -> handleDependents hq
            ListDependenciesI hq ->
              -- todo: add flag to handle transitive efficiently
              resolveHQToLabeledDependencies hq >>= \lds ->
                if null lds
                  then respond $ LabeledReferenceNotFound hq
                  else for_ lds $ \ld -> do
                    dependencies :: Set Reference <-
                      let tp r@(Reference.DerivedId i) =
                            eval (LoadType i) <&> \case
                              Nothing -> error $ "What happened to " ++ show i ++ "?"
                              Just decl -> Set.delete r . DD.dependencies $ DD.asDataDecl decl
                          tp _ = pure mempty
                          tm (Referent.Ref r@(Reference.DerivedId i)) =
                            eval (LoadTerm i) <&> \case
                              Nothing -> error $ "What happened to " ++ show i ++ "?"
                              Just tm -> Set.delete r $ Term.dependencies tm
                          tm con@(Referent.Con (ConstructorReference (Reference.DerivedId i) cid) _ct) =
                            eval (LoadType i) <&> \case
                              Nothing -> error $ "What happened to " ++ show i ++ "?"
                              Just decl -> case DD.typeOfConstructor (DD.asDataDecl decl) cid of
                                Nothing -> error $ "What happened to " ++ show con ++ "?"
                                Just tp -> Type.dependencies tp
                          tm _ = pure mempty
                       in LD.fold tp tm ld
                    (missing, names0) <- eval . Eval $ Branch.findHistoricalRefs' dependencies root'
                    let types = R.toList $ Names.types names0
                    let terms = fmap (second Referent.toReference) $ R.toList $ Names.terms names0
                    let names = types <> terms
                    LoopState.numberedArgs .= fmap (Text.unpack . Reference.toText) ((fmap snd names) <> toList missing)
                    respond $ ListDependencies hqLength ld names missing
            NamespaceDependenciesI namespacePath' -> do
              let path = maybe currentPath' resolveToAbsolute namespacePath'
              case (Branch.getAt (Path.unabsolute path) root') of
                Nothing -> respond $ BranchEmpty (Right (Path.absoluteToPath' path))
                Just b -> do
                  externalDependencies <- NamespaceDependencies.namespaceDependencies (Branch.head b)
                  ppe <- PPE.unsuffixifiedPPE <$> currentPrettyPrintEnvDecl
                  respond $ ListNamespaceDependencies ppe path externalDependencies
            DebugNumberedArgsI -> use LoopState.numberedArgs >>= respond . DumpNumberedArgs
            DebugTypecheckedUnisonFileI -> case uf of
              Nothing -> respond NoUnisonFile
              Just uf ->
                let datas, effects, terms :: [(Name, Reference.Id)]
                    datas = [(Name.unsafeFromVar v, r) | (v, (r, _d)) <- Map.toList $ UF.dataDeclarationsId' uf]
                    effects = [(Name.unsafeFromVar v, r) | (v, (r, _e)) <- Map.toList $ UF.effectDeclarationsId' uf]
                    terms = [(Name.unsafeFromVar v, r) | (v, (r, _wk, _tm, _tp)) <- Map.toList $ UF.hashTermsId uf]
                 in eval . Notify $ DumpUnisonFileHashes hqLength datas effects terms
            DebugDumpNamespacesI -> do
              let seen h = State.gets (Set.member h)
                  set h = State.modify (Set.insert h)
                  getCausal b = (Branch.headHash b, pure $ Branch._history b)
                  goCausal :: forall m. Monad m => [(Branch.Hash, m (Branch.UnwrappedBranch m))] -> StateT (Set Branch.Hash) m ()
                  goCausal [] = pure ()
                  goCausal ((h, mc) : queue) = do
                    ifM (seen h) (goCausal queue) do
                      lift mc >>= \case
                        Causal.One h b -> goBranch h b mempty queue
                        Causal.Cons h b tail -> goBranch h b [fst tail] (tail : queue)
                        Causal.Merge h b (Map.toList -> tails) -> goBranch h b (map fst tails) (tails ++ queue)
                  goBranch :: forall m. Monad m => Branch.Hash -> Branch0 m -> [Branch.Hash] -> [(Branch.Hash, m (Branch.UnwrappedBranch m))] -> StateT (Set Branch.Hash) m ()
                  goBranch h b (Set.fromList -> causalParents) queue = case b of
                    Branch0 terms0 types0 children0 patches0 _ _ _ _ _ _ ->
                      let wrangleMetadata :: (Ord r, Ord n) => Metadata.Star r n -> r -> (r, (Set n, Set Metadata.Value))
                          wrangleMetadata s r =
                            (r, (R.lookupDom r $ Star3.d1 s, Set.map snd . R.lookupDom r $ Star3.d3 s))
                          terms = Map.fromList . map (wrangleMetadata terms0) . Foldable.toList $ Star3.fact terms0
                          types = Map.fromList . map (wrangleMetadata types0) . Foldable.toList $ Star3.fact types0
                          patches = fmap fst patches0
                          children = fmap Branch.headHash children0
                       in do
                            let d = Output.DN.DumpNamespace terms types patches children causalParents
                            -- the alternate implementation that doesn't rely on `traceM` blows up
                            traceM $ P.toPlain 200 (prettyDump (h, d))
                            set h
                            goCausal (map getCausal (Foldable.toList children0) ++ queue)
                  prettyDump (h, Output.DN.DumpNamespace terms types patches children causalParents) =
                    P.lit "Namespace " <> P.shown h <> P.newline
                      <> ( P.indentN 2 $
                             P.linesNonEmpty
                               [ Monoid.unlessM (null causalParents) $ P.lit "Causal Parents:" <> P.newline <> P.indentN 2 (P.lines (map P.shown $ Set.toList causalParents)),
                                 Monoid.unlessM (null terms) $ P.lit "Terms:" <> P.newline <> P.indentN 2 (P.lines (map (prettyDefn Referent.toText) $ Map.toList terms)),
                                 Monoid.unlessM (null types) $ P.lit "Types:" <> P.newline <> P.indentN 2 (P.lines (map (prettyDefn Reference.toText) $ Map.toList types)),
                                 Monoid.unlessM (null patches) $ P.lit "Patches:" <> P.newline <> P.indentN 2 (P.column2 (map (bimap P.shown P.shown) $ Map.toList patches)),
                                 Monoid.unlessM (null children) $ P.lit "Children:" <> P.newline <> P.indentN 2 (P.column2 (map (bimap P.shown P.shown) $ Map.toList children))
                               ]
                         )
                    where
                      prettyLinks renderR r [] = P.indentN 2 $ P.text (renderR r)
                      prettyLinks renderR r links = P.indentN 2 (P.lines (P.text (renderR r) : (links <&> \r -> "+ " <> P.text (Reference.toText r))))
                      prettyDefn renderR (r, (Foldable.toList -> names, Foldable.toList -> links)) =
                        P.lines (P.shown <$> if null names then [NameSegment "<unnamed>"] else names) <> P.newline <> prettyLinks renderR r links
              void . eval . Eval . flip State.execStateT mempty $ goCausal [getCausal root']
            DebugDumpNamespaceSimpleI -> do
              for_ (Relation.toList . Branch.deepTypes . Branch.head $ root') \(r, name) ->
                traceM $ show name ++ ",Type," ++ Text.unpack (Reference.toText r)
              for_ (Relation.toList . Branch.deepTerms . Branch.head $ root') \(r, name) ->
                traceM $ show name ++ ",Term," ++ Text.unpack (Referent.toText r)
            DebugClearWatchI {} -> eval ClearWatchCache
            DeprecateTermI {} -> notImplemented
            DeprecateTypeI {} -> notImplemented
            RemoveTermReplacementI from patchPath ->
              doRemoveReplacement from patchPath True
            RemoveTypeReplacementI from patchPath ->
              doRemoveReplacement from patchPath False
            ShowDefinitionByPrefixI {} -> notImplemented
            UpdateBuiltinsI -> notImplemented
            QuitI -> MaybeT $ pure Nothing
            GistI input -> handleGist input
      where
        notImplemented = eval $ Notify NotImplemented
        success = respond Success

  case e of
    Right input -> LoopState.lastInput .= Just input
    _ -> pure ()

handleDependents :: Monad m => HQ.HashQualified Name -> Action' m v ()
handleDependents hq = do
  hqLength <- eval CodebaseHashLength
  -- todo: add flag to handle transitive efficiently
  resolveHQToLabeledDependencies hq >>= \lds ->
    if null lds
      then respond $ LabeledReferenceNotFound hq
      else for_ lds \ld -> do
        -- The full set of dependent references, any number of which may not have names in the current namespace.
        dependents <-
          let tp r = eval $ GetDependents r
              tm (Referent.Ref r) = eval $ GetDependents r
              tm (Referent.Con (ConstructorReference r _cid) _ct) = eval $ GetDependents r
           in LD.fold tp tm ld
        -- Use an unsuffixified PPE here, so we display full names (relative to the current path), rather than the shortest possible
        -- unambiguous name.
        ppe <- PPE.unsuffixifiedPPE <$> currentPrettyPrintEnvDecl
        let results :: [(Reference, Maybe Name)]
            results =
              -- Currently we only retain dependents that are named in the current namespace (hence `mapMaybe`). In the future, we could
              -- take a flag to control whether we want to show all dependents
              mapMaybe f (Set.toList dependents)
              where
                f :: Reference -> Maybe (Reference, Maybe Name)
                f reference =
                  asum
                    [ g <$> PPE.terms ppe (Referent.Ref reference),
                      g <$> PPE.types ppe reference
                    ]
                  where
                    g :: HQ'.HashQualified Name -> (Reference, Maybe Name)
                    g hqName =
                      (reference, Just (HQ'.toName hqName))
        LoopState.numberedArgs .= map (Text.unpack . Reference.toText . fst) results
        respond (ListDependents hqLength ld results)

-- | Handle a @gist@ command.
handleGist :: MonadUnliftIO m => GistInput -> Action' m v ()
handleGist (GistInput repo) =
  doPushRemoteBranch repo Path.relativeEmpty' SyncMode.ShortCircuit Nothing

-- | Handle a @push@ command.
handlePushRemoteBranch ::
  forall m v.
  MonadUnliftIO m =>
  -- | The repo to push to. If missing, it is looked up in `.unisonConfig`.
  Maybe WriteRemotePath ->
  -- | The local path to push. If relative, it's resolved relative to the current path (`cd`).
  Path' ->
  -- | The push behavior (whether the remote branch is required to be empty or non-empty).
  PushBehavior ->
  SyncMode.SyncMode ->
  Action' m v ()
handlePushRemoteBranch mayRepo path pushBehavior syncMode = do
  unlessError do
    (repo, remotePath) <- maybe (resolveConfiguredGitUrl Push path) pure mayRepo
    lift (doPushRemoteBranch repo path syncMode (Just (remotePath, pushBehavior)))

-- Internal helper that implements pushing to a remote repo, which generalizes @gist@ and @push@.
doPushRemoteBranch ::
  forall m v.
  MonadUnliftIO m =>
  -- | The repo to push to.
  WriteRepo ->
  -- | The local path to push. If relative, it's resolved relative to the current path (`cd`).
  Path' ->
  SyncMode.SyncMode ->
  -- | The remote target. If missing, the given branch contents should be pushed to the remote repo without updating the
  -- root namespace.
  Maybe (Path, PushBehavior) ->
  Action' m v ()
doPushRemoteBranch repo localPath syncMode remoteTarget = do
  sourceBranch <- do
    currentPath' <- use LoopState.currentPath
    getAt (Path.resolve currentPath' localPath)

  unlessError do
    withExceptT Output.GitError $ do
      case remoteTarget of
        Nothing -> do
          let opts = PushGitBranchOpts {setRoot = False, syncMode}
          syncRemoteBranch sourceBranch repo opts
          sbhLength <- (eval BranchHashLength)
          respond (GistCreated sbhLength repo (Branch.headHash sourceBranch))
        Just (remotePath, pushBehavior) -> do
          let withRemoteRoot remoteRoot = do
                let -- We don't merge `sourceBranch` with `remoteBranch`, we just replace it. This push will be rejected if this
                    -- rewinds time or misses any new updates in the remote branch that aren't in `sourceBranch` already.
                    f remoteBranch = if shouldPushTo pushBehavior remoteBranch then Just sourceBranch else Nothing
                Branch.modifyAtM remotePath f remoteRoot & \case
                  Nothing -> respond (RefusedToPush pushBehavior)
                  Just newRemoteRoot -> do
                    let opts = PushGitBranchOpts {setRoot = True, syncMode}
                    runExceptT (syncRemoteBranch newRemoteRoot repo opts) >>= \case
                      Left gitErr -> respond (Output.GitError gitErr)
                      Right () -> respond Success
          viewRemoteBranch (writeToRead repo, Nothing, Path.empty) withRemoteRoot >>= \case
            Left (GitSqliteCodebaseError NoDatabaseFile{}) -> withRemoteRoot Branch.empty
            Left err -> throwError err
            Right () -> pure ()
  where
    -- Per `pushBehavior`, we are either:
    --
    --   (1) updating an empty branch, which fails if the branch isn't empty (`push.create`)
    --   (2) updating a non-empty branch, which fails if the branch is empty (`push`)
    shouldPushTo :: PushBehavior -> Branch m -> Bool
    shouldPushTo pushBehavior remoteBranch =
      case pushBehavior of
        PushBehavior.RequireEmpty -> Branch.isEmpty0 (Branch.head remoteBranch)
        PushBehavior.RequireNonEmpty -> not (Branch.isEmpty0 (Branch.head remoteBranch))

-- | Handle a @ShowDefinitionI@ input command, i.e. `view` or `edit`.
handleShowDefinition ::
  forall m v.
  Functor m =>
  OutputLocation ->
  [HQ.HashQualified Name] ->
  Action' m v ()
handleShowDefinition outputLoc inputQuery = do
  -- If the query is empty, run a fuzzy search.
  query <-
    if null inputQuery
      then do
        branch <- fuzzyBranch
        fuzzySelectDefinition Relative branch >>= \case
          Nothing -> case outputLoc of
            ConsoleLocation -> respond (HelpMessage InputPatterns.view) $> []
            _ -> respond (HelpMessage InputPatterns.edit) $> []
          Just defs -> pure defs
      else pure inputQuery
  currentPath' <- Path.unabsolute <$> use LoopState.currentPath
  root' <- use LoopState.root
  hqLength <- eval CodebaseHashLength
  Backend.DefinitionResults terms types misses <-
    eval (GetDefinitionsBySuffixes (Just currentPath') root' includeCycles query)
  outputPath <- getOutputPath
  when (not (null types && null terms)) do
    let printNames = Backend.getCurrentPrettyNames (Backend.AllNames currentPath') root'
    let ppe = PPE.fromNamesDecl hqLength printNames
    respond (DisplayDefinitions outputPath ppe types terms)
  when (not (null misses)) (respond (SearchTermsNotFound misses))
  -- We set latestFile to be programmatically generated, if we
  -- are viewing these definitions to a file - this will skip the
  -- next update for that file (which will happen immediately)
  LoopState.latestFile .= ((,True) <$> outputPath)
  where
    -- `view`: fuzzy find globally; `edit`: fuzzy find local to current branch
    fuzzyBranch :: Action' m v (Branch0 m)
    fuzzyBranch =
      case outputLoc of
        ConsoleLocation {} -> Branch.head <$> use LoopState.root
        -- fuzzy finding for 'edit's are local to the current branch
        LatestFileLocation {} -> currentBranch0
        FileLocation {} -> currentBranch0
      where
        currentBranch0 = do
          currentPath' <- use LoopState.currentPath
          currentBranch <- getAt currentPath'
          pure (Branch.head currentBranch)
    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ -> Backend.IncludeCycles
        LatestFileLocation -> Backend.IncludeCycles

    -- Get the file path to send the definition(s) to. `Nothing` means the terminal.
    getOutputPath :: Action' m v (Maybe FilePath)
    getOutputPath =
      case outputLoc of
        ConsoleLocation -> pure Nothing
        FileLocation path -> pure (Just path)
        LatestFileLocation ->
          use LoopState.latestFile <&> \case
            Nothing -> Just "scratch.u"
            Just (path, _) -> Just path

-- | Handle an @update@ command.
handleUpdate :: forall m v. (Monad m, Var v) => Input -> Maybe PatchPath -> Set Name -> Action' m v ()
handleUpdate input maybePatchPath names = do
  let vars = Set.map Name.toVar names
  use LoopState.latestTypecheckedFile >>= \case
    Nothing -> respond NoUnisonFile
    Just uf -> do
      currentPath' <- use LoopState.currentPath
      let defaultPatchPath :: PatchPath
          defaultPatchPath = (Path' $ Left currentPath', defaultPatchNameSegment)
          getPatchAt :: Path.Split' -> Action' m v Patch
          getPatchAt patchPath' = do
            let (p, seg) = Path.toAbsoluteSplit currentPath' patchPath'
            b <- getAt p
            eval . Eval $ Branch.getPatch seg (Branch.head b)
      let patchPath = fromMaybe defaultPatchPath maybePatchPath
      slurpCheckNames <- slurpResultNames
      let currentPathNames = slurpCheckNames
      let sr = Slurp.slurpFile uf vars (Just Slurp.UpdateOp) slurpCheckNames currentPath'
          addsAndUpdates :: SlurpComponent v
          addsAndUpdates = Slurp.updates sr <> Slurp.adds sr
          fileNames :: Names
          fileNames = UF.typecheckedToNames uf
          -- todo: display some error if typeEdits or termEdits itself contains a loop
          typeEdits :: Map Name (Reference, Reference)
          typeEdits = Map.fromList $ map f (toList $ SC.types (updates sr))
            where
              f v = case ( toList (Names.typesNamed slurpCheckNames n),
                           toList (Names.typesNamed fileNames n)
                         ) of
                ([old], [new]) -> (n, (old, new))
                actual ->
                  error $
                    "Expected unique matches for var \""
                      ++ Var.nameStr v
                      ++ "\" but got: "
                      ++ show actual
                where
                  n = Name.unsafeFromVar v
          hashTerms :: Map Reference (Type v Ann)
          hashTerms = Map.fromList (toList hashTerms0)
            where
              hashTerms0 = (\(r, _wk, _tm, typ) -> (r, typ)) <$> UF.hashTerms uf
          termEdits :: Map Name (Reference, Reference)
          termEdits = Map.fromList $ map g (toList $ SC.terms (updates sr))
            where
              g v = case ( toList (Names.refTermsNamed slurpCheckNames n),
                           toList (Names.refTermsNamed fileNames n)
                         ) of
                ([old], [new]) -> (n, (old, new))
                actual ->
                  error $
                    "Expected unique matches for "
                      ++ Var.nameStr v
                      ++ " but got: "
                      ++ show actual
                where
                  n = Name.unsafeFromVar v
          termDeprecations :: [(Name, Referent)]
          termDeprecations =
            [ (n, r)
              | (oldTypeRef, _) <- Map.elems typeEdits,
                (n, r) <- Names.constructorsForType oldTypeRef currentPathNames
            ]

      ye'ol'Patch <- getPatchAt patchPath
      -- If `uf` updates a -> a', we want to replace all (a0 -> a) in patch
      -- with (a0 -> a') in patch'.
      -- So for all (a0 -> a) in patch, for all (a -> a') in `uf`,
      -- we must know the type of a0, a, a'.
      let -- we need:
          -- all of the `old` references from the `new` edits,
          -- plus all of the `old` references for edits from patch we're replacing
          collectOldForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
          collectOldForTyping new old = foldl' f mempty (new ++ fromOld)
            where
              f acc (r, _r') = Set.insert r acc
              newLHS = Set.fromList . fmap fst $ new
              fromOld :: [(Reference, Reference)]
              fromOld =
                [ (r, r') | (r, TermEdit.Replace r' _) <- R.toList . Patch._termEdits $ old, Set.member r' newLHS
                ]
          neededTypes = collectOldForTyping (toList termEdits) ye'ol'Patch

      allTypes :: Map Reference (Type v Ann) <-
        fmap Map.fromList . for (toList neededTypes) $ \r ->
          (r,) . fromMaybe (Type.builtin External "unknown type")
            <$> (eval . LoadTypeOfTerm) r

      let typing r1 r2 = case (Map.lookup r1 allTypes, Map.lookup r2 hashTerms) of
            (Just t1, Just t2)
              | Typechecker.isEqual t1 t2 -> TermEdit.Same
              | Typechecker.isSubtype t1 t2 -> TermEdit.Subtype
              | otherwise -> TermEdit.Different
            e ->
              error $
                "compiler bug: typing map not constructed properly\n"
                  <> "typing "
                  <> show r1
                  <> " "
                  <> show r2
                  <> " : "
                  <> show e

      let updatePatch :: Patch -> Patch
          updatePatch p = foldl' step2 p' termEdits
            where
              p' = foldl' step1 p typeEdits
              step1 p (r, r') = Patch.updateType r (TypeEdit.Replace r') p
              step2 p (r, r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
          (p, seg) = Path.toAbsoluteSplit currentPath' patchPath
          updatePatches :: Branch0 m -> m (Branch0 m)
          updatePatches = Branch.modifyPatches seg updatePatch

      when (Slurp.isNonempty sr) $ do
        -- take a look at the `updates` from the SlurpResult
        -- and make a patch diff to record a replacement from the old to new references
        stepManyAtMNoSync Branch.CompressHistory
          [ ( Path.unabsolute currentPath',
              pure . doSlurpUpdates typeEdits termEdits termDeprecations
            ),
            ( Path.unabsolute currentPath',
              pure . doSlurpAdds addsAndUpdates uf
            ),
            (Path.unabsolute p, updatePatches)
          ]
        eval . AddDefsToCodebase . filterBySlurpResult sr $ uf
      ppe <- prettyPrintEnvDecl =<< displayNames uf
      respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
      -- propagatePatch prints TodoOutput
      void $ propagatePatchNoSync (updatePatch ye'ol'Patch) currentPath'
      addDefaultMetadata addsAndUpdates
      let patchString :: Text
          patchString =
            patchPath
              & Path.unsplit'
              & Path.resolve @_ @_ @Path.Absolute currentPath'
              & tShow
      syncRoot ("update " <> patchString)

-- Add default metadata to all added types and terms in a slurp component.
--
-- No-op if the slurp component is empty.
addDefaultMetadata :: (Monad m, Var v) => SlurpComponent v -> Action m (Either Event Input) v ()
addDefaultMetadata adds =
  when (not (SC.isEmpty adds)) do
    currentPath' <- use LoopState.currentPath
    let addedVs = Set.toList $ SC.types adds <> SC.terms adds
        addedNs = traverse (Path.hqSplitFromName' . Name.unsafeFromVar) addedVs
    case addedNs of
      Nothing ->
        error $
          "I couldn't parse a name I just added to the codebase! "
            <> "-- Added names: "
            <> show addedVs
      Just addedNames -> do
        dm <- resolveDefaultMetadata currentPath'
        case toList dm of
          [] -> pure ()
          dm' -> do
            let hqs = traverse InputPatterns.parseHashQualifiedName dm'
            case hqs of
              Left e ->
                respond $
                  ConfiguredMetadataParseError
                    (Path.absoluteToPath' currentPath')
                    (show dm')
                    e
              Right defaultMeta ->
                manageLinks True addedNames defaultMeta Metadata.insert

resolveDefaultMetadata :: Path.Absolute -> Action' m v [String]
resolveDefaultMetadata path = do
  let superpaths = Path.ancestors path
  xs <-
    for
      superpaths
      ( \path -> do
          mayNames <-
            eval . ConfigLookup @[String] $ configKey "DefaultMetadata" path
          pure . join $ toList mayNames
      )
  pure . join $ toList xs

-- Add/remove links between definitions and metadata.
-- `silent` controls whether this produces any output to the user.
-- `srcs` is (names of the) definitions to pass to `op`
-- `mdValues` is (names of the) metadata to pass to `op`
-- `op` is the operation to add/remove/alter metadata mappings.
--   e.g. `Metadata.insert` is passed to add metadata links.
manageLinks ::
  forall m v.
  (Monad m, Var v) =>
  Bool ->
  [(Path', HQ'.HQSegment)] ->
  [HQ.HashQualified Name] ->
  ( forall r.
    Ord r =>
    (r, Metadata.Type, Metadata.Value) ->
    Branch.Star r NameSegment ->
    Branch.Star r NameSegment
  ) ->
  Action m (Either Event Input) v ()
manageLinks silent srcs mdValues op = do
  runExceptT (for mdValues \val -> ExceptT (getMetadataFromName val)) >>= \case
    Left output -> respond output
    Right metadata -> do
      before <- Branch.head <$> use LoopState.root
      traverse_ go metadata
      if silent
        then respond DefaultMetadataNotification
        else do
          after <- Branch.head <$> use LoopState.root
          (ppe, outputDiff) <- diffHelper before after
          if OBranchDiff.isEmpty outputDiff
            then respond NoOp
            else
              respondNumbered $
                ShowDiffNamespace
                  (Right Path.absoluteEmpty)
                  (Right Path.absoluteEmpty)
                  ppe
                  outputDiff
  where
    go :: (Metadata.Type, Metadata.Value) -> Action m (Either Event Input) v ()
    go (mdType, mdValue) = do
      newRoot <- use LoopState.root
      currentPath' <- use LoopState.currentPath
      let resolveToAbsolute :: Path' -> Path.Absolute
          resolveToAbsolute = Path.resolve currentPath'
          resolveSplit' :: (Path', a) -> (Path, a)
          resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
          r0 = Branch.head newRoot
          getTerms p = BranchUtil.getTerm (resolveSplit' p) r0
          getTypes p = BranchUtil.getType (resolveSplit' p) r0
          !srcle = toList . getTerms =<< srcs
          !srclt = toList . getTypes =<< srcs
      let step b0 =
            let tmUpdates terms = foldl' go terms srcle
                  where
                    go terms src = op (src, mdType, mdValue) terms
                tyUpdates types = foldl' go types srclt
                  where
                    go types src = op (src, mdType, mdValue) types
              in over Branch.terms tmUpdates . over Branch.types tyUpdates $ b0
          steps = srcs <&> \(path, _hq) -> (Path.unabsolute (resolveToAbsolute path), step)
      stepManyAtNoSync Branch.CompressHistory steps

-- Takes a maybe (namespace address triple); returns it as-is if `Just`;
-- otherwise, tries to load a value from .unisonConfig, and complains
-- if needed.
resolveConfiguredGitUrl ::
  PushPull ->
  Path' ->
  ExceptT (Output v) (Action m i v) WriteRemotePath
resolveConfiguredGitUrl pushPull destPath' = ExceptT do
  currentPath' <- use LoopState.currentPath
  let destPath = Path.resolve currentPath' destPath'
  let configKey = gitUrlKey destPath
  (eval . ConfigLookup) configKey >>= \case
    Just url ->
      case P.parse UriParser.writeRepoPath (Text.unpack configKey) url of
        Left e ->
          pure . Left $
            ConfiguredGitUrlParseError pushPull destPath' url (show e)
        Right ns ->
          pure . Right $ ns
    Nothing ->
      pure . Left $ NoConfiguredGitUrl pushPull destPath'

gitUrlKey :: Path.Absolute -> Text
gitUrlKey = configKey "GitUrl"

configKey :: Text -> Path.Absolute -> Text
configKey k p =
  Text.intercalate "." . toList $
    k
      :<| fmap
        NameSegment.toText
        (Path.toSeq $ Path.unabsolute p)

viewRemoteBranch :: (MonadCommand n m i v, MonadUnliftIO m) => ReadRemoteNamespace -> (Branch m -> Free (Command m i v) r) -> n (Either GitError r)
viewRemoteBranch ns action = do
  eval $ ViewRemoteBranch ns action

syncRemoteBranch :: MonadCommand n m i v => Branch m -> WriteRepo -> PushGitBranchOpts -> ExceptT GitError n ()
syncRemoteBranch b repo opts =
  ExceptT . eval $ SyncRemoteBranch b repo opts

-- todo: compare to `getHQTerms` / `getHQTypes`.  Is one universally better?
resolveHQToLabeledDependencies :: Functor m => HQ.HashQualified Name -> Action' m v (Set LabeledDependency)
resolveHQToLabeledDependencies = \case
  HQ.NameOnly n -> do
    parseNames <- basicParseNames
    let terms, types :: Set LabeledDependency
        terms = Set.map LD.referent . Name.searchBySuffix n $ Names.terms parseNames
        types = Set.map LD.typeRef . Name.searchBySuffix n $ Names.types parseNames
    pure $ terms <> types
  -- rationale: the hash should be unique enough that the name never helps
  HQ.HashQualified _n sh -> resolveHashOnly sh
  HQ.HashOnly sh -> resolveHashOnly sh
  where
    resolveHashOnly sh = do
      terms <- eval $ TermReferentsByShortHash sh
      types <- eval $ TypeReferencesByShortHash sh
      pure $ Set.map LD.referent terms <> Set.map LD.typeRef types

doDisplay :: OutputLocation -> NamesWithHistory -> Term Symbol () -> Action' m Symbol ()
doDisplay outputLoc names tm = do
  ppe <- prettyPrintEnvDecl names
  tf <- use LoopState.latestTypecheckedFile
  let (tms, typs) = maybe mempty UF.indexByReference tf
  latestFile' <- use LoopState.latestFile
  let loc = case outputLoc of
        ConsoleLocation -> Nothing
        FileLocation path -> Just path
        LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
      useCache = True
      evalTerm tm =
        fmap ErrorUtil.hush . fmap (fmap Term.unannotate) . eval $
          Evaluate1 (PPE.suffixifiedPPE ppe) useCache (Term.amap (const External) tm)
      loadTerm (Reference.DerivedId r) = case Map.lookup r tms of
        Nothing -> fmap (fmap Term.unannotate) . eval $ LoadTerm r
        Just (tm, _) -> pure (Just $ Term.unannotate tm)
      loadTerm _ = pure Nothing
      loadDecl (Reference.DerivedId r) = case Map.lookup r typs of
        Nothing -> fmap (fmap $ DD.amap (const ())) . eval $ LoadType r
        Just decl -> pure (Just $ DD.amap (const ()) decl)
      loadDecl _ = pure Nothing
      loadTypeOfTerm' (Referent.Ref (Reference.DerivedId r))
        | Just (_, ty) <- Map.lookup r tms = pure $ Just (void ty)
      loadTypeOfTerm' r = fmap (fmap void) . loadTypeOfTerm $ r
  rendered <- DisplayValues.displayTerm ppe loadTerm loadTypeOfTerm' evalTerm loadDecl tm
  respond $ DisplayRendered loc rendered

getLinks ::
  (Var v, Monad m) =>
  SrcLoc ->
  Path.HQSplit' ->
  Either (Set Reference) (Maybe String) ->
  ExceptT
    (Output v)
    (Action' m v)
    ( PPE.PrettyPrintEnv,
      --  e.g. ("Foo.doc", #foodoc, Just (#builtin.Doc)
      [(HQ.HashQualified Name, Reference, Maybe (Type v Ann))]
    )
getLinks srcLoc src mdTypeStr = ExceptT $ do
  let go = fmap Right . getLinks' src
  case mdTypeStr of
    Left s -> go (Just s)
    Right Nothing -> go Nothing
    Right (Just mdTypeStr) ->
      parseType srcLoc mdTypeStr >>= \case
        Left e -> pure $ Left e
        Right typ -> go . Just . Set.singleton $ Hashing.typeToReference typ

getLinks' ::
  (Var v, Monad m) =>
  Path.HQSplit' -> -- definition to print metadata of
  Maybe (Set Reference) -> -- return all metadata if empty
  Action'
    m
    v
    ( PPE.PrettyPrintEnv,
      --  e.g. ("Foo.doc", #foodoc, Just (#builtin.Doc)
      [(HQ.HashQualified Name, Reference, Maybe (Type v Ann))]
    )
getLinks' src selection0 = do
  root0 <- Branch.head <$> use LoopState.root
  currentPath' <- use LoopState.currentPath
  let resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      p = resolveSplit' src -- ex: the (parent,hqsegment) of `List.map` - `List`
      -- all metadata (type+value) associated with name `src`
      allMd =
        R4.d34 (BranchUtil.getTermMetadataHQNamed p root0)
          <> R4.d34 (BranchUtil.getTypeMetadataHQNamed p root0)
      allMd' = maybe allMd (`R.restrictDom` allMd) selection0
      -- then list the values after filtering by type
      allRefs :: Set Reference = R.ran allMd'
  sigs <- for (toList allRefs) (loadTypeOfTerm . Referent.Ref)
  let deps =
        Set.map LD.termRef allRefs
          <> Set.unions [Set.map LD.typeRef . Type.dependencies $ t | Just t <- sigs]
  ppe <- prettyPrintEnvDecl =<< makePrintNamesFromLabeled' deps
  let ppeDecl = PPE.unsuffixifiedPPE ppe
  let sortedSigs = sortOn snd (toList allRefs `zip` sigs)
  let out = [(PPE.termName ppeDecl (Referent.Ref r), r, t) | (r, t) <- sortedSigs]
  pure (PPE.suffixifiedPPE ppe, out)

resolveShortBranchHash ::
  ShortBranchHash -> ExceptT (Output v) (Action' m v) (Branch m)
resolveShortBranchHash hash = ExceptT do
  hashSet <- eval $ BranchHashesByPrefix hash
  len <- eval BranchHashLength
  case Set.toList hashSet of
    [] -> pure . Left $ NoBranchWithHash hash
    [h] -> fmap Right . eval $ LoadLocalBranch h
    _ -> pure . Left $ BranchHashAmbiguous hash (Set.map (SBH.fromHash len) hashSet)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatchNoSync ::
  (Monad m, Var v) =>
  Patch ->
  Path.Absolute ->
  Action' m v Bool
propagatePatchNoSync patch scopePath = do
  r <- use LoopState.root
  let nroot = Branch.toNames (Branch.head r)
  stepAtMNoSync' Branch.CompressHistory
    ( Path.unabsolute scopePath,
      lift . lift . Propagate.propagateAndApply nroot patch
    )

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch ::
  (Monad m, Var v) =>
  LoopState.InputDescription ->
  Patch ->
  Path.Absolute ->
  Action' m v Bool
propagatePatch inputDescription patch scopePath = do
  r <- use LoopState.root
  let nroot = Branch.toNames (Branch.head r)
  stepAtM' Branch.CompressHistory
    (inputDescription <> " (applying patch)")
    ( Path.unabsolute scopePath,
      lift . lift . Propagate.propagateAndApply nroot patch
    )

-- | Create the args needed for showTodoOutput and call it
doShowTodoOutput :: Monad m => Patch -> Path.Absolute -> Action' m v ()
doShowTodoOutput patch scopePath = do
  scope <- getAt scopePath
  let names0 = Branch.toNames (Branch.head scope)
  -- only needs the local references to check for obsolete defs
  let getPpe = do
        names <- makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
        prettyPrintEnvDecl names
  showTodoOutput getPpe patch names0

-- | Show todo output if there are any conflicts or edits.
showTodoOutput ::
  -- | Action that fetches the pretty print env. It's expensive because it
  -- involves looking up historical names, so only call it if necessary.
  Action' m v PPE.PrettyPrintEnvDecl ->
  Patch ->
  Names ->
  Action' m v ()
showTodoOutput getPpe patch names0 = do
  todo <- checkTodo patch names0
  if TO.noConflicts todo && TO.noEdits todo
    then respond NoConflictsOrEdits
    else do
      LoopState.numberedArgs
        .= ( Text.unpack . Reference.toText . view _2
               <$> fst (TO.todoFrontierDependents todo)
           )
      ppe <- getPpe
      respond $ TodoOutput ppe todo

checkTodo :: Patch -> Names -> Action m i v (TO.TodoOutput v Ann)
checkTodo patch names0 = do
  f <- computeFrontier (eval . GetDependents) patch names0
  let dirty = R.dom f
      frontier = R.ran f
  (frontierTerms, frontierTypes) <- loadDisplayInfo frontier
  (dirtyTerms, dirtyTypes) <- loadDisplayInfo dirty
  -- todo: something more intelligent here?
  let scoreFn = const 1
  remainingTransitive <-
    frontierTransitiveDependents (eval . GetDependents) names0 frontier
  let scoredDirtyTerms =
        List.sortOn (view _1) [(scoreFn r, r, t) | (r, t) <- dirtyTerms]
      scoredDirtyTypes =
        List.sortOn (view _1) [(scoreFn r, r, t) | (r, t) <- dirtyTypes]
  pure $
    TO.TodoOutput
      (Set.size remainingTransitive)
      (frontierTerms, frontierTypes)
      (scoredDirtyTerms, scoredDirtyTypes)
      (Names.conflicts names0)
      (Patch.conflicts patch)
  where
    frontierTransitiveDependents ::
      Monad m => (Reference -> m (Set Reference)) -> Names -> Set Reference -> m (Set Reference)
    frontierTransitiveDependents dependents names0 rs = do
      let branchDependents r = Set.filter (Names.contains names0) <$> dependents r
      tdeps <- transitiveClosure branchDependents rs
      -- we don't want the frontier in the result
      pure $ tdeps `Set.difference` rs

-- (d, f) when d is "dirty" (needs update),
--             f is in the frontier (an edited dependency of d),
--         and d depends on f
-- a ⋖ b = a depends directly on b
-- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
--
-- The range of this relation is the frontier, and the domain is
-- the set of dirty references.
computeFrontier ::
  forall m.
  Monad m =>
  (Reference -> m (Set Reference)) -> -- eg Codebase.dependents codebase
  Patch ->
  Names ->
  m (R.Relation Reference Reference)
computeFrontier getDependents patch names =
  let edited :: Set Reference
      edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)
      addDependents :: R.Relation Reference Reference -> Reference -> m (R.Relation Reference Reference)
      addDependents dependents ref =
        (\ds -> R.insertManyDom ds ref dependents) . Set.filter (Names.contains names)
          <$> getDependents ref
   in do
        -- (r,r2) ∈ dependsOn if r depends on r2
        dependsOn <- foldM addDependents R.empty edited
        -- Dirty is everything that `dependsOn` Frontier, minus already edited defns
        pure $ R.filterDom (not . flip Set.member edited) dependsOn

confirmedCommand :: Input -> Action m i v Bool
confirmedCommand i = do
  i0 <- use LoopState.lastInput
  pure $ Just i == i0

listBranch :: Branch0 m -> [SearchResult]
listBranch (Branch.toNames -> b) =
  List.sortOn (\s -> (SR.name s, s)) (SR.fromNames b)

-- | restores the full hash to these search results, for _numberedArgs purposes
searchResultToHQString :: SearchResult -> String
searchResultToHQString = \case
  SR.Tm' n r _ -> HQ.toString $ HQ.requalify n r
  SR.Tp' n r _ -> HQ.toString $ HQ.requalify n (Referent.Ref r)
  _ -> error "impossible match failure"

-- Return a list of definitions whose names fuzzy match the given queries.
fuzzyNameDistance :: Name -> Name -> Maybe Int
fuzzyNameDistance (Name.toString -> q) (Name.toString -> n) =
  Find.simpleFuzzyScore q n

-- return `name` and `name.<everything>...`
_searchBranchPrefix :: Branch m -> Name -> [SearchResult]
_searchBranchPrefix b n = case Path.unsnoc (Path.fromName n) of
  Nothing -> []
  Just (init, last) -> case Branch.getAt init b of
    Nothing -> []
    Just b -> SR.fromNames . Names.prefix0 n $ names0
      where
        lastName = Path.toName (Path.singleton last)
        subnames =
          Branch.toNames . Branch.head $
            Branch.getAt' (Path.singleton last) b
        rootnames =
          Names.filter (== lastName)
            . Branch.toNames
            . set Branch.children mempty
            $ Branch.head b
        names0 = rootnames <> Names.prefix0 lastName subnames

searchResultsFor :: Names -> [Referent] -> [Reference] -> [SearchResult]
searchResultsFor ns terms types =
  [ SR.termSearchResult ns name ref
    | ref <- terms,
      name <- toList (Names.namesForReferent ns ref)
  ]
    <> [ SR.typeSearchResult ns name ref
         | ref <- types,
           name <- toList (Names.namesForReference ns ref)
       ]

searchBranchScored ::
  forall score.
  (Ord score) =>
  Names ->
  (Name -> Name -> Maybe score) ->
  [HQ.HashQualified Name] ->
  [SearchResult]
searchBranchScored names0 score queries =
  nubOrd . fmap snd . toList $ searchTermNamespace <> searchTypeNamespace
  where
    searchTermNamespace = foldMap do1query queries
      where
        do1query :: HQ.HashQualified Name -> Set (Maybe score, SearchResult)
        do1query q = foldMap (score1hq q) (R.toList . Names.terms $ names0)
        score1hq :: HQ.HashQualified Name -> (Name, Referent) -> Set (Maybe score, SearchResult)
        score1hq query (name, ref) = case query of
          HQ.NameOnly qn ->
            pair qn
          HQ.HashQualified qn h
            | h `SH.isPrefixOf` Referent.toShortHash ref ->
              pair qn
          HQ.HashOnly h
            | h `SH.isPrefixOf` Referent.toShortHash ref ->
              Set.singleton (Nothing, result)
          _ -> mempty
          where
            result = SR.termSearchResult names0 name ref
            pair qn = case score qn name of
              Just score -> Set.singleton (Just score, result)
              Nothing -> mempty
    searchTypeNamespace = foldMap do1query queries
      where
        do1query :: HQ.HashQualified Name -> Set (Maybe score, SearchResult)
        do1query q = foldMap (score1hq q) (R.toList . Names.types $ names0)
        score1hq :: HQ.HashQualified Name -> (Name, Reference) -> Set (Maybe score, SearchResult)
        score1hq query (name, ref) = case query of
          HQ.NameOnly qn ->
            pair qn
          HQ.HashQualified qn h
            | h `SH.isPrefixOf` Reference.toShortHash ref ->
              pair qn
          HQ.HashOnly h
            | h `SH.isPrefixOf` Reference.toShortHash ref ->
              Set.singleton (Nothing, result)
          _ -> mempty
          where
            result = SR.typeSearchResult names0 name ref
            pair qn = case score qn name of
              Just score -> Set.singleton (Just score, result)
              Nothing -> mempty

handleBackendError :: Backend.BackendError -> Action m i v ()
handleBackendError = \case
  Backend.NoSuchNamespace path ->
    respond . BranchNotFound $ Path.absoluteToPath' path
  Backend.BadRootBranch e -> respond $ BadRootBranch e
  Backend.NoBranchForHash h -> do
    sbhLength <- eval BranchHashLength
    respond . NoBranchWithHash $ SBH.fromHash sbhLength h
  Backend.CouldntLoadBranch h -> do
    respond . CouldntLoadBranch $ h
  Backend.CouldntExpandBranchHash sbh -> respond $ NoBranchWithHash sbh
  Backend.AmbiguousBranchHash h hashes ->
    respond $ BranchHashAmbiguous h hashes
  Backend.MissingSignatureForTerm r ->
    respond $ TermMissingType r

respond :: MonadCommand n m i v => Output v -> n ()
respond output = eval $ Notify output

respondNumbered :: NumberedOutput v -> Action m i v ()
respondNumbered output = do
  args <- eval $ NotifyNumbered output
  unless (null args) $
    LoopState.numberedArgs .= toList args

unlessError :: ExceptT (Output v) (Action' m v) () -> Action' m v ()
unlessError ma = runExceptT ma >>= either respond pure

unlessError' :: (e -> Output v) -> ExceptT e (Action' m v) () -> Action' m v ()
unlessError' f ma = unlessError $ withExceptT f ma

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch ::
  (Monad m, Var v) =>
  Branch.MergeMode ->
  LoopState.InputDescription ->
  Maybe (Output v) ->
  Branch m ->
  Maybe Path.Path' ->
  Path.Absolute ->
  Action' m v ()
mergeBranchAndPropagateDefaultPatch mode inputDescription unchangedMessage srcb dest0 dest =
  ifM
    (mergeBranch mode inputDescription srcb dest0 dest)
    (loadPropagateDiffDefaultPatch inputDescription dest0 dest)
    (for_ unchangedMessage respond)
  where
    mergeBranch ::
      (Monad m, Var v) =>
      Branch.MergeMode ->
      LoopState.InputDescription ->
      Branch m ->
      Maybe Path.Path' ->
      Path.Absolute ->
      Action' m v Bool
    mergeBranch mode inputDescription srcb dest0 dest = unsafeTime "Merge Branch" $ do
      destb <- getAt dest
      merged <- eval $ Merge mode srcb destb
      b <- updateAtM inputDescription dest (const $ pure merged)
      for_ dest0 $ \dest0 ->
        diffHelper (Branch.head destb) (Branch.head merged)
          >>= respondNumbered . uncurry (ShowDiffAfterMerge dest0 dest)
      pure b

loadPropagateDiffDefaultPatch ::
  (Monad m, Var v) =>
  LoopState.InputDescription ->
  Maybe Path.Path' ->
  Path.Absolute ->
  Action' m v ()
loadPropagateDiffDefaultPatch inputDescription dest0 dest = unsafeTime "Propagate Default Patch" $ do
  original <- getAt dest
  patch <- eval . Eval $ Branch.getPatch defaultPatchNameSegment (Branch.head original)
  patchDidChange <- propagatePatch inputDescription patch dest
  when patchDidChange . for_ dest0 $ \dest0 -> do
    patched <- getAt dest
    let patchPath = snoc dest0 defaultPatchNameSegment
    diffHelper (Branch.head original) (Branch.head patched)
      >>= respondNumbered . uncurry (ShowDiffAfterMergePropagate dest0 dest patchPath)

-- | Get metadata type/value from a name.
--
-- May fail with either:
--
--   * 'MetadataMissingType', if the given name is associated with a single reference, but that reference doesn't have a
--     type.
--   * 'MetadataAmbiguous', if the given name is associated with more than one reference.
getMetadataFromName ::
  Var v =>
  HQ.HashQualified Name ->
  Action m (Either Event Input) v (Either (Output v) (Metadata.Type, Metadata.Value))
getMetadataFromName name = do
  (Set.toList <$> getHQTerms name) >>= \case
    [ref@(Referent.Ref val)] ->
      eval (LoadTypeOfTerm val) >>= \case
        Nothing -> do
          ppe <- getPPE
          pure (Left (MetadataMissingType ppe ref))
        Just ty -> pure (Right (Hashing.typeToReference ty, val))
    -- FIXME: we want a different error message if the given name is associated with a data constructor (`Con`).
    refs -> do
      ppe <- getPPE
      pure (Left (MetadataAmbiguous name ppe refs))
  where
    getPPE :: Action m (Either Event Input) v PPE.PrettyPrintEnv
    getPPE = do
      currentPath' <- use LoopState.currentPath
      sbhLength <- eval BranchHashLength
      Backend.basicSuffixifiedNames sbhLength <$> use LoopState.root <*> pure (Backend.AllNames $ Path.unabsolute currentPath')

-- | Get the set of terms related to a hash-qualified name.
getHQTerms :: HQ.HashQualified Name -> Action' m v (Set Referent)
getHQTerms = \case
  HQ.NameOnly n -> do
    root0 <- Branch.head <$> use LoopState.root
    currentPath' <- use LoopState.currentPath
    -- absolute-ify the name, then lookup in deepTerms of root
    let path =
          n
            & Path.fromName'
            & Path.resolve currentPath'
            & Path.unabsolute
            & Path.toName
    pure $ R.lookupRan path (Branch.deepTerms root0)
  HQ.HashOnly sh -> hashOnly sh
  HQ.HashQualified _ sh -> hashOnly sh
  where
    hashOnly sh = eval $ TermReferentsByShortHash sh

getAt :: Functor m => Path.Absolute -> Action m i v (Branch m)
getAt (Path.Absolute p) =
  use LoopState.root <&> fromMaybe Branch.empty . Branch.getAt p

-- Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM ::
  Applicative m =>
  LoopState.InputDescription ->
  Path.Absolute ->
  (Branch m -> Action m i v (Branch m)) ->
  Action m i v Bool
updateAtM reason (Path.Absolute p) f = do
  b <- use LoopState.lastSavedRoot
  b' <- Branch.modifyAtM p f b
  updateRoot b' reason
  pure $ b /= b'

stepAt ::
  forall m i v.
  Monad m =>
  LoopState.InputDescription ->
  Branch.UpdateStrategy ->
  (Path, Branch0 m -> Branch0 m) ->
  Action m i v ()
stepAt cause strat = stepManyAt @m @[] cause strat . pure

stepAtNoSync ::
  forall m i v.
  Monad m =>
  Branch.UpdateStrategy ->
  (Path, Branch0 m -> Branch0 m) ->
  Action m i v ()
stepAtNoSync strat = stepManyAtNoSync @m @[] strat . pure

stepAtM ::
  forall m i v.
  Monad m =>
  Branch.UpdateStrategy ->
  LoopState.InputDescription ->
  (Path, Branch0 m -> m (Branch0 m)) ->
  Action m i v ()
stepAtM cause strat = stepManyAtM @m @[] cause strat . pure

stepAtM' ::
  forall m i v.
  Monad m =>
  Branch.UpdateStrategy ->
  LoopState.InputDescription ->
  (Path, Branch0 m -> Action m i v (Branch0 m)) ->
  Action m i v Bool
stepAtM' cause strat = stepManyAtM' @m @[] cause strat . pure

stepAtMNoSync' ::
  forall m i v.
  Monad m =>
  Branch.UpdateStrategy ->
  (Path, Branch0 m -> Action m i v (Branch0 m)) ->
  Action m i v Bool
stepAtMNoSync' strat = stepManyAtMNoSync' @m @[] strat . pure

stepManyAt ::
  (Monad m, Foldable f) =>
  LoopState.InputDescription ->
  Branch.UpdateStrategy ->
  f (Path, Branch0 m -> Branch0 m) ->
  Action m i v ()
stepManyAt reason strat actions = do
  stepManyAtNoSync strat actions
  b <- use LoopState.root
  updateRoot b reason

-- Like stepManyAt, but doesn't update the LoopState.root
stepManyAtNoSync ::
  (Monad m, Foldable f) =>
  Branch.UpdateStrategy ->
  f (Path, Branch0 m -> Branch0 m) ->
  Action m i v ()
stepManyAtNoSync strat actions = do
  b <- use LoopState.root
  let new = Branch.stepManyAt strat actions b
  LoopState.root .= new

stepManyAtM ::
  (Monad m, Foldable f) =>
  Branch.UpdateStrategy ->
  LoopState.InputDescription ->
  f (Path, Branch0 m -> m (Branch0 m)) ->
  Action m i v ()
stepManyAtM strat reason actions = do
  stepManyAtMNoSync strat actions
  b <- use LoopState.root
  updateRoot b reason

stepManyAtMNoSync ::
  (Monad m, Foldable f) =>
  Branch.UpdateStrategy ->
  f (Path, Branch0 m -> m (Branch0 m)) ->
  Action m i v ()
stepManyAtMNoSync strat actions = do
  b <- use LoopState.root
  b' <- eval . Eval $ Branch.stepManyAtM strat actions b
  LoopState.root .= b'

stepManyAtM' ::
  (Monad m, Foldable f) =>
  Branch.UpdateStrategy ->
  LoopState.InputDescription ->
  f (Path, Branch0 m -> Action m i v (Branch0 m)) ->
  Action m i v Bool
stepManyAtM' strat reason actions = do
  b <- use LoopState.root
  b' <- Branch.stepManyAtM strat actions b
  updateRoot b' reason
  pure (b /= b')

stepManyAtMNoSync' ::
  (Monad m, Foldable f) =>
  Branch.UpdateStrategy ->
  f (Path, Branch0 m -> Action m i v (Branch0 m)) ->
  Action m i v Bool
stepManyAtMNoSync' strat actions = do
  b <- use LoopState.root
  b' <- Branch.stepManyAtM strat actions b
  LoopState.root .= b'
  pure (b /= b')

-- | Sync the in-memory root branch.
syncRoot :: LoopState.InputDescription -> Action m i v ()
syncRoot description = do
  root' <- use LoopState.root
  Unison.Codebase.Editor.HandleInput.updateRoot root' description

updateRoot :: Branch m -> LoopState.InputDescription -> Action m i v ()
updateRoot new reason = do
  old <- use LoopState.lastSavedRoot
  when (old /= new) $ do
    LoopState.root .= new
    eval $ SyncLocalRootBranch new
    eval $ AppendToReflog reason old new
    LoopState.lastSavedRoot .= new

-- cata for 0, 1, or more elements of a Foldable
-- tries to match as lazily as possible
zeroOneOrMore :: Foldable f => f a -> b -> (a -> b) -> (f a -> b) -> b
zeroOneOrMore f zero one more = case toList f of
  _ : _ : _ -> more f
  a : _ -> one a
  _ -> zero

-- | Goal: When deleting, we might be removing the last name of a given definition (i.e. the
-- definition is going "extinct"). In this case we may wish to take some action or warn the
-- user about these "endangered" definitions which would now contain unnamed references.
getEndangeredDependents ::
  forall m.
  Monad m =>
  -- | Function to acquire dependencies
  (Reference -> m (Set Reference)) ->
  -- | Which names we want to delete
  Names ->
  -- | All names from the root branch
  Names ->
  -- | map from references going extinct to the set of endangered dependents
  m (Map LabeledDependency (NESet LabeledDependency))
getEndangeredDependents getDependents namesToDelete rootNames = do
  let remainingNames :: Names
      remainingNames = rootNames `Names.difference` namesToDelete
      refsToDelete, remainingRefs, extinct :: Set LabeledDependency
      refsToDelete = Names.labeledReferences namesToDelete
      remainingRefs = Names.labeledReferences remainingNames -- left over after delete
      extinct = refsToDelete `Set.difference` remainingRefs -- deleting and not left over
      accumulateDependents :: LabeledDependency -> m (Map LabeledDependency (Set LabeledDependency))
      accumulateDependents ld =
        let ref = LD.fold id Referent.toReference ld
         in Map.singleton ld . Set.map LD.termRef <$> getDependents ref
  -- All dependents of extinct, including terms which might themselves be in the process of being deleted.
  allDependentsOfExtinct :: Map LabeledDependency (Set LabeledDependency) <-
    Map.unionsWith (<>) <$> for (Set.toList extinct) accumulateDependents

  -- Filtered to only include dependencies which are not being deleted, but depend one which
  -- is going extinct.
  let extinctToEndangered :: Map LabeledDependency (NESet LabeledDependency)
      extinctToEndangered = allDependentsOfExtinct & Map.mapMaybe \endangeredDeps ->
        let remainingEndangered = endangeredDeps `Set.intersection` remainingRefs
         in NESet.nonEmptySet remainingEndangered
  pure extinctToEndangered

displayI ::
  Monad m =>
  Names ->
  OutputLocation ->
  HQ.HashQualified Name ->
  Action m (Either Event Input) Symbol ()
displayI prettyPrintNames outputLoc hq = do
  uf <- use LoopState.latestTypecheckedFile >>= addWatch (HQ.toString hq)
  case uf of
    Nothing -> do
      let parseNames = (`NamesWithHistory.NamesWithHistory` mempty) prettyPrintNames
          results = NamesWithHistory.lookupHQTerm hq parseNames
      if Set.null results
        then respond $ SearchTermsNotFound [hq]
        else
          if Set.size results > 1
            then respond $ TermAmbiguous hq results
            else -- ... but use the unsuffixed names for display
            do
              let tm = Term.fromReferent External $ Set.findMin results
              pped <- prettyPrintEnvDecl parseNames
              tm <- eval $ Evaluate1 (PPE.suffixifiedPPE pped) True tm
              case tm of
                Left e -> respond (EvaluationFailure e)
                Right tm -> doDisplay outputLoc parseNames (Term.unannotate tm)
    Just (toDisplay, unisonFile) -> do
      ppe <- executePPE unisonFile
      unlessError' EvaluationFailure do
        evalResult <- ExceptT . eval . Evaluate ppe $ unisonFile
        case Command.lookupEvalResult toDisplay evalResult of
          Nothing -> error $ "Evaluation dropped a watch expression: " <> HQ.toString hq
          Just tm -> lift do
            ns <- displayNames unisonFile
            doDisplay outputLoc ns tm

docsI ::
  Monad m =>
  SrcLoc ->
  Names ->
  Path.HQSplit' ->
  Action m (Either Event Input) Symbol ()
docsI srcLoc prettyPrintNames src = do
  fileByName
  where
    {- Given `docs foo`, we look for docs in 3 places, in this order:
       (fileByName) First check the file for `foo.doc`, and if found do `display foo.doc`
       (codebaseByMetadata) Next check for doc metadata linked to `foo` in the codebase
       (codebaseByName) Lastly check for `foo.doc` in the codebase and if found do `display foo.doc`
    -}
    hq :: HQ.HashQualified Name
    hq =
      let hq' :: HQ'.HashQualified Name
          hq' = Name.convert @Path.Path' @Name <$> Name.convert src
       in Name.convert hq'

    dotDoc :: HQ.HashQualified Name
    dotDoc = hq <&> \n -> Name.joinDot n "doc"

    fileByName = do
      ns <- maybe mempty UF.typecheckedToNames <$> use LoopState.latestTypecheckedFile
      fnames <- pure $ NamesWithHistory.NamesWithHistory ns mempty
      case NamesWithHistory.lookupHQTerm dotDoc fnames of
        s | Set.size s == 1 -> do
          -- the displayI command expects full term names, so we resolve
          -- the hash back to its full name in the file
          fname' <- pure $ NamesWithHistory.longestTermName 10 (Set.findMin s) fnames
          displayI prettyPrintNames ConsoleLocation fname'
        _ -> codebaseByMetadata

    codebaseByMetadata = unlessError do
      (ppe, out) <- getLinks srcLoc src (Left $ Set.fromList [DD.docRef, DD.doc2Ref])
      lift case out of
        [] -> codebaseByName
        [(_name, ref, _tm)] -> do
          len <- eval BranchHashLength
          let names = NamesWithHistory.NamesWithHistory prettyPrintNames mempty
          let tm = Term.ref External ref
          tm <- eval $ Evaluate1 (PPE.fromNames len names) True tm
          case tm of
            Left e -> respond (EvaluationFailure e)
            Right tm -> doDisplay ConsoleLocation names (Term.unannotate tm)
        out -> do
          LoopState.numberedArgs .= fmap (HQ.toString . view _1) out
          respond $ ListOfLinks ppe out

    codebaseByName = do
      parseNames <- basicParseNames
      case NamesWithHistory.lookupHQTerm dotDoc (NamesWithHistory.NamesWithHistory parseNames mempty) of
        s
          | Set.size s == 1 -> displayI prettyPrintNames ConsoleLocation dotDoc
          | Set.size s == 0 -> respond $ ListOfLinks mempty []
          | otherwise -> -- todo: return a list of links here too
            respond $ ListOfLinks mempty []

filterBySlurpResult ::
  Ord v =>
  SlurpResult v ->
  UF.TypecheckedUnisonFile v Ann ->
  UF.TypecheckedUnisonFile v Ann
filterBySlurpResult
  SlurpResult {adds, updates}
  ( UF.TypecheckedUnisonFileId
      dataDeclarations'
      effectDeclarations'
      topLevelComponents'
      watchComponents
      hashTerms
    ) =
    UF.TypecheckedUnisonFileId datas effects tlcs watches hashTerms'
    where
      keep = updates <> adds
      keepTerms = SC.terms keep
      keepTypes = SC.types keep
      hashTerms' = Map.restrictKeys hashTerms keepTerms
      datas = Map.restrictKeys dataDeclarations' keepTypes
      effects = Map.restrictKeys effectDeclarations' keepTypes
      tlcs = filter (not . null) $ fmap (List.filter filterTLC) topLevelComponents'
      watches = filter (not . null . snd) $ fmap (second (List.filter filterTLC)) watchComponents
      filterTLC (v, _, _) = Set.member v keepTerms

-- updates the namespace for adding `slurp`
doSlurpAdds ::
  forall m v.
  (Monad m, Var v) =>
  SlurpComponent v ->
  UF.TypecheckedUnisonFile v Ann ->
  (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.batchUpdates (typeActions <> termActions)
  where
    typeActions = map doType . toList $ SC.types slurp
    termActions =
      map doTerm . toList $
        SC.terms slurp <> Slurp.constructorsFor (SC.types slurp) uf
    names = UF.typecheckedToNames uf
    tests = Set.fromList $ fst <$> UF.watchesOfKind WK.TestWatch (UF.discardTypes uf)
    (isTestType, isTestValue) = isTest
    md v =
      if Set.member v tests
        then Metadata.singleton isTestType isTestValue
        else Metadata.empty
    doTerm :: v -> (Path, Branch0 m -> Branch0 m)
    doTerm v = case toList (Names.termsNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] -> case Path.splitFromName (Name.unsafeFromVar v) of
        Nothing -> errorEmptyVar
        Just split -> BranchUtil.makeAddTermName split r (md v)
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple terms named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    doType :: v -> (Path, Branch0 m -> Branch0 m)
    doType v = case toList (Names.typesNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] -> case Path.splitFromName (Name.unsafeFromVar v) of
        Nothing -> errorEmptyVar
        Just split -> BranchUtil.makeAddTypeName split r Metadata.empty
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple types named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    errorEmptyVar = error "encountered an empty var name"
    errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

doSlurpUpdates ::
  Monad m =>
  Map Name (Reference, Reference) ->
  Map Name (Reference, Reference) ->
  [(Name, Referent)] ->
  (Branch0 m -> Branch0 m)
doSlurpUpdates typeEdits termEdits deprecated b0 =
  Branch.batchUpdates (typeActions <> termActions <> deprecateActions) b0
  where
    typeActions = join . map doType . Map.toList $ typeEdits
    termActions = join . map doTerm . Map.toList $ termEdits
    deprecateActions = join . map doDeprecate $ deprecated
      where
        doDeprecate (n, r) = case Path.splitFromName n of
          Nothing -> errorEmptyVar
          Just split -> [BranchUtil.makeDeleteTermName split r]

    -- we copy over the metadata on the old thing
    -- todo: if the thing being updated, m, is metadata for something x in b0
    -- update x's md to reference `m`
    doType,
      doTerm ::
        (Name, (Reference, Reference)) -> [(Path, Branch0 m -> Branch0 m)]
    doType (n, (old, new)) = case Path.splitFromName n of
      Nothing -> errorEmptyVar
      Just split ->
        [ BranchUtil.makeDeleteTypeName split old,
          BranchUtil.makeAddTypeName split new oldMd
        ]
        where
          oldMd = BranchUtil.getTypeMetadataAt split old b0
    doTerm (n, (old, new)) = case Path.splitFromName n of
      Nothing -> errorEmptyVar
      Just split ->
        [ BranchUtil.makeDeleteTermName split (Referent.Ref old),
          BranchUtil.makeAddTermName split (Referent.Ref new) oldMd
        ]
        where
          -- oldMd is the metadata linked to the old definition
          -- we relink it to the new definition
          oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0
    errorEmptyVar = error "encountered an empty var name"

loadDisplayInfo ::
  Set Reference ->
  Action
    m
    i
    v
    ( [(Reference, Maybe (Type v Ann))],
      [(Reference, DisplayObject () (DD.Decl v Ann))]
    )
loadDisplayInfo refs = do
  termRefs <- filterM (eval . IsTerm) (toList refs)
  typeRefs <- filterM (eval . IsType) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> eval (LoadTypeOfTerm r)
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayObject r
  pure (terms, types)

-- Any absolute names in the input which have `currentPath` as a prefix
-- are converted to names relative to current path. all other names are
-- converted to absolute names. For example:
--
-- e.g. if LoopState.currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names -> Names
fixupNamesRelative currentPath' = Names.map fixName
  where
    prefix = Path.toName (Path.unabsolute currentPath')
    fixName n =
      if currentPath' == Path.absoluteEmpty
        then n
        else fromMaybe (Name.makeAbsolute n) (Name.stripNamePrefix prefix n)

makeHistoricalParsingNames ::
  Monad m => Set (HQ.HashQualified Name) -> Action' m v NamesWithHistory
makeHistoricalParsingNames lexedHQs = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- basicParseNames
  curPath <- use LoopState.currentPath
  pure $
    NamesWithHistory
      basicNames
      ( Names.makeAbsolute rawHistoricalNames
          <> fixupNamesRelative curPath rawHistoricalNames
      )

loadTypeDisplayObject ::
  Reference -> Action m i v (DisplayObject () (DD.Decl v Ann))
loadTypeDisplayObject = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> eval (LoadType id)

lexedSource :: Monad m => SourceName -> Source -> Action' m v (NamesWithHistory, LexedSource)
lexedSource name src = do
  let tokens = L.lexer (Text.unpack name) (Text.unpack src)
      getHQ = \case
        L.WordyId s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.SymbolyId s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.Hash sh -> Just (HQ.HashOnly sh)
        _ -> Nothing
      hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
  parseNames <- makeHistoricalParsingNames hqs
  pure (parseNames, (src, tokens))

suffixifiedPPE :: NamesWithHistory -> Action' m v PPE.PrettyPrintEnv
suffixifiedPPE ns = eval CodebaseHashLength <&> (`PPE.fromSuffixNames` ns)

fqnPPE :: NamesWithHistory -> Action' m v PPE.PrettyPrintEnv
fqnPPE ns = eval CodebaseHashLength <&> (`PPE.fromNames` ns)

parseSearchType ::
  (Monad m, Var v) =>
  SrcLoc ->
  String ->
  Action' m v (Either (Output v) (Type v Ann))
parseSearchType srcLoc typ = fmap Type.removeAllEffectVars <$> parseType srcLoc typ

-- | A description of where the given parse was triggered from, for error messaging purposes.
type SrcLoc = String

parseType ::
  (Monad m, Var v) =>
  SrcLoc ->
  String ->
  Action' m v (Either (Output v) (Type v Ann))
parseType input src = do
  -- `show Input` is the name of the "file" being lexed
  (names0, lexed) <- lexedSource (Text.pack input) (Text.pack src)
  parseNames <- basicParseNames
  let names =
        NamesWithHistory.push
          (NamesWithHistory.currentNames names0)
          (NamesWithHistory.NamesWithHistory parseNames (NamesWithHistory.oldNames names0))
  e <- eval $ ParseType names lexed
  pure $ case e of
    Left err -> Left $ TypeParseError src err
    Right typ -> case Type.bindNames mempty (NamesWithHistory.currentNames names) $
      Type.generalizeLowercase mempty typ of
      Left es -> Left $ ParseResolutionFailures src (toList es)
      Right typ -> Right typ

makeShadowedPrintNamesFromLabeled ::
  Monad m => Set LabeledDependency -> Names -> Action' m v NamesWithHistory
makeShadowedPrintNamesFromLabeled deps shadowing =
  NamesWithHistory.shadowing shadowing <$> makePrintNamesFromLabeled' deps

makePrintNamesFromLabeled' ::
  Monad m => Set LabeledDependency -> Action' m v NamesWithHistory
makePrintNamesFromLabeled' deps = do
  root' <- use LoopState.root
  curPath <- use LoopState.currentPath
  (_missing, rawHistoricalNames) <-
    eval . Eval $
      Branch.findHistoricalRefs
        deps
        root'
  basicNames <- basicPrettyPrintNamesA
  pure $ NamesWithHistory basicNames (fixupNamesRelative curPath rawHistoricalNames)

getTermsIncludingHistorical ::
  Monad m => Path.HQSplit -> Branch0 m -> Action' m v (Set Referent)
getTermsIncludingHistorical (p, hq) b = case Set.toList refs of
  [] -> case hq of
    HQ'.HashQualified n hs -> do
      names <-
        findHistoricalHQs $
          Set.fromList [HQ.HashQualified (Name.unsafeFromText (NameSegment.toText n)) hs]
      pure . R.ran $ Names.terms names
    _ -> pure Set.empty
  _ -> pure refs
  where
    refs = BranchUtil.getTerm (p, hq) b

-- discards inputs that aren't hashqualified;
-- I'd enforce it with finer-grained types if we had them.
findHistoricalHQs :: Monad m => Set (HQ.HashQualified Name) -> Action' m v Names
findHistoricalHQs lexedHQs0 = do
  root' <- use LoopState.root
  curPath <- use LoopState.currentPath
  let -- omg this nightmare name-to-path parsing code is littered everywhere.
      -- We need to refactor so that the absolute-ness of a name isn't represented
      -- by magical text combinations.
      -- Anyway, this function takes a name, tries to determine whether it is
      -- relative or absolute, and tries to return the corresponding name that is
      -- /relative/ to the LoopState.root.
      preprocess n = case Name.toString n of
        -- some absolute name that isn't just "."
        '.' : t@(_ : _) -> Name.unsafeFromString t
        -- something in current path
        _ ->
          if Path.isRoot curPath
            then n
            else Name.joinDot (Path.toName . Path.unabsolute $ curPath) n

      lexedHQs = Set.map (fmap preprocess) . Set.filter HQ.hasHash $ lexedHQs0
  (_missing, rawHistoricalNames) <- eval . Eval $ Branch.findHistoricalHQs lexedHQs root'
  pure rawHistoricalNames

basicPrettyPrintNamesA :: Functor m => Action' m v Names
basicPrettyPrintNamesA = snd <$> basicNames'

makeShadowedPrintNamesFromHQ :: Monad m => Set (HQ.HashQualified Name) -> Names -> Action' m v NamesWithHistory
makeShadowedPrintNamesFromHQ lexedHQs shadowing = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- basicPrettyPrintNamesA
  curPath <- use LoopState.currentPath
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    NamesWithHistory.shadowing
      shadowing
      (NamesWithHistory basicNames (fixupNamesRelative curPath rawHistoricalNames))

basicParseNames, slurpResultNames :: Functor m => Action' m v Names
basicParseNames = fst <$> basicNames'
-- we check the file against everything in the current path
slurpResultNames = currentPathNames

currentPathNames :: Functor m => Action' m v Names
currentPathNames = do
  currentPath' <- use LoopState.currentPath
  currentBranch' <- getAt currentPath'
  pure $ Branch.toNames (Branch.head currentBranch')

-- implementation detail of basicParseNames and basicPrettyPrintNames
basicNames' :: (Functor m) => Action m i v (Names, Names)
basicNames' = do
  root' <- use LoopState.root
  currentPath' <- use LoopState.currentPath
  pure $ Backend.basicNames' root' (Backend.AllNames $ Path.unabsolute currentPath')

data AddRunMainResult v
  = NoTermWithThatName
  | TermHasBadType (Type v Ann)
  | RunMainSuccess (TypecheckedUnisonFile v Ann)

-- Adds a watch expression of the given name to the file, if
-- it would resolve to a TLD in the file. Returns the freshened
-- variable name and the new typechecked file.
--
-- Otherwise, returns `Nothing`.
addWatch ::
  (Monad m, Var v) =>
  String ->
  Maybe (TypecheckedUnisonFile v Ann) ->
  Action' m v (Maybe (v, TypecheckedUnisonFile v Ann))
addWatch _watchName Nothing = pure Nothing
addWatch watchName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == watchName) . view _1) components
  case mainComponent of
    [(v, tm, ty)] ->
      pure . pure $
        let v2 = Var.freshIn (Set.fromList [v]) v
            a = ABT.annotation tm
         in ( v2,
              UF.typecheckedUnisonFile
                (UF.dataDeclarationsId' uf)
                (UF.effectDeclarationsId' uf)
                (UF.topLevelComponents' uf)
                (UF.watchComponents uf <> [(WK.RegularWatch, [(v2, Term.var a v, ty)])])
            )
    _ -> addWatch watchName Nothing

-- Given a typechecked file with a main function called `mainName`
-- of the type `'{IO} ()`, adds an extra binding which
-- forces the `main` function.
--
-- If that function doesn't exist in the typechecked file, the
-- codebase is consulted.
addRunMain ::
  (Monad m, Var v) =>
  String ->
  Maybe (TypecheckedUnisonFile v Ann) ->
  Action' m v (AddRunMainResult v)
addRunMain mainName Nothing = do
  parseNames <- basicParseNames
  let loadTypeOfTerm ref = eval $ LoadTypeOfTerm ref
  mainType <- eval RuntimeMain
  mainToFile
    <$> MainTerm.getMainTerm loadTypeOfTerm parseNames mainName mainType
  where
    mainToFile (MainTerm.NotAFunctionName _) = NoTermWithThatName
    mainToFile (MainTerm.NotFound _) = NoTermWithThatName
    mainToFile (MainTerm.BadType _ ty) = maybe NoTermWithThatName TermHasBadType ty
    mainToFile (MainTerm.Success hq tm typ) =
      RunMainSuccess $
        let v = Var.named (HQ.toText hq)
         in UF.typecheckedUnisonFile mempty mempty mempty [("main", [(v, tm, typ)])] -- mempty
addRunMain mainName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == mainName) . view _1) components
  mainType <- eval RuntimeMain
  case mainComponent of
    [(v, tm, ty)] ->
      pure $
        let v2 = Var.freshIn (Set.fromList [v]) v
            a = ABT.annotation tm
         in if Typechecker.isSubtype ty mainType
              then
                RunMainSuccess $
                  let runMain = DD.forceTerm a a (Term.var a v)
                   in UF.typecheckedUnisonFile
                        (UF.dataDeclarationsId' uf)
                        (UF.effectDeclarationsId' uf)
                        (UF.topLevelComponents' uf)
                        (UF.watchComponents uf <> [("main", [(v2, runMain, mainType)])])
              else TermHasBadType ty
    _ -> addRunMain mainName Nothing

executePPE ::
  (Var v, Monad m) =>
  TypecheckedUnisonFile v a ->
  Action' m v PPE.PrettyPrintEnv
executePPE unisonFile =
  suffixifiedPPE =<< displayNames unisonFile

-- Produce a `Names` needed to display all the hashes used in the given file.
displayNames ::
  (Var v, Monad m) =>
  TypecheckedUnisonFile v a ->
  Action' m v NamesWithHistory
displayNames unisonFile =
  -- voodoo
  makeShadowedPrintNamesFromLabeled
    (UF.termSignatureExternalLabeledDependencies unisonFile)
    (UF.typecheckedToNames unisonFile)

diffHelper ::
  (Monad m) =>
  Branch0 m ->
  Branch0 m ->
  Action' m v (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput v Ann)
diffHelper before after = do
  currentRoot <- use LoopState.root
  currentPath <- use LoopState.currentPath
  diffHelperCmd currentRoot currentPath before after

-- | A version of diffHelper that only requires a MonadCommand constraint
diffHelperCmd ::
  (Monad m, MonadCommand n m i v) =>
  Branch m ->
  Path.Absolute ->
  Branch0 m ->
  Branch0 m ->
  n (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput v Ann)
diffHelperCmd currentRoot currentPath before after = do
  hqLength <- eval CodebaseHashLength
  diff <- eval . Eval $ BranchDiff.diff0 before after
  let (_parseNames, prettyNames0) = Backend.basicNames' currentRoot (Backend.AllNames $ Path.unabsolute currentPath)
  ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl (NamesWithHistory prettyNames0 mempty)
  (ppe,)
    <$> OBranchDiff.toOutput
      loadTypeOfTerm
      declOrBuiltin
      hqLength
      (Branch.toNames before)
      (Branch.toNames after)
      ppe
      diff


loadTypeOfTerm :: MonadCommand n m i v => Referent -> n (Maybe (Type v Ann))
loadTypeOfTerm (Referent.Ref r) = eval $ LoadTypeOfTerm r
loadTypeOfTerm (Referent.Con (ConstructorReference (Reference.DerivedId r) cid) _) = do
  decl <- eval $ LoadType r
  case decl of
    Just (either DD.toDataDecl id -> dd) -> pure $ DD.typeOfConstructor dd cid
    Nothing -> pure Nothing
loadTypeOfTerm Referent.Con {} =
  error $
    reportBug "924628772" "Attempt to load a type declaration which is a builtin!"

declOrBuiltin :: MonadCommand n m i v => Reference -> n (Maybe (DD.DeclOrBuiltin v Ann))
declOrBuiltin r = case r of
  Reference.Builtin {} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> eval (LoadType id)

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
fuzzySelectDefinition :: Position -> Branch0 m -> Action m (Either Event Input) v (Maybe [HQ.HashQualified Name])
fuzzySelectDefinition pos searchBranch0 = do
  let termsAndTypes =
        Relation.dom (Names.hashQualifyTermsRelation (Relation.swap $ Branch.deepTerms searchBranch0))
          <> Relation.dom (Names.hashQualifyTypesRelation (Relation.swap $ Branch.deepTypes searchBranch0))
  let inputs :: [HQ.HashQualified Name]
      inputs =
        termsAndTypes
          & Set.toList
          & map (fmap (Name.setPosition pos))
  eval (FuzzySelect Fuzzy.defaultOptions HQ.toText inputs)

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
fuzzySelectNamespace :: Position -> Branch0 m -> Action m (Either Event Input) v (Maybe [Path'])
fuzzySelectNamespace pos searchBranch0 = do
  let intoPath' :: Path -> Path'
      intoPath' = case pos of
        Relative -> Path' . Right . Path.Relative
        Absolute -> Path' . Left . Path.Absolute
  let inputs :: [Path']
      inputs =
        searchBranch0
          & Branch.deepPaths
          & Set.toList
          & map intoPath'
  eval
    ( FuzzySelect
        Fuzzy.defaultOptions {Fuzzy.allowMultiSelect = False}
        tShow
        inputs
    )

-- | Get a branch from a BranchId, returning an empty one if missing, or failing with an
-- appropriate error message if a hash cannot be found.
branchForBranchId :: Functor m => AbsBranchId -> ExceptT (Output v) (Action' m v) (Branch m)
branchForBranchId = \case
  Left hash -> do
    resolveShortBranchHash hash
  Right path -> do
    lift $ getAt path

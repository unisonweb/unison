{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}

module Unison.Codebase.Editor.HandleInput
  ( loop
  , loopState0
  , LoopState(..)
  , currentPath
  , parseSearchType
  )
where

import           Unison.Prelude

import qualified Unison.Codebase.MainTerm as MainTerm
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.DisplayThing
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.RemoteRepo (RemoteNamespace, printNamespace)
import qualified Unison.CommandLine.InputPattern as InputPattern
import qualified Unison.CommandLine.InputPatterns as InputPatterns

import           Control.Lens
import           Control.Monad.State            ( StateT )
import           Control.Monad.Except           ( ExceptT(..), runExceptT, withExceptT)
import           Data.Bifunctor                 ( second, first )
import           Data.Configurator              ()
import qualified Data.List                      as List
import           Data.List                      ( partition )
import           Data.List.Extra                ( nubOrd, sort )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Text.Megaparsec               as P
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq(..) )
import qualified Unison.ABT                    as ABT
import qualified Unison.Codebase.BranchDiff    as BranchDiff
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Codebase.Metadata      as Metadata
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Path           ( Path
                                                , Path'(..) )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Reflog        as Reflog
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Codebase.SyncMode      as SyncMode
import qualified Unison.Builtin.Decls          as DD
import qualified Unison.DataDeclaration        as DD
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name )
import           Unison.Names3                  ( Names(..), Names0
                                                , pattern Names0 )
import qualified Unison.Names2                 as Names
import qualified Unison.Names3                 as Names3
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference(..) )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Result                  ( pattern Result )
import qualified Unison.ShortHash as SH
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Util.List               ( uniqueBy )
import qualified Unison.Util.Relation          as R
import qualified Unison.Util.Relation4          as R4
import           Unison.Util.Timing             (unsafeTime)
import           Unison.Util.TransitiveClosure  (transitiveClosure)
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Codebase.TermEdit (TermEdit(..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Typechecker as Typechecker
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Runtime.IOSource       ( isTest )
import qualified Unison.Runtime.IOSource as IOSource
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Monoid            as Monoid
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.Codebase.Editor.TodoOutput as TO
import qualified Unison.Lexer as L
import Unison.Codebase.Editor.SearchResult' (SearchResult')
import qualified Unison.Codebase.Editor.SearchResult' as SR'
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.Builtin as Builtin
import qualified Unison.Builtin.Terms as Builtin
import Unison.NameSegment (NameSegment(..))
import qualified Unison.NameSegment as NameSegment
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.Editor.Propagate as Propagate
import qualified Unison.Codebase.Editor.UriParser as UriParser
import Data.Tuple.Extra (uncurry3)
import qualified Unison.CommandLine.DisplayValues as DisplayValues
import qualified Control.Error.Util as ErrorUtil
import Unison.Util.Monoid (intercalateMap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import Unison.Codebase.Editor.AuthorInfo (AuthorInfo(..))

type F m i v = Free (Command m i v)

-- type (Action m i v) a
type Action m i v = MaybeT (StateT (LoopState m v) (F m i v))

_liftToAction :: m a -> Action m i v a
_liftToAction = lift . lift . Free.eval . Eval

data LoopState m v
  = LoopState
      { _root :: Branch m
      , _lastSavedRoot :: Branch m
      -- the current position in the namespace
      , _currentPathStack :: NonEmpty Path.Absolute

      -- TBD
      -- , _activeEdits :: Set Branch.EditGuid

      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann)

      -- The previous user input. Used to request confirmation of
      -- questionable user commands.
      , _lastInput :: Maybe Input

      -- A 1-indexed list of strings that can be referenced by index at the
      -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
      -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
      , _numberedArgs :: NumberedArgs
      }

type SkipNextUpdate = Bool
type InputDescription = Text

makeLenses ''LoopState

-- replacing the old read/write scalar Lens with "peek" Getter for the NonEmpty
currentPath :: Getter (LoopState m v) Path.Absolute
currentPath = currentPathStack . to Nel.head

loopState0 :: Branch m -> Path.Absolute -> LoopState m v
loopState0 b p = LoopState b b (pure p) Nothing Nothing Nothing []

type Action' m v = Action m (Either Event Input) v

defaultPatchNameSegment :: NameSegment
defaultPatchNameSegment = "patch"

loop :: forall m v . (Monad m, Var v) => Action m (Either Event Input) v ()
loop = do
  uf           <- use latestTypecheckedFile
  root'        <- use root
  currentPath' <- use currentPath
  latestFile'  <- use latestFile
  currentBranch' <- getAt currentPath'
  e           <- eval Input
  hqLength    <- eval CodebaseHashLength
  sbhLength   <- eval BranchHashLength
  let
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
      getHQTerms :: HQ.HashQualified -> Action' m v (Set Referent)
      getHQTerms hq = case hq of
        HQ.NameOnly n -> let
          -- absolute-ify the name, then lookup in deepTerms of root
          path :: Path.Path'
          path = Path.fromName' n
          Path.Absolute absPath = resolveToAbsolute path
          in pure $ R.lookupRan (Path.toName absPath) (Branch.deepTerms root0)
        HQ.HashOnly sh -> hashOnly sh
        HQ.HashQualified _ sh -> hashOnly sh
        where
        hashOnly sh = eval $ TermReferentsByShortHash sh

      resolveHHQS'Types :: HashOrHQSplit' -> Action' m v (Set Reference)
      resolveHHQS'Types = either
        (eval . TypeReferencesByShortHash)
        (pure . getHQ'Types)
      -- Term Refs and Cons
      resolveHHQS'Referents = either
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
        let
          getHQ = \case
            L.Backticks s (Just sh) ->
              Just (HQ.HashQualified (Name.unsafeFromString s) sh)
            L.WordyId s (Just sh) ->
              Just (HQ.HashQualified (Name.unsafeFromString s) sh)
            L.SymbolyId s (Just sh) ->
              Just (HQ.HashQualified (Name.unsafeFromString s) sh)
            L.Hash sh -> Just (HQ.HashOnly sh)
            _         -> Nothing
          hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
        parseNames :: Names <- makeHistoricalParsingNames hqs
        latestFile .= Just (Text.unpack sourceName, False)
        latestTypecheckedFile .= Nothing
        Result notes r <- eval $ Typecheck ambient parseNames sourceName lexed
        case r of
          -- Parsing failed
          Nothing -> respond $
            ParseErrors text [ err | Result.Parsing err <- toList notes ]
          Just (Left errNames) -> do
            ppe <- prettyPrintEnv =<< makeShadowedPrintNamesFromHQ hqs errNames
            let tes = [ err | Result.TypeError err <- toList notes ]
                cbs = [ bug
                      | Result.CompilerBug (Result.TypecheckerBug bug)
                          <- toList notes
                      ]
            when (not $ null tes) . respond $ TypeErrors text ppe tes
            when (not $ null cbs) . respond $ CompilerBugs text ppe cbs
          Just (Right uf) -> k uf
      loadUnisonFile sourceName text = do
        let lexed = L.lexer (Text.unpack sourceName) (Text.unpack text)
        withFile [] sourceName (text, lexed) $ \unisonFile -> do
          sr <- toSlurpResult currentPath' unisonFile <$> slurpResultNames0
          names <- makeShadowedPrintNamesFromLabeled
                      (UF.termSignatureExternalLabeledDependencies unisonFile)
                      (UF.typecheckedToNames0 unisonFile)
          ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl names
          eval . Notify $ Typechecked sourceName ppe sr unisonFile
          unlessError' EvaluationFailure do
            (bindings, e) <- ExceptT . eval . Evaluate ppe $ unisonFile
            lift do
              let e' = Map.map go e
                  go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
              unless (null e') $
                eval . Notify $ Evaluated text ppe bindings e'
              latestTypecheckedFile .= Just unisonFile

  case e of
    Left (IncomingRootBranch hashes) ->
      eval . Notify $ WarnIncomingRootBranch
                        (SBH.fromHash sbhLength $ Branch.headHash root')
                        (Set.map (SBH.fromHash sbhLength) hashes)
    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      if maybe False snd latestFile'
        then modifying latestFile (fmap (const False) <$>)
        else loadUnisonFile sourceName text
    Right input ->
      let
        ifConfirmed = ifM (confirmedCommand input)
        branchNotFound = respond . BranchNotFound
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
        hqNameQuery' doSuffixify hqs = do
          let (hqnames, hashes) = partition (isJust . HQ.toName) hqs
          termRefs <- filter (not . Set.null . snd) . zip hashes <$> traverse
            (eval . TermReferentsByShortHash)
            (catMaybes (HQ.toHash <$> hashes))
          typeRefs <- filter (not . Set.null . snd) . zip hashes <$> traverse
            (eval . TypeReferencesByShortHash)
            (catMaybes (HQ.toHash <$> hashes))
          parseNames0 <- makeHistoricalParsingNames $ Set.fromList hqnames
          let
            mkTermResult n r = SR.termResult (HQ'.fromHQ' n) r Set.empty
            mkTypeResult n r = SR.typeResult (HQ'.fromHQ' n) r Set.empty
            termResults =
              (\(n, tms) -> (n, toList $ mkTermResult n <$> toList tms)) <$> termRefs
            typeResults =
              (\(n, tps) -> (n, toList $ mkTypeResult n <$> toList tps)) <$> typeRefs
            parseNames = (if doSuffixify then Names3.suffixify else id) parseNames0
            resultss   = searchBranchExact hqLength parseNames hqnames
            missingRefs =
              [ x
              | x <- hashes
              , isNothing (lookup x termRefs) && isNothing (lookup x typeRefs)
              ]
            (misses, hits) =
              partition (\(_, results) -> null results) (zip hqs resultss)
            results =
              List.sort
                .   uniqueBy SR.toReferent
                $   (hits ++ termResults ++ typeResults)
                >>= snd
          pure (missingRefs ++ (fst <$> misses), results)
        hqNameQuery = hqNameQuery' False
        hqNameQuerySuffixify = hqNameQuery' True
        typeReferences :: [SearchResult] -> [Reference]
        typeReferences rs
          = [ r | SR.Tp (SR.TypeResult _ r _) <- rs ]
        termReferences :: [SearchResult] -> [Reference]
        termReferences rs =
          [ r | SR.Tm (SR.TermResult _ (Referent.Ref r) _) <- rs ]
        termResults rs = [ r | SR.Tm r <- rs ]
        typeResults rs = [ r | SR.Tp r <- rs ]
        doRemoveReplacement from patchPath isTerm = do
          let patchPath' = fromMaybe defaultPatchPath patchPath
          patch <- getPatchAt patchPath'
          (misses', hits) <- hqNameQuery [from]
          let tpRefs = Set.fromList $ typeReferences hits
              tmRefs = Set.fromList $ termReferences hits
              misses = Set.difference (Set.fromList misses') if isTerm
                then Set.fromList $ HQ'.toHQ . SR.termName <$> termResults hits
                else Set.fromList $ HQ'.toHQ . SR.typeName <$> typeResults hits
              go :: Reference -> Action m (Either Event Input) v ()
              go fr = do
                let termPatch =
                      over Patch.termEdits (R.deleteDom fr) patch
                    typePatch =
                      over Patch.typeEdits (R.deleteDom fr) patch
                    (patchPath'', patchName) = resolveSplit' patchPath'
                  -- Save the modified patch
                stepAtM inputDescription
                          (patchPath'',
                           Branch.modifyPatches
                             patchName
                             (const (if isTerm then termPatch else typePatch)))
                -- Say something
                success
          unless (Set.null misses) $
            respond $ SearchTermsNotFound (Set.toList misses)
          traverse_ go (if isTerm then tmRefs else tpRefs)
        branchExists dest _x = respond $ BranchAlreadyExists dest
        branchExistsSplit = branchExists . Path.unsplit'
        typeExists dest = respond . TypeAlreadyExists dest
        termExists dest = respond . TermAlreadyExists dest
        -- | try to get these as close as possible to the command that caused the change
        inputDescription :: InputDescription
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
          DeleteBranchI opath -> "delete.namespace " <> ops' opath
          DeletePatchI path -> "delete.patch " <> ps' path
          ReplaceTermI src target p ->
            "replace.term " <> HQ.toText src <> " "
                            <> HQ.toText target <> " "
                            <> opatch p
          ReplaceTypeI src target p ->
            "replace.type " <> HQ.toText src <> " "
                            <> HQ.toText target <> " "
                            <> opatch p
          ResolveTermNameI path -> "resolve.termName " <> hqs' path
          ResolveTypeNameI path -> "resolve.typeName " <> hqs' path
          AddI _selection -> "add"
          UpdateI p _selection -> "update " <> opatch p
          PropagatePatchI p scope -> "patch " <> ps' p <> " " <> p' scope
          UndoI{} -> "undo"
          ExecuteI s -> "execute " <> Text.pack s
          IOTestI hq -> "io.test " <> HQ.toText hq
          LinkI md defs ->
            "link " <> HQ.toText md <> " " <> intercalateMap " " hqs' defs
          UnlinkI md defs ->
            "unlink " <> HQ.toText md <> " " <> intercalateMap " " hqs' defs
          UpdateBuiltinsI -> "builtins.update"
          MergeBuiltinsI -> "builtins.merge"
          MergeIOBuiltinsI -> "builtins.mergeio"
          PullRemoteBranchI orepo dest _syncMode ->
            (Text.pack . InputPattern.patternName
              $ InputPatterns.patternFromInput input)
              <> " "
              -- todo: show the actual config-loaded namespace
              <> maybe "(remote namespace from .unisonConfig)"
                       (uncurry3 printNamespace) orepo
              <> " "
              <> p' dest
          LoadI{} -> wat
          PreviewAddI{} -> wat
          PreviewUpdateI{} -> wat
          CreateAuthorI (NameSegment id) name -> "create.author " <> id <> " " <> name
          CreatePullRequestI{} -> wat
          LoadPullRequestI base head dest ->
            "pr.load "
              <> uncurry3 printNamespace base
              <> " "
              <> uncurry3 printNamespace head
              <> " "
              <> p' dest
          PushRemoteBranchI{} -> wat
          PreviewMergeLocalBranchI{} -> wat
          DiffNamespaceI{} -> wat
          SwitchBranchI{} -> wat
          PopBranchI{} -> wat
          NamesI{} -> wat
          TodoI{} -> wat
          ListEditsI{} -> wat
          ListDependenciesI{} -> wat
          ListDependentsI{} -> wat
          HistoryI{} -> wat
          TestI{} -> wat
          LinksI{} -> wat
          SearchByNameI{} -> wat
          FindShallowI{} -> wat
          FindPatchI{} -> wat
          ShowDefinitionI{} -> wat
          DisplayI{} -> wat
          DocsI{} -> wat
          ShowDefinitionByPrefixI{} -> wat
          ShowReflogI{} -> wat
          DebugNumberedArgsI{} -> wat
          DebugBranchHistoryI{} -> wat
          DebugTypecheckedUnisonFileI{} -> wat
          QuitI{} -> wat
          DeprecateTermI{} -> undefined
          DeprecateTypeI{} -> undefined
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
        stepManyAtNoSync =
          Unison.Codebase.Editor.HandleInput.stepManyAtNoSync
        updateRoot = flip Unison.Codebase.Editor.HandleInput.updateRoot inputDescription
        syncRoot = use root >>= updateRoot
        updateAtM = Unison.Codebase.Editor.HandleInput.updateAtM inputDescription
        unlessGitError = unlessError' (Output.GitError input)
        importRemoteBranch ns mode = ExceptT . eval $ ImportRemoteBranch ns mode
        viewRemoteBranch ns = ExceptT . eval $ ViewRemoteBranch ns
        syncRemoteRootBranch repo b mode =
          ExceptT . eval $ SyncRemoteRootBranch repo b mode
        handleFailedDelete failed failedDependents = do
          failed           <- loadSearchResults $ SR.fromNames failed
          failedDependents <- loadSearchResults $ SR.fromNames failedDependents
          ppe              <- prettyPrintEnv =<< makePrintNamesFromLabeled'
            (foldMap SR'.labeledDependencies $ failed <> failedDependents)
          respond $ CantDelete ppe failed failedDependents
        saveAndApplyPatch patchPath'' patchName patch' = do
          stepAtM (inputDescription <> " (1/2)")
                  (patchPath'',
                   Branch.modifyPatches patchName (const patch'))
          -- Apply the modified patch to the current path
          -- since we might be able to propagate further.
          void $ propagatePatch inputDescription patch' currentPath'
          -- Say something
          success
        previewResponse sourceName sr uf = do
          names <- makeShadowedPrintNamesFromLabeled
                      (UF.termSignatureExternalLabeledDependencies uf)
                      (UF.typecheckedToNames0 uf)
          ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl names
          respond $ Typechecked (Text.pack sourceName) ppe sr uf

        addDefaultMetadata
          :: SlurpComponent v
          -> Action m (Either Event Input) v ()
        addDefaultMetadata adds = do
          let addedVs = Set.toList $ SC.types adds <> SC.terms adds
              addedNs = traverse (Path.hqSplitFromName' . Name.fromVar) addedVs
          case addedNs of
            Nothing ->
              error $ "I couldn't parse a name I just added to the codebase! "
                    <> "-- Added names: " <> show addedVs
            Just addedNames -> do
              dm <- resolveDefaultMetadata currentPath'
              case toList dm of
                []  -> pure ()
                dm' -> do
                  let hqs = traverse InputPatterns.parseHashQualifiedName dm'
                  case hqs of
                    Left e -> respond $ ConfiguredMetadataParseError
                      (Path.absoluteToPath' currentPath')
                      (show dm')
                      e
                    Right defaultMeta ->
                      manageLinks True addedNames defaultMeta Metadata.insert

        -- Add/remove links between definitions and metadata.
        -- `silent` controls whether this produces any output to the user.
        -- `srcs` is (names of the) definitions to pass to `op`
        -- `mdValues` is (names of the) metadata to pass to `op`
        -- `op` is the operation to add/remove/alter metadata mappings.
        --   e.g. `Metadata.insert` is passed to add metadata links.
        manageLinks :: Bool
                    -> [(Path', HQ'.HQSegment)]
                    -> [HQ.HashQualified]
                    -> (forall r. Ord r
                        => (r, Metadata.Type, Metadata.Value)
                        ->  Branch.Star r NameSegment
                        ->  Branch.Star r NameSegment)
                    -> Action m (Either Event Input) v ()
        manageLinks silent srcs mdValues op = do
          mdValuels <- fmap (first toList) <$>
            traverse (\x -> fmap (,x) (getHQTerms x)) mdValues
          before <- Branch.head <$> use root
          traverse_ go mdValuels
          after  <- Branch.head <$> use root
          (ppe, outputDiff) <- diffHelper before after
          if not silent then
            if OBranchDiff.isEmpty outputDiff
            then respond NoOp
            else respondNumbered $ ShowDiffNamespace Path.absoluteEmpty
                                                     Path.absoluteEmpty
                                                     ppe
                                                     outputDiff
          else unless (OBranchDiff.isEmpty outputDiff) $
                 respond DefaultMetadataNotification
          where
            go (mdl, hqn) = do
              newRoot <- use root
              let r0 = Branch.head newRoot
                  getTerms p = BranchUtil.getTerm (resolveSplit' p) r0
                  getTypes p = BranchUtil.getType (resolveSplit' p) r0
                  !srcle = toList . getTerms =<< srcs
                  !srclt = toList . getTypes =<< srcs
              names0 <- basicPrettyPrintNames0
              ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl (Names names0 mempty)
              case mdl of
                [r@(Referent.Ref mdValue)] -> do
                  mdType <- eval $ LoadTypeOfTerm mdValue
                  case mdType of
                    Nothing -> respond $ MetadataMissingType ppe r
                    Just ty -> do
                      let steps =
                            bimap (Path.unabsolute . resolveToAbsolute)
                                  (const . step $ Type.toReference ty)
                              <$> srcs
                      stepManyAtNoSync steps
                 where
                  step mdType b0 =
                    let tmUpdates terms = foldl' go terms srcle
                            where go terms src = op (src, mdType, mdValue) terms
                        tyUpdates types = foldl' go types srclt
                            where go types src = op (src, mdType, mdValue) types
                    in  over Branch.terms tmUpdates . over Branch.types tyUpdates $ b0
                mdValues -> respond $ MetadataAmbiguous hqn ppe mdValues
        delete
          :: (Path.HQSplit' -> Set Referent) -- compute matching terms
          -> (Path.HQSplit' -> Set Reference) -- compute matching types
          -> Path.HQSplit'
          -> Action' m v ()
        delete getHQ'Terms getHQ'Types hq = do
          let matchingTerms = toList (getHQ'Terms hq)
          let matchingTypes = toList (getHQ'Types hq)
          case (matchingTerms, matchingTypes) of
            ([], []) -> respond (NameNotFound hq)
            (Set.fromList -> tms, Set.fromList -> tys) -> goMany tms tys
          where
          resolvedPath = resolveSplit' (HQ'.toName <$> hq)
          goMany tms tys = do
            let rootNames = Branch.toNames0 root0
                name = Path.toName (Path.unsplit resolvedPath)
                toRel :: Ord ref => Set ref -> R.Relation Name ref
                toRel = R.fromList . fmap (name,) . toList
                -- these names are relative to the root
                toDelete = Names0 (toRel tms) (toRel tys)
            (failed, failedDependents) <-
              getEndangeredDependents (eval . GetDependents) toDelete rootNames
            if failed == mempty then do
              let makeDeleteTermNames = fmap (BranchUtil.makeDeleteTermName resolvedPath) . toList $ tms
              let makeDeleteTypeNames = fmap (BranchUtil.makeDeleteTypeName resolvedPath) . toList $ tys
              stepManyAt (makeDeleteTermNames ++ makeDeleteTypeNames)
              root'' <- use root
              diffHelper (Branch.head root') (Branch.head root'') >>=
                respondNumbered . uncurry ShowDiffAfterDeleteDefinitions
            else handleFailedDelete failed failedDependents
      in case input of
      ShowReflogI -> do
        entries <- convertEntries Nothing [] <$> eval LoadReflog
        numberedArgs .=
          fmap (('#':) . SBH.toString . Output.hash) entries
        respond $ ShowReflog entries
        where
        -- reverses & formats entries, adds synthetic entries when there is a
        -- discontinuity in the reflog.
        convertEntries :: Maybe Branch.Hash
                       -> [Output.ReflogEntry]
                       -> [Reflog.Entry]
                       -> [Output.ReflogEntry]
        convertEntries _ acc [] = acc
        convertEntries Nothing acc entries@(Reflog.Entry old _ _ : _) =
          convertEntries
            (Just old)
            (Output.ReflogEntry (SBH.fromHash sbhLength old) "(initial reflogged namespace)" : acc)
            entries
        convertEntries (Just lastHash) acc entries@(Reflog.Entry old new reason : rest) =
          if lastHash /= old then
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
            if Branch.isEmpty newRoot then respond $ BranchNotFound path'
            else do
              updateRoot newRoot
              success
      ForkLocalBranchI src0 dest0 -> do
        let tryUpdateDest srcb dest0 = do
              let dest = resolveToAbsolute dest0
              -- if dest isn't empty: leave dest unchanged, and complain.
              destb <- getAt dest
              if Branch.isEmpty destb then do
                ok <- updateAtM dest (const $ pure srcb)
                if ok then success else respond $ BranchEmpty src0
              else respond $ BranchAlreadyExists dest0
        case src0 of
          Left hash -> unlessError do
            srcb <- resolveShortBranchHash hash
            lift $ tryUpdateDest srcb dest0
          Right path' -> do
            srcb <- getAt $ resolveToAbsolute path'
            if Branch.isEmpty srcb then respond $ BranchNotFound path'
            else tryUpdateDest srcb dest0
      MergeLocalBranchI src0 dest0 mergeMode -> do
        let [src, dest] = resolveToAbsolute <$> [src0, dest0]
        srcb <- getAt src
        if Branch.isEmpty srcb then branchNotFound src0
        else do
          let err = Just $ MergeAlreadyUpToDate src0 dest0
          mergeBranchAndPropagateDefaultPatch mergeMode inputDescription err srcb (Just dest0) dest

      PreviewMergeLocalBranchI src0 dest0 -> do
        let [src, dest] = resolveToAbsolute <$> [src0, dest0]
        srcb <- getAt src
        if Branch.isEmpty srcb then branchNotFound src0
        else do
          destb <- getAt dest
          merged <- eval . Eval $ Branch.merge srcb destb
          if merged == destb
          then respond (PreviewMergeAlreadyUpToDate src0 dest0)
          else
            diffHelper (Branch.head destb) (Branch.head merged) >>=
              respondNumbered . uncurry (ShowDiffAfterMergePreview dest0 dest)

      DiffNamespaceI before0 after0 -> do
        let [beforep, afterp] =
              resolveToAbsolute <$> [before0, after0]
        before <- Branch.head <$> getAt beforep
        after <- Branch.head <$> getAt afterp
        (ppe, outputDiff) <- diffHelper before after
        respondNumbered $ ShowDiffNamespace beforep afterp ppe outputDiff

      CreatePullRequestI baseRepo headRepo -> unlessGitError do
        baseBranch <- viewRemoteBranch baseRepo
        headBranch <- viewRemoteBranch headRepo
        lift do
          merged <- eval . Eval $ Branch.merge baseBranch headBranch
          (ppe, diff) <- diffHelper (Branch.head baseBranch) (Branch.head merged)
          respondNumbered $ ShowDiffAfterCreatePR baseRepo headRepo ppe diff

      LoadPullRequestI baseRepo headRepo dest0 -> do
        let desta = resolveToAbsolute dest0
        let dest = Path.unabsolute desta
        destb <- getAt desta
        if Branch.isEmpty0 (Branch.head destb) then unlessGitError do
          baseb <- importRemoteBranch baseRepo SyncMode.ShortCircuit
          headb <- importRemoteBranch headRepo SyncMode.ShortCircuit
          lift $ do
            mergedb <- eval . Eval $ Branch.merge baseb headb
            squashedb <- eval . Eval $ Branch.merge' Branch.SquashMerge headb baseb
            stepManyAt
              [BranchUtil.makeSetBranch (dest, "base") baseb
              ,BranchUtil.makeSetBranch (dest, "head") headb
              ,BranchUtil.makeSetBranch (dest, "merged") mergedb
              ,BranchUtil.makeSetBranch (dest, "squashed") squashedb]
            let base = snoc dest0 "base"
                head = snoc dest0 "head"
                merged = snoc dest0 "merged"
                squashed = snoc dest0 "squashed"
            respond $ LoadPullRequest baseRepo headRepo base head merged squashed
            loadPropagateDiffDefaultPatch
              inputDescription
              (Just merged)
              (snoc desta "merged")
        else
          respond . BranchNotEmpty . Path.Path' . Left $ currentPath'


      -- move the root to a sub-branch
      MoveBranchI Nothing dest -> do
        b <- use root
        stepManyAt [ (Path.empty, const Branch.empty0)
                   , BranchUtil.makeSetBranch (resolveSplit' dest) b ]
        success

      MoveBranchI (Just src) dest ->
        maybe (branchNotFound' src) srcOk (getAtSplit' src)
        where
        srcOk b = maybe (destOk b) (branchExistsSplit dest) (getAtSplit' dest)
        destOk b = do
          stepManyAt
            [ BranchUtil.makeSetBranch (resolveSplit' src) Branch.empty
            , BranchUtil.makeSetBranch (resolveSplit' dest) b ]
          success -- could give rando stats about new defns

      MovePatchI src dest -> do
        psrc <- getPatchAtSplit' src
        pdest <- getPatchAtSplit' dest
        case (psrc, pdest) of
          (Nothing, _) -> patchNotFound src
          (_, Just _) -> patchExists dest
          (Just p, Nothing) -> do
            stepManyAt [
              BranchUtil.makeDeletePatch (resolveSplit' src),
              BranchUtil.makeReplacePatch (resolveSplit' dest) p ]
            success

      CopyPatchI src dest -> do
        psrc <- getPatchAtSplit' src
        pdest <- getPatchAtSplit' dest
        case (psrc, pdest) of
          (Nothing, _) -> patchNotFound src
          (_, Just _) -> patchExists dest
          (Just p, Nothing) -> do
            stepAt (BranchUtil.makeReplacePatch (resolveSplit' dest) p)
            success

      DeletePatchI src -> do
        psrc <- getPatchAtSplit' src
        case psrc of
          Nothing -> patchNotFound src
          Just _ -> do
            stepAt (BranchUtil.makeDeletePatch (resolveSplit' src))
            success

      DeleteBranchI Nothing ->
        ifConfirmed
            (do
              stepAt (Path.empty, const Branch.empty0)
              respond DeletedEverything)
            (respond DeleteEverythingConfirmation)

      DeleteBranchI (Just p) ->
        maybe (branchNotFound' p) go $ getAtSplit' p
        where
        go (Branch.head -> b) = do
          (failed, failedDependents) <-
            let rootNames = Branch.toNames0 root0
                toDelete = Names.prefix0
                  (Path.toName . Path.unsplit . resolveSplit' $ p) -- resolveSplit' incorporates currentPath
                  (Branch.toNames0 b)
            in getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then do
            stepAt $ BranchUtil.makeSetBranch (resolveSplit' p) Branch.empty
            -- Looks similar to the 'toDelete' above... investigate me! ;)
            diffHelper b Branch.empty0 >>=
              respondNumbered
                . uncurry (ShowDiffAfterDeleteBranch
                            $ resolveToAbsolute (Path.unsplit' p))
          else handleFailedDelete failed failedDependents
      SwitchBranchI path' -> do
        let path = resolveToAbsolute path'
        currentPathStack %= Nel.cons path
        branch' <- getAt path
        when (Branch.isEmpty branch') (respond $ CreatedNewBranch path)

      PopBranchI -> use (currentPathStack . to Nel.uncons) >>= \case
        (_, Nothing) -> respond StartOfCurrentPathHistory
        (_, Just t) -> currentPathStack .= t

      HistoryI resultsCap diffCap from -> case from of
        Left hash -> unlessError do
          b <- resolveShortBranchHash hash
          lift $ doHistory 0 b []
        Right path' -> do
          let path = resolveToAbsolute path'
          branch' <- getAt path
          if Branch.isEmpty branch' then respond $ CreatedNewBranch path
          else doHistory 0 branch' []
        where
          doHistory !n b acc =
            if maybe False (n >=) resultsCap then
              respond $ History diffCap acc (PageEnd (sbh $ Branch.headHash b) n)
            else case Branch._history b of
              Causal.One{} ->
                respond $ History diffCap acc (EndOfLog . sbh $ Branch.headHash b)
              Causal.Merge{..} ->
                respond $ History diffCap acc (MergeTail (sbh $ Branch.headHash b) . map sbh $ Map.keys tails)
              Causal.Cons{..} -> do
                b' <- fmap Branch.Branch . eval . Eval $ snd tail
                let elem = (sbh $ Branch.headHash b, Branch.namesDiff b' b)
                doHistory (n+1) b' (elem : acc)

      UndoI -> do
        prev <- eval . Eval $ Branch.uncons root'
        case prev of
          Nothing ->
            respond . CantUndo $ if Branch.isOne root' then CantUndoPastStart
                                 else CantUndoPastMerge
          Just (_, prev) -> do
            updateRoot prev
            diffHelper (Branch.head prev) (Branch.head root') >>=
              respondNumbered . uncurry Output.ShowDiffAfterUndo

      AliasTermI src dest -> do
        referents <- resolveHHQS'Referents src
        case (toList referents, toList (getTerms dest)) of
          ([r],       []) -> do
            stepAt (BranchUtil.makeAddTermName (resolveSplit' dest) r (oldMD r))
            success
          ([_], rs@(_:_)) -> termExists dest (Set.fromList rs)
          ([],         _) -> either termNotFound' termNotFound src
          (rs,         _) ->
            either hashConflicted termConflicted src (Set.fromList rs)
          where
          oldMD r = either (const mempty)
                           (\src ->
                            let p = resolveSplit' src in
                            BranchUtil.getTermMetadataAt p r root0)
                           src

      AliasTypeI src dest -> do
        refs <- resolveHHQS'Types src
        case (toList refs, toList (getTypes dest)) of
          ([r],       []) -> do
            stepAt (BranchUtil.makeAddTypeName (resolveSplit' dest) r (oldMD r))
            success
          ([_], rs@(_:_)) -> typeExists dest (Set.fromList rs)
          ([],         _) -> either typeNotFound' typeNotFound src
          (rs,         _) ->
            either
              (\src -> hashConflicted src . Set.map Referent.Ref)
              typeConflicted
              src
              (Set.fromList rs)


          where
          oldMD r =
            either (const mempty)
                   (\src ->
                    let p = resolveSplit' src in
                    BranchUtil.getTypeMetadataAt p r root0)
                   src

      -- this implementation will happily produce name conflicts,
      -- but will surface them in a normal diff at the end of the operation.
      AliasManyI srcs dest' -> do
        let destAbs = resolveToAbsolute dest'
        old <- getAt destAbs
        let (unknown, actions) = foldl' go mempty srcs
        stepManyAt actions
        new <- getAt destAbs
        diffHelper (Branch.head old) (Branch.head new) >>=
            respondNumbered . uncurry (ShowDiffAfterModifyBranch dest' destAbs)
        unless (null unknown) $
          respond . SearchTermsNotFound . fmap fixupOutput $ unknown
        where
        -- a list of missing sources (if any) and the actions that do the work
        go :: ([Path.HQSplit], [(Path, Branch0 m -> Branch0 m)])
           -> Path.HQSplit
           -> ([Path.HQSplit], [(Path, Branch0 m -> Branch0 m)])
        go (missingSrcs, actions) hqsrc =
          let
            src :: Path.Split
            src = second HQ'.toName hqsrc
            proposedDest :: Path.Split
            proposedDest = second HQ'.toName hqProposedDest
            hqProposedDest :: Path.HQSplit
            hqProposedDest = first Path.unabsolute $
                              Path.resolve (resolveToAbsolute dest') hqsrc
            -- `Nothing` if src doesn't exist
            doType :: Maybe [(Path, Branch0 m -> Branch0 m)]
            doType = case ( BranchUtil.getType hqsrc currentBranch0
                          , BranchUtil.getType hqProposedDest root0
                          ) of
              (null -> True, _) -> Nothing -- missing src
              (rsrcs, existing) -> -- happy path
                Just . map addAlias . toList $ Set.difference rsrcs existing
                where
                addAlias r = BranchUtil.makeAddTypeName proposedDest r (oldMD r)
                oldMD r = BranchUtil.getTypeMetadataAt src r currentBranch0
            doTerm :: Maybe [(Path, Branch0 m -> Branch0 m)]
            doTerm = case ( BranchUtil.getTerm hqsrc currentBranch0
                          , BranchUtil.getTerm hqProposedDest root0
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

        fixupOutput :: Path.HQSplit -> HQ.HashQualified
        fixupOutput = fmap Path.toName . HQ'.toHQ . Path.unsplitHQ

      NamesI thing -> do
        parseNames0 <- Names3.suffixify0 <$> basicParseNames0
        let filtered = case thing of
              HQ.HashOnly shortHash ->
                Names.filterBySHs (Set.singleton shortHash) parseNames0
              HQ.HashQualified n sh ->
                Names.filterByHQs (Set.singleton $ HQ'.HashQualified n sh) parseNames0
              HQ.NameOnly n ->
                Names.filterByHQs (Set.singleton $ HQ'.NameOnly n) parseNames0
        printNames0 <- basicPrettyPrintNames0
        let printNames = Names printNames0 mempty
        let terms' ::Set (Referent, Set HQ'.HashQualified)
            terms' = (`Set.map` Names.termReferents filtered) $
                        \r -> (r, Names3.termName hqLength r printNames)
            types' :: Set (Reference, Set HQ'.HashQualified)
            types' = (`Set.map` Names.typeReferences filtered) $
                        \r -> (r, Names3.typeName hqLength r printNames)
        respond $ ListNames hqLength (toList types') (toList terms')
--          let (p, hq) = p0
--              namePortion = HQ'.toName hq
--          case hq of
--            HQ'.NameOnly _ ->
--              respond $ uncurry ListNames (results p namePortion)
--            HQ'.HashQualified _ sh -> let
--              (terms, types) = results p namePortion
--              -- filter terms and types based on `sh : ShortHash`
--              terms' = filter (Reference.isPrefixOf sh . Referent.toReference . fst) terms
--              types' = filter (Reference.isPrefixOf sh . fst) types
--              in respond $ ListNames terms' types'
--          where
--            results p namePortion = let
--              name = Path.toName . Path.unprefix currentPath' . Path.snoc' p
--                   $ namePortion
--              ns = prettyPrintNames0
--              terms = [ (r, Names.namesForReferent ns r)
--                      | r <- toList $ Names.termsNamed ns name ]
--              types = [ (r, Names.namesForReference ns r)
--                      | r <- toList $ Names.typesNamed ns name ]
--              in (terms, types)

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
        (ppe, out) <- getLinks input src (Right mdTypeStr)
        lift do
          numberedArgs .= fmap (HQ.toString . view _1) out
          respond $ ListOfLinks ppe out

      DocsI src -> unlessError do
        (ppe, out) <- getLinks input src (Left $ Set.singleton DD.docRef)
        lift case out of
          [(_name, ref, _tm)] -> do
            names <- basicPrettyPrintNames0
            doDisplay ConsoleLocation (Names3.Names names mempty) (Referent.Ref ref)
          out -> do
            numberedArgs .= fmap (HQ.toString . view _1) out
            respond $ ListOfLinks ppe out

      CreateAuthorI authorNameSegment authorFullName -> do
        initialBranch <- getAt currentPath'
        AuthorInfo
          guid@(guidRef, _, _)
          author@(authorRef, _, _)
          copyrightHolder@(copyrightHolderRef, _, _) <-
          eval $ CreateAuthorInfo authorFullName
        -- add the new definitions to the codebase and to the namespace
        traverse_ (eval . uncurry3 PutTerm) [guid, author, copyrightHolder]
        stepManyAt
          [ BranchUtil.makeAddTermName (resolveSplit' authorPath) (d authorRef) mempty
          , BranchUtil.makeAddTermName (resolveSplit' copyrightHolderPath) (d copyrightHolderRef) mempty
          , BranchUtil.makeAddTermName (resolveSplit' guidPath) (d guidRef) mempty
          ]
        finalBranch <- getAt currentPath'
        -- print some output
        diffHelper (Branch.head initialBranch) (Branch.head finalBranch) >>=
          respondNumbered
            . uncurry (ShowDiffAfterCreateAuthor
                        authorNameSegment
                        (Path.unsplit' base)
                        currentPath')
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
            stepManyAt
              [ BranchUtil.makeDeleteTermName p r
              , BranchUtil.makeAddTermName (resolveSplit' dest) r (mdSrc r)]
            success
          ([_], rs) -> termExists dest (Set.fromList rs)
          ([],   _) -> termNotFound src
          (rs,   _) -> termConflicted src (Set.fromList rs)
        where p = resolveSplit' (HQ'.toName <$> src)
              mdSrc r = BranchUtil.getTermMetadataAt p r root0

      MoveTypeI src dest ->
        case (toList (getHQ'Types src), toList (getTypes dest)) of
          ([r], []) -> do
            stepManyAt
              [ BranchUtil.makeDeleteTypeName p r
              , BranchUtil.makeAddTypeName (resolveSplit' dest) r (mdSrc r) ]
            success
          ([_], rs) -> typeExists dest (Set.fromList rs)
          ([], _)   -> typeNotFound src
          (rs, _)   -> typeConflicted src (Set.fromList rs)
        where
        p = resolveSplit' (HQ'.toName <$> src)
        mdSrc r = BranchUtil.getTypeMetadataAt p r root0

      DeleteI     hq -> delete getHQ'Terms       getHQ'Types       hq
      DeleteTypeI hq -> delete (const Set.empty) getHQ'Types       hq
      DeleteTermI hq -> delete getHQ'Terms       (const Set.empty) hq

      DisplayI outputLoc hq -> do
        parseNames0 <- (`Names3.Names` mempty) <$> basicPrettyPrintNames0
        -- use suffixed names for resolving the argument to display
        let parseNames = Names3.suffixify parseNames0
        let results = Names3.lookupHQTerm hq parseNames
        if Set.null results then
          respond $ SearchTermsNotFound [hq]
        else if Set.size results > 1 then
          respond $ TermAmbiguous hq results
        -- ... but use the unsuffixed names for display
        else doDisplay outputLoc parseNames0 (Set.findMin results)

      ShowDefinitionI outputLoc query -> do
        (misses, results) <- hqNameQuerySuffixify query
        results' <- loadSearchResults results
        let termTypes :: Map.Map Reference (Type v Ann)
            termTypes =
              Map.fromList
                [ (r, t) | SR'.Tm _ (Just t) (Referent.Ref r) _ <- results' ]
            (collatedTypes, collatedTerms) = collateReferences
              (mapMaybe SR'.tpReference results')
              (mapMaybe SR'.tmReferent results')
        -- load the `collatedTerms` and types into a Map Reference.Id Term/Type
        -- for later
        loadedDerivedTerms <-
          fmap (Map.fromList . catMaybes) . for (toList collatedTerms) $ \case
            Reference.DerivedId i -> fmap (i,) <$> eval (LoadTerm i)
            Reference.Builtin{} -> pure Nothing
        loadedDerivedTypes <-
          fmap (Map.fromList . catMaybes) . for (toList collatedTypes) $ \case
            Reference.DerivedId i -> fmap (i,) <$> eval (LoadType i)
            Reference.Builtin{} -> pure Nothing
        -- Populate DisplayThings for the search results, in anticipation of
        -- displaying the definitions.
        loadedDisplayTerms :: Map Reference (DisplayThing (Term v Ann)) <-
         fmap Map.fromList . for (toList collatedTerms) $ \case
          r@(Reference.DerivedId i) -> do
            let tm = Map.lookup i loadedDerivedTerms
            -- We add a type annotation to the term using if it doesn't
            -- already have one that the user provided
            pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
              Nothing        -> MissingThing i
              Just (tm, typ) -> case tm of
                Term.Ann' _ _ -> RegularThing tm
                _ -> RegularThing (Term.ann (ABT.annotation tm) tm typ)
          r@(Reference.Builtin _) -> pure (r, BuiltinThing)
        let loadedDisplayTypes :: Map Reference (DisplayThing (DD.Decl v Ann))
            loadedDisplayTypes =
              Map.fromList . (`fmap` toList collatedTypes) $ \case
                r@(Reference.DerivedId i) ->
                  (r,) . maybe (MissingThing i) RegularThing
                       $ Map.lookup i loadedDerivedTypes
                r@(Reference.Builtin _) -> (r, BuiltinThing)
        -- the SR' deps include the result term/type names, and the
        let deps = foldMap SR'.labeledDependencies results'
                <> foldMap Term.labeledDependencies loadedDerivedTerms
        printNames <- makePrintNamesFromLabeled' deps

        -- We might like to make sure that the user search terms get used as
        -- the names in the pretty-printer, but the current implementation
        -- doesn't.
        ppe <- prettyPrintEnvDecl printNames
        let loc = case outputLoc of
              ConsoleLocation    -> Nothing
              FileLocation path  -> Just path
              LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
        do
          unless (null loadedDisplayTypes && null loadedDisplayTerms) $
            eval . Notify $
              DisplayDefinitions loc ppe loadedDisplayTypes loadedDisplayTerms
          unless (null misses) $
            eval . Notify $ SearchTermsNotFound misses
          -- We set latestFile to be programmatically generated, if we
          -- are viewing these definitions to a file - this will skip the
          -- next update for that file (which will happen immediately)
          latestFile .= ((, True) <$> loc)

      FindPatchI -> do
        let patches =
              [ Path.toName $ Path.snoc p seg
              | (p, b) <- Branch.toList0 currentBranch0
              , (seg, _) <- Map.toList (Branch._edits b) ]
        respond $ ListOfPatches $ Set.fromList patches
        numberedArgs .= fmap Name.toString patches

      FindShallowI pathArg -> do
        prettyPrintNames0 <- basicPrettyPrintNames0
        ppe <- fmap PPE.suffixifiedPPE . prettyPrintEnvDecl $ Names prettyPrintNames0 mempty
        let pathArgAbs = resolveToAbsolute pathArg
        b0 <- Branch.head <$> getAt pathArgAbs
        let
          hqTerm b0 ns r =
            let refs = Star3.lookupD1 ns . _terms $ b0
            in case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hqLength $ HQ'.fromNamedReferent ns r
          hqType b0 ns r =
            let refs = Star3.lookupD1 ns . _types $ b0
            in case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hqLength $ HQ'.fromNamedReference ns r
          defnCount b =
            (R.size . deepTerms $ Branch.head b) +
            (R.size . deepTypes $ Branch.head b)

        termEntries <- for (R.toList . Star3.d1 $ _terms b0) $
          \(r, ns) -> do
            ot <- loadReferentType r
            pure $ ShallowTermEntry r (hqTerm b0 ns r) ot
        let
          typeEntries =
            [ ShallowTypeEntry r (hqType b0 ns r)
            | (r, ns) <- R.toList . Star3.d1 $ _types b0 ]
          branchEntries =
            [ ShallowBranchEntry ns (defnCount b)
            | (ns, b) <- Map.toList $ _children b0 ]
          patchEntries =
            [ ShallowPatchEntry ns
            | (ns, (_h, _mp)) <- Map.toList $ _edits b0 ]
        let
          entries :: [ShallowListEntry v Ann]
          entries = sort $ termEntries ++ typeEntries ++ branchEntries ++ patchEntries
          entryToHQString :: ShallowListEntry v Ann -> String
          -- caching the result as an absolute path, for easier jumping around
          entryToHQString e = fixup $ case e of
            ShallowTypeEntry _ hq   -> HQ'.toString hq
            ShallowTermEntry _ hq _ -> HQ'.toString hq
            ShallowBranchEntry ns _ -> NameSegment.toString ns
            ShallowPatchEntry ns    -> NameSegment.toString ns
            where
            fixup s =
              if last pathArgStr == '.'
              then pathArgStr ++ s
              else pathArgStr ++ "." ++ s
            pathArgStr = show pathArgAbs
        numberedArgs .= fmap entryToHQString entries
        respond $ ListShallow ppe entries
        where

      SearchByNameI isVerbose _showAll ws -> do
        prettyPrintNames0 <- basicPrettyPrintNames0
        unlessError do
          results <- case ws of
            -- no query, list everything
            [] -> pure . listBranch $ Branch.head currentBranch'

            -- type query
            ":" : ws -> ExceptT (parseSearchType input (unwords ws)) >>= \typ -> ExceptT $ do
              let named = Branch.deepReferents root0
              matches <- fmap toList . eval $ GetTermsOfType typ
              matches <- filter (`Set.member` named) <$>
                if null matches then do
                  respond NoExactTypeMatches
                  fmap toList . eval $ GetTermsMentioningType typ
                else pure matches
              let results =
                    -- in verbose mode, aliases are shown, so we collapse all
                    -- aliases to a single search result; in non-verbose mode,
                    -- a separate result may be shown for each alias
                    (if isVerbose then uniqueBy SR.toReferent else id) $
                    searchResultsFor prettyPrintNames0 matches []
              pure . pure $ results

            -- name query
            (map HQ.unsafeFromString -> qs) -> do
              ns <- lift basicPrettyPrintNames0
              let srs = searchBranchScored ns fuzzyNameDistance qs
              pure $ uniqueBy SR.toReferent srs
          lift do
            numberedArgs .= fmap searchResultToHQString results
            results' <- loadSearchResults results
            ppe <- prettyPrintEnv . Names3.suffixify =<<
              makePrintNamesFromLabeled'
                (foldMap SR'.labeledDependencies results')
            respond $ ListOfDefinitions ppe isVerbose results'

      ResolveTypeNameI hq ->
        zeroOneOrMore (getHQ'Types hq) (typeNotFound hq) go (typeConflicted hq)
        where
        conflicted = getHQ'Types (fmap HQ'.toNameOnly hq)
        makeDelete =
          BranchUtil.makeDeleteTypeName (resolveSplit' (HQ'.toName <$> hq))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ResolveTermNameI hq -> do
        refs <- getHQ'TermsIncludingHistorical hq
        zeroOneOrMore refs (termNotFound hq) go (termConflicted hq)
        where
        conflicted = getHQ'Terms (fmap HQ'.toNameOnly hq)
        makeDelete =
          BranchUtil.makeDeleteTermName (resolveSplit' (HQ'.toName <$> hq))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ReplaceTermI from to patchPath -> do
        let patchPath' = fromMaybe defaultPatchPath patchPath
        patch <- getPatchAt patchPath'
        (fromMisses', fromHits) <- hqNameQuery [from]
        (toMisses', toHits) <- hqNameQuery [to]
        let fromRefs = termReferences fromHits
            toRefs = termReferences toHits
            -- Type hits are term misses
            fromMisses = fromMisses'
                       <> (HQ'.toHQ . SR.typeName <$> typeResults fromHits)
            toMisses = toMisses'
                       <> (HQ'.toHQ . SR.typeName <$> typeResults fromHits)
            go :: Reference
               -> Reference
               -> Action m (Either Event Input) v ()
            go fr tr = do
              mft <- eval $ LoadTypeOfTerm fr
              mtt <- eval $ LoadTypeOfTerm tr
              let termNotFound = respond . TermNotFound'
                                         . SH.take hqLength
                                         . Reference.toShortHash
              case (mft, mtt) of
                (Nothing, _) -> termNotFound fr
                (_, Nothing) -> termNotFound tr
                (Just ft, Just tt) -> do
                  let
                      patch' =
                        -- The modified patch
                        over Patch.termEdits
                          (R.insert fr (Replace tr (TermEdit.typing tt ft))
                           . R.deleteDom fr)
                          patch
                      (patchPath'', patchName) = resolveSplit' patchPath'
                  saveAndApplyPatch patchPath'' patchName patch'
            misses = fromMisses <> toMisses
            ambiguous t rs =
              let rs' = Set.map Referent.Ref $ Set.fromList rs
              in  case t of
                    HQ.HashOnly h ->
                      hashConflicted h rs'
                    (Path.parseHQSplit' . HQ.toString -> Right n) ->
                      termConflicted n rs'
                    _ -> respond . BadName $ HQ.toString t
        unless (null misses) $
          respond $ SearchTermsNotFound misses
        case (fromRefs, toRefs) of
          ([fr], [tr]) -> go fr tr
          ([_], tos) -> ambiguous to tos
          (frs, _) -> ambiguous from frs
      ReplaceTypeI from to patchPath -> do
        let patchPath' = fromMaybe defaultPatchPath patchPath
        (fromMisses', fromHits) <- hqNameQuery [from]
        (toMisses', toHits) <- hqNameQuery [to]
        patch <- getPatchAt patchPath'
        let fromRefs = typeReferences fromHits
            toRefs = typeReferences toHits
            -- Term hits are type misses
            fromMisses = fromMisses'
                       <> (HQ'.toHQ . SR.termName <$> termResults fromHits)
            toMisses = toMisses'
                       <> (HQ'.toHQ . SR.termName <$> termResults fromHits)
            go :: Reference
               -> Reference
               -> Action m (Either Event Input) v ()
            go fr tr = do
              let patch' =
                    -- The modified patch
                    over Patch.typeEdits
                      (R.insert fr (TypeEdit.Replace tr) . R.deleteDom fr) patch
                  (patchPath'', patchName) = resolveSplit' patchPath'
              saveAndApplyPatch patchPath'' patchName patch'
            misses = fromMisses <> toMisses
            ambiguous t rs =
              let rs' = Set.map Referent.Ref $ Set.fromList rs
              in  case t of
                    HQ.HashOnly h ->
                      hashConflicted h rs'
                    (Path.parseHQSplit' . HQ.toString -> Right n) ->
                      typeConflicted n $ Set.fromList rs
                    -- This is unlikely to happen, as t has to be a parsed
                    -- hash-qualified name already.
                    -- Still, the types say we need to handle this case.
                    _ -> respond . BadName $ HQ.toString t
        unless (null misses) $
          respond $ SearchTermsNotFound misses
        case (fromRefs, toRefs) of
          ([fr], [tr]) -> go fr tr
          ([_], tos) -> ambiguous to tos
          (frs, _) -> ambiguous from frs
      LoadI maybePath ->
        case maybePath <|> (fst <$> latestFile') of
          Nothing   -> respond NoUnisonFile
          Just path -> do
            res <- eval . LoadSource . Text.pack $ path
            case res of
              InvalidSourceNameError -> respond $ InvalidSourceName path
              LoadError -> respond $ SourceLoadFailed path
              LoadSuccess contents -> loadUnisonFile (Text.pack path) contents

      AddI hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> do
          sr <- Slurp.disallowUpdates
              . applySelection hqs uf
              . toSlurpResult currentPath' uf
             <$> slurpResultNames0
          let adds = Slurp.adds sr
          when (Slurp.isNonempty sr) $ do
            stepAtNoSync ( Path.unabsolute currentPath'
                   , doSlurpAdds adds uf)
            eval . AddDefsToCodebase . filterBySlurpResult sr $ uf
          ppe <- prettyPrintEnvDecl =<<
            makeShadowedPrintNamesFromLabeled
              (UF.termSignatureExternalLabeledDependencies uf)
              (UF.typecheckedToNames0 uf)
          respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
          addDefaultMetadata adds
          syncRoot

      PreviewAddI hqs -> case (latestFile', uf) of
        (Just (sourceName, _), Just uf) -> do
          sr <-  Slurp.disallowUpdates
                    .  applySelection hqs uf
                    .  toSlurpResult currentPath' uf
                   <$> slurpResultNames0
          previewResponse sourceName sr uf
        _ -> respond NoUnisonFile

      UpdateI maybePatchPath hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> do
          let patchPath = fromMaybe defaultPatchPath maybePatchPath
          slurpCheckNames0 <- slurpResultNames0
          currentPathNames0 <- currentPathNames0
          let sr = applySelection hqs uf
                 . toSlurpResult currentPath' uf
                 $ slurpCheckNames0
              addsAndUpdates = Slurp.updates sr <> Slurp.adds sr
              fileNames0 = UF.typecheckedToNames0 uf
              -- todo: display some error if typeEdits or termEdits itself contains a loop
              typeEdits :: Map Name (Reference, Reference)
              typeEdits = Map.fromList $ map f (toList $ SC.types (updates sr)) where
                f v = case (toList (Names.typesNamed slurpCheckNames0 n)
                           ,toList (Names.typesNamed fileNames0 n)) of
                  ([old],[new]) -> (n, (old, new))
                  _ -> error $ "Expected unique matches for "
                                  ++ Var.nameStr v ++ " but got: "
                                  ++ show otherwise
                  where n = Name.fromVar v
              hashTerms :: Map Reference (Type v Ann)
              hashTerms = Map.fromList (toList hashTerms0) where
                hashTerms0 = (\(r, _, typ) -> (r, typ)) <$> UF.hashTerms uf
              termEdits :: Map Name (Reference, Reference)
              termEdits = Map.fromList $ map g (toList $ SC.terms (updates sr)) where
                g v = case ( toList (Names.refTermsNamed slurpCheckNames0 n)
                           , toList (Names.refTermsNamed fileNames0 n)) of
                  ([old], [new]) -> (n, (old, new))
                  _ -> error $ "Expected unique matches for "
                                 ++ Var.nameStr v ++ " but got: "
                                 ++ show otherwise
                  where n = Name.fromVar v
              termDeprecations :: [(Name, Referent)]
              termDeprecations =
                [ (n, r) | (oldTypeRef,_) <- Map.elems typeEdits
                         , (n, r) <- Names3.constructorsForType0 oldTypeRef currentPathNames0 ]

          ye'ol'Patch <- getPatchAt patchPath
          -- If `uf` updates a -> a', we want to replace all (a0 -> a) in patch
          -- with (a0 -> a') in patch'.
          -- So for all (a0 -> a) in patch, for all (a -> a') in `uf`,
          -- we must know the type of a0, a, a'.
          let
            -- we need:
            -- all of the `old` references from the `new` edits,
            -- plus all of the `old` references for edits from patch we're replacing
            collectOldForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
            collectOldForTyping new old = foldl' f mempty (new ++ fromOld) where
              f acc (r, _r') = Set.insert r acc
              newLHS = Set.fromList . fmap fst $ new
              fromOld :: [(Reference, Reference)]
              fromOld = [ (r,r') | (r, TermEdit.Replace r' _) <- R.toList . Patch._termEdits $ old
                                 , Set.member r' newLHS ]
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
                e -> error $ "compiler bug: typing map not constructed properly\n" <>
                  "typing " <> show r1 <> " " <> show r2 <> " : " <> show e

          let updatePatch :: Patch -> Patch
              updatePatch p = foldl' step2 p' termEdits
                where
                p' = foldl' step1 p typeEdits
                step1 p (r,r') = Patch.updateType r (TypeEdit.Replace r') p
                step2 p (r,r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
              (p, seg) = Path.toAbsoluteSplit currentPath' patchPath
              updatePatches :: Branch0 m -> m (Branch0 m)
              updatePatches = Branch.modifyPatches seg updatePatch

          when (Slurp.isNonempty sr) $ do
          -- take a look at the `updates` from the SlurpResult
          -- and make a patch diff to record a replacement from the old to new references
            stepManyAtMNoSync
              [( Path.unabsolute currentPath'
               , pure . doSlurpUpdates typeEdits termEdits termDeprecations)
              ,( Path.unabsolute currentPath'
               , pure . doSlurpAdds addsAndUpdates uf)
              ,( Path.unabsolute p, updatePatches )]
            eval . AddDefsToCodebase . filterBySlurpResult sr $ uf
          ppe <- prettyPrintEnvDecl =<<
            makeShadowedPrintNamesFromLabeled
              (UF.termSignatureExternalLabeledDependencies uf)
              (UF.typecheckedToNames0 uf)
          respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
          -- propagatePatch prints TodoOutput
          void $ propagatePatchNoSync (updatePatch ye'ol'Patch) currentPath'
          addDefaultMetadata addsAndUpdates
          syncRoot

      PreviewUpdateI hqs -> case (latestFile', uf) of
        (Just (sourceName, _), Just uf) -> do
          sr <-  applySelection hqs uf
                    .  toSlurpResult currentPath' uf
                   <$> slurpResultNames0
          previewResponse sourceName sr uf
        _ -> respond NoUnisonFile

      TodoI patchPath branchPath' -> do
        patch <- getPatchAt (fromMaybe defaultPatchPath patchPath)
        doShowTodoOutput patch $ resolveToAbsolute branchPath'

      TestI showOk showFail -> do
        let
          testTerms = Map.keys . R4.d1 . uncurry R4.selectD34 isTest
                    . Branch.deepTermMetadata $ currentBranch0
          testRefs = Set.fromList [ r | Referent.Ref r <- toList testTerms ]
          oks results =
            [ (r, msg)
            | (r, Term.Sequence' ts) <- Map.toList results
            , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
            , cid == DD.okConstructorId && ref == DD.testResultRef ]
          fails results =
            [ (r, msg)
            | (r, Term.Sequence' ts) <- Map.toList results
            , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
            , cid == DD.failConstructorId && ref == DD.testResultRef ]
        cachedTests <- fmap Map.fromList . eval $ LoadWatches UF.TestWatch testRefs
        let stats = Output.CachedTests (Set.size testRefs) (Map.size cachedTests)
        names <- makePrintNamesFromLabeled' $
          LD.referents testTerms <>
          LD.referents [ DD.okConstructorReferent, DD.failConstructorReferent ]
        ppe <- prettyPrintEnv names
        respond $ TestResults stats ppe showOk showFail
                    (oks cachedTests) (fails cachedTests)
        let toCompute = Set.difference testRefs (Map.keysSet cachedTests)
        unless (Set.null toCompute) $ do
          let total = Set.size toCompute
          computedTests <- fmap join . for (toList toCompute `zip` [1..]) $ \(r,n) ->
            case r of
              Reference.DerivedId rid -> do
                tm <- eval $ LoadTerm rid
                case tm of
                  Nothing -> [] <$ respond (TermNotFound' . SH.take hqLength . Reference.toShortHash $ Reference.DerivedId rid)
                  Just tm -> do
                    respond $ TestIncrementalOutputStart ppe (n,total) r tm
                    tm' <- eval $ Evaluate1 ppe tm
                    case tm' of
                      Left e -> respond (EvaluationFailure e) $> []
                      Right tm' -> do
                        eval $ PutWatch UF.TestWatch rid tm'
                        respond $ TestIncrementalOutputEnd ppe (n,total) r tm'
                        pure [(r, tm')]
              r -> error $ "unpossible, tests can't be builtins: " <> show r

          let m = Map.fromList computedTests
          respond $ TestResults Output.NewlyComputed ppe showOk showFail (oks m) (fails m)

      -- ListBranchesI ->
      --   eval ListBranches >>= respond . ListOfBranches currentBranchName'
      -- DeleteBranchI branchNames -> withBranches branchNames $ \bnbs -> do
      --   uniqueToDelete <- prettyUniqueDefinitions bnbs
      --   let deleteBranches b =
      --         traverse (eval . DeleteBranch) b >> respond (Success input)
      --   if (currentBranchName' `elem` branchNames)
      --     then respond DeletingCurrentBranch
      --     else if null uniqueToDelete
      --       then deleteBranches branchNames
      --       else ifM (confirmedCommand input)
      --                (deleteBranches branchNames)
      --                (respond . DeleteBranchConfirmation $ uniqueToDelete)

      PropagatePatchI patchPath scopePath -> do
        patch <- getPatchAt patchPath
        updated <- propagatePatch inputDescription patch (resolveToAbsolute scopePath)
        unless updated (respond $ NothingToPatch patchPath scopePath)

      ExecuteI main -> addRunMain main uf >>= \case
        NoTermWithThatName -> do
          names0 <- basicPrettyPrintNames0
          ppe <- prettyPrintEnv (Names3.Names names0 mempty)
          mainType <- eval RuntimeMain
          respond $ NoMainFunction main ppe [mainType]
        TermHasBadType ty -> do
          names0 <- basicPrettyPrintNames0
          ppe <- prettyPrintEnv (Names3.Names names0 mempty)
          mainType <- eval RuntimeMain
          respond $ BadMainFunction main ty ppe [mainType]
        RunMainSuccess unisonFile -> do
          ppe <- executePPE unisonFile
          e <- eval $ Execute ppe unisonFile

          case e of
            Left e -> respond $ EvaluationFailure e
            Right _ -> pure () -- TODO

      IOTestI main -> do
        testType <- eval RuntimeTest
        parseNames0 <- (`Names3.Names` mempty) <$> basicPrettyPrintNames0
        ppe <- prettyPrintEnv parseNames0
        -- use suffixed names for resolving the argument to display
        let
            parseNames = Names3.suffixify parseNames0

            oks results =
              [ (r, msg)
              | (r, Term.Sequence' ts) <- results
              , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
              , cid == DD.okConstructorId && ref == DD.testResultRef ]
            fails results =
              [ (r, msg)
              | (r, Term.Sequence' ts) <- results
              , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
              , cid == DD.failConstructorId && ref == DD.testResultRef ]

            results = Names3.lookupHQTerm main parseNames in
            case toList results of
               [Referent.Ref ref] -> do
                 typ <- loadTypeOfTerm (Referent.Ref ref)
                 case typ of
                   Just typ | Typechecker.isSubtype testType typ -> do
                     let a = ABT.annotation tm
                         tm = DD.forceTerm a a (Term.ref a ref) in do
                         tm' <- eval $ Evaluate1 ppe tm
                         case tm' of
                           Left e -> respond (EvaluationFailure e)
                           Right tm' ->
                               respond $ TestResults Output.NewlyComputed ppe True True (oks [(ref, tm')]) (fails [(ref, tm')])
                   _ -> respond $ NoMainFunction "main" ppe [testType]
               _ -> respond $ NoMainFunction "main" ppe [testType]

      -- UpdateBuiltinsI -> do
      --   stepAt updateBuiltins
      --   checkTodo

      MergeBuiltinsI -> do
        -- these were added once, but maybe they've changed and need to be
        -- added again.
        let uf = UF.typecheckedUnisonFile (Map.fromList Builtin.builtinDataDecls)
                                          (Map.fromList Builtin.builtinEffectDecls)
                                          [Builtin.builtinTermsSrc Intrinsic]
                                          mempty
        eval $ AddDefsToCodebase uf
        -- add the names; note, there are more names than definitions
        -- due to builtin terms; so we don't just reuse `uf` above.
        let srcb = BranchUtil.fromNames0 Builtin.names0
        _ <- updateAtM (currentPath' `snoc` "builtin") $ \destb ->
               eval . Eval $ Branch.merge srcb destb
        success

      MergeIOBuiltinsI -> do
        -- these were added once, but maybe they've changed and need to be
        -- added again.
        let uf = UF.typecheckedUnisonFile (Map.fromList Builtin.builtinDataDecls)
                                          (Map.fromList Builtin.builtinEffectDecls)
                                          [Builtin.builtinTermsSrc Intrinsic]
                                          mempty
        eval $ AddDefsToCodebase uf
        -- these have not neceesarily been added yet
        eval $ AddDefsToCodebase IOSource.typecheckedFile'

        -- add the names; note, there are more names than definitions
        -- due to builtin terms; so we don't just reuse `uf` above.
        let names0 = Builtin.names0
                     <> UF.typecheckedToNames0 @v IOSource.typecheckedFile'
        let srcb = BranchUtil.fromNames0 names0
        _ <- updateAtM (currentPath' `snoc` "builtin") $ \destb ->
               eval . Eval $ Branch.merge srcb destb

        success

      ListEditsI maybePath -> do
        let (p, seg) =
              maybe (Path.toAbsoluteSplit currentPath' defaultPatchPath)
                    (Path.toAbsoluteSplit currentPath')
                    maybePath
        patch <- eval . Eval . Branch.getPatch seg . Branch.head =<< getAt p
        ppe <- prettyPrintEnv =<<
          makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
        respond $ ListEdits patch ppe

      PullRemoteBranchI mayRepo path syncMode -> unlessError do
        ns <- resolveConfiguredGitUrl Pull path mayRepo
        lift $ unlessGitError do
          b <- importRemoteBranch ns syncMode
          let msg = Just $ PullAlreadyUpToDate ns path
          let destAbs = resolveToAbsolute path
          lift $ mergeBranchAndPropagateDefaultPatch Branch.RegularMerge inputDescription msg b (Just path) destAbs

      PushRemoteBranchI mayRepo path syncMode -> do
        let srcAbs = resolveToAbsolute path
        srcb <- getAt srcAbs
        let expandRepo (r, rp) = (r, Nothing, rp)
        unlessError do
          (repo, sbh, remotePath) <-
            resolveConfiguredGitUrl Push path (fmap expandRepo mayRepo)
          case sbh of
            Nothing -> lift $ unlessGitError do
              remoteRoot <- viewRemoteBranch (repo, Nothing, Path.empty)
              newRemoteRoot <- lift . eval . Eval $
                Branch.modifyAtM remotePath (Branch.merge srcb) remoteRoot
              syncRemoteRootBranch repo newRemoteRoot syncMode
              lift $ respond Success
            Just{} ->
              error $ "impossible match, resolveConfiguredGitUrl shouldn't return"
                  <> " `Just` unless it was passed `Just`; and here it is passed"
                  <> " `Nothing` by `expandRepo`."
      ListDependentsI hq -> -- todo: add flag to handle transitive efficiently
        resolveHQToLabeledDependencies hq >>= \lds ->
          if null lds
          then respond $ LabeledReferenceNotFound hq
          else for_ lds $ \ld -> do
            dependents <- let
              tp r = eval $ GetDependents r
              tm (Referent.Ref r) = eval $ GetDependents r
              tm (Referent.Con r _i _ct) = eval $ GetDependents r
              in LD.fold tp tm ld
            (missing, names0) <- eval . Eval $ Branch.findHistoricalRefs' dependents root'
            let types = R.toList $ Names3.types0 names0
            let terms = fmap (second Referent.toReference) $ R.toList $ Names.terms names0
            let names = types <> terms
            numberedArgs .= fmap (Text.unpack . Reference.toText) ((fmap snd names) <> toList missing)
            respond $ ListDependents hqLength ld names missing
      ListDependenciesI hq -> -- todo: add flag to handle transitive efficiently
        resolveHQToLabeledDependencies hq >>= \lds ->
          if null lds
          then respond $ LabeledReferenceNotFound hq
          else for_ lds $ \ld -> do
            dependencies :: Set Reference <- let
              tp r@(Reference.DerivedId i) = eval (LoadType i) <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just decl -> Set.delete r . DD.dependencies $ DD.asDataDecl decl
              tp _ = pure mempty
              tm (Referent.Ref r@(Reference.DerivedId i)) = eval (LoadTerm i) <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just tm -> Set.delete r $ Term.dependencies tm
              tm con@(Referent.Con (Reference.DerivedId i) cid _ct) = eval (LoadType i) <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just decl -> case DD.typeOfConstructor (DD.asDataDecl decl) cid of
                  Nothing -> error $ "What happened to " ++ show con ++ "?"
                  Just tp -> Type.dependencies tp
              tm _ = pure mempty
              in LD.fold tp tm ld
            (missing, names0) <- eval . Eval $ Branch.findHistoricalRefs' dependencies root'
            let types = R.toList $ Names3.types0 names0
            let terms = fmap (second Referent.toReference) $ R.toList $ Names.terms names0
            let names = types <> terms
            numberedArgs .= fmap (Text.unpack . Reference.toText) ((fmap snd names) <> toList missing)
            respond $ ListDependencies hqLength ld names missing
      DebugNumberedArgsI -> use numberedArgs >>= respond . DumpNumberedArgs
      DebugBranchHistoryI ->
        eval . Notify . DumpBitBooster (Branch.headHash currentBranch') =<<
          (eval . Eval $ Causal.hashToRaw (Branch._history currentBranch'))
      DebugTypecheckedUnisonFileI -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> let
          datas, effects, terms :: [(Name, Reference.Id)]
          datas = [ (Name.fromVar v, r) | (v, (r, _d)) <- Map.toList $ UF.dataDeclarationsId' uf ]
          effects = [ (Name.fromVar v, r) | (v, (r, _e)) <- Map.toList $ UF.effectDeclarationsId' uf ]
          terms = [ (Name.fromVar v, r) | (v, (r, _tm, _tp)) <- Map.toList $ UF.hashTermsId uf ]
          in eval . Notify $ DumpUnisonFileHashes hqLength datas effects terms

      DeprecateTermI {} -> notImplemented
      DeprecateTypeI {} -> notImplemented
      RemoveTermReplacementI from patchPath ->
        doRemoveReplacement from patchPath True
      RemoveTypeReplacementI from patchPath ->
        doRemoveReplacement from patchPath False
      ShowDefinitionByPrefixI {} -> notImplemented
      UpdateBuiltinsI -> notImplemented
      QuitI -> MaybeT $ pure Nothing
     where
      notImplemented = eval $ Notify NotImplemented
      success = respond Success

      resolveDefaultMetadata :: Path.Absolute -> Action' m v [String]
      resolveDefaultMetadata path = do
        let superpaths = Path.ancestors path
        xs <- for
          superpaths
          (\path -> do
            mayNames <-
              eval . ConfigLookup @[String] $ configKey "DefaultMetadata" path
            pure . join $ toList mayNames
          )
        pure . join $ toList xs

      configKey k p =
        Text.intercalate "." . toList $ k :<| fmap
          NameSegment.toText
          (Path.toSeq $ Path.unabsolute p)

      -- Takes a maybe (namespace address triple); returns it as-is if `Just`;
      -- otherwise, tries to load a value from .unisonConfig, and complains
      -- if needed.
      resolveConfiguredGitUrl
        :: PushPull
        -> Path'
        -> Maybe RemoteNamespace
        -> ExceptT (Output v) (Action' m v) RemoteNamespace
      resolveConfiguredGitUrl pushPull destPath' = \case
        Just ns -> pure ns
        Nothing -> ExceptT do
          let destPath = resolveToAbsolute destPath'
          let configKey = gitUrlKey destPath
          (eval . ConfigLookup) configKey >>= \case
            Just url ->
              case P.parse UriParser.repoPath (Text.unpack configKey) url of
                Left e ->
                  pure . Left $
                    ConfiguredGitUrlParseError pushPull destPath' url (show e)
                Right (repo, Just sbh, remotePath) ->
                  pure . Left $
                    ConfiguredGitUrlIncludesShortBranchHash pushPull repo sbh remotePath
                Right ns ->
                  pure . Right $ ns
            Nothing ->
              pure . Left $ NoConfiguredGitUrl pushPull destPath'

      gitUrlKey = configKey "GitUrl"

  case e of
    Right input -> lastInput .= Just input
    _ -> pure ()

-- todo: compare to `getHQTerms` / `getHQTypes`.  Is one universally better?
resolveHQToLabeledDependencies :: Functor m => HQ.HashQualified -> Action' m v (Set LabeledDependency)
resolveHQToLabeledDependencies = \case
  HQ.NameOnly n -> do
    parseNames <- Names3.suffixify0 <$> basicParseNames0
    let terms, types :: Set LabeledDependency
        terms = Set.map LD.referent . R.lookupDom n $ Names3.terms0 parseNames
        types = Set.map LD.typeRef . R.lookupDom n $ Names3.types0 parseNames
    pure $ terms <> types
  -- rationale: the hash should be unique enough that the name never helps
  HQ.HashQualified _n sh -> resolveHashOnly sh
  HQ.HashOnly sh -> resolveHashOnly sh
  where
  resolveHashOnly sh = do
    terms <- eval $ TermReferentsByShortHash sh
    types <- eval $ TypeReferencesByShortHash sh
    pure $ Set.map LD.referent terms <> Set.map LD.typeRef types

doDisplay :: Var v => OutputLocation -> Names -> Referent -> Action' m v ()
doDisplay outputLoc names r = do
  let tm = Term.fromReferent External r
  ppe <- prettyPrintEnvDecl names
  latestFile' <- use latestFile
  let
    loc = case outputLoc of
      ConsoleLocation    -> Nothing
      FileLocation path  -> Just path
      LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
    evalTerm r = fmap ErrorUtil.hush . eval $
      Evaluate1 (PPE.suffixifiedPPE ppe) (Term.ref External r)
    loadTerm (Reference.DerivedId r) = eval $ LoadTerm r
    loadTerm _ = pure Nothing
    loadDecl (Reference.DerivedId r) = eval $ LoadType r
    loadDecl _ = pure Nothing
  rendered <- DisplayValues.displayTerm ppe loadTerm loadTypeOfTerm evalTerm loadDecl tm
  respond $ DisplayRendered loc rendered

getLinks :: (Var v, Monad m)
         => Input
         -> Path.HQSplit'
         -> Either (Set Reference) (Maybe String)
         -> ExceptT (Output v)
                    (Action' m v)
                    (PPE.PrettyPrintEnv,
                       --  e.g. ("Foo.doc", #foodoc, Just (#builtin.Doc)
                       [(HQ.HashQualified, Reference, Maybe (Type v Ann))])
getLinks input src mdTypeStr = ExceptT $ do
  let go = fmap Right . getLinks' src
  case mdTypeStr of
    Left s -> go (Just s)
    Right Nothing -> go Nothing
    Right (Just mdTypeStr) -> parseType input mdTypeStr >>= \case
      Left e -> pure $ Left e
      Right typ -> go . Just . Set.singleton $ Type.toReference typ

getLinks' :: (Var v, Monad m)
         => Path.HQSplit'         -- definition to print metadata of
         -> Maybe (Set Reference) -- return all metadata if empty
         -> Action' m v (PPE.PrettyPrintEnv,
                          --  e.g. ("Foo.doc", #foodoc, Just (#builtin.Doc)
                         [(HQ.HashQualified, Reference, Maybe (Type v Ann))])
getLinks' src selection0 = do
  root0 <- Branch.head <$> use root
  currentPath' <- use currentPath
  let resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      p = resolveSplit' src -- ex: the (parent,hqsegment) of `List.map` - `List`
      -- all metadata (type+value) associated with name `src`
      allMd = R4.d34 (BranchUtil.getTermMetadataHQNamed p root0)
           <> R4.d34 (BranchUtil.getTypeMetadataHQNamed p root0)
      allMd' = maybe allMd (`R.restrictDom` allMd) selection0
      -- then list the values after filtering by type
      allRefs :: Set Reference = R.ran allMd'
  sigs <- for (toList allRefs) (loadTypeOfTerm . Referent.Ref)
  let deps = Set.map LD.termRef allRefs <>
             Set.unions [ Set.map LD.typeRef . Type.dependencies $ t | Just t <- sigs ]
  ppe <- prettyPrintEnvDecl =<< makePrintNamesFromLabeled' deps
  let ppeDecl = PPE.unsuffixifiedPPE ppe
  let sortedSigs = sortOn snd (toList allRefs `zip` sigs)
  let out = [(PPE.termName ppeDecl (Referent.Ref r), r, t) | (r, t) <- sortedSigs ]
  pure (PPE.suffixifiedPPE ppe, out)

resolveShortBranchHash ::
  ShortBranchHash -> ExceptT (Output v) (Action' m v) (Branch m)
resolveShortBranchHash hash = ExceptT do
  hashSet <- eval $ BranchHashesByPrefix hash
  len <- eval BranchHashLength
  case Set.toList hashSet of
    []  -> pure . Left $ NoBranchWithHash hash
    [h] -> fmap Right . eval $ LoadLocalBranch h
    _   -> pure . Left $ BranchHashAmbiguous hash (Set.map (SBH.fromHash len) hashSet)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatchNoSync
  :: (Monad m, Var v)
  => Patch
  -> Path.Absolute
  -> Action' m v Bool
propagatePatchNoSync patch scopePath = stepAtMNoSync'
  (Path.unabsolute scopePath, lift . lift . Propagate.propagateAndApply patch)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch :: (Monad m, Var v) =>
  InputDescription -> Patch -> Path.Absolute -> Action' m v Bool
propagatePatch inputDescription patch scopePath =
  stepAtM' (inputDescription <> " (applying patch)")
           (Path.unabsolute scopePath,
              lift . lift . Propagate.propagateAndApply patch)

-- | Create the args needed for showTodoOutput and call it
doShowTodoOutput :: Monad m => Patch -> Path.Absolute -> Action' m v ()
doShowTodoOutput patch scopePath = do
  scope <- getAt scopePath
  let names0 = Branch.toNames0 (Branch.head scope)
  -- only needs the local references to check for obsolete defs
  let getPpe = do
        names <- makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
        prettyPrintEnvDecl names
  showTodoOutput getPpe patch names0

-- | Show todo output if there are any conflicts or edits.
showTodoOutput
  :: Action' m v PPE.PrettyPrintEnvDecl
     -- ^ Action that fetches the pretty print env. It's expensive because it
     -- involves looking up historical names, so only call it if necessary.
  -> Patch
  -> Names0
  -> Action' m v ()
showTodoOutput getPpe patch names0 = do
  todo <- checkTodo patch names0
  if TO.noConflicts todo && TO.noEdits todo
    then respond NoConflictsOrEdits
    else do
      numberedArgs .=
        (Text.unpack . Reference.toText . view _2 <$>
          fst (TO.todoFrontierDependents todo))
      ppe <- getPpe
      respond $ TodoOutput ppe todo

checkTodo :: Patch -> Names0 -> Action m i v (TO.TodoOutput v Ann)
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
  let
    scoredDirtyTerms =
      List.sortOn (view _1) [ (scoreFn r, r, t) | (r,t) <- dirtyTerms ]
    scoredDirtyTypes =
      List.sortOn (view _1) [ (scoreFn r, r, t) | (r,t) <- dirtyTypes ]
  pure $
    TO.TodoOutput
      (Set.size remainingTransitive)
      (frontierTerms, frontierTypes)
      (scoredDirtyTerms, scoredDirtyTypes)
      (Names.conflicts names0)
      (Patch.conflicts patch)
  where
  frontierTransitiveDependents ::
    Monad m => (Reference -> m (Set Reference)) -> Names0 -> Set Reference -> m (Set Reference)
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
computeFrontier :: forall m . Monad m
         => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
         -> Patch
         -> Names0
         -> m (R.Relation Reference Reference)
computeFrontier getDependents patch names = let
  edited :: Set Reference
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

eval :: Command m i v a -> Action m i v a
eval = lift . lift . Free.eval

confirmedCommand :: Input -> Action m i v Bool
confirmedCommand i = do
  i0 <- use lastInput
  pure $ Just i == i0

listBranch :: Branch0 m -> [SearchResult]
listBranch (Branch.toNames0 -> b) =
  List.sortOn (\s -> (SR.name s, s)) (SR.fromNames b)

-- | restores the full hash to these search results, for _numberedArgs purposes
searchResultToHQString :: SearchResult -> String
searchResultToHQString = \case
  SR.Tm' n r _ -> HQ'.toString $ HQ'.requalify n r
  SR.Tp' n r _ -> HQ'.toString $ HQ'.requalify n (Referent.Ref r)
  _ -> error "unpossible match failure"

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
      subnames = Branch.toNames0 . Branch.head $
                   Branch.getAt' (Path.singleton last) b
      rootnames =
        Names.filter (== lastName) .
        Branch.toNames0 . set Branch.children mempty $ Branch.head b
      names0 = rootnames <> Names.prefix0 lastName subnames

searchResultsFor :: Names0 -> [Referent] -> [Reference] -> [SearchResult]
searchResultsFor ns terms types =
  [ SR.termSearchResult ns name ref
  | ref <- terms
  , name <- toList (Names.namesForReferent ns ref)
  ] <>
  [ SR.typeSearchResult ns name ref
  | ref <- types
  , name <- toList (Names.namesForReference ns ref)
  ]

searchBranchScored :: forall score. (Ord score)
              => Names0
              -> (Name -> Name -> Maybe score)
              -> [HQ.HashQualified]
              -> [SearchResult]
searchBranchScored names0 score queries =
  nubOrd . fmap snd . toList $ searchTermNamespace <> searchTypeNamespace
  where
  searchTermNamespace = foldMap do1query queries
    where
    do1query :: HQ.HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.terms $ names0)
    score1hq :: HQ.HashQualified -> (Name, Referent) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` Referent.toShortHash ref ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` Referent.toShortHash ref ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = SR.termSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty
  searchTypeNamespace = foldMap do1query queries
    where
    do1query :: HQ.HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.types $ names0)
    score1hq :: HQ.HashQualified -> (Name, Reference) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` Reference.toShortHash ref ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` Reference.toShortHash ref ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = SR.typeSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty

-- Separates type references from term references and returns types and terms,
-- respectively. For terms that are constructors, turns them into their data
-- types.
collateReferences
  :: Foldable f
  => Foldable g
  => f Reference -- types requested
  -> g Referent -- terms requested, including ctors
  -> (Set Reference, Set Reference)
collateReferences (toList -> types) (toList -> terms) =
  let terms' = [ r | Referent.Ref r <- terms ]
      types' = [ r | Referent.Con r _ _ <- terms ]
  in  (Set.fromList types' <> Set.fromList types, Set.fromList terms')

-- | The output list (of lists) corresponds to the query list.
searchBranchExact :: Int -> Names -> [HQ.HashQualified] -> [[SearchResult]]
searchBranchExact len names queries = let
  searchTypes :: HQ.HashQualified -> [SearchResult]
  searchTypes query =
    -- a bunch of references will match a HQ ref.
    let refs = toList $ Names3.lookupHQType query names in
    refs <&> \r ->
      let hqNames = Names3.typeName len r names in
      let primaryName =
            last . sortOn (\n -> HQ.matchesNamedReference (HQ'.toName n) r query)
                 $ toList hqNames in
      let aliases = Set.delete primaryName hqNames in
      SR.typeResult primaryName r aliases
  searchTerms :: HQ.HashQualified -> [SearchResult]
  searchTerms query =
    -- a bunch of references will match a HQ ref.
    let refs = toList $ Names3.lookupHQTerm query names in
    refs <&> \r ->
      let hqNames = Names3.termName len r names in
      let primaryName =
            last . sortOn (\n -> HQ.matchesNamedReferent (HQ'.toName n) r query)
                 $ toList hqNames in
      let aliases = Set.delete primaryName hqNames in
      SR.termResult primaryName r aliases
  in [ searchTypes q <> searchTerms q | q <- queries ]

respond :: Output v -> Action m i v ()
respond output = eval $ Notify output

respondNumbered :: NumberedOutput v -> Action m i v ()
respondNumbered output = do
  args <- eval $ NotifyNumbered output
  unless (null args) $
    numberedArgs .= toList args

unlessError :: ExceptT (Output v) (Action' m v) () -> Action' m v ()
unlessError ma = runExceptT ma >>= either (eval . Notify) pure

unlessError' :: (e -> Output v) -> ExceptT e (Action' m v) () -> Action' m v ()
unlessError' f ma = unlessError $ withExceptT f ma

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch :: (Monad m, Var v) => Branch.MergeMode ->
  InputDescription -> Maybe (Output v) -> Branch m -> Maybe Path.Path' -> Path.Absolute -> Action' m v ()
mergeBranchAndPropagateDefaultPatch mode inputDescription unchangedMessage srcb dest0 dest =
  ifM (mergeBranch mode inputDescription srcb dest0 dest)
      (loadPropagateDiffDefaultPatch inputDescription dest0 dest)
      (for_ unchangedMessage respond)
  where
  mergeBranch :: (Monad m, Var v) =>
    Branch.MergeMode -> InputDescription -> Branch m -> Maybe Path.Path' -> Path.Absolute -> Action' m v Bool
  mergeBranch mode inputDescription srcb dest0 dest = unsafeTime "Merge Branch" $ do
    destb <- getAt dest
    merged <- eval . Eval $ Branch.merge' mode srcb destb
    b <- updateAtM inputDescription dest (const $ pure merged)
    for_ dest0 $ \dest0 ->
      diffHelper (Branch.head destb) (Branch.head merged) >>=
        respondNumbered . uncurry (ShowDiffAfterMerge dest0 dest)
    pure b

loadPropagateDiffDefaultPatch :: (Monad m, Var v) =>
  InputDescription -> Maybe Path.Path' -> Path.Absolute -> Action' m v ()
loadPropagateDiffDefaultPatch inputDescription dest0 dest = unsafeTime "Propagate Default Patch" $ do
    original <- getAt dest
    patch <- eval . Eval $ Branch.getPatch defaultPatchNameSegment (Branch.head original)
    patchDidChange <- propagatePatch inputDescription patch dest
    when patchDidChange . for_ dest0 $ \dest0 -> do
      patched <- getAt dest
      let patchPath = snoc dest0 defaultPatchNameSegment
      diffHelper (Branch.head original) (Branch.head patched) >>=
        respondNumbered . uncurry (ShowDiffAfterMergePropagate dest0 dest patchPath)

getAt :: Functor m => Path.Absolute -> Action m i v (Branch m)
getAt (Path.Absolute p) =
  use root <&> fromMaybe Branch.empty . Branch.getAt p

-- Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM :: Applicative m
          => InputDescription
          -> Path.Absolute
          -> (Branch m -> Action m i v (Branch m))
          -> Action m i v Bool
updateAtM reason (Path.Absolute p) f = do
  b  <- use lastSavedRoot
  b' <- Branch.modifyAtM p f b
  updateRoot b' reason
  pure $ b /= b'

stepAt
  :: forall m i v
   . Monad m
  => InputDescription
  -> (Path, Branch0 m -> Branch0 m)
  -> Action m i v ()
stepAt cause = stepManyAt @m @[] cause . pure

stepAtNoSync :: forall m i v. Monad m
       => (Path, Branch0 m -> Branch0 m)
       -> Action m i v ()
stepAtNoSync = stepManyAtNoSync @m @[] . pure

stepAtM :: forall m i v. Monad m
        => InputDescription
        -> (Path, Branch0 m -> m (Branch0 m))
        -> Action m i v ()
stepAtM cause = stepManyAtM @m @[] cause . pure

stepAtM'
  :: forall m i v
   . Monad m
  => InputDescription
  -> (Path, Branch0 m -> Action m i v (Branch0 m))
  -> Action m i v Bool
stepAtM' cause = stepManyAtM' @m @[] cause . pure

stepAtMNoSync'
  :: forall m i v
   . Monad m
  => (Path, Branch0 m -> Action m i v (Branch0 m))
  -> Action m i v Bool
stepAtMNoSync' = stepManyAtMNoSync' @m @[] . pure

stepManyAt
  :: (Monad m, Foldable f)
  => InputDescription
  -> f (Path, Branch0 m -> Branch0 m)
  -> Action m i v ()
stepManyAt reason actions = do
  stepManyAtNoSync actions
  b <- use root
  updateRoot b reason

-- Like stepManyAt, but doesn't update the root
stepManyAtNoSync
  :: (Monad m, Foldable f)
  => f (Path, Branch0 m -> Branch0 m)
  -> Action m i v ()
stepManyAtNoSync actions = do
  b <- use root
  let new = Branch.stepManyAt actions b
  root .= new

stepManyAtM :: (Monad m, Foldable f)
           => InputDescription
           -> f (Path, Branch0 m -> m (Branch0 m))
           -> Action m i v ()
stepManyAtM reason actions = do
    stepManyAtMNoSync actions
    b <- use root
    updateRoot b reason

stepManyAtMNoSync :: (Monad m, Foldable f)
           => f (Path, Branch0 m -> m (Branch0 m))
           -> Action m i v ()
stepManyAtMNoSync actions = do
    b <- use root
    b' <- eval . Eval $ Branch.stepManyAtM actions b
    root .= b'

stepManyAtM' :: (Monad m, Foldable f)
           => InputDescription
           -> f (Path, Branch0 m -> Action m i v (Branch0 m))
           -> Action m i v Bool
stepManyAtM' reason actions = do
    b <- use root
    b' <- Branch.stepManyAtM actions b
    updateRoot b' reason
    pure (b /= b')

stepManyAtMNoSync' :: (Monad m, Foldable f)
           => f (Path, Branch0 m -> Action m i v (Branch0 m))
           -> Action m i v Bool
stepManyAtMNoSync' actions = do
    b <- use root
    b' <- Branch.stepManyAtM actions b
    root .= b'
    pure (b /= b')

updateRoot :: Branch m -> InputDescription -> Action m i v ()
updateRoot new reason = do
  old <- use lastSavedRoot
  when (old /= new) $ do
    root .= new
    eval $ SyncLocalRootBranch new
    eval $ AppendToReflog reason old new
    lastSavedRoot .= new

-- cata for 0, 1, or more elements of a Foldable
-- tries to match as lazily as possible
zeroOneOrMore :: Foldable f => f a -> b -> (a -> b) -> (f a -> b) -> b
zeroOneOrMore f zero one more = case toList f of
  _ : _ : _ -> more f
  a : _ -> one a
  _ -> zero

-- Goal: If `remaining = root - toBeDeleted` contains definitions X which
-- depend on definitions Y not in `remaining` (which should also be in
-- `toBeDeleted`), then complain by returning (Y, X).
getEndangeredDependents :: forall m. Monad m
                        => (Reference -> m (Set Reference))
                        -> Names0
                        -> Names0
                        -> m (Names0, Names0)
getEndangeredDependents getDependents toDelete root = do
  let remaining  = root `Names.difference` toDelete
      toDelete', remaining', extinct :: Set Reference
      toDelete'  = Names.allReferences toDelete
      remaining' = Names.allReferences remaining          -- left over after delete
      extinct    = toDelete'  `Set.difference` remaining' -- deleting and not left over
      accumulateDependents m r = getDependents r <&> \ds -> Map.insert r ds m
  dependentsOfExtinct :: Map Reference (Set Reference) <-
    foldM accumulateDependents mempty extinct
  let orphaned, endangered, failed :: Set Reference
      orphaned   = fold dependentsOfExtinct
      endangered = orphaned `Set.intersection` remaining'
      failed = Set.filter hasEndangeredDependent extinct
      hasEndangeredDependent r = any (`Set.member` endangered)
                                     (dependentsOfExtinct Map.! r)
  pure ( Names.restrictReferences failed toDelete
       , Names.restrictReferences endangered root `Names.difference` toDelete)

-- Applies the selection filter to the adds/updates of a slurp result,
-- meaning that adds/updates should only contain the selection or its transitive
-- dependencies, any unselected transitive dependencies of the selection will
-- be added to `extraDefinitions`.
applySelection :: forall v a. Var v =>
  [HQ'.HashQualified] -> UF.TypecheckedUnisonFile v a -> SlurpResult v -> SlurpResult v
applySelection [] _ = id
applySelection hqs file = \sr@SlurpResult{..} ->
  sr { adds = adds `SC.intersection` closed
     , updates = updates `SC.intersection` closed
     , extraDefinitions = closed `SC.difference` selection
     }
  where
  selectedNames0 =
    Names.filterByHQs (Set.fromList hqs) (UF.typecheckedToNames0 file)
  selection, closed :: SlurpComponent v
  selection = SlurpComponent selectedTypes selectedTerms
  closed = SC.closeWithDependencies file selection
  selectedTypes, selectedTerms :: Set v
  selectedTypes = Set.map var $ R.dom (Names.types selectedNames0)
  selectedTerms = Set.map var $ R.dom (Names.terms selectedNames0)

var :: Var v => Name -> v
var name = Var.named (Name.toText name)

toSlurpResult
  :: forall v
   . Var v
  => Path.Absolute
  -> UF.TypecheckedUnisonFile v Ann
  -> Names0
  -> SlurpResult v
toSlurpResult currentPath uf existingNames =
  Slurp.subtractComponent (conflicts <> ctorCollisions) $ SlurpResult
    uf
    mempty
    adds
    dups
    mempty
    conflicts
    updates
    termCtorCollisions
    ctorTermCollisions
    termAliases
    typeAliases
    mempty
  where
  fileNames0 = UF.typecheckedToNames0 uf

  sc :: R.Relation Name Referent -> R.Relation Name Reference -> SlurpComponent v
  sc terms types = SlurpComponent { terms = Set.map var (R.dom terms)
                                  , types = Set.map var (R.dom types) }

  -- conflict (n,r) if n is conflicted in names0
  conflicts :: SlurpComponent v
  conflicts = sc terms types where
    terms = R.filterDom (conflicted . Names.termsNamed existingNames)
                        (Names.terms fileNames0)
    types = R.filterDom (conflicted . Names.typesNamed existingNames)
                        (Names.types fileNames0)
    conflicted s = Set.size s > 1

  ctorCollisions :: SlurpComponent v
  ctorCollisions =
    mempty { SC.terms = termCtorCollisions <> ctorTermCollisions }

  -- termCtorCollision (n,r) if (n, r' /= r) exists in existingNames and
  -- r is Ref and r' is Con
  termCtorCollisions :: Set v
  termCtorCollisions = Set.fromList
    [ var n
    | (n, Referent.Ref{}) <- R.toList (Names.terms fileNames0)
    , [r@Referent.Con{}]  <- [toList $ Names.termsNamed existingNames n]
    -- ignore collisions w/ ctors of types being updated
    , Set.notMember (Referent.toReference r) typesToUpdate
    ]

  -- the set of typerefs that are being updated by this file
  typesToUpdate :: Set Reference
  typesToUpdate = Set.fromList
    [ r
    | (n, r') <- R.toList (Names.types fileNames0)
    , r       <- toList (Names.typesNamed existingNames n)
    , r /= r'
    ]

  -- ctorTermCollisions (n,r) if (n, r' /= r) exists in names0 and r is Con
  -- and r' is Ref except we relaxed it to where r' can be Con or Ref
  -- what if (n,r) and (n,r' /= r) exists in names and r, r' are Con
  ctorTermCollisions :: Set v
  ctorTermCollisions = Set.fromList
    [ var n
    | (n, Referent.Con{}) <- R.toList (Names.terms fileNames0)
    , r                   <- toList $ Names.termsNamed existingNames n
    -- ignore collisions w/ ctors of types being updated
    , Set.notMember (Referent.toReference r) typesToUpdate
    , Set.notMember (var n) (terms dups)
    ]

  -- duplicate (n,r) if (n,r) exists in names0
  dups :: SlurpComponent v
  dups = sc terms types where
    terms = R.intersection (Names.terms existingNames) (Names.terms fileNames0)
    types = R.intersection (Names.types existingNames) (Names.types fileNames0)

  -- update (n,r) if (n,r' /= r) exists in existingNames and r, r' are Ref
  updates :: SlurpComponent v
  updates = SlurpComponent (Set.fromList types) (Set.fromList terms) where
    terms =
      [ var n
      | (n, r'@Referent.Ref{}) <- R.toList (Names.terms fileNames0)
      , [r@Referent.Ref{}]     <- [toList $ Names.termsNamed existingNames n]
      , r' /= r
      ]
    types =
      [ var n
      | (n, r') <- R.toList (Names.types fileNames0)
      , [r]     <- [toList $ Names.typesNamed existingNames n]
      , r' /= r
      ]

  buildAliases
    :: R.Relation Name Referent
    -> R.Relation Name Referent
    -> Set v
    -> Map v Slurp.Aliases
  buildAliases existingNames namesFromFile duplicates = Map.fromList
    [ ( var n
      , if null aliasesOfOld
        then Slurp.AddAliases aliasesOfNew
        else Slurp.UpdateAliases aliasesOfOld aliasesOfNew
      )
    | (n, r@Referent.Ref{}) <- R.toList namesFromFile
  -- All the refs whose names include `n`, and are not `r`
    , let
      refs = Set.delete r $ R.lookupDom n existingNames
      aliasesOfNew =
        Set.map (Path.unprefixName currentPath) . Set.delete n $
          R.lookupRan r existingNames
      aliasesOfOld =
        Set.map (Path.unprefixName currentPath) . Set.delete n . R.dom $
          R.restrictRan existingNames refs
    , not (null aliasesOfNew && null aliasesOfOld)
    , Set.notMember (var n) duplicates
    ]

  termAliases :: Map v Slurp.Aliases
  termAliases = buildAliases (Names.terms existingNames)
                             (Names.terms fileNames0)
                             (SC.terms dups)

  typeAliases :: Map v Slurp.Aliases
  typeAliases = buildAliases (R.mapRan Referent.Ref $ Names.types existingNames)
                             (R.mapRan Referent.Ref $ Names.types fileNames0)
                             (SC.types dups)

  -- (n,r) is in `adds` if n isn't in existingNames
  adds = sc terms types where
    terms = addTerms (Names.terms existingNames) (Names.terms fileNames0)
    types = addTypes (Names.types existingNames) (Names.types fileNames0)
    addTerms existingNames = R.filter go where
      go (n, Referent.Ref{}) = (not . R.memberDom n) existingNames
      go _ = False
    addTypes existingNames = R.filter go where
      go (n, _) = (not . R.memberDom n) existingNames

filterBySlurpResult :: Ord v
           => SlurpResult v
           -> UF.TypecheckedUnisonFile v Ann
           -> UF.TypecheckedUnisonFile v Ann
filterBySlurpResult SlurpResult{..}
                    (UF.TypecheckedUnisonFileId
                      dataDeclarations'
                      effectDeclarations'
                      topLevelComponents'
                      watchComponents
                      hashTerms) =
  UF.TypecheckedUnisonFileId datas effects tlcs watches hashTerms'
  where
  keep = updates <> adds
  keepTerms = SC.terms keep
  keepTypes = SC.types keep
  hashTerms' = Map.restrictKeys hashTerms keepTerms
  datas = Map.restrictKeys dataDeclarations' keepTypes
  effects = Map.restrictKeys effectDeclarations' keepTypes
  tlcs = filter (not.null) $ fmap (List.filter filterTLC) topLevelComponents'
  watches = filter (not.null.snd) $ fmap (second (List.filter filterTLC)) watchComponents
  filterTLC (v,_,_) = Set.member v keepTerms

-- updates the namespace for adding `slurp`
doSlurpAdds :: forall m v. (Monad m, Var v)
            => SlurpComponent v
            -> UF.TypecheckedUnisonFile v Ann
            -> (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.stepManyAt0 (typeActions <> termActions)
  where
  typeActions = map doType . toList $ SC.types slurp
  termActions = map doTerm . toList $
    SC.terms slurp <> Slurp.constructorsFor (SC.types slurp) uf
  names = UF.typecheckedToNames0 uf
  tests = Set.fromList $ fst <$> UF.watchesOfKind UF.TestWatch (UF.discardTypes uf)
  (isTestType, isTestValue) = isTest
  md v =
    if Set.member v tests then Metadata.singleton isTestType isTestValue
    else Metadata.empty
  doTerm :: v -> (Path, Branch0 m -> Branch0 m)
  doTerm v = case toList (Names.termsNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTermName split r (md v)
    wha -> error $ "Unison bug, typechecked file w/ multiple terms named "
                <> Var.nameStr v <> ": " <> show wha
  doType :: v -> (Path, Branch0 m -> Branch0 m)
  doType v = case toList (Names.typesNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTypeName split r Metadata.empty
    wha -> error $ "Unison bug, typechecked file w/ multiple types named "
                <> Var.nameStr v <> ": " <> show wha
  errorEmptyVar = error "encountered an empty var name"
  errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

doSlurpUpdates :: Monad m
               => Map Name (Reference, Reference)
               -> Map Name (Reference, Reference)
               -> [(Name, Referent)]
               -> (Branch0 m -> Branch0 m)
doSlurpUpdates typeEdits termEdits deprecated b0 =
  Branch.stepManyAt0 (typeActions <> termActions <> deprecateActions) b0
  where
  typeActions = join . map doType . Map.toList $ typeEdits
  termActions = join . map doTerm . Map.toList $ termEdits
  deprecateActions = join . map doDeprecate $ deprecated where
    doDeprecate (n, r) = case Path.splitFromName n of
      Nothing -> errorEmptyVar
      Just split -> [BranchUtil.makeDeleteTermName split r]

  -- we copy over the metadata on the old thing
  -- todo: if the thing being updated, m, is metadata for something x in b0
  -- update x's md to reference `m`
  doType, doTerm ::
    (Name, (Reference, Reference)) -> [(Path, Branch0 m -> Branch0 m)]
  doType (n, (old, new)) = case Path.splitFromName n of
    Nothing -> errorEmptyVar
    Just split -> [ BranchUtil.makeDeleteTypeName split old
                  , BranchUtil.makeAddTypeName split new oldMd ]
      where
      oldMd = BranchUtil.getTypeMetadataAt split old b0
  doTerm (n, (old, new)) = case Path.splitFromName n of
    Nothing -> errorEmptyVar
    Just split -> [ BranchUtil.makeDeleteTermName split (Referent.Ref old)
                  , BranchUtil.makeAddTermName split (Referent.Ref new) oldMd ]
      where
      -- oldMd is the metadata linked to the old definition
      -- we relink it to the new definition
      oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0
  errorEmptyVar = error "encountered an empty var name"

loadSearchResults :: Ord v => [SR.SearchResult] -> Action m i v [SearchResult' v Ann]
loadSearchResults = traverse loadSearchResult
  where
  loadSearchResult = \case
    SR.Tm (SR.TermResult name r aliases) -> do
      typ <- loadReferentType r
      pure $ SR'.Tm name typ r aliases
    SR.Tp (SR.TypeResult name r aliases) -> do
      dt <- loadTypeDisplayThing r
      pure $ SR'.Tp name dt r aliases

loadDisplayInfo ::
  Set Reference -> Action m i v ([(Reference, Maybe (Type v Ann))]
                                ,[(Reference, DisplayThing (DD.Decl v Ann))])
loadDisplayInfo refs = do
  termRefs <- filterM (eval . IsTerm) (toList refs)
  typeRefs <- filterM (eval . IsType) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> eval (LoadTypeOfTerm r)
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayThing r
  pure (terms, types)

loadReferentType :: Referent -> Action m i v (Maybe (Type v Ann))
loadReferentType = \case
  Referent.Ref r -> eval $ LoadTypeOfTerm r
  Referent.Con r cid _ -> getTypeOfConstructor r cid
  where
  getTypeOfConstructor :: Reference -> Int -> Action m i v (Maybe (Type v Ann))
  getTypeOfConstructor (Reference.DerivedId r) cid = do
    maybeDecl <- eval $ LoadType r
    pure $ case maybeDecl of
      Nothing -> Nothing
      Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
  getTypeOfConstructor r cid =
    error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid

loadTypeDisplayThing :: Reference -> Action m i v (DisplayThing (DD.Decl v Ann))
loadTypeDisplayThing = \case
  Reference.Builtin _ -> pure BuiltinThing
  Reference.DerivedId id ->
    maybe (MissingThing id) RegularThing <$> eval (LoadType id)

lexedSource :: Monad m => SourceName -> Source -> Action' m v (Names, LexedSource)
lexedSource name src = do
  let tokens = L.lexer (Text.unpack name) (Text.unpack src)
      getHQ = \case
        L.Backticks s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.WordyId   s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.SymbolyId s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
        L.Hash      sh          -> Just (HQ.HashOnly sh)
        _                       -> Nothing
      hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
  parseNames <- makeHistoricalParsingNames hqs
  pure (parseNames, (src, tokens))

prettyPrintEnv :: Names -> Action' m v PPE.PrettyPrintEnv
prettyPrintEnv ns = eval CodebaseHashLength <&> (`PPE.fromNames` ns)

prettyPrintEnvDecl :: Names -> Action' m v PPE.PrettyPrintEnvDecl
prettyPrintEnvDecl ns = eval CodebaseHashLength <&> (`PPE.fromNamesDecl` ns)

parseSearchType :: (Monad m, Var v)
  => Input -> String -> Action' m v (Either (Output v) (Type v Ann))
parseSearchType input typ = fmap Type.removeAllEffectVars <$> parseType input typ

parseType :: (Monad m, Var v)
  => Input -> String -> Action' m v (Either (Output v) (Type v Ann))
parseType input src = do
  -- `show Input` is the name of the "file" being lexed
  (names0, lexed) <- lexedSource (Text.pack $ show input) (Text.pack src)
  parseNames <- Names3.suffixify0 <$> basicParseNames0
  let names = Names3.push (Names3.currentNames names0)
                          (Names3.Names parseNames (Names3.oldNames names0))
  e <- eval $ ParseType names lexed
  pure $ case e of
    Left err -> Left $ TypeParseError src err
    Right typ -> case Type.bindNames mempty (Names3.currentNames names)
                    $ Type.generalizeLowercase mempty typ of
      Left es -> Left $ ParseResolutionFailures src (toList es)
      Right typ -> Right typ

makeShadowedPrintNamesFromLabeled
  :: Monad m => Set LabeledDependency -> Names0 -> Action' m v Names
makeShadowedPrintNamesFromLabeled deps shadowing =
  Names3.shadowing shadowing <$> makePrintNamesFromLabeled' deps

getTermsIncludingHistorical
  :: Monad m => Path.HQSplit -> Branch0 m -> Action' m v (Set Referent)
getTermsIncludingHistorical (p, hq) b = case Set.toList refs of
  [] -> case hq of
    HQ'.HashQualified n hs -> do
      names <- findHistoricalHQs
        $ Set.fromList [HQ.HashQualified (Name.unsafeFromText (NameSegment.toText n)) hs]
      pure . R.ran $ Names.terms names
    _ -> pure Set.empty
  _ -> pure refs
  where refs = BranchUtil.getTerm (p, hq) b

-- discards inputs that aren't hashqualified;
-- I'd enforce it with finer-grained types if we had them.
findHistoricalHQs :: Monad m => Set HQ.HashQualified -> Action' m v Names0
findHistoricalHQs lexedHQs0 = do
  root <- use root
  currentPath <- use currentPath
  let
    -- omg this nightmare name-to-path parsing code is littered everywhere.
    -- We need to refactor so that the absolute-ness of a name isn't represented
    -- by magical text combinations.
    -- Anyway, this function takes a name, tries to determine whether it is
    -- relative or absolute, and tries to return the corresponding name that is
    -- /relative/ to the root.
    preprocess n = case Name.toString n of
      -- some absolute name that isn't just "."
      '.' : t@(_:_)  -> Name.unsafeFromString t
      -- something in current path
      _ ->  if Path.isRoot currentPath then n
            else Name.joinDot (Path.toName . Path.unabsolute $ currentPath) n

    lexedHQs = Set.map (fmap preprocess) . Set.filter HQ.hasHash $ lexedHQs0
  (_missing, rawHistoricalNames) <- eval . Eval $ Branch.findHistoricalHQs lexedHQs root
  pure rawHistoricalNames

makeShadowedPrintNamesFromHQ :: Monad m => Set HQ.HashQualified -> Names0 -> Action' m v Names
makeShadowedPrintNamesFromHQ lexedHQs shadowing = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames0 <- basicPrettyPrintNames0
  currentPath <- use currentPath
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    Names3.shadowing
      shadowing
      (Names basicNames0 (fixupNamesRelative currentPath rawHistoricalNames))

makePrintNamesFromLabeled'
  :: Monad m => Set LabeledDependency -> Action' m v Names
makePrintNamesFromLabeled' deps = do
  root                           <- use root
  currentPath                    <- use currentPath
  (_missing, rawHistoricalNames) <- eval . Eval $ Branch.findHistoricalRefs
    deps
    root
  basicNames0 <- basicPrettyPrintNames0
  pure $ Names basicNames0 (fixupNamesRelative currentPath rawHistoricalNames)

-- Any absolute names in the input which have `currentPath` as a prefix
-- are converted to names relative to current path. All other names are
-- converted to absolute names. For example:
--
-- e.g. if currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names0 -> Names0
fixupNamesRelative currentPath' = Names3.map0 fixName where
  prefix = Path.toName (Path.unabsolute currentPath')
  fixName n = if currentPath' == Path.absoluteEmpty then n else
    fromMaybe (Name.makeAbsolute n) (Name.stripNamePrefix prefix n)

makeHistoricalParsingNames ::
  Monad m => Set HQ.HashQualified -> Action' m v Names
makeHistoricalParsingNames lexedHQs = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames0 <- basicParseNames0
  currentPath <- use currentPath
  pure $ Names basicNames0
               (Names3.makeAbsolute0 rawHistoricalNames <>
                 fixupNamesRelative currentPath rawHistoricalNames)

basicParseNames0, basicPrettyPrintNames0, slurpResultNames0 :: Functor m => Action' m v Names0
basicParseNames0 = fst <$> basicNames0'
basicPrettyPrintNames0 = snd <$> basicNames0'
-- we check the file against everything in the current path
slurpResultNames0 = currentPathNames0

currentPathNames0 :: Functor m => Action' m v Names0
currentPathNames0 = do
  currentPath' <- use currentPath
  currentBranch' <- getAt currentPath'
  pure $ Branch.toNames0 (Branch.head currentBranch')

-- implementation detail of baseicParseNames0 and basicPrettyPrintNames0
basicNames0' :: Functor m => Action' m v (Names0, Names0)
basicNames0' = do
  root' <- use root
  currentPath' <- use currentPath
  currentBranch' <- getAt currentPath'
  let root0 = Branch.head root'
      absoluteRootNames0 = Names3.makeAbsolute0 (Branch.toNames0 root0)
      currentBranch0 = Branch.head currentBranch'
      currentPathNames0 = Branch.toNames0 currentBranch0
      -- all names, but with local names in their relative form only, rather
      -- than absolute; external names appear as absolute
      currentAndExternalNames0 = currentPathNames0 `Names3.unionLeft0` absDot externalNames where
        absDot = Names.prefix0 (Name.unsafeFromText "")
        externalNames = rootNames `Names.difference` pathPrefixed currentPathNames0
        rootNames = Branch.toNames0 root0
        pathPrefixed = case Path.unabsolute currentPath' of
          Path.Path (toList -> []) -> id
          p -> Names.prefix0 (Path.toName p)
      -- parsing should respond to local and absolute names
      parseNames00 = currentPathNames0 <> absoluteRootNames0
      -- pretty-printing should use local names where available
      prettyPrintNames00 = currentAndExternalNames0
  pure (parseNames00, prettyPrintNames00)

data AddRunMainResult v
  = NoTermWithThatName
  | TermHasBadType (Type v Ann)
  | RunMainSuccess (TypecheckedUnisonFile  v Ann)

-- Given a typechecked file with a main function called `mainName`
-- of the type `'{IO} ()`, adds an extra binding which
-- forces the `main` function.
--
-- If that function doesn't exist in the typechecked file, the
-- codebase is consulted.
addRunMain
  :: (Monad m, Var v)
  => String
  -> Maybe (TypecheckedUnisonFile v Ann)
  -> Action' m v (AddRunMainResult v)
addRunMain mainName Nothing = do
  parseNames0 <- basicParseNames0
  let loadTypeOfTerm ref = eval $ LoadTypeOfTerm ref
  mainType <- eval RuntimeMain
  mainToFile <$>
    MainTerm.getMainTerm loadTypeOfTerm parseNames0 mainName mainType
  where
    mainToFile (MainTerm.NotAFunctionName _) = NoTermWithThatName
    mainToFile (MainTerm.NotFound _) = NoTermWithThatName
    mainToFile (MainTerm.BadType _ ty) = maybe NoTermWithThatName TermHasBadType ty
    mainToFile (MainTerm.Success hq tm typ) = RunMainSuccess $
      let v = Var.named (HQ.toText hq) in
      UF.typecheckedUnisonFile mempty mempty mempty [("main",[(v, tm, typ)])] -- mempty
addRunMain mainName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == mainName) . view _1) components
  mainType <- eval RuntimeMain
  case mainComponent of
    [(v, tm, ty)] -> pure $ let
      v2 = Var.freshIn (Set.fromList [v]) v
      a = ABT.annotation tm
      in
      if Typechecker.isSubtype mainType ty then RunMainSuccess $ let
        runMain = DD.forceTerm a a (Term.var a v)
        in UF.typecheckedUnisonFile
             (UF.dataDeclarationsId' uf)
             (UF.effectDeclarationsId' uf)
             (UF.topLevelComponents' uf)
             (UF.watchComponents uf <> [("main", [(v2, runMain, mainType)])])
      else TermHasBadType ty
    _ -> addRunMain mainName Nothing

executePPE
  :: (Var v, Monad m)
  => TypecheckedUnisonFile v a
  -> Action' m v PPE.PrettyPrintEnv
executePPE unisonFile =
  -- voodoo
  prettyPrintEnv =<<
    makeShadowedPrintNamesFromLabeled
      (UF.termSignatureExternalLabeledDependencies unisonFile)
      (UF.typecheckedToNames0 unisonFile)

diffHelper :: Monad m
  => Branch0 m
  -> Branch0 m
  -> Action' m v (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput v Ann)
diffHelper before after = do
  hqLength <- eval CodebaseHashLength
  diff     <- eval . Eval $ BranchDiff.diff0 before after
  names0 <- basicPrettyPrintNames0
  ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl (Names names0 mempty)
  (ppe,) <$>
    OBranchDiff.toOutput
      loadTypeOfTerm
      declOrBuiltin
      hqLength
      (Branch.toNames0 before)
      (Branch.toNames0 after)
      ppe
      diff

loadTypeOfTerm :: Referent -> Action m i v (Maybe (Type v Ann))
loadTypeOfTerm (Referent.Ref r) = eval $ LoadTypeOfTerm r
loadTypeOfTerm (Referent.Con (Reference.DerivedId r) cid _) = do
  decl <- eval $ LoadType r
  case decl of
    Just (either DD.toDataDecl id -> dd) -> pure $ DD.typeOfConstructor dd cid
    Nothing -> pure Nothing
loadTypeOfTerm Referent.Con{} = error $
  reportBug "924628772" "Attempt to load a type declaration which is a builtin!"

declOrBuiltin :: Reference -> Action m i v (Maybe (DD.DeclOrBuiltin v Ann))
declOrBuiltin r = case r of
  Reference.Builtin{} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> eval (LoadType id)

module Unison.Codebase.Editor.HandleInput
  ( loop,
  )
where

-- TODO: Don't import backend

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import qualified Control.Error.Util as ErrorUtil
import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Control.Monad.Writer (WriterT (..))
import Data.Bifunctor (first, second)
import Data.Configurator ()
import qualified Data.Foldable as Foldable
import qualified Data.Foldable.Extra as Foldable
import qualified Data.List as List
import Data.List.Extra (nubOrd)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as Text
import Data.Tuple.Extra (uncurry3)
import qualified System.Console.Regions as Console.Regions
import System.Environment (withArgs)
import qualified Text.Megaparsec as P
import U.Codebase.HashTags (CausalHash (unCausalHash))
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Util.Hash as Hash
import U.Util.Hash32 (Hash32)
import qualified U.Util.Hash32 as Hash32
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as Builtin
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Builtin.Terms as Builtin
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Codebase (Codebase, Preprocessing (..), PushGitBranchOpts (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchDiff as BranchDiff
import qualified Unison.Codebase.BranchUtil as BranchUtil
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.AuthorInfo (AuthorInfo (..))
import qualified Unison.Codebase.Editor.AuthorInfo as AuthorInfo
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin, ensureAuthenticatedWithCodeserver)
import Unison.Codebase.Editor.HandleInput.MoveBranch (doMoveBranch)
import qualified Unison.Codebase.Editor.HandleInput.NamespaceDependencies as NamespaceDependencies
import Unison.Codebase.Editor.Input
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import qualified Unison.Codebase.Editor.Output.DumpNamespace as Output.DN
import Unison.Codebase.Editor.Output.PushPull (PushPull (Pull, Push))
import qualified Unison.Codebase.Editor.Propagate as Propagate
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    ReadShareRemoteNamespace (..),
    WriteGitRemotePath (..),
    WriteGitRepo,
    WriteRemotePath (..),
    WriteShareRemotePath (..),
    printNamespace,
    writePathToRead,
    writeToReadGit,
  )
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import qualified Unison.Codebase.Editor.Slurp as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.SlurpResult (SlurpResult (..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import qualified Unison.Codebase.Editor.SlurpResult as SlurpResult
import qualified Unison.Codebase.Editor.TodoOutput as TO
import qualified Unison.Codebase.Editor.UriParser as UriParser
import qualified Unison.Codebase.IntegrityCheck as IntegrityCheck (integrityCheckFullCodebase)
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
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.TermEdit (TermEdit (..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TermEdit.Typing as TermEdit
import Unison.Codebase.Type (GitPushBehavior (..))
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Unison.Codebase.Verbosity as Verbosity
import qualified Unison.CommandLine.Completion as Completion
import qualified Unison.CommandLine.DisplayValues as DisplayValues
import qualified Unison.CommandLine.FuzzySelect as Fuzzy
import qualified Unison.CommandLine.InputPattern as InputPattern
import qualified Unison.CommandLine.InputPatterns as IP
import qualified Unison.CommandLine.InputPatterns as InputPatterns
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.DataDeclaration as DD
import Unison.FileParsers (parseAndSynthesizeFile)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import Unison.Names (Names (Names))
import qualified Unison.Names as Names
import Unison.NamesWithHistory (NamesWithHistory (..))
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann (..))
import qualified Unison.Parser.Ann as Ann
import qualified Unison.Parsers as Parsers
import Unison.Position (Position (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Names as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPE
import Unison.Reference (Reference (..), TermReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Result as Result
import Unison.Runtime.IOSource (isTest)
import qualified Unison.Runtime.IOSource as DD
import qualified Unison.Runtime.IOSource as IOSource
import Unison.Server.Backend (ShallowListEntry (..), TermEntry (..), TypeEntry (..))
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.CodebaseServer as Server
import Unison.Server.QueryResult
import Unison.Server.SearchResult (SearchResult)
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Share.Sync as Share
import qualified Unison.Share.Sync.Types as Share
import Unison.Share.Types (codeserverBaseURL)
import qualified Unison.ShortHash as SH
import Unison.Symbol (Symbol)
import qualified Unison.Sync.Types as Share (Path (..), hashJWTHash)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Type.Names as Type
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Typechecker.TypeLookup as TypeLookup
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.Util.Find as Find
import Unison.Util.List (uniqueBy)
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Set as Set
import qualified Unison.Util.Star3 as Star3
import Unison.Util.TransitiveClosure (transitiveClosure)
import Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.WatchKind as WK
import Web.Browser (openBrowser)

prettyPrintEnvDecl :: NamesWithHistory -> Cli r PPE.PrettyPrintEnvDecl
prettyPrintEnvDecl ns = do
  Cli.Env {codebase} <- ask
  liftIO (Codebase.hashLength codebase) <&> (`PPE.fromNamesDecl` ns)

-- | Get a pretty print env decl for the current names at the current path.
currentPrettyPrintEnvDecl :: (Path -> Backend.NameScoping) -> Cli r PPE.PrettyPrintEnvDecl
currentPrettyPrintEnvDecl scoping = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  hqLen <- liftIO (Codebase.hashLength codebase)
  pure $ Backend.getCurrentPrettyNames hqLen (scoping (Path.unabsolute currentPath)) root'

------------------------------------------------------------------------------------------------------------------------
-- Main loop

loop :: Either Event Input -> Cli r ()
loop e = do
  let withFile ::
        -- ambient abilities
        [Type Symbol Ann] ->
        Text ->
        (Text, [L.Token L.Lexeme]) ->
        Cli r (TypecheckedUnisonFile Symbol Ann)
      withFile ambient sourceName lexed@(text, tokens) = do
        let getHQ = \case
              L.WordyId s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
              L.SymbolyId s (Just sh) -> Just (HQ.HashQualified (Name.unsafeFromString s) sh)
              L.Hash sh -> Just (HQ.HashOnly sh)
              _ -> Nothing
            hqs = Set.fromList . mapMaybe (getHQ . L.payload) $ tokens
        rootBranch <- Cli.getRootBranch
        currentPath <- Cli.getCurrentPath
        let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) rootBranch
        State.modify' \loopState ->
          loopState
            & #latestFile .~ Just (Text.unpack sourceName, False)
            & #latestTypecheckedFile .~ Nothing
        MaybeT (WriterT (Identity (r, notes))) <- typecheck ambient parseNames sourceName lexed
        result <- r & onNothing (Cli.returnEarly (ParseErrors text [err | Result.Parsing err <- toList notes]))
        result & onLeft \uf -> do
          ns <- makeShadowedPrintNamesFromHQ hqs (UF.toNames uf)
          ppe <- suffixifiedPPE ns
          let tes = [err | Result.TypeError err <- toList notes]
              cbs =
                [ bug
                  | Result.CompilerBug (Result.TypecheckerBug bug) <-
                      toList notes
                ]
          when (not (null tes)) do
            currentPath <- Cli.getCurrentPath
            Cli.respond (TypeErrors currentPath text ppe tes)
          when (not (null cbs)) do
            Cli.respond (CompilerBugs text ppe cbs)
          Cli.returnEarlyWithoutOutput

      loadUnisonFile :: Text -> Text -> Cli r ()
      loadUnisonFile sourceName text = do
        let lexed = L.lexer (Text.unpack sourceName) (Text.unpack text)
        unisonFile <- withFile [] sourceName (text, lexed)
        currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
        let sr = Slurp.slurpFile unisonFile mempty Slurp.CheckOp currentNames
        names <- displayNames unisonFile
        pped <- prettyPrintEnvDecl names
        let ppe = PPE.suffixifiedPPE pped
        Cli.respond $ Typechecked sourceName ppe sr unisonFile
        (bindings, e) <- evalUnisonFile False ppe unisonFile []
        let e' = Map.map go e
            go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
        when (not (null e')) do
          Cli.respond $ Evaluated text ppe bindings e'
        #latestTypecheckedFile .= (Just unisonFile)

  case e of
    Left (IncomingRootBranch hashes) -> do
      Cli.Env {codebase} <- ask
      sbhLength <- liftIO (Codebase.branchHashLength codebase)
      rootBranch <- Cli.getRootBranch
      Cli.respond $
        WarnIncomingRootBranch
          (SBH.fromHash sbhLength $ Branch.headHash rootBranch)
          (Set.map (SBH.fromHash sbhLength) hashes)
    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      Cli.getLatestFile >>= \case
        Just (_, True) -> (#latestFile . _Just . _2) .= False
        _ -> loadUnisonFile sourceName text
    Right input ->
      let typeReferences :: [SearchResult] -> [Reference]
          typeReferences rs =
            [r | SR.Tp (SR.TypeResult _ r _) <- rs]
          termReferences :: [SearchResult] -> [Reference]
          termReferences rs =
            [r | SR.Tm (SR.TermResult _ (Referent.Ref r) _) <- rs]
          termResults rs = [r | SR.Tm r <- rs]
          typeResults rs = [r | SR.Tp r <- rs]
          doRemoveReplacement :: HQ.HashQualified Name -> Maybe PatchPath -> Bool -> Cli r ()
          doRemoveReplacement from patchPath isTerm = do
            let patchPath' = fromMaybe Cli.defaultPatchPath patchPath
            patch <- Cli.getPatchAt patchPath'
            QueryResult misses allHits <- hqNameQuery [from]
            let tpRefs = Set.fromList $ typeReferences allHits
                tmRefs = Set.fromList $ termReferences allHits
                (hits, opHits) =
                  let tmResults = Set.fromList $ SR.termName <$> termResults allHits
                      tpResults = Set.fromList $ SR.typeName <$> typeResults allHits
                   in case isTerm of
                        True -> (tmResults, tpResults)
                        False -> (tpResults, tmResults)
                go :: Text -> Reference -> Cli r ()
                go description fr = do
                  let termPatch = over Patch.termEdits (R.deleteDom fr) patch
                      typePatch = over Patch.typeEdits (R.deleteDom fr) patch
                  (patchPath'', patchName) <- Cli.resolveSplit' patchPath'
                  -- Save the modified patch
                  stepAtM
                    description
                    ( Path.unabsolute patchPath'',
                      Branch.modifyPatches
                        patchName
                        (const (if isTerm then termPatch else typePatch))
                    )
                  -- Say something
                  Cli.respond Success
            when (Set.null hits) do
              Cli.respond (SearchTermsNotFoundDetailed isTerm misses (Set.toList opHits))
            description <- inputDescription input
            traverse_ (go description) (if isTerm then tmRefs else tpRefs)
          saveAndApplyPatch :: Path -> NameSegment -> Patch -> Cli r ()
          saveAndApplyPatch patchPath'' patchName patch' = do
            description <- inputDescription input
            stepAtM
              (description <> " (1/2)")
              ( patchPath'',
                Branch.modifyPatches patchName (const patch')
              )
            -- Apply the modified patch to the current path
            -- since we might be able to propagate further.
            currentPath <- Cli.getCurrentPath
            void $ propagatePatch description patch' currentPath
            Cli.respond Success
          previewResponse sourceName sr uf = do
            names <- displayNames uf
            ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl names
            Cli.respond $ Typechecked (Text.pack sourceName) ppe sr uf

          delete ::
            ((Path.Absolute, HQ'.HQSegment) -> Cli r (Set Referent)) -> -- compute matching terms
            ((Path.Absolute, HQ'.HQSegment) -> Cli r (Set Reference)) -> -- compute matching types
            Path.HQSplit' ->
            Cli r ()
          delete getTerms getTypes hq' = do
            hq <- Cli.resolveSplit' hq'
            terms <- getTerms hq
            types <- getTypes hq
            when (Set.null terms && Set.null types) (Cli.returnEarly (NameNotFound hq'))
            -- Mitchell: stripping hash seems wrong here...
            resolvedPath <- Path.convert <$> Cli.resolveSplit' (HQ'.toName <$> hq')
            rootNames <- Branch.toNames <$> Cli.getRootBranch0
            let name = Path.unsafeToName (Path.unsplit resolvedPath)
                toRel :: Ord ref => Set ref -> R.Relation Name ref
                toRel = R.fromList . fmap (name,) . toList
                -- these names are relative to the root
                toDelete = Names (toRel terms) (toRel types)
            endangerments <- getEndangeredDependents toDelete rootNames
            if null endangerments
              then do
                let makeDeleteTermNames = map (BranchUtil.makeDeleteTermName resolvedPath) . Set.toList $ terms
                let makeDeleteTypeNames = map (BranchUtil.makeDeleteTypeName resolvedPath) . Set.toList $ types
                before <- Cli.getRootBranch0
                description <- inputDescription input
                stepManyAt description (makeDeleteTermNames ++ makeDeleteTypeNames)
                after <- Cli.getRootBranch0
                (ppe, diff) <- diffHelper before after
                Cli.respondNumbered (ShowDiffAfterDeleteDefinitions ppe diff)
              else do
                ppeDecl <- currentPrettyPrintEnvDecl Backend.Within
                Cli.respondNumbered (CantDeleteDefinitions ppeDecl endangerments)
       in case input of
            ApiI -> do
              Cli.Env {serverBaseUrl} <- ask
              whenJust serverBaseUrl \baseUrl ->
                Cli.respond $
                  PrintMessage $
                    P.lines
                      [ "The API information is as follows:",
                        P.newline,
                        P.indentN 2 (P.hiBlue ("UI: " <> fromString (Server.urlFor Server.UI baseUrl))),
                        P.newline,
                        P.indentN 2 (P.hiBlue ("API: " <> fromString (Server.urlFor Server.Api baseUrl)))
                      ]
            CreateMessage pretty ->
              Cli.respond $ PrintMessage pretty
            ShowReflogI -> do
              Cli.Env {codebase} <- ask
              sbhLength <- liftIO (Codebase.branchHashLength codebase)
              entries <- convertEntries sbhLength Nothing [] <$> liftIO (Codebase.getReflog codebase)
              #numberedArgs .= (fmap (('#' :) . SBH.toString . Output.hash) entries)
              Cli.respond $ ShowReflog entries
              where
                -- reverses & formats entries, adds synthetic entries when there is a
                -- discontinuity in the reflog.
                convertEntries ::
                  Int ->
                  Maybe Branch.CausalHash ->
                  [Output.ReflogEntry] ->
                  [Reflog.Entry Branch.CausalHash] ->
                  [Output.ReflogEntry]
                convertEntries _ _ acc [] = acc
                convertEntries sbhLength Nothing acc entries@(Reflog.Entry old _ _ : _) =
                  convertEntries
                    sbhLength
                    (Just old)
                    (Output.ReflogEntry (SBH.fromHash sbhLength old) "(initial reflogged namespace)" : acc)
                    entries
                convertEntries sbhLength (Just lastHash) acc entries@(Reflog.Entry old new reason : rest) =
                  if lastHash /= old
                    then
                      convertEntries
                        sbhLength
                        (Just old)
                        (Output.ReflogEntry (SBH.fromHash sbhLength old) "(external change)" : acc)
                        entries
                    else
                      convertEntries
                        sbhLength
                        (Just new)
                        (Output.ReflogEntry (SBH.fromHash sbhLength new) reason : acc)
                        rest
            ResetRootI src0 ->
              Cli.time "reset-root" do
                newRoot <-
                  case src0 of
                    Left hash -> Cli.resolveShortBranchHash hash
                    Right path' -> Cli.expectBranchAtPath' path'
                description <- inputDescription input
                Cli.updateRoot newRoot description
                Cli.respond Success
            ForkLocalBranchI src0 dest0 -> do
              srcb <-
                case src0 of
                  Left hash -> Cli.resolveShortBranchHash hash
                  Right path' -> Cli.expectBranchAtPath' path'
              Cli.assertNoBranchAtPath' dest0
              description <- inputDescription input
              dest <- Cli.resolvePath' dest0
              ok <- Cli.updateAtM description dest (const $ pure srcb)
              Cli.respond if ok then Success else BranchEmpty src0
            MergeLocalBranchI src0 dest0 mergeMode -> do
              description <- inputDescription input
              srcb <- Cli.expectBranchAtPath' src0
              dest <- Cli.resolvePath' dest0
              let err = Just $ MergeAlreadyUpToDate src0 dest0
              mergeBranchAndPropagateDefaultPatch mergeMode description err srcb (Just dest0) dest
            PreviewMergeLocalBranchI src0 dest0 -> do
              Cli.Env {codebase} <- ask
              srcb <- Cli.expectBranchAtPath' src0
              dest <- Cli.resolvePath' dest0
              destb <- Cli.getBranchAt dest
              merged <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge srcb destb)
              if merged == destb
                then Cli.respond (PreviewMergeAlreadyUpToDate src0 dest0)
                else do
                  (ppe, diff) <- diffHelper (Branch.head destb) (Branch.head merged)
                  Cli.respondNumbered (ShowDiffAfterMergePreview dest0 dest ppe diff)
            DiffNamespaceI before after -> do
              absBefore <- traverseOf _Right Cli.resolvePath' before
              absAfter <- traverseOf _Right Cli.resolvePath' after
              beforeBranch0 <- Branch.head <$> Cli.resolveAbsBranchId absBefore
              afterBranch0 <- Branch.head <$> Cli.resolveAbsBranchId absAfter
              case (Branch.isEmpty0 beforeBranch0, Branch.isEmpty0 afterBranch0) of
                (True, True) -> Cli.returnEarly . NamespaceEmpty $ (absBefore Nel.:| [absAfter])
                (True, False) -> Cli.returnEarly . NamespaceEmpty $ (absBefore Nel.:| [])
                (False, True) -> Cli.returnEarly . NamespaceEmpty $ (absAfter Nel.:| [])
                (False, False) -> pure ()
              (ppe, diff) <- diffHelper beforeBranch0 afterBranch0
              Cli.respondNumbered (ShowDiffNamespace absBefore absAfter ppe diff)
            CreatePullRequestI baseRepo headRepo -> handleCreatePullRequest baseRepo headRepo
            LoadPullRequestI baseRepo headRepo dest0 -> do
              Cli.assertNoBranchAtPath' dest0
              Cli.Env {codebase} <- ask
              description <- inputDescription input
              destAbs <- Cli.resolvePath' dest0
              let getBranch = \case
                    ReadRemoteNamespaceGit repo ->
                      Cli.ioE (Codebase.importRemoteBranch codebase repo SyncMode.ShortCircuit Unmodified) \err ->
                        Cli.returnEarly (Output.GitError err)
                    ReadRemoteNamespaceShare repo -> importRemoteShareBranch repo
              baseb <- getBranch baseRepo
              headb <- getBranch headRepo
              mergedb <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge baseb headb)
              squashedb <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.SquashMerge headb baseb)
              -- Perform all child updates in a single step.
              Cli.updateAt description destAbs $ Branch.step \destBranch0 ->
                destBranch0 & Branch.children
                  %~ ( \childMap ->
                         childMap & at "base" ?~ baseb
                           & at "head" ?~ headb
                           & at "merged" ?~ mergedb
                           & at "squashed" ?~ squashedb
                     )
              let base = snoc dest0 "base"
                  head = snoc dest0 "head"
                  merged = snoc dest0 "merged"
                  squashed = snoc dest0 "squashed"
              Cli.respond $ LoadPullRequest baseRepo headRepo base head merged squashed
              loadPropagateDiffDefaultPatch
                description
                (Just merged)
                (snoc destAbs "merged")
            MoveBranchI src' dest' -> do
              hasConfirmed <- confirmedCommand input
              description <- inputDescription input
              doMoveBranch description hasConfirmed src' dest'
            MovePatchI src' dest' -> do
              description <- inputDescription input
              p <- Cli.expectPatchAt src'
              Cli.assertNoPatchAt dest'
              src <- Cli.resolveSplit' src'
              dest <- Cli.resolveSplit' dest'
              stepManyAt
                description
                [ BranchUtil.makeDeletePatch (Path.convert src),
                  BranchUtil.makeReplacePatch (Path.convert dest) p
                ]
              Cli.respond Success
            CopyPatchI src dest' -> do
              description <- inputDescription input
              p <- Cli.expectPatchAt src
              Cli.assertNoPatchAt dest'
              dest <- Cli.resolveSplit' dest'
              stepAt
                description
                (BranchUtil.makeReplacePatch (Path.convert dest) p)
              Cli.respond Success
            DeletePatchI src' -> do
              _ <- Cli.expectPatchAt src'
              description <- inputDescription input
              src <- Cli.resolveSplit' src'
              stepAt
                description
                (BranchUtil.makeDeletePatch (Path.convert src))
              Cli.respond Success
            DeleteBranchI insistence Nothing -> do
              hasConfirmed <- confirmedCommand input
              if hasConfirmed || insistence == Force
                then do
                  description <- inputDescription input
                  stepAt
                    description
                    (Path.empty, const Branch.empty0)
                  Cli.respond DeletedEverything
                else Cli.respond DeleteEverythingConfirmation
            DeleteBranchI insistence (Just p) -> do
              branch <- Cli.expectBranchAtPath' (Path.unsplit' p)
              description <- inputDescription input
              absPath <- Cli.resolveSplit' p
              let toDelete =
                    Names.prefix0
                      (Path.unsafeToName (Path.unsplit (Path.convert absPath)))
                      (Branch.toNames (Branch.head branch))
              afterDelete <- do
                rootNames <- Branch.toNames <$> Cli.getRootBranch0
                endangerments <- getEndangeredDependents toDelete rootNames
                case (null endangerments, insistence) of
                  (True, _) -> pure (Cli.respond Success)
                  (False, Force) -> do
                    ppeDecl <- currentPrettyPrintEnvDecl Backend.Within
                    pure do
                      Cli.respond Success
                      Cli.respondNumbered $ DeletedDespiteDependents ppeDecl endangerments
                  (False, Try) -> do
                    ppeDecl <- currentPrettyPrintEnvDecl Backend.Within
                    Cli.respondNumbered $ CantDeleteNamespace ppeDecl endangerments
                    Cli.returnEarlyWithoutOutput
              stepAt description $
                BranchUtil.makeDeleteBranch (Path.convert absPath)
              afterDelete
            SwitchBranchI maybePath' -> do
              path' <-
                maybePath' & onNothing do
                  root0 <- Cli.getRootBranch0
                  fuzzySelectNamespace Absolute root0 >>= \case
                    -- Shouldn't be possible to get multiple paths here, we can just take
                    -- the first.
                    Just (p : _) -> pure p
                    _ -> Cli.returnEarly (HelpMessage InputPatterns.cd)
              path <- Cli.resolvePath' path'
              #currentPathStack %= Nel.cons path
              branch' <- Cli.getBranch0At path
              when (Branch.isEmpty0 branch') (Cli.respond $ CreatedNewBranch path)
            UpI -> do
              path0 <- Cli.getCurrentPath
              whenJust (unsnoc path0) \(path, _) ->
                #currentPathStack %= Nel.cons path
            PopBranchI -> do
              loopState <- State.get
              case Nel.uncons (loopState ^. #currentPathStack) of
                (_, Nothing) -> Cli.respond StartOfCurrentPathHistory
                (_, Just paths) -> State.put $! (loopState & #currentPathStack .~ paths)
            HistoryI resultsCap diffCap from -> do
              branch <-
                case from of
                  Left hash -> Cli.resolveShortBranchHash hash
                  Right path' -> do
                    path <- Cli.resolvePath' path'
                    Cli.getMaybeBranchAt path & onNothingM (Cli.returnEarly (CreatedNewBranch path))
              Cli.Env {codebase} <- ask
              sbhLength <- liftIO (Codebase.branchHashLength codebase)
              history <- liftIO (doHistory sbhLength 0 branch [])
              Cli.respondNumbered history
              where
                doHistory :: Int -> Int -> Branch IO -> [(Causal.CausalHash, NamesWithHistory.Diff)] -> IO NumberedOutput
                doHistory sbhLength !n b acc =
                  if maybe False (n >=) resultsCap
                    then pure (History diffCap sbhLength acc (PageEnd (Branch.headHash b) n))
                    else case Branch._history b of
                      Causal.One {} -> pure (History diffCap sbhLength acc (EndOfLog $ Branch.headHash b))
                      Causal.Merge _ _ _ tails ->
                        pure (History diffCap sbhLength acc (MergeTail (Branch.headHash b) $ Map.keys tails))
                      Causal.Cons _ _ _ tail -> do
                        b' <- fmap Branch.Branch $ snd tail
                        let elem = (Branch.headHash b, Branch.namesDiff b' b)
                        doHistory sbhLength (n + 1) b' (elem : acc)
            UndoI -> do
              rootBranch <- Cli.getRootBranch
              (_, prev) <-
                liftIO (Branch.uncons rootBranch) & onNothingM do
                  Cli.returnEarly . CantUndo $
                    if Branch.isOne rootBranch
                      then CantUndoPastStart
                      else CantUndoPastMerge
              description <- inputDescription input
              Cli.updateRoot prev description
              (ppe, diff) <- diffHelper (Branch.head prev) (Branch.head rootBranch)
              Cli.respondNumbered (Output.ShowDiffAfterUndo ppe diff)
            UiI -> do
              Cli.Env {serverBaseUrl} <- ask
              whenJust serverBaseUrl \url -> do
                _success <- liftIO (openBrowser (Server.urlFor Server.UI url))
                pure ()
            DocsToHtmlI namespacePath' sourceDirectory -> do
              Cli.Env {codebase, sandboxedRuntime} <- ask
              rootBranch <- Cli.getRootBranch
              absPath <- Path.unabsolute <$> Cli.resolvePath' namespacePath'
              liftIO (Backend.docsInBranchToHtmlFiles sandboxedRuntime codebase rootBranch absPath sourceDirectory)
            AliasTermI src' dest' -> do
              Cli.Env {codebase} <- ask
              src <- traverseOf _Right Cli.resolveSplit' src'
              srcTerms <-
                either
                  (liftIO . Backend.termReferentsByShortHash codebase)
                  Cli.getTermsAt
                  src
              srcTerm <-
                Set.asSingleton srcTerms & onNothing do
                  Cli.returnEarly =<< case (Set.null srcTerms, src') of
                    (True, Left hash) -> pure (TermNotFound' hash)
                    (True, Right name) -> pure (TermNotFound name)
                    (False, Left hash) -> pure (HashAmbiguous hash srcTerms)
                    (False, Right name) -> do
                      Cli.Env {codebase} <- ask
                      hqLength <- liftIO (Codebase.hashLength codebase)
                      pure (DeleteNameAmbiguous hqLength name srcTerms Set.empty)
              dest <- Cli.resolveSplit' dest'
              destTerms <- Cli.getTermsAt (Path.convert dest)
              when (not (Set.null destTerms)) do
                Cli.returnEarly (TermAlreadyExists dest' destTerms)
              description <- inputDescription input
              srcMetadata <-
                case src of
                  Left _ -> pure Metadata.empty
                  Right (path, _) -> do
                    root0 <- Cli.getRootBranch0
                    pure (BranchUtil.getTermMetadataAt (Path.convert path, ()) srcTerm root0)
              stepAt
                description
                (BranchUtil.makeAddTermName (Path.convert dest) srcTerm srcMetadata)
              Cli.respond Success
            AliasTypeI src' dest' -> do
              Cli.Env {codebase} <- ask
              src <- traverseOf _Right Cli.resolveSplit' src'
              srcTypes <-
                either
                  (liftIO . Backend.typeReferencesByShortHash codebase)
                  Cli.getTypesAt
                  src
              srcType <-
                Set.asSingleton srcTypes & onNothing do
                  Cli.returnEarly =<< case (Set.null srcTypes, src') of
                    (True, Left hash) -> pure (TypeNotFound' hash)
                    (True, Right name) -> pure (TypeNotFound name)
                    (False, Left hash) -> pure (HashAmbiguous hash (Set.map Referent.Ref srcTypes))
                    (False, Right name) -> do
                      Cli.Env {codebase} <- ask
                      hqLength <- liftIO (Codebase.hashLength codebase)
                      pure (DeleteNameAmbiguous hqLength name Set.empty srcTypes)
              dest <- Cli.resolveSplit' dest'
              destTypes <- Cli.getTypesAt (Path.convert dest)
              when (not (Set.null destTypes)) do
                Cli.returnEarly (TypeAlreadyExists dest' destTypes)
              description <- inputDescription input
              srcMetadata <-
                case src of
                  Left _ -> pure Metadata.empty
                  Right (path, _) -> do
                    root0 <- Cli.getRootBranch0
                    pure (BranchUtil.getTypeMetadataAt (Path.convert path, ()) srcType root0)
              stepAt
                description
                (BranchUtil.makeAddTypeName (Path.convert dest) srcType srcMetadata)
              Cli.respond Success

            -- this implementation will happily produce name conflicts,
            -- but will surface them in a normal diff at the end of the operation.
            AliasManyI srcs dest' -> do
              root0 <- Cli.getRootBranch0
              currentBranch0 <- Cli.getCurrentBranch0
              destAbs <- Cli.resolvePath' dest'
              old <- Cli.getBranch0At destAbs
              description <- inputDescription input
              let (unknown, actions) = foldl' (go root0 currentBranch0 destAbs) mempty srcs
              stepManyAt description actions
              new <- Cli.getBranch0At destAbs
              (ppe, diff) <- diffHelper old new
              Cli.respondNumbered (ShowDiffAfterModifyBranch dest' destAbs ppe diff)
              when (not (null unknown)) do
                Cli.respond . SearchTermsNotFound . fmap fixupOutput $ unknown
              where
                -- a list of missing sources (if any) and the actions that do the work
                go ::
                  Branch0 IO ->
                  Branch0 IO ->
                  Path.Absolute ->
                  ([Path.HQSplit], [(Path, Branch0 m -> Branch0 m)]) ->
                  Path.HQSplit ->
                  ([Path.HQSplit], [(Path, Branch0 m -> Branch0 m)])
                go root0 currentBranch0 dest (missingSrcs, actions) hqsrc =
                  let src :: Path.Split
                      src = second HQ'.toName hqsrc
                      proposedDest :: Path.Split
                      proposedDest = second HQ'.toName hqProposedDest
                      hqProposedDest :: Path.HQSplit
                      hqProposedDest = first Path.unabsolute $ Path.resolve dest hqsrc
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
                fixupOutput = fmap Path.unsafeToName . HQ'.toHQ . Path.unsplitHQ
            NamesI global query -> do
              Cli.Env {codebase} <- ask
              currentPath' <- Path.unabsolute <$> Cli.getCurrentPath
              hqLength <- liftIO (Codebase.hashLength codebase)
              root <- Cli.getRootBranch
              (names, pped) <-
                if global || any Name.isAbsolute query
                  then do
                    let root0 = Branch.head root
                    let names = NamesWithHistory.fromCurrentNames . Names.makeAbsolute $ Branch.toNames root0
                    -- Use an absolutely qualified ppe for view.global
                    let pped = PPE.fromNamesDecl hqLength names
                    pure (names, pped)
                  else do
                    currentBranch <- Cli.getCurrentBranch0
                    let currentNames = NamesWithHistory.fromCurrentNames $ Branch.toNames currentBranch
                    let pped = Backend.getCurrentPrettyNames hqLength (Backend.Within currentPath') root
                    pure (currentNames, pped)

              let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
                  terms = NamesWithHistory.lookupHQTerm query names
                  types = NamesWithHistory.lookupHQType query names
                  terms' :: Set (Referent, Set (HQ'.HashQualified Name))
                  terms' = Set.map go terms
                    where
                      go r = (r, Set.fromList $ PPE.allTermNames unsuffixifiedPPE r)
                  types' :: Set (Reference, Set (HQ'.HashQualified Name))
                  types' = Set.map go types
                    where
                      go r = (r, Set.fromList $ PPE.allTypeNames unsuffixifiedPPE r)
              Cli.respond $ ListNames global hqLength (toList types') (toList terms')
            LinkI mdValue srcs -> do
              description <- inputDescription input
              manageLinks False srcs [mdValue] Metadata.insert
              syncRoot description
            UnlinkI mdValue srcs -> do
              description <- inputDescription input
              manageLinks False srcs [mdValue] Metadata.delete
              syncRoot description

            -- > links List.map (.Docs .English)
            -- > links List.map -- give me all the
            -- > links Optional License
            LinksI src mdTypeStr -> do
              (ppe, out) <- getLinks (show input) src (Right mdTypeStr)
              #numberedArgs .= fmap (HQ.toString . view _1) out
              let biasedPPE = (PPE.biasTo (maybeToList . Path.toName' . HQ'.toName $ Path.unsplitHQ' src) ppe)
              Cli.respond $ ListOfLinks biasedPPE out
            DocsI srcs -> do
              currentBranch0 <- Cli.getCurrentBranch0
              basicPrettyPrintNames <- getBasicPrettyPrintNames
              srcs' <- case srcs of
                [] -> do
                  defs <-
                    fuzzySelectDefinition Absolute currentBranch0 & onNothingM do
                      Cli.returnEarly (HelpMessage InputPatterns.docs)
                  -- HQ names should always parse as a valid split, so we just discard any
                  -- that don't to satisfy the type-checker.
                  pure . mapMaybe (eitherToMaybe . Path.parseHQSplit' . HQ.toString) $ defs
                xs -> pure xs
              for_ srcs' (docsI (show input) basicPrettyPrintNames)
            CreateAuthorI authorNameSegment authorFullName -> do
              Cli.Env {codebase} <- ask
              initialBranch <- Cli.getCurrentBranch
              AuthorInfo
                guid@(guidRef, _, _)
                author@(authorRef, _, _)
                copyrightHolder@(copyrightHolderRef, _, _) <-
                AuthorInfo.createAuthorInfo Ann.External authorFullName
              description <- inputDescription input
              -- add the new definitions to the codebase and to the namespace
              traverse_ (liftIO . uncurry3 (Codebase.putTerm codebase)) [guid, author, copyrightHolder]
              authorPath <- Cli.resolveSplit' authorPath'
              copyrightHolderPath <- Cli.resolveSplit' (base |> "copyrightHolders" |> authorNameSegment)
              guidPath <- Cli.resolveSplit' (authorPath' |> "guid")
              stepManyAt
                description
                [ BranchUtil.makeAddTermName (Path.convert authorPath) (d authorRef) mempty,
                  BranchUtil.makeAddTermName (Path.convert copyrightHolderPath) (d copyrightHolderRef) mempty,
                  BranchUtil.makeAddTermName (Path.convert guidPath) (d guidRef) mempty
                ]
              currentPath <- Cli.getCurrentPath
              finalBranch <- Cli.getCurrentBranch0
              (ppe, diff) <- diffHelper (Branch.head initialBranch) finalBranch
              Cli.respondNumbered $
                ShowDiffAfterCreateAuthor
                  authorNameSegment
                  (Path.unsplit' base)
                  currentPath
                  ppe
                  diff
              where
                d :: Reference.Id -> Referent
                d = Referent.Ref . Reference.DerivedId
                base :: Path.Split' = (Path.relativeEmpty', "metadata")
                authorPath' = base |> "authors" |> authorNameSegment
            MoveTermI src' dest' -> do
              src <- Cli.resolveSplit' src'
              srcTerms <- Cli.getTermsAt src
              srcTerm <-
                Set.asSingleton srcTerms & onNothing do
                  if Set.null srcTerms
                    then Cli.returnEarly (TermNotFound src')
                    else do
                      Cli.Env {codebase} <- ask
                      hqLength <- liftIO (Codebase.hashLength codebase)
                      Cli.returnEarly (DeleteNameAmbiguous hqLength src' srcTerms Set.empty)
              dest <- Cli.resolveSplit' dest'
              destTerms <- Cli.getTermsAt (Path.convert dest)
              when (not (Set.null destTerms)) do
                Cli.returnEarly (TermAlreadyExists dest' destTerms)
              description <- inputDescription input
              let p = Path.convert src
              srcMetadata <- do
                root0 <- Cli.getRootBranch0
                pure (BranchUtil.getTermMetadataAt p srcTerm root0)
              stepManyAt
                description
                [ -- Mitchell: throwing away any hash-qualification here seems wrong!
                  BranchUtil.makeDeleteTermName (over _2 HQ'.toName p) srcTerm,
                  BranchUtil.makeAddTermName (Path.convert dest) srcTerm srcMetadata
                ]
              Cli.respond Success
            MoveTypeI src' dest' -> do
              src <- Cli.resolveSplit' src'
              srcTypes <- Cli.getTypesAt src
              srcType <-
                Set.asSingleton srcTypes & onNothing do
                  if Set.null srcTypes
                    then Cli.returnEarly (TypeNotFound src')
                    else do
                      Cli.Env {codebase} <- ask
                      hqLength <- liftIO (Codebase.hashLength codebase)
                      Cli.returnEarly (DeleteNameAmbiguous hqLength src' Set.empty srcTypes)
              dest <- Cli.resolveSplit' dest'
              destTypes <- Cli.getTypesAt (Path.convert dest)
              when (not (Set.null destTypes)) do
                Cli.returnEarly (TypeAlreadyExists dest' destTypes)
              description <- inputDescription input
              let p = Path.convert src
              srcMetadata <- do
                root0 <- Cli.getRootBranch0
                pure (BranchUtil.getTypeMetadataAt p srcType root0)
              stepManyAt
                description
                [ -- Mitchell: throwing away any hash-qualification here seems wrong!
                  BranchUtil.makeDeleteTypeName (over _2 HQ'.toName p) srcType,
                  BranchUtil.makeAddTypeName (Path.convert dest) srcType srcMetadata
                ]
              Cli.respond Success
            DeleteI hq -> delete Cli.getTermsAt Cli.getTypesAt hq
            DeleteTypeI hq -> delete (const (pure Set.empty)) Cli.getTypesAt hq
            DeleteTermI hq -> delete Cli.getTermsAt (const (pure Set.empty)) hq
            DisplayI outputLoc names' -> do
              currentBranch0 <- Cli.getCurrentBranch0
              basicPrettyPrintNames <- getBasicPrettyPrintNames
              names <- case names' of
                [] ->
                  fuzzySelectDefinition Absolute currentBranch0 & onNothingM do
                    Cli.returnEarly (HelpMessage InputPatterns.display)
                ns -> pure ns
              traverse_ (displayI basicPrettyPrintNames outputLoc) names
            ShowDefinitionI outputLoc showDefinitionScope query -> handleShowDefinition outputLoc showDefinitionScope query
            FindPatchI -> do
              branch <- Cli.getCurrentBranch0
              let patches =
                    [ Path.unsafeToName $ Path.snoc p seg
                      | (p, b) <- Branch.toList0 branch,
                        (seg, _) <- Map.toList (Branch._edits b)
                    ]
              Cli.respond $ ListOfPatches $ Set.fromList patches
              #numberedArgs .= fmap Name.toString patches
            FindShallowI pathArg -> do
              Cli.Env {codebase} <- ask
              sbhLength <- liftIO (Codebase.branchHashLength codebase)
              rootBranch <- Cli.getRootBranch

              pathArgAbs <- Cli.resolvePath' pathArg
              entries <- liftIO (Backend.findShallow codebase pathArgAbs)
              -- caching the result as an absolute path, for easier jumping around
              #numberedArgs .= fmap entryToHQString entries
              let ppe =
                    Backend.basicSuffixifiedNames
                      sbhLength
                      rootBranch
                      (Backend.AllNames (Path.unabsolute pathArgAbs))
              Cli.respond $ ListShallow ppe entries
              where
                entryToHQString :: ShallowListEntry v Ann -> String
                entryToHQString e =
                  fixup case e of
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
            FindI isVerbose fscope ws -> handleFindI isVerbose fscope ws input
            ResolveTypeNameI path' -> do
              description <- inputDescription input
              path <- Cli.resolveSplit' path'
              ty <- do
                types <- Cli.getTypesAt path
                Set.asSingleton types & onNothing do
                  if Set.null types
                    then Cli.returnEarly (TypeNotFound path')
                    else do
                      Cli.Env {codebase} <- ask
                      hqLength <- liftIO (Codebase.hashLength codebase)
                      Cli.returnEarly (DeleteNameAmbiguous hqLength path' Set.empty types)
              stepAt
                description
                -- Mitchell: throwing away HQ seems wrong
                (BranchUtil.makeDeleteTypeName (Path.convert (over _2 HQ'.toName path)) ty)
            ResolveTermNameI path' -> do
              path <- Cli.resolveSplit' path'
              term <- do
                rootBranch0 <- Cli.getRootBranch0
                terms <- getTermsIncludingHistorical (Path.convert path) rootBranch0
                Set.asSingleton terms
                  & onNothing
                    if Set.null terms
                      then Cli.returnEarly (TermNotFound path')
                      else do
                        Cli.Env {codebase} <- ask
                        hqLength <- liftIO (Codebase.hashLength codebase)
                        Cli.returnEarly (DeleteNameAmbiguous hqLength path' terms Set.empty)
              description <- inputDescription input
              terms <- Cli.getTermsAt path
              terms
                & Set.delete term
                & Set.toList
                -- Mitchell: throwing away HQ seems wrong
                & map (BranchUtil.makeDeleteTermName (Path.convert (over _2 HQ'.toName path)))
                & stepManyAt description
            ReplaceI from to patchPath -> do
              Cli.Env {codebase} <- ask
              hqLength <- liftIO (Codebase.hashLength codebase)

              let patchPath' = fromMaybe Cli.defaultPatchPath patchPath
              patch <- Cli.getPatchAt patchPath'
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
                  termFromMisses = fromMisses' <> (SR.typeName <$> typeResults fromHits)
                  termToMisses = toMisses' <> (SR.typeName <$> typeResults toHits)
                  -- Term hits are type misses
                  typeFromMisses = fromMisses' <> (SR.termName <$> termResults fromHits)
                  typeToMisses = toMisses' <> (SR.termName <$> termResults toHits)

                  termMisses = termFromMisses <> termToMisses
                  typeMisses = typeFromMisses <> typeToMisses

                  replaceTerms :: Reference -> Reference -> Cli r ()
                  replaceTerms fr tr = do
                    mft <- liftIO (Codebase.getTypeOfTerm codebase fr)
                    mtt <- liftIO (Codebase.getTypeOfTerm codebase tr)
                    let termNotFound =
                          Cli.returnEarly
                            . TermNotFound'
                            . SH.take hqLength
                            . Reference.toShortHash
                    ft <- mft & onNothing (termNotFound fr)
                    tt <- mtt & onNothing (termNotFound tr)
                    let patch' =
                          -- The modified patch
                          over
                            Patch.termEdits
                            ( R.insert fr (Replace tr (TermEdit.typing tt ft))
                                . R.deleteDom fr
                            )
                            patch
                    (patchPath'', patchName) <- Cli.resolveSplit' patchPath'
                    saveAndApplyPatch (Path.convert patchPath'') patchName patch'

                  replaceTypes :: Reference -> Reference -> Cli r ()
                  replaceTypes fr tr = do
                    let patch' =
                          -- The modified patch
                          over
                            Patch.typeEdits
                            (R.insert fr (TypeEdit.Replace tr) . R.deleteDom fr)
                            patch
                    (patchPath'', patchName) <- Cli.resolveSplit' patchPath'
                    saveAndApplyPatch (Path.convert patchPath'') patchName patch'

                  ambiguous :: HQ.HashQualified Name -> [TermReference] -> Cli r a
                  ambiguous t rs =
                    Cli.returnEarly case t of
                      HQ.HashOnly h -> HashAmbiguous h rs'
                      (Path.parseHQSplit' . HQ.toString -> Right n) -> DeleteNameAmbiguous hqLength n rs' Set.empty
                      _ -> BadName (HQ.toString t)
                    where
                      rs' = Set.map Referent.Ref $ Set.fromList rs

                  mismatch typeName termName = Cli.respond $ TypeTermMismatch typeName termName

              case (termsFromRefs, termsToRefs, typesFromRefs, typesToRefs) of
                ([], [], [], []) -> Cli.respond $ SearchTermsNotFound termMisses
                ([_], [], [], [_]) -> mismatch to from
                ([], [_], [_], []) -> mismatch from to
                ([_], [], _, _) -> Cli.respond $ SearchTermsNotFound termMisses
                ([], [_], _, _) -> Cli.respond $ SearchTermsNotFound termMisses
                (_, _, [_], []) -> Cli.respond $ SearchTermsNotFound typeMisses
                (_, _, [], [_]) -> Cli.respond $ SearchTermsNotFound typeMisses
                ([fr], [tr], [], []) -> replaceTerms fr tr
                ([], [], [fr], [tr]) -> replaceTypes fr tr
                (froms, [_], [], []) -> ambiguous from froms
                ([], [], froms, [_]) -> ambiguous from froms
                ([_], tos, [], []) -> ambiguous to tos
                ([], [], [_], tos) -> ambiguous to tos
                (_, _, _, _) -> error "unpossible"
            LoadI maybePath -> do
              latestFile <- Cli.getLatestFile
              path <- (maybePath <|> fst <$> latestFile) & onNothing (Cli.returnEarly NoUnisonFile)
              Cli.Env {loadSource} <- ask
              contents <-
                liftIO (loadSource (Text.pack path)) >>= \case
                  Cli.InvalidSourceNameError -> Cli.returnEarly $ InvalidSourceName path
                  Cli.LoadError -> Cli.returnEarly $ SourceLoadFailed path
                  Cli.LoadSuccess contents -> pure contents
              loadUnisonFile (Text.pack path) contents
            AddI requestedNames -> do
              description <- inputDescription input
              let vars = Set.map Name.toVar requestedNames
              uf <- Cli.expectLatestTypecheckedFile
              Cli.Env {codebase} <- ask
              currentPath <- Cli.getCurrentPath
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.AddOp currentNames
              let adds = SlurpResult.adds sr
              stepAtNoSync (Path.unabsolute currentPath, doSlurpAdds adds uf)
              liftIO . Codebase.addDefsToCodebase codebase . filterBySlurpResult sr $ uf
              ppe <- prettyPrintEnvDecl =<< displayNames uf
              Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
              addDefaultMetadata adds
              syncRoot description
            SaveExecuteResultI resultName -> do
              description <- inputDescription input
              let resultVar = Name.toVar resultName
              uf <- addSavedTermToUnisonFile resultName
              Cli.Env {codebase} <- ask
              currentPath <- Cli.getCurrentPath
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf (Set.singleton resultVar) Slurp.AddOp currentNames
              let adds = SlurpResult.adds sr
              stepAtNoSync (Path.unabsolute currentPath, doSlurpAdds adds uf)
              liftIO . Codebase.addDefsToCodebase codebase . filterBySlurpResult sr $ uf
              ppe <- prettyPrintEnvDecl =<< displayNames uf
              Cli.returnEarly $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
              addDefaultMetadata adds
              syncRoot description
            PreviewAddI requestedNames -> do
              (sourceName, _) <- Cli.expectLatestFile
              uf <- Cli.expectLatestTypecheckedFile
              let vars = Set.map Name.toVar requestedNames
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.AddOp currentNames
              previewResponse sourceName sr uf
            UpdateI optionalPatch requestedNames -> handleUpdate input optionalPatch requestedNames
            PreviewUpdateI requestedNames -> do
              (sourceName, _) <- Cli.expectLatestFile
              uf <- Cli.expectLatestTypecheckedFile
              let vars = Set.map Name.toVar requestedNames
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.UpdateOp currentNames
              previewResponse sourceName sr uf
            TodoI patchPath branchPath' -> do
              patch <- Cli.getPatchAt (fromMaybe Cli.defaultPatchPath patchPath)
              branchPath <- Cli.resolvePath' branchPath'
              doShowTodoOutput patch branchPath
            TestI testInput -> handleTest testInput
            PropagatePatchI patchPath scopePath' -> do
              description <- inputDescription input
              patch <- Cli.getPatchAt patchPath
              scopePath <- Cli.resolvePath' scopePath'
              updated <- propagatePatch description patch scopePath
              when (not updated) (Cli.respond $ NothingToPatch patchPath scopePath')
            ExecuteI main args -> do
              (unisonFile, mainResType) <- do
                (sym, term, typ, otyp) <- getTerm main
                uf <- createWatcherFile sym term typ
                pure (uf, otyp)
              ppe <- executePPE unisonFile
              (_, xs) <- evalUnisonFile False ppe unisonFile args
              mainRes :: Term Symbol () <-
                let bonk (_, (_ann, watchKind, _id, _term0, term1, _isCacheHit)) = (watchKind, term1)
                 in case lookup magicMainWatcherString (map bonk (Map.toList xs)) of
                      Nothing ->
                        error
                          ( "impossible: we manually added the watcher "
                              <> show magicMainWatcherString
                              <> " with 'createWatcherFile', but it isn't here."
                          )
                      Just x -> pure (stripUnisonFileReferences unisonFile x)
              #lastRunResult .= Just (Term.amap (\() -> External) mainRes, mainResType, unisonFile)
              Cli.respond (RunResult ppe mainRes)
            MakeStandaloneI output main -> do
              Cli.Env {codebase, runtime} <- ask
              let mainType = Runtime.mainType runtime
              parseNames <-
                flip NamesWithHistory.NamesWithHistory mempty <$> basicPrettyPrintNamesA
              ppe <- suffixifiedPPE parseNames
              let resolved = toList $ NamesWithHistory.lookupHQTerm main parseNames
                  smain = HQ.toString main
              filtered <-
                catMaybes
                  <$> traverse (\r -> fmap (r,) <$> liftIO (loadTypeOfTerm codebase r)) resolved
              case filtered of
                [(Referent.Ref ref, ty)]
                  | Typechecker.fitsScheme ty mainType -> do
                      let codeLookup = () <$ Codebase.toCodeLookup codebase
                      whenJustM (liftIO (Runtime.compileTo runtime codeLookup ppe ref (output <> ".uc"))) \err ->
                        Cli.returnEarly (EvaluationFailure err)
                  | otherwise -> Cli.returnEarly (BadMainFunction smain ty ppe [mainType])
                _ -> Cli.returnEarly (NoMainFunction smain ppe [mainType])
            IOTestI main -> do
              Cli.Env {codebase, runtime} <- ask
              -- todo - allow this to run tests from scratch file, using addRunMain
              let testType = Runtime.ioTestType runtime
              parseNames <- (`NamesWithHistory.NamesWithHistory` mempty) <$> basicParseNames
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
              ref <- do
                let noMain = Cli.returnEarly $ NoMainFunction (HQ.toString main) ppe [testType]
                case toList results of
                  [Referent.Ref ref] -> do
                    liftIO (loadTypeOfTerm codebase (Referent.Ref ref)) >>= \case
                      Just typ | Typechecker.isSubtype typ testType -> pure ref
                      _ -> noMain
                  _ -> noMain
              let a = ABT.annotation tm
                  tm = DD.forceTerm a a (Term.ref a ref)
              -- Don't cache IO tests
              tm' <- evalUnisonTerm False ppe False tm
              Cli.respond $ TestResults Output.NewlyComputed ppe True True (oks [(ref, tm')]) (fails [(ref, tm')])

            -- UpdateBuiltinsI -> do
            --   stepAt updateBuiltins
            --   checkTodo

            MergeBuiltinsI -> do
              Cli.Env {codebase} <- ask
              description <- inputDescription input
              -- these were added once, but maybe they've changed and need to be
              -- added again.
              let uf =
                    UF.typecheckedUnisonFile
                      (Map.fromList Builtin.builtinDataDecls)
                      (Map.fromList Builtin.builtinEffectDecls)
                      [Builtin.builtinTermsSrc Intrinsic]
                      mempty
              liftIO (Codebase.addDefsToCodebase codebase uf)
              -- add the names; note, there are more names than definitions
              -- due to builtin terms; so we don't just reuse `uf` above.
              let srcb = BranchUtil.fromNames Builtin.names0
              currentPath <- Cli.getCurrentPath
              _ <- Cli.updateAtM description (currentPath `snoc` "builtin") \destb ->
                liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge srcb destb)
              Cli.respond Success
            MergeIOBuiltinsI -> do
              Cli.Env {codebase} <- ask
              description <- inputDescription input
              -- these were added once, but maybe they've changed and need to be
              -- added again.
              let uf =
                    UF.typecheckedUnisonFile
                      (Map.fromList Builtin.builtinDataDecls)
                      (Map.fromList Builtin.builtinEffectDecls)
                      [Builtin.builtinTermsSrc Intrinsic]
                      mempty
              liftIO (Codebase.addDefsToCodebase codebase uf)
              -- these have not necessarily been added yet
              liftIO (Codebase.addDefsToCodebase codebase IOSource.typecheckedFile')

              -- add the names; note, there are more names than definitions
              -- due to builtin terms; so we don't just reuse `uf` above.
              let names0 = Builtin.names0 <> UF.typecheckedToNames IOSource.typecheckedFile'
              let srcb = BranchUtil.fromNames names0
              currentPath <- Cli.getCurrentPath
              _ <- Cli.updateAtM description (currentPath `snoc` "builtin") \destb ->
                liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge srcb destb)
              Cli.respond Success
            ListEditsI maybePath -> do
              patch <- Cli.getPatchAt (fromMaybe Cli.defaultPatchPath maybePath)
              ppe <-
                suffixifiedPPE
                  =<< makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
              Cli.respondNumbered $ ListEdits patch ppe
            PullRemoteBranchI mayRepo path syncMode pullMode verbosity -> do
              Cli.Env {codebase} <- ask
              let preprocess = case pullMode of
                    Input.PullWithHistory -> Unmodified
                    Input.PullWithoutHistory -> Preprocessed $ pure . Branch.discardHistory
              ns <- maybe (writePathToRead <$> resolveConfiguredUrl Pull path) pure mayRepo
              remoteBranch <- case ns of
                ReadRemoteNamespaceGit repo ->
                  Cli.ioE (Codebase.importRemoteBranch codebase repo syncMode preprocess) \err ->
                    Cli.returnEarly (Output.GitError err)
                ReadRemoteNamespaceShare repo -> importRemoteShareBranch repo
              description <- inputDescription input
              let unchangedMsg = PullAlreadyUpToDate ns path
              destAbs <- Cli.resolvePath' path
              let printDiffPath = if Verbosity.isSilent verbosity then Nothing else Just path
              case pullMode of
                Input.PullWithHistory -> do
                  destBranch <- Cli.getBranch0At destAbs
                  if Branch.isEmpty0 destBranch
                    then do
                      void $ Cli.updateAtM description destAbs (const $ pure remoteBranch)
                      Cli.respond $ MergeOverEmpty path
                    else
                      mergeBranchAndPropagateDefaultPatch
                        Branch.RegularMerge
                        description
                        (Just unchangedMsg)
                        remoteBranch
                        printDiffPath
                        destAbs
                Input.PullWithoutHistory -> do
                  didUpdate <-
                    Cli.updateAtM
                      description
                      destAbs
                      (\destBranch -> pure $ remoteBranch `Branch.consBranchSnapshot` destBranch)
                  Cli.respond
                    if didUpdate
                      then PullSuccessful ns path
                      else unchangedMsg
            PushRemoteBranchI pushRemoteBranchInput -> handlePushRemoteBranch pushRemoteBranchInput
            ListDependentsI hq -> handleDependents hq
            ListDependenciesI hq -> do
              Cli.Env {codebase} <- ask
              hqLength <- liftIO (Codebase.hashLength codebase)
              -- todo: add flag to handle transitive efficiently
              lds <- resolveHQToLabeledDependencies hq
              when (null lds) do
                Cli.returnEarly (LabeledReferenceNotFound hq)
              rootBranch <- Cli.getRootBranch
              for_ lds \ld -> do
                dependencies :: Set Reference <-
                  let tp r@(Reference.DerivedId i) =
                        liftIO (Codebase.getTypeDeclaration codebase i) <&> \case
                          Nothing -> error $ "What happened to " ++ show i ++ "?"
                          Just decl -> Set.delete r . DD.dependencies $ DD.asDataDecl decl
                      tp _ = pure mempty
                      tm (Referent.Ref r@(Reference.DerivedId i)) =
                        liftIO (Codebase.getTerm codebase i) <&> \case
                          Nothing -> error $ "What happened to " ++ show i ++ "?"
                          Just tm -> Set.delete r $ Term.dependencies tm
                      tm con@(Referent.Con (ConstructorReference (Reference.DerivedId i) cid) _ct) =
                        liftIO (Codebase.getTypeDeclaration codebase i) <&> \case
                          Nothing -> error $ "What happened to " ++ show i ++ "?"
                          Just decl -> case DD.typeOfConstructor (DD.asDataDecl decl) cid of
                            Nothing -> error $ "What happened to " ++ show con ++ "?"
                            Just tp -> Type.dependencies tp
                      tm _ = pure mempty
                   in LD.fold tp tm ld
                (missing, names0) <- liftIO (Branch.findHistoricalRefs' dependencies rootBranch)
                let types = R.toList $ Names.types names0
                let terms = fmap (second Referent.toReference) $ R.toList $ Names.terms names0
                let names = types <> terms
                #numberedArgs .= fmap (Text.unpack . Reference.toText) ((fmap snd names) <> toList missing)
                Cli.respond $ ListDependencies hqLength ld names missing
            NamespaceDependenciesI namespacePath' -> do
              path <- maybe Cli.getCurrentPath Cli.resolvePath' namespacePath'
              Cli.getMaybeBranchAt path >>= \case
                Nothing -> Cli.respond $ BranchEmpty (Right (Path.absoluteToPath' path))
                Just b -> do
                  externalDependencies <- NamespaceDependencies.namespaceDependencies (Branch.head b)
                  ppe <- PPE.unsuffixifiedPPE <$> currentPrettyPrintEnvDecl Backend.Within
                  Cli.respond $ ListNamespaceDependencies ppe path externalDependencies
            DebugNumberedArgsI -> do
              numArgs <- use #numberedArgs
              Cli.respond (DumpNumberedArgs numArgs)
            DebugTypecheckedUnisonFileI -> do
              Cli.Env {codebase} <- ask
              hqLength <- liftIO (Codebase.hashLength codebase)
              uf <- Cli.expectLatestTypecheckedFile
              let datas, effects, terms :: [(Name, Reference.Id)]
                  datas = [(Name.unsafeFromVar v, r) | (v, (r, _d)) <- Map.toList $ UF.dataDeclarationsId' uf]
                  effects = [(Name.unsafeFromVar v, r) | (v, (r, _e)) <- Map.toList $ UF.effectDeclarationsId' uf]
                  terms = [(Name.unsafeFromVar v, r) | (v, (r, _wk, _tm, _tp)) <- Map.toList $ UF.hashTermsId uf]
              Cli.respond $ DumpUnisonFileHashes hqLength datas effects terms
            DebugTabCompletionI inputs -> do
              Cli.Env {codebase} <- ask
              currentPath <- Cli.getCurrentPath
              let completionFunc = Completion.haskelineTabComplete IP.patternMap codebase currentPath
              (_, completions) <- liftIO $ completionFunc (reverse (unwords inputs), "")
              Cli.respond (DisplayDebugCompletions completions)
            DebugDumpNamespacesI -> do
              let seen h = State.gets (Set.member h)
                  set h = State.modify (Set.insert h)
                  getCausal b = (Branch.headHash b, pure $ Branch._history b)
                  goCausal :: forall m. Monad m => [(Branch.CausalHash, m (Branch.UnwrappedBranch m))] -> StateT (Set Branch.CausalHash) m ()
                  goCausal [] = pure ()
                  goCausal ((h, mc) : queue) = do
                    ifM (seen h) (goCausal queue) do
                      lift mc >>= \case
                        Causal.One h _bh b -> goBranch h b mempty queue
                        Causal.Cons h _bh b tail -> goBranch h b [fst tail] (tail : queue)
                        Causal.Merge h _bh b (Map.toList -> tails) -> goBranch h b (map fst tails) (tails ++ queue)
                  goBranch :: forall m. Monad m => Branch.CausalHash -> Branch0 m -> [Branch.CausalHash] -> [(Branch.CausalHash, m (Branch.UnwrappedBranch m))] -> StateT (Set Branch.CausalHash) m ()
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
              rootBranch <- Cli.getRootBranch
              void . liftIO . flip State.execStateT mempty $ goCausal [getCausal rootBranch]
            DebugDumpNamespaceSimpleI -> do
              rootBranch0 <- Cli.getRootBranch0
              for_ (Relation.toList . Branch.deepTypes $ rootBranch0) \(r, name) ->
                traceM $ show name ++ ",Type," ++ Text.unpack (Reference.toText r)
              for_ (Relation.toList . Branch.deepTerms $ rootBranch0) \(r, name) ->
                traceM $ show name ++ ",Term," ++ Text.unpack (Referent.toText r)
            DebugClearWatchI {} -> do
              Cli.Env {codebase} <- ask
              liftIO (Codebase.clearWatches codebase)
            DebugDoctorI {} -> do
              Cli.Env {codebase} <- ask
              r <- liftIO (Codebase.runTransaction codebase IntegrityCheck.integrityCheckFullCodebase)
              Cli.respond (IntegrityCheck r)
            DeprecateTermI {} -> Cli.respond NotImplemented
            DeprecateTypeI {} -> Cli.respond NotImplemented
            RemoveTermReplacementI from patchPath -> doRemoveReplacement from patchPath True
            RemoveTypeReplacementI from patchPath -> doRemoveReplacement from patchPath False
            ShowDefinitionByPrefixI {} -> Cli.respond NotImplemented
            UpdateBuiltinsI -> Cli.respond NotImplemented
            QuitI -> Cli.haltRepl
            GistI input -> handleGist input
            AuthLoginI -> authLogin (Codeserver.resolveCodeserver RemoteRepo.DefaultCodeserver)
            VersionI -> do
              Cli.Env {ucmVersion} <- ask
              Cli.respond $ PrintVersion ucmVersion

magicMainWatcherString :: String
magicMainWatcherString = "main"

inputDescription :: Input -> Cli r Text
inputDescription input =
  case input of
    SaveExecuteResultI _str -> pure "save-execute-result"
    ForkLocalBranchI src0 dest0 -> do
      src <- hp' src0
      dest <- p' dest0
      pure ("fork " <> src <> " " <> dest)
    MergeLocalBranchI src0 dest0 mode -> do
      src <- p' src0
      dest <- p' dest0
      let command =
            case mode of
              Branch.RegularMerge -> "merge"
              Branch.SquashMerge -> "merge.squash"
      pure (command <> " " <> src <> " " <> dest)
    ResetRootI src0 -> do
      src <- hp' src0
      pure ("reset-root " <> src)
    AliasTermI src0 dest0 -> do
      src <- hhqs' src0
      dest <- ps' dest0
      pure ("alias.term " <> src <> " " <> dest)
    AliasTypeI src0 dest0 -> do
      src <- hhqs' src0
      dest <- ps' dest0
      pure ("alias.type " <> src <> " " <> dest)
    AliasManyI srcs0 dest0 -> do
      srcs <- traverse hqs srcs0
      dest <- p' dest0
      pure ("alias.many " <> Text.intercalate " " srcs <> " " <> dest)
    MoveTermI src0 dest0 -> do
      src <- hqs' src0
      dest <- ps' dest0
      pure ("move.term " <> src <> " " <> dest)
    MoveTypeI src0 dest0 -> do
      src <- hqs' src0
      dest <- ps' dest0
      pure ("move.type " <> src <> " " <> dest)
    MoveBranchI src0 dest0 -> do
      src <- p' src0
      dest <- p' dest0
      pure ("move.namespace " <> src <> " " <> dest)
    MovePatchI src0 dest0 -> do
      src <- ps' src0
      dest <- ps' dest0
      pure ("move.patch " <> src <> " " <> dest)
    CopyPatchI src0 dest0 -> do
      src <- ps' src0
      dest <- ps' dest0
      pure ("copy.patch " <> src <> " " <> dest)
    DeleteI thing0 -> do
      thing <- hqs' thing0
      pure ("delete " <> thing)
    DeleteTermI def0 -> do
      def <- hqs' def0
      pure ("delete.term " <> def)
    DeleteTypeI def0 -> do
      def <- hqs' def0
      pure ("delete.type " <> def)
    DeleteBranchI Try opath0 -> do
      opath <- ops' opath0
      pure ("delete.namespace " <> opath)
    DeleteBranchI Force opath0 -> do
      opath <- ops' opath0
      pure ("delete.namespace.force " <> opath)
    DeletePatchI path0 -> do
      path <- ps' path0
      pure ("delete.patch " <> path)
    ReplaceI src target p0 -> do
      p <- opatch p0
      pure $
        "replace "
          <> HQ.toText src
          <> " "
          <> HQ.toText target
          <> " "
          <> p
    ResolveTermNameI path0 -> do
      path <- hqs' path0
      pure ("resolve.termName " <> path)
    ResolveTypeNameI path0 -> do
      path <- hqs' path0
      pure ("resolve.typeName " <> path)
    AddI _selection -> pure "add"
    UpdateI p0 _selection -> do
      p <-
        case p0 of
          NoPatch -> pure ".nopatch"
          DefaultPatch -> (" " <>) <$> ps' Cli.defaultPatchPath
          UsePatch p0 -> (" " <>) <$> ps' p0
      pure ("update" <> p)
    PropagatePatchI p0 scope0 -> do
      p <- ps' p0
      scope <- p' scope0
      pure ("patch " <> p <> " " <> scope)
    UndoI {} -> pure "undo"
    ExecuteI s args -> pure ("execute " <> Text.unwords (fmap Text.pack (s : args)))
    IOTestI hq -> pure ("io.test " <> HQ.toText hq)
    LinkI md defs0 -> do
      defs <- traverse hqs' defs0
      pure ("link " <> HQ.toText md <> " " <> Text.intercalate " " defs)
    UnlinkI md defs0 -> do
      defs <- traverse hqs' defs0
      pure ("unlink " <> HQ.toText md <> " " <> Text.intercalate " " defs)
    UpdateBuiltinsI -> pure "builtins.update"
    MergeBuiltinsI -> pure "builtins.merge"
    MergeIOBuiltinsI -> pure "builtins.mergeio"
    MakeStandaloneI out nm -> pure ("compile " <> Text.pack out <> " " <> HQ.toText nm)
    PullRemoteBranchI orepo dest0 _syncMode pullMode _ -> do
      dest <- p' dest0
      let command =
            Text.pack . InputPattern.patternName $
              case pullMode of
                PullWithoutHistory -> InputPatterns.pullWithoutHistory
                PullWithHistory -> InputPatterns.pull
      pure $
        command
          <> " "
          -- todo: show the actual config-loaded namespace
          <> maybe
            "(remote namespace from .unisonConfig)"
            printNamespace
            orepo
          <> " "
          <> dest
    CreateAuthorI (NameSegment id) name -> pure ("create.author " <> id <> " " <> name)
    LoadPullRequestI base head dest0 -> do
      dest <- p' dest0
      pure $
        "pr.load "
          <> printNamespace base
          <> " "
          <> printNamespace head
          <> " "
          <> dest
    RemoveTermReplacementI src p0 -> do
      p <- opatch p0
      pure ("delete.term-replacement" <> HQ.toText src <> " " <> p)
    RemoveTypeReplacementI src p0 -> do
      p <- opatch p0
      pure ("delete.type-replacement" <> HQ.toText src <> " " <> p)
    --
    ApiI -> wat
    AuthLoginI {} -> wat
    CreateMessage {} -> wat
    CreatePullRequestI {} -> wat
    DebugClearWatchI {} -> wat
    DebugDoctorI {} -> wat
    DebugDumpNamespaceSimpleI {} -> wat
    DebugDumpNamespacesI {} -> wat
    DebugNumberedArgsI {} -> wat
    DebugTypecheckedUnisonFileI {} -> wat
    DeprecateTermI {} -> wat
    DeprecateTypeI {} -> wat
    DiffNamespaceI {} -> wat
    DisplayI {} -> wat
    DocsI {} -> wat
    DocsToHtmlI {} -> wat
    FindI {} -> wat
    FindPatchI {} -> wat
    FindShallowI {} -> wat
    GistI {} -> wat
    HistoryI {} -> wat
    LinksI {} -> wat
    ListDependenciesI {} -> wat
    ListDependentsI {} -> wat
    ListEditsI {} -> wat
    LoadI {} -> wat
    NamesI {} -> wat
    NamespaceDependenciesI {} -> wat
    PopBranchI {} -> wat
    PreviewAddI {} -> wat
    PreviewMergeLocalBranchI {} -> wat
    PreviewUpdateI {} -> wat
    PushRemoteBranchI {} -> wat
    QuitI {} -> wat
    ShowDefinitionByPrefixI {} -> wat
    ShowDefinitionI {} -> wat
    ShowReflogI {} -> wat
    SwitchBranchI {} -> wat
    TestI {} -> wat
    TodoI {} -> wat
    UiI -> wat
    UpI {} -> wat
    VersionI -> wat
    DebugTabCompletionI _input -> wat
  where
    hp' :: Either SBH.ShortBranchHash Path' -> Cli r Text
    hp' = either (pure . Text.pack . show) p'
    p' :: Path' -> Cli r Text
    p' = fmap tShow . Cli.resolvePath'
    ops' :: Maybe Path.Split' -> Cli r Text
    ops' = maybe (pure ".") ps'
    opatch :: Maybe Path.Split' -> Cli r Text
    opatch = ps' . fromMaybe Cli.defaultPatchPath
    wat = error $ show input ++ " is not expected to alter the branch"
    hhqs' :: Either SH.ShortHash Path.HQSplit' -> Cli r Text
    hhqs' = \case
      Left sh -> pure (SH.toText sh)
      Right x -> hqs' x
    hqs' :: Path.HQSplit' -> Cli r Text
    hqs' (p0, hq) = do
      p <- if Path.isRoot' p0 then pure mempty else p' p0
      pure (p <> "." <> tShow hq)
    hqs (p, hq) = hqs' (Path' . Right . Path.Relative $ p, hq)
    ps' = p' . Path.unsplit'

handleCreatePullRequest :: ReadRemoteNamespace -> ReadRemoteNamespace -> Cli r ()
handleCreatePullRequest baseRepo0 headRepo0 = do
  Cli.Env {codebase} <- ask

  let withBranch :: ReadRemoteNamespace -> (forall x. (forall r. Branch IO -> Cli r x) -> Cli r x)
      withBranch rrn k = case rrn of
        ReadRemoteNamespaceGit repo -> do
          Cli.withE (Codebase.viewRemoteBranch codebase repo Git.RequireExistingBranch) \case
            Left err -> Cli.returnEarly (Output.GitError err)
            Right x -> k x
        ReadRemoteNamespaceShare repo -> k =<< importRemoteShareBranch repo

  (ppe, diff) <- withBranch baseRepo0 \baseBranch -> withBranch headRepo0 \headBranch -> do
    merged <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge baseBranch headBranch)
    diffHelper (Branch.head baseBranch) (Branch.head merged)
  Cli.respondNumbered (ShowDiffAfterCreatePR baseRepo0 headRepo0 ppe diff)

handleFindI ::
  Bool ->
  FindScope ->
  [String] ->
  Input ->
  Cli r ()
handleFindI isVerbose fscope ws input = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath' <- Cli.getCurrentPath
  currentBranch0 <- Cli.getCurrentBranch0
  let getNames :: FindScope -> Names
      getNames findScope =
        let cp = Path.unabsolute currentPath'
            nameScope = case findScope of
              FindLocal -> Backend.Within cp
              FindLocalAndDeps -> Backend.Within cp
              FindGlobal -> Backend.AllNames cp
            scopeFilter = case findScope of
              FindLocal ->
                let f n =
                      case Name.segments n of
                        "lib" Nel.:| _ : _ -> False
                        _ -> True
                 in Names.filter f
              FindGlobal -> id
              FindLocalAndDeps ->
                let f n =
                      case Name.segments n of
                        "lib" Nel.:| (_ : "lib" : _) -> False
                        _ -> True
                 in Names.filter f
         in scopeFilter (Backend.prettyNamesForBranch root' nameScope)
  let getResults :: Names -> Cli r [SearchResult]
      getResults names = do
        case ws of
          [] -> pure (List.sortOn (\s -> (SR.name s, s)) (SR.fromNames names))
          -- type query
          ":" : ws -> do
            typ <- parseSearchType (show input) (unwords ws)
            let named = Branch.deepReferents currentBranch0
            matches <-
              fmap (filter (`Set.member` named) . toList) $
                liftIO (Codebase.termsOfType codebase typ)
            matches <-
              if null matches
                then do
                  Cli.respond NoExactTypeMatches
                  fmap (filter (`Set.member` named) . toList) $
                    liftIO (Codebase.termsMentioningType codebase typ)
                else pure matches
            pure $
              -- in verbose mode, aliases are shown, so we collapse all
              -- aliases to a single search result; in non-verbose mode,
              -- a separate result may be shown for each alias
              (if isVerbose then uniqueBy SR.toReferent else id) $
                searchResultsFor names matches []

          -- name query
          (map HQ.unsafeFromString -> qs) -> do
            let srs = searchBranchScored names fuzzyNameDistance qs
            pure $ uniqueBy SR.toReferent srs
  let respondResults results = do
        #numberedArgs .= fmap searchResultToHQString results
        results' <- liftIO (Backend.loadSearchResults codebase results)
        ppe <-
          suffixifiedPPE
            =<< makePrintNamesFromLabeled'
              (foldMap SR'.labeledDependencies results')
        Cli.respond $ ListOfDefinitions fscope ppe isVerbose results'
  results <- getResults (getNames fscope)
  case (results, fscope) of
    ([], FindLocal) -> do
      Cli.respond FindNoLocalMatches
      respondResults =<< getResults (getNames FindLocalAndDeps)
    _ -> respondResults results

handleDependents :: HQ.HashQualified Name -> Cli r ()
handleDependents hq = do
  Cli.Env {codebase} <- ask
  hqLength <- liftIO (Codebase.hashLength codebase)
  -- todo: add flag to handle transitive efficiently
  lds <- resolveHQToLabeledDependencies hq

  when (null lds) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  for_ lds \ld -> do
    -- The full set of dependent references, any number of which may not have names in the current namespace.
    dependents <-
      let tp r = liftIO (Codebase.dependents codebase r)
          tm (Referent.Ref r) = liftIO (Codebase.dependents codebase r)
          tm (Referent.Con (ConstructorReference r _cid) _ct) = liftIO (Codebase.dependents codebase r)
       in LD.fold tp tm ld
    -- Use an unsuffixified PPE here, so we display full names (relative to the current path), rather than the shortest possible
    -- unambiguous name.
    ppe <- PPE.unsuffixifiedPPE <$> currentPrettyPrintEnvDecl Backend.Within
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
    #numberedArgs .= map (Text.unpack . Reference.toText . fst) results
    Cli.respond (ListDependents hqLength ld results)

-- | Handle a @gist@ command.
handleGist :: GistInput -> Cli r ()
handleGist (GistInput repo) =
  doPushRemoteBranch (GistyPush repo) Path.relativeEmpty' SyncMode.ShortCircuit

-- | Handle a @push@ command.
handlePushRemoteBranch :: PushRemoteBranchInput -> Cli r ()
handlePushRemoteBranch PushRemoteBranchInput {maybeRemoteRepo = mayRepo, localPath = path, pushBehavior, syncMode} =
  Cli.time "handlePushRemoteBranch" do
    repo <- mayRepo & onNothing (resolveConfiguredUrl Push path)
    doPushRemoteBranch (NormalPush repo pushBehavior) path syncMode

-- | Either perform a "normal" push (updating a remote path), which takes a 'PushBehavior' (to control whether creating
-- a new namespace is allowed), or perform a "gisty" push, which doesn't update any paths (and also is currently only
-- uploaded for remote git repos, not remote Share repos).
data PushFlavor
  = NormalPush WriteRemotePath PushBehavior
  | GistyPush WriteGitRepo

-- Internal helper that implements pushing to a remote repo, which generalizes @gist@ and @push@.
doPushRemoteBranch ::
  -- | The repo to push to.
  PushFlavor ->
  -- | The local path to push. If relative, it's resolved relative to the current path (`cd`).
  Path' ->
  SyncMode.SyncMode ->
  Cli r ()
doPushRemoteBranch pushFlavor localPath0 syncMode = do
  Cli.Env {codebase} <- ask
  localPath <- Cli.resolvePath' localPath0
  case pushFlavor of
    NormalPush (writeRemotePath@(WriteRemotePathGit WriteGitRemotePath {repo, path = remotePath})) pushBehavior -> do
      sourceBranch <- Cli.getBranchAt localPath
      let withRemoteRoot :: Branch IO -> Either Output (Branch IO)
          withRemoteRoot remoteRoot = do
            let -- We don't merge `sourceBranch` with `remoteBranch`, we just replace it. This push will be rejected if
                -- this rewinds time or misses any new updates in the remote branch that aren't in `sourceBranch`
                -- already.
                f remoteBranch = if shouldPushTo pushBehavior remoteBranch then Just sourceBranch else Nothing
            case Branch.modifyAtM remotePath f remoteRoot of
              Nothing -> Left (RefusedToPush pushBehavior writeRemotePath)
              Just newRemoteRoot -> Right newRemoteRoot
      let opts =
            PushGitBranchOpts
              { behavior =
                  case pushBehavior of
                    PushBehavior.ForcePush -> GitPushBehaviorForce
                    PushBehavior.RequireEmpty -> GitPushBehaviorFf
                    PushBehavior.RequireNonEmpty -> GitPushBehaviorFf,
                syncMode
              }
      result <-
        Cli.ioE (Codebase.pushGitBranch codebase repo opts (\remoteRoot -> pure (withRemoteRoot remoteRoot))) \err ->
          Cli.returnEarly (Output.GitError err)
      _branch <- result & onLeft Cli.returnEarly
      Cli.respond Success
    NormalPush (WriteRemotePathShare sharePath) pushBehavior -> handlePushToUnisonShare sharePath localPath pushBehavior
    GistyPush repo -> do
      sourceBranch <- Cli.getBranchAt localPath
      let opts =
            PushGitBranchOpts
              { behavior = GitPushBehaviorGist,
                syncMode
              }
      result <-
        Cli.ioE (Codebase.pushGitBranch codebase repo opts (\_remoteRoot -> pure (Right sourceBranch))) \err ->
          Cli.returnEarly (Output.GitError err)
      _branch <- result & onLeft Cli.returnEarly
      sbhLength <- liftIO (Codebase.branchHashLength codebase)
      Cli.respond $
        GistCreated
          ( ReadRemoteNamespaceGit
              ReadGitRemoteNamespace
                { repo = writeToReadGit repo,
                  sbh = Just (SBH.fromHash sbhLength (Branch.headHash sourceBranch)),
                  path = Path.empty
                }
          )
  where
    -- Per `pushBehavior`, we are either:
    --
    --   (1) force-pushing, in which case the remote branch state doesn't matter
    --   (2) updating an empty branch, which fails if the branch isn't empty (`push.create`)
    --   (3) updating a non-empty branch, which fails if the branch is empty (`push`)
    shouldPushTo :: PushBehavior -> Branch m -> Bool
    shouldPushTo pushBehavior remoteBranch =
      case pushBehavior of
        PushBehavior.ForcePush -> True
        PushBehavior.RequireEmpty -> Branch.isEmpty0 (Branch.head remoteBranch)
        PushBehavior.RequireNonEmpty -> not (Branch.isEmpty0 (Branch.head remoteBranch))

handlePushToUnisonShare :: WriteShareRemotePath -> Path.Absolute -> PushBehavior -> Cli r ()
handlePushToUnisonShare remote@WriteShareRemotePath {server, repo, path = remotePath} localPath behavior = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  let sharePath = Share.Path (repo Nel.:| pathToSegments remotePath)
  ensureAuthenticatedWithCodeserver codeserver

  Cli.Env {authHTTPClient, codebase} <- ask

  -- doesn't handle the case where a non-existent path is supplied
  localCausalHash <-
    liftIO (Codebase.runTransaction codebase (Ops.loadCausalHashAtPath (pathToSegments (Path.unabsolute localPath)))) & onNothingM do
      Cli.returnEarly (EmptyPush . Path.absoluteToPath' $ localPath)

  let checkAndSetPush :: Maybe Hash32 -> IO (Either (Share.SyncError Share.CheckAndSetPushError) ())
      checkAndSetPush remoteHash =
        withEntitiesUploadedProgressCallback \uploadedCallback ->
          if Just (Hash32.fromHash (unCausalHash localCausalHash)) == remoteHash
            then pure (Right ())
            else
              Share.checkAndSetPush
                authHTTPClient
                baseURL
                (Codebase.withConnectionIO codebase)
                sharePath
                remoteHash
                localCausalHash
                uploadedCallback

  case behavior of
    PushBehavior.ForcePush -> do
      maybeHashJwt <-
        Cli.ioE (Share.getCausalHashByPath authHTTPClient baseURL sharePath) \err ->
          Cli.returnEarly (Output.ShareError (ShareErrorGetCausalHashByPath err))
      Cli.ioE (checkAndSetPush (Share.hashJWTHash <$> maybeHashJwt)) (pushError ShareErrorCheckAndSetPush)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireEmpty -> do
      Cli.ioE (checkAndSetPush Nothing) (pushError ShareErrorCheckAndSetPush)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireNonEmpty -> do
      let push :: IO (Either (Share.SyncError Share.FastForwardPushError) ())
          push = do
            withEntitiesUploadedProgressCallback \uploadedCallback ->
              Share.fastForwardPush
                authHTTPClient
                baseURL
                (Codebase.withConnectionIO codebase)
                sharePath
                localCausalHash
                uploadedCallback
      Cli.ioE push (pushError ShareErrorFastForwardPush)
      Cli.respond (ViewOnShare remote)
  where
    pathToSegments :: Path -> [Text]
    pathToSegments =
      coerce Path.toList

    -- Provide the given action a callback that displays to the terminal.
    withEntitiesUploadedProgressCallback :: ((Int -> IO ()) -> IO a) -> IO a
    withEntitiesUploadedProgressCallback action = do
      entitiesUploadedVar <- newTVarIO 0
      Console.Regions.displayConsoleRegions do
        Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
          Console.Regions.setConsoleRegion region do
            entitiesUploaded <- readTVar entitiesUploadedVar
            pure $
              "\n  Uploaded "
                <> tShow entitiesUploaded
                <> " entities...\n\n"
          result <- action (\n -> atomically (modifyTVar' entitiesUploadedVar (+ n)))
          entitiesUploaded <- readTVarIO entitiesUploadedVar
          Console.Regions.finishConsoleRegion region $
            "\n  Uploaded " <> tShow entitiesUploaded <> " entities."
          pure result

    pushError :: (a -> Output.ShareError) -> Share.SyncError a -> Cli r b
    pushError f err0 = do
      Cli.returnEarly case err0 of
        Share.SyncError err -> Output.ShareError (f err)
        Share.TransportError err -> Output.ShareError (ShareErrorTransport err)

-- | Handle a @ShowDefinitionI@ input command, i.e. `view` or `edit`.
handleShowDefinition :: OutputLocation -> ShowDefinitionScope -> [HQ.HashQualified Name] -> Cli r ()
handleShowDefinition outputLoc showDefinitionScope inputQuery = do
  Cli.Env {codebase} <- ask
  hqLength <- liftIO (Codebase.hashLength codebase)
  -- If the query is empty, run a fuzzy search.
  query <-
    if null inputQuery
      then do
        branch <- Cli.getCurrentBranch0
        fuzzySelectDefinition Relative branch & onNothingM do
          Cli.returnEarly case outputLoc of
            ConsoleLocation -> HelpMessage InputPatterns.view
            _ -> HelpMessage InputPatterns.edit
      else pure inputQuery
  root <- Cli.getRootBranch
  let root0 = Branch.head root
  currentPath' <- Path.unabsolute <$> Cli.getCurrentPath
  let hasAbsoluteQuery = any (any Name.isAbsolute) inputQuery
  (names, unbiasedPPE) <- case (hasAbsoluteQuery, showDefinitionScope) of
    (True, _) -> do
      let namingScope = Backend.AllNames currentPath'
      let parseNames = NamesWithHistory.fromCurrentNames $ Backend.parseNamesForBranch root namingScope
      let ppe = Backend.getCurrentPrettyNames hqLength (Backend.Within currentPath') root
      pure (parseNames, ppe)
    (_, ShowDefinitionGlobal) -> do
      let names = NamesWithHistory.fromCurrentNames . Names.makeAbsolute $ Branch.toNames root0
      -- Use an absolutely qualified ppe for view.global
      let ppe = PPE.fromNamesDecl hqLength names
      pure (names, ppe)
    (_, ShowDefinitionLocal) -> do
      currentBranch <- Cli.getCurrentBranch0
      let currentNames = NamesWithHistory.fromCurrentNames $ Branch.toNames currentBranch
      let ppe = Backend.getCurrentPrettyNames hqLength (Backend.Within currentPath') root
      pure (currentNames, ppe)
  Backend.DefinitionResults terms types misses <- do
    let nameSearch = Backend.makeNameSearch hqLength names
    liftIO (Backend.definitionsBySuffixes codebase nameSearch includeCycles query)
  outputPath <- getOutputPath
  when (not (null types && null terms)) do
    let ppe = PPED.biasTo (mapMaybe HQ.toName inputQuery) unbiasedPPE
    Cli.respond (DisplayDefinitions outputPath ppe types terms)
  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  -- We set latestFile to be programmatically generated, if we
  -- are viewing these definitions to a file - this will skip the
  -- next update for that file (which will happen immediately)
  #latestFile .= ((,True) <$> outputPath)
  where
    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ -> Backend.IncludeCycles
        LatestFileLocation -> Backend.IncludeCycles

    -- Get the file path to send the definition(s) to. `Nothing` means the terminal.
    getOutputPath :: Cli r (Maybe FilePath)
    getOutputPath =
      case outputLoc of
        ConsoleLocation -> pure Nothing
        FileLocation path -> pure (Just path)
        LatestFileLocation -> do
          loopState <- State.get
          pure case loopState ^. #latestFile of
            Nothing -> Just "scratch.u"
            Just (path, _) -> Just path

-- | Handle a @test@ command.
handleTest :: TestInput -> Cli r ()
handleTest TestInput {includeLibNamespace, showFailures, showSuccesses} = do
  Cli.Env {codebase} <- ask

  testTerms <- do
    branch <- Cli.getCurrentBranch0
    branch
      & Branch.deepTermMetadata
      & R4.restrict34d12 isTest
      & (if includeLibNamespace then id else R.filterRan (not . isInLibNamespace))
      & R.dom
      & pure
  let testRefs = Set.mapMaybe Referent.toTermReference testTerms
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
  cachedTests <- do
    fmap Map.fromList do
      Set.toList testRefs & wither \case
        Reference.Builtin _ -> pure Nothing
        r@(Reference.DerivedId rid) -> liftIO (fmap (r,) <$> Codebase.getWatch codebase WK.TestWatch rid)
  let stats = Output.CachedTests (Set.size testRefs) (Map.size cachedTests)
  names <-
    makePrintNamesFromLabeled' $
      LD.referents testTerms
        <> LD.referents [DD.okConstructorReferent, DD.failConstructorReferent]
  ppe <- fqnPPE names
  Cli.respond $
    TestResults
      stats
      ppe
      showSuccesses
      showFailures
      (oks cachedTests)
      (fails cachedTests)
  let toCompute = Set.difference testRefs (Map.keysSet cachedTests)
  when (not (Set.null toCompute)) do
    let total = Set.size toCompute
    computedTests <- fmap join . for (toList toCompute `zip` [1 ..]) $ \(r, n) ->
      case r of
        Reference.DerivedId rid ->
          liftIO (Codebase.getTerm codebase rid) >>= \case
            Nothing -> do
              hqLength <- liftIO (Codebase.hashLength codebase)
              Cli.respond (TermNotFound' . SH.take hqLength . Reference.toShortHash $ Reference.DerivedId rid)
              pure []
            Just tm -> do
              Cli.respond $ TestIncrementalOutputStart ppe (n, total) r tm
              --                          v don't cache; test cache populated below
              tm' <- evalUnisonTermE True ppe False tm
              case tm' of
                Left e -> do
                  Cli.respond (EvaluationFailure e)
                  pure []
                Right tm' -> do
                  -- After evaluation, cache the result of the test
                  liftIO (Codebase.putWatch codebase WK.TestWatch rid tm')
                  Cli.respond $ TestIncrementalOutputEnd ppe (n, total) r tm'
                  pure [(r, tm')]
        r -> error $ "unpossible, tests can't be builtins: " <> show r

    let m = Map.fromList computedTests
    Cli.respond $ TestResults Output.NewlyComputed ppe showSuccesses showFailures (oks m) (fails m)
  where
    isInLibNamespace :: Name -> Bool
    isInLibNamespace name =
      case Name.segments name of
        "lib" Nel.:| _ : _ -> True
        _ -> False

-- | Handle an @update@ command.
handleUpdate :: Input -> OptionalPatch -> Set Name -> Cli r ()
handleUpdate input optionalPatch requestedNames = do
  Cli.Env {codebase} <- ask
  currentPath' <- Cli.getCurrentPath
  uf <- Cli.expectLatestTypecheckedFile
  let patchPath =
        case optionalPatch of
          NoPatch -> Nothing
          DefaultPatch -> Just Cli.defaultPatchPath
          UsePatch p -> Just p
  slurpCheckNames <- Branch.toNames <$> Cli.getCurrentBranch0
  let requestedVars = Set.map Name.toVar requestedNames
  let sr = Slurp.slurpFile uf requestedVars Slurp.UpdateOp slurpCheckNames
      addsAndUpdates :: SlurpComponent Symbol
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
      hashTerms :: Map Reference (Type Symbol Ann)
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
                "Expected unique matches for var \""
                  ++ Var.nameStr v
                  ++ "\" but got: "
                  ++ show actual
            where
              n = Name.unsafeFromVar v
      termDeprecations :: [(Name, Referent)]
      termDeprecations =
        [ (n, r)
          | (oldTypeRef, _) <- Map.elems typeEdits,
            (n, r) <- Names.constructorsForType oldTypeRef slurpCheckNames
        ]
  patchOps <- for patchPath \patchPath -> do
    ye'ol'Patch <- Cli.getPatchAt patchPath
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
          <$> (liftIO . Codebase.getTypeOfTerm codebase) r

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

        updatePatch :: Patch -> Patch
        updatePatch p = foldl' step2 p' termEdits
          where
            p' = foldl' step1 p typeEdits
            step1 p (r, r') = Patch.updateType r (TypeEdit.Replace r') p
            step2 p (r, r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
        (p, seg) = Path.toAbsoluteSplit currentPath' patchPath
        updatePatches :: Monad m => Branch0 m -> m (Branch0 m)
        updatePatches = Branch.modifyPatches seg updatePatch
    pure (updatePatch ye'ol'Patch, updatePatches, p)

  when (Slurp.hasAddsOrUpdates sr) $ do
    -- take a look at the `updates` from the SlurpResult
    -- and make a patch diff to record a replacement from the old to new references
    stepManyAtMNoSync
      ( [ ( Path.unabsolute currentPath',
            pure . doSlurpUpdates typeEdits termEdits termDeprecations
          ),
          ( Path.unabsolute currentPath',
            pure . doSlurpAdds addsAndUpdates uf
          )
        ]
          ++ case patchOps of
            Nothing -> []
            Just (_, update, p) -> [(Path.unabsolute p, update)]
      )
    liftIO . Codebase.addDefsToCodebase codebase . filterBySlurpResult sr $ uf
  ppe <- prettyPrintEnvDecl =<< displayNames uf
  Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
  whenJust patchOps \(updatedPatch, _, _) ->
    void $ propagatePatchNoSync updatedPatch currentPath'
  addDefaultMetadata addsAndUpdates
  syncRoot case patchPath of
    Nothing -> "update.nopatch"
    Just p ->
      p & Path.unsplit'
        & Path.resolve @_ @_ @Path.Absolute currentPath'
        & tShow

-- Add default metadata to all added types and terms in a slurp component.
--
-- No-op if the slurp component is empty.
addDefaultMetadata :: SlurpComponent Symbol -> Cli r ()
addDefaultMetadata adds =
  when (not (SC.isEmpty adds)) do
    Cli.time "add-default-metadata" do
      currentPath' <- Cli.getCurrentPath

      let addedVs = Set.toList $ SC.types adds <> SC.terms adds
          addedNs = traverse (Path.hqSplitFromName' . Name.unsafeFromVar) addedVs
      case addedNs of
        Nothing ->
          error $
            "I couldn't parse a name I just added to the codebase! "
              <> "-- Added names: "
              <> show addedVs
        Just addedNames ->
          resolveDefaultMetadata currentPath' >>= \case
            [] -> pure ()
            dm -> do
              traverse InputPatterns.parseHashQualifiedName dm & \case
                Left err -> do
                  Cli.respond $
                    ConfiguredMetadataParseError
                      (Path.absoluteToPath' currentPath')
                      (show dm)
                      err
                Right defaultMeta -> do
                  manageLinks True addedNames defaultMeta Metadata.insert

resolveDefaultMetadata :: Path.Absolute -> Cli r [String]
resolveDefaultMetadata path = do
  let superpaths = Path.ancestors path
  xs <-
    for
      superpaths
      ( \path -> do
          mayNames <- Cli.getConfig @[String] (configKey "DefaultMetadata" path)
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
  Bool ->
  [Path.HQSplit'] ->
  [HQ.HashQualified Name] ->
  ( forall r.
    Ord r =>
    (r, Metadata.Type, Metadata.Value) ->
    Branch.Star r NameSegment ->
    Branch.Star r NameSegment
  ) ->
  Cli r ()
manageLinks silent srcs' metadataNames op = do
  metadatas <- traverse resolveMetadata metadataNames
  before <- Cli.getRootBranch0
  srcs <- traverse Cli.resolveSplit' srcs'
  srcle <- Monoid.foldMapM Cli.getTermsAt srcs
  srclt <- Monoid.foldMapM Cli.getTypesAt srcs
  for_ metadatas \case
    Left errOutput -> Cli.respond errOutput
    Right (mdType, mdValue) -> do
      let step =
            let tmUpdates terms = foldl' go terms srcle
                  where
                    go terms src = op (src, mdType, mdValue) terms
                tyUpdates types = foldl' go types srclt
                  where
                    go types src = op (src, mdType, mdValue) types
             in over Branch.terms tmUpdates . over Branch.types tyUpdates
      let steps = map (\(path, _hq) -> (Path.unabsolute path, step)) srcs
      stepManyAtNoSync steps
  if silent
    then Cli.respond DefaultMetadataNotification
    else do
      after <- Cli.getRootBranch0
      (ppe, diff) <- diffHelper before after
      if OBranchDiff.isEmpty diff
        then Cli.respond NoOp
        else
          Cli.respondNumbered $
            ShowDiffNamespace
              (Right Path.absoluteEmpty)
              (Right Path.absoluteEmpty)
              ppe
              diff

-- | Resolve a metadata name to its type/value, or return early if no such metadata is found.
resolveMetadata :: HQ.HashQualified Name -> Cli r (Either Output (Metadata.Type, Metadata.Value))
resolveMetadata name = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath' <- Cli.getCurrentPath
  sbhLength <- liftIO (Codebase.branchHashLength codebase)

  let ppe :: PPE.PrettyPrintEnv
      ppe =
        Backend.basicSuffixifiedNames sbhLength root' (Backend.Within $ Path.unabsolute currentPath')

  terms <- getHQTerms name
  ref <-
    case Set.asSingleton terms of
      Just (Referent.Ref ref) -> pure ref
      -- FIXME: we want a different error message if the given name is associated with a data constructor (`Con`).
      _ -> Cli.returnEarly (MetadataAmbiguous name ppe (Set.toList terms))
  liftIO (Codebase.getTypeOfTerm codebase ref) >>= \case
    Just ty -> pure $ Right (Hashing.typeToReference ty, ref)
    Nothing ->
      pure (Left (MetadataMissingType ppe (Referent.Ref ref)))

-- Takes a maybe (namespace address triple); returns it as-is if `Just`;
-- otherwise, tries to load a value from .unisonConfig, and complains
-- if needed.
resolveConfiguredUrl :: PushPull -> Path' -> Cli r WriteRemotePath
resolveConfiguredUrl pushPull destPath' = do
  destPath <- Cli.resolvePath' destPath'
  whenNothingM (remoteMappingForPath pushPull destPath) do
    let gitUrlConfigKey = gitUrlKey destPath
    -- Fall back to deprecated GitUrl key
    Cli.getConfig gitUrlConfigKey >>= \case
      Just url ->
        (WriteRemotePathGit <$> P.parse UriParser.deprecatedWriteGitRemotePath (Text.unpack gitUrlConfigKey) url) & onLeft \err ->
          Cli.returnEarly (ConfiguredRemoteMappingParseError pushPull destPath url (show err))
      Nothing -> Cli.returnEarly (NoConfiguredRemoteMapping pushPull destPath)
  where
    gitUrlKey :: Path.Absolute -> Text
    gitUrlKey = configKey "GitUrl"

configKey :: Text -> Path.Absolute -> Text
configKey k p =
  Text.intercalate "." . toList $
    k
      :<| fmap
        NameSegment.toText
        (Path.toSeq $ Path.unabsolute p)

-- | Tries to look up a remote mapping for a given path.
-- Will also resolve paths relative to any mapping which is configured for a parent of that
-- path.
--
-- E.g.
--
-- A config which maps:
--
-- .myshare.foo -> .me.public.foo
--
-- Will resolve the following local paths into share paths like so:
--
-- .myshare.foo -> .me.public.foo
-- .myshare.foo.bar -> .me.public.foo.bar
-- .myshare.foo.bar.baz -> .me.public.foo.bar.baz
-- .myshare -> <Nothing>
remoteMappingForPath :: PushPull -> Path.Absolute -> Cli r (Maybe WriteRemotePath)
remoteMappingForPath pushPull dest = do
  pathPrefixes dest & Foldable.firstJustM \(prefix, suffix) -> do
    let remoteMappingConfigKey = remoteMappingKey prefix
    Cli.getConfig remoteMappingConfigKey >>= \case
      Just url -> do
        let parseResult = P.parse UriParser.writeRemotePath (Text.unpack remoteMappingConfigKey) url
         in case parseResult of
              Left err -> Cli.returnEarly (ConfiguredRemoteMappingParseError pushPull dest url (show err))
              Right wrp -> do
                let remote = wrp & RemoteRepo.remotePath_ %~ \p -> Path.resolve p suffix
                 in pure $ Just remote
      Nothing -> pure Nothing
  where
    -- Produces a list of path prefixes and suffixes, from longest prefix to shortest
    --
    -- E.g.
    --
    -- >>> pathPrefixes ("a" :< "b" :< Path.absoluteEmpty)
    -- fromList [(.a.b,),(.a,b),(.,a.b)]
    pathPrefixes :: Path.Absolute -> Seq (Path.Absolute, Path.Path)
    pathPrefixes p =
      Path.unabsolute p
        & Path.toSeq
        & \seq ->
          Seq.zip (Seq.inits seq) (Seq.tails seq)
            & Seq.reverse
            <&> bimap (Path.Absolute . Path.Path) (Path.Path)
    remoteMappingKey :: Path.Absolute -> Text
    remoteMappingKey = configKey "RemoteMapping"

importRemoteShareBranch :: ReadShareRemoteNamespace -> Cli r (Branch IO)
importRemoteShareBranch rrn@(ReadShareRemoteNamespace {server, repo, path}) = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  -- Auto-login to share if pulling from a non-public path
  when (not $ RemoteRepo.isPublic rrn) $ ensureAuthenticatedWithCodeserver codeserver
  let shareFlavoredPath = Share.Path (repo Nel.:| coerce @[NameSegment] @[Text] (Path.toList path))
  Cli.Env {authHTTPClient, codebase} <- ask
  let pull :: IO (Either (Share.SyncError Share.PullError) CausalHash)
      pull =
        withEntitiesDownloadedProgressCallback \downloadedCallback ->
          Share.pull
            authHTTPClient
            baseURL
            (Codebase.withConnectionIO codebase)
            shareFlavoredPath
            downloadedCallback
  causalHash <-
    Cli.ioE pull \err0 ->
      (Cli.returnEarly . Output.ShareError) case err0 of
        Share.SyncError err -> Output.ShareErrorPull err
        Share.TransportError err -> Output.ShareErrorTransport err
  liftIO (Codebase.getBranchForHash codebase (Cv.causalHash2to1 causalHash)) & onNothingM do
    error $ reportBug "E412939" "`pull` \"succeeded\", but I can't find the result in the codebase. (This is a bug.)"
  where
    -- Provide the given action a callback that display to the terminal.
    withEntitiesDownloadedProgressCallback :: ((Int -> IO ()) -> IO a) -> IO a
    withEntitiesDownloadedProgressCallback action = do
      entitiesDownloadedVar <- newTVarIO 0
      Console.Regions.displayConsoleRegions do
        Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
          Console.Regions.setConsoleRegion region do
            entitiesDownloaded <- readTVar entitiesDownloadedVar
            pure $
              "\n  Downloaded "
                <> tShow entitiesDownloaded
                <> " entities...\n\n"
          result <- action (\n -> atomically (modifyTVar' entitiesDownloadedVar (+ n)))
          entitiesDownloaded <- readTVarIO entitiesDownloadedVar
          Console.Regions.finishConsoleRegion region $
            "\n  Downloaded " <> tShow entitiesDownloaded <> " entities."
          pure result

-- todo: compare to `getHQTerms` / `getHQTypes`.  Is one universally better?
resolveHQToLabeledDependencies :: HQ.HashQualified Name -> Cli r (Set LabeledDependency)
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
      Cli.Env {codebase} <- ask
      terms <- liftIO (Backend.termReferentsByShortHash codebase sh)
      types <- liftIO (Backend.typeReferencesByShortHash codebase sh)
      pure $ Set.map LD.referent terms <> Set.map LD.typeRef types

doDisplay :: OutputLocation -> NamesWithHistory -> Term Symbol () -> Cli r ()
doDisplay outputLoc names tm = do
  Cli.Env {codebase} <- ask
  loopState <- State.get

  ppe <- prettyPrintEnvDecl names
  let (tms, typs) = maybe mempty UF.indexByReference (loopState ^. #latestTypecheckedFile)
  let loc = case outputLoc of
        ConsoleLocation -> Nothing
        FileLocation path -> Just path
        LatestFileLocation -> fmap fst (loopState ^. #latestFile) <|> Just "scratch.u"
      useCache = True
      evalTerm tm =
        fmap ErrorUtil.hush . fmap (fmap Term.unannotate) $
          evalUnisonTermE True (PPE.suffixifiedPPE ppe) useCache (Term.amap (const External) tm)
      loadTerm (Reference.DerivedId r) = case Map.lookup r tms of
        Nothing -> fmap (fmap Term.unannotate) $ liftIO (Codebase.getTerm codebase r)
        Just (tm, _) -> pure (Just $ Term.unannotate tm)
      loadTerm _ = pure Nothing
      loadDecl (Reference.DerivedId r) = case Map.lookup r typs of
        Nothing -> fmap (fmap $ DD.amap (const ())) $ liftIO (Codebase.getTypeDeclaration codebase r)
        Just decl -> pure (Just $ DD.amap (const ()) decl)
      loadDecl _ = pure Nothing
      loadTypeOfTerm' (Referent.Ref (Reference.DerivedId r))
        | Just (_, ty) <- Map.lookup r tms = pure $ Just (void ty)
      loadTypeOfTerm' r = fmap (fmap void) . loadTypeOfTerm codebase $ r
  rendered <- DisplayValues.displayTerm ppe (liftIO . loadTerm) (liftIO . loadTypeOfTerm') evalTerm loadDecl tm
  Cli.respond $ DisplayRendered loc rendered

getLinks ::
  SrcLoc ->
  Path.HQSplit' ->
  Either (Set Reference) (Maybe String) ->
  Cli
    r
    ( PPE.PrettyPrintEnv,
      --  e.g. ("Foo.doc", #foodoc, Just (#builtin.Doc)
      [(HQ.HashQualified Name, Reference, Maybe (Type Symbol Ann))]
    )
getLinks srcLoc src =
  getLinks' src <=< \case
    Left s -> pure (Just s)
    Right Nothing -> pure Nothing
    Right (Just mdTypeStr) -> do
      typ <- parseType srcLoc mdTypeStr
      pure (Just (Set.singleton (Hashing.typeToReference typ)))

getLinks' ::
  Path.HQSplit' -> -- definition to print metadata of
  Maybe (Set Reference) -> -- return all metadata if empty
  Cli
    r
    ( PPE.PrettyPrintEnv,
      --  e.g. ("Foo.doc", #foodoc, Just (#builtin.Doc)
      [(HQ.HashQualified Name, Reference, Maybe (Type Symbol Ann))]
    )
getLinks' src selection0 = do
  Cli.Env {codebase} <- ask
  root0 <- Cli.getRootBranch0
  p <- Path.convert <$> Cli.resolveSplit' src -- ex: the (parent,hqsegment) of `List.map` - `List`
  let -- all metadata (type+value) associated with name `src`
      allMd =
        R4.d34 (BranchUtil.getTermMetadataHQNamed p root0)
          <> R4.d34 (BranchUtil.getTypeMetadataHQNamed p root0)
      allMd' = maybe allMd (`R.restrictDom` allMd) selection0
      -- then list the values after filtering by type
      allRefs :: Set Reference = R.ran allMd'
  sigs <- for (toList allRefs) (liftIO . loadTypeOfTerm codebase . Referent.Ref)
  let deps =
        Set.map LD.termRef allRefs
          <> Set.unions [Set.map LD.typeRef . Type.dependencies $ t | Just t <- sigs]
  ppe <- prettyPrintEnvDecl =<< makePrintNamesFromLabeled' deps
  let ppeDecl = PPE.unsuffixifiedPPE ppe
  let sortedSigs = sortOn snd (toList allRefs `zip` sigs)
  let out = [(PPE.termName ppeDecl (Referent.Ref r), r, t) | (r, t) <- sortedSigs]
  pure (PPE.suffixifiedPPE ppe, out)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatchNoSync ::
  Patch ->
  Path.Absolute ->
  Cli r Bool
propagatePatchNoSync patch scopePath =
  Cli.time "propagatePatch" do
    stepAtNoSync' (Path.unabsolute scopePath, Propagate.propagateAndApply patch)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch ::
  Text ->
  Patch ->
  Path.Absolute ->
  Cli r Bool
propagatePatch inputDescription patch scopePath = do
  Cli.time "propagatePatch" do
    stepAt'
      (inputDescription <> " (applying patch)")
      (Path.unabsolute scopePath, Propagate.propagateAndApply patch)

-- | Create the args needed for showTodoOutput and call it
doShowTodoOutput :: Patch -> Path.Absolute -> Cli r ()
doShowTodoOutput patch scopePath = do
  names0 <- Branch.toNames <$> Cli.getBranch0At scopePath
  -- only needs the local references to check for obsolete defs
  let getPpe = do
        names <- makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
        prettyPrintEnvDecl names
  showTodoOutput getPpe patch names0

-- | Show todo output if there are any conflicts or edits.
showTodoOutput ::
  -- | Action that fetches the pretty print env. It's expensive because it
  -- involves looking up historical names, so only call it if necessary.
  Cli r PPE.PrettyPrintEnvDecl ->
  Patch ->
  Names ->
  Cli r ()
showTodoOutput getPpe patch names0 = do
  todo <- checkTodo patch names0
  if TO.noConflicts todo && TO.noEdits todo
    then Cli.respond NoConflictsOrEdits
    else do
      #numberedArgs
        .= ( Text.unpack . Reference.toText . view _2
               <$> fst (TO.todoFrontierDependents todo)
           )
      ppe <- getPpe
      Cli.respondNumbered $ TodoOutput ppe todo

checkTodo :: Patch -> Names -> Cli r (TO.TodoOutput Symbol Ann)
checkTodo patch names0 = do
  Cli.Env {codebase} <- ask
  let shouldUpdate = Names.contains names0
  f <- Propagate.computeFrontier (liftIO . Codebase.dependents codebase) patch shouldUpdate
  let dirty = R.dom f
      frontier = R.ran f
  (frontierTerms, frontierTypes) <- loadDisplayInfo frontier
  (dirtyTerms, dirtyTypes) <- loadDisplayInfo dirty
  -- todo: something more intelligent here?
  let scoreFn = const 1
  remainingTransitive <-
    frontierTransitiveDependents (liftIO . Codebase.dependents codebase) names0 frontier
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

confirmedCommand :: Input -> Cli r Bool
confirmedCommand i = do
  loopState <- State.get
  pure $ Just i == (loopState ^. #lastInput)

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
        lastName = Name.fromSegment last
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

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch ::
  Branch.MergeMode ->
  Text ->
  Maybe Output ->
  Branch IO ->
  Maybe Path.Path' ->
  Path.Absolute ->
  Cli r ()
mergeBranchAndPropagateDefaultPatch mode inputDescription unchangedMessage srcb maybeDest0 dest =
  ifM
    mergeBranch
    (loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest)
    (for_ unchangedMessage Cli.respond)
  where
    mergeBranch :: Cli r Bool
    mergeBranch =
      Cli.time "mergeBranch" do
        Cli.Env {codebase} <- ask
        destb <- Cli.getBranchAt dest
        merged <- liftIO (Branch.merge'' (Codebase.lca codebase) mode srcb destb)
        b <- Cli.updateAtM inputDescription dest (const $ pure merged)
        for_ maybeDest0 \dest0 -> do
          (ppe, diff) <- diffHelper (Branch.head destb) (Branch.head merged)
          Cli.respondNumbered (ShowDiffAfterMerge dest0 dest ppe diff)
        pure b

loadPropagateDiffDefaultPatch ::
  Text ->
  Maybe Path.Path' ->
  Path.Absolute ->
  Cli r ()
loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest = do
  Cli.time "loadPropagateDiffDefaultPatch" do
    original <- Cli.getBranch0At dest
    patch <- liftIO $ Branch.getPatch Cli.defaultPatchNameSegment original
    patchDidChange <- propagatePatch inputDescription patch dest
    when patchDidChange do
      whenJust maybeDest0 \dest0 -> do
        patched <- Cli.getBranchAt dest
        let patchPath = snoc dest0 Cli.defaultPatchNameSegment
        (ppe, diff) <- diffHelper original (Branch.head patched)
        Cli.respondNumbered (ShowDiffAfterMergePropagate dest0 dest patchPath ppe diff)

-- | Get the set of terms related to a hash-qualified name.
getHQTerms :: HQ.HashQualified Name -> Cli r (Set Referent)
getHQTerms = \case
  HQ.NameOnly n -> do
    root0 <- Cli.getRootBranch0
    currentPath' <- Cli.getCurrentPath
    -- absolute-ify the name, then lookup in deepTerms of root
    let path =
          n
            & Path.fromName'
            & Path.resolve currentPath'
            & Path.unabsolute
            & Path.unsafeToName
    pure $ R.lookupRan path (Branch.deepTerms root0)
  HQ.HashOnly sh -> hashOnly sh
  HQ.HashQualified _ sh -> hashOnly sh
  where
    hashOnly sh = do
      Cli.Env {codebase} <- ask
      liftIO (Backend.termReferentsByShortHash codebase sh)

stepAt ::
  Text ->
  (Path, Branch0 IO -> Branch0 IO) ->
  Cli r ()
stepAt cause = stepManyAt @[] cause . pure

stepAt' ::
  Text ->
  (Path, Branch0 IO -> Cli r (Branch0 IO)) ->
  Cli r Bool
stepAt' cause = stepManyAt' @[] cause . pure

stepAtNoSync' ::
  (Path, Branch0 IO -> Cli r (Branch0 IO)) ->
  Cli r Bool
stepAtNoSync' = stepManyAtNoSync' @[] . pure

stepAtNoSync ::
  (Path, Branch0 IO -> Branch0 IO) ->
  Cli r ()
stepAtNoSync = stepManyAtNoSync @[] . pure

stepAtM ::
  Text ->
  (Path, Branch0 IO -> IO (Branch0 IO)) ->
  Cli r ()
stepAtM cause = stepManyAtM @[] cause . pure

stepManyAt ::
  Foldable f =>
  Text ->
  f (Path, Branch0 IO -> Branch0 IO) ->
  Cli r ()
stepManyAt reason actions = do
  stepManyAtNoSync actions
  syncRoot reason

stepManyAt' ::
  Foldable f =>
  Text ->
  f (Path, Branch0 IO -> Cli r (Branch0 IO)) ->
  Cli r Bool
stepManyAt' reason actions = do
  res <- stepManyAtNoSync' actions
  syncRoot reason
  pure res

stepManyAtNoSync' ::
  Foldable f =>
  f (Path, Branch0 IO -> Cli r (Branch0 IO)) ->
  Cli r Bool
stepManyAtNoSync' actions = do
  origRoot <- Cli.getRootBranch
  newRoot <- Branch.stepManyAtM actions origRoot
  Cli.setRootBranch newRoot
  pure (origRoot /= newRoot)

-- Like stepManyAt, but doesn't update the last saved root
stepManyAtNoSync ::
  Foldable f =>
  f (Path, Branch0 IO -> Branch0 IO) ->
  Cli r ()
stepManyAtNoSync actions =
  void $ Cli.modifyRootBranch $ Branch.stepManyAt actions

stepManyAtM ::
  Foldable f =>
  Text ->
  f (Path, Branch0 IO -> IO (Branch0 IO)) ->
  Cli r ()
stepManyAtM reason actions = do
  stepManyAtMNoSync actions
  syncRoot reason

stepManyAtMNoSync ::
  Foldable f =>
  f (Path, Branch0 IO -> IO (Branch0 IO)) ->
  Cli r ()
stepManyAtMNoSync actions = do
  oldRoot <- Cli.getRootBranch
  newRoot <- liftIO (Branch.stepManyAtM actions oldRoot)
  Cli.setRootBranch newRoot

-- | Sync the in-memory root branch.
syncRoot :: Text -> Cli r ()
syncRoot description = do
  rootBranch <- Cli.getRootBranch
  Cli.updateRoot rootBranch description

-- | Goal: When deleting, we might be removing the last name of a given definition (i.e. the
-- definition is going "extinct"). In this case we may wish to take some action or warn the
-- user about these "endangered" definitions which would now contain unnamed references.
getEndangeredDependents ::
  -- | Which names we want to delete
  Names ->
  -- | All names from the root branch
  Names ->
  -- | map from references going extinct to the set of endangered dependents
  Cli r (Map LabeledDependency (NESet LabeledDependency))
getEndangeredDependents namesToDelete rootNames = do
  Cli.Env {codebase} <- ask
  let remainingNames :: Names
      remainingNames = rootNames `Names.difference` namesToDelete
      refsToDelete, remainingRefs, extinct :: Set LabeledDependency
      refsToDelete = Names.labeledReferences namesToDelete
      remainingRefs = Names.labeledReferences remainingNames -- left over after delete
      extinct = refsToDelete `Set.difference` remainingRefs -- deleting and not left over
      accumulateDependents :: LabeledDependency -> IO (Map LabeledDependency (Set LabeledDependency))
      accumulateDependents ld =
        let ref = LD.fold id Referent.toReference ld
         in Map.singleton ld . Set.map LD.termRef <$> Codebase.dependents codebase ref
  -- All dependents of extinct, including terms which might themselves be in the process of being deleted.
  allDependentsOfExtinct :: Map LabeledDependency (Set LabeledDependency) <-
    liftIO (Map.unionsWith (<>) <$> for (Set.toList extinct) accumulateDependents)

  -- Filtered to only include dependencies which are not being deleted, but depend one which
  -- is going extinct.
  let extinctToEndangered :: Map LabeledDependency (NESet LabeledDependency)
      extinctToEndangered =
        allDependentsOfExtinct & Map.mapMaybe \endangeredDeps ->
          let remainingEndangered = endangeredDeps `Set.intersection` remainingRefs
           in NESet.nonEmptySet remainingEndangered
  pure extinctToEndangered

displayI ::
  Names ->
  OutputLocation ->
  HQ.HashQualified Name ->
  Cli r ()
displayI prettyPrintNames outputLoc hq = do
  let bias = maybeToList $ HQ.toName hq
  latestTypecheckedFile <- Cli.getLatestTypecheckedFile
  case addWatch (HQ.toString hq) latestTypecheckedFile of
    Nothing -> do
      let parseNames = (`NamesWithHistory.NamesWithHistory` mempty) prettyPrintNames
          results = NamesWithHistory.lookupHQTerm hq parseNames
      ref <-
        Set.asSingleton results & onNothing do
          Cli.returnEarly
            if Set.null results
              then SearchTermsNotFound [hq]
              else TermAmbiguous hq results
      let tm = Term.fromReferent External ref
      pped <- prettyPrintEnvDecl parseNames
      tm <- evalUnisonTerm True (PPE.biasTo bias $ PPE.suffixifiedPPE pped) True tm
      doDisplay outputLoc parseNames (Term.unannotate tm)
    Just (toDisplay, unisonFile) -> do
      ppe <- PPE.biasTo bias <$> executePPE unisonFile
      (_, watches) <- evalUnisonFile True ppe unisonFile []
      (_, _, _, _, tm, _) <-
        Map.lookup toDisplay watches & onNothing (error $ "Evaluation dropped a watch expression: " <> HQ.toString hq)
      ns <- displayNames unisonFile
      doDisplay outputLoc ns tm

docsI :: SrcLoc -> Names -> Path.HQSplit' -> Cli r ()
docsI srcLoc prettyPrintNames src =
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
          hq' = Path.unsafeToName' <$> Name.convert src
       in Name.convert hq'

    dotDoc :: HQ.HashQualified Name
    dotDoc = hq <&> \n -> Name.joinDot n "doc"

    fileByName :: Cli r ()
    fileByName = do
      loopState <- State.get
      let ns = maybe mempty UF.typecheckedToNames (loopState ^. #latestTypecheckedFile)
      fnames <- pure $ NamesWithHistory.NamesWithHistory ns mempty
      case NamesWithHistory.lookupHQTerm dotDoc fnames of
        s | Set.size s == 1 -> do
          -- the displayI command expects full term names, so we resolve
          -- the hash back to its full name in the file
          fname' <- pure $ NamesWithHistory.longestTermName 10 (Set.findMin s) fnames
          displayI prettyPrintNames ConsoleLocation fname'
        _ -> codebaseByMetadata

    codebaseByMetadata :: Cli r ()
    codebaseByMetadata = do
      (ppe, out) <- getLinks srcLoc src (Left $ Set.fromList [DD.docRef, DD.doc2Ref])
      case out of
        [] -> codebaseByName
        [(_name, ref, _tm)] -> do
          Cli.Env {codebase} <- ask
          len <- liftIO (Codebase.branchHashLength codebase)
          let names = NamesWithHistory.NamesWithHistory prettyPrintNames mempty
          let tm = Term.ref External ref
          tm <- evalUnisonTerm True (PPE.fromNames len names) True tm
          doDisplay ConsoleLocation names (Term.unannotate tm)
        out -> do
          #numberedArgs .= fmap (HQ.toString . view _1) out
          Cli.respond $ ListOfLinks ppe out

    codebaseByName :: Cli r ()
    codebaseByName = do
      parseNames <- basicParseNames
      case NamesWithHistory.lookupHQTerm dotDoc (NamesWithHistory.NamesWithHistory parseNames mempty) of
        s
          | Set.size s == 1 -> displayI prettyPrintNames ConsoleLocation dotDoc
          | Set.size s == 0 -> Cli.respond $ ListOfLinks PPE.empty []
          -- todo: return a list of links here too
          | otherwise -> Cli.respond $ ListOfLinks PPE.empty []

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
        SC.terms slurp <> UF.constructorsForDecls (SC.types slurp) uf
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
  Cli
    r
    ( [(Reference, Maybe (Type Symbol Ann))],
      [(Reference, DisplayObject () (DD.Decl Symbol Ann))]
    )
loadDisplayInfo refs = do
  Cli.Env {codebase} <- ask
  termRefs <- filterM (liftIO . Codebase.isTerm codebase) (toList refs)
  typeRefs <- filterM (liftIO . Codebase.isType codebase) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> liftIO (Codebase.getTypeOfTerm codebase r)
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayObject r
  pure (terms, types)

-- Any absolute names in the input which have `currentPath` as a prefix
-- are converted to names relative to current path. all other names are
-- converted to absolute names. For example:
--
-- e.g. if Command.currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names -> Names
fixupNamesRelative currentPath' = Names.map fixName
  where
    fixName n =
      if currentPath' == Path.absoluteEmpty
        then n
        else fromMaybe (Name.makeAbsolute n) do
          prefix <- Path.toName (Path.unabsolute currentPath')
          Name.stripNamePrefix prefix n

makeHistoricalParsingNames :: Set (HQ.HashQualified Name) -> Cli r NamesWithHistory
makeHistoricalParsingNames lexedHQs = do
  currentPath <- Cli.getCurrentPath

  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- basicParseNames
  pure $
    NamesWithHistory
      basicNames
      ( Names.makeAbsolute rawHistoricalNames
          <> fixupNamesRelative currentPath rawHistoricalNames
      )

loadTypeDisplayObject :: Reference -> Cli r (DisplayObject () (DD.Decl Symbol Ann))
loadTypeDisplayObject = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id -> do
    Cli.Env {codebase} <- ask
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> liftIO (Codebase.getTypeDeclaration codebase id)

lexedSource :: Text -> Text -> Cli r (NamesWithHistory, (Text, [L.Token L.Lexeme]))
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

suffixifiedPPE :: NamesWithHistory -> Cli r PPE.PrettyPrintEnv
suffixifiedPPE ns = do
  Cli.Env {codebase} <- ask
  liftIO (Codebase.hashLength codebase) <&> (`PPE.fromSuffixNames` ns)

fqnPPE :: NamesWithHistory -> Cli r PPE.PrettyPrintEnv
fqnPPE ns = do
  Cli.Env {codebase} <- ask
  liftIO (Codebase.hashLength codebase) <&> (`PPE.fromNames` ns)

parseSearchType :: SrcLoc -> String -> Cli r (Type Symbol Ann)
parseSearchType srcLoc typ = Type.removeAllEffectVars <$> parseType srcLoc typ

-- | A description of where the given parse was triggered from, for error messaging purposes.
type SrcLoc = String

parseType :: SrcLoc -> String -> Cli r (Type Symbol Ann)
parseType input src = do
  -- `show Input` is the name of the "file" being lexed
  (names0, lexed) <- lexedSource (Text.pack input) (Text.pack src)
  parseNames <- basicParseNames
  let names =
        NamesWithHistory.push
          (NamesWithHistory.currentNames names0)
          (NamesWithHistory.NamesWithHistory parseNames (NamesWithHistory.oldNames names0))
  typ <-
    Parsers.parseType (Text.unpack (fst lexed)) (Parser.ParsingEnv mempty names) & onLeft \err ->
      Cli.returnEarly (TypeParseError src err)

  Type.bindNames mempty (NamesWithHistory.currentNames names) (Type.generalizeLowercase mempty typ) & onLeft \errs ->
    Cli.returnEarly (ParseResolutionFailures src (toList errs))

makeShadowedPrintNamesFromLabeled :: Set LabeledDependency -> Names -> Cli r NamesWithHistory
makeShadowedPrintNamesFromLabeled deps shadowing =
  NamesWithHistory.shadowing shadowing <$> makePrintNamesFromLabeled' deps

makePrintNamesFromLabeled' :: Set LabeledDependency -> Cli r NamesWithHistory
makePrintNamesFromLabeled' deps = do
  root' <- Cli.getRootBranch
  curPath <- Cli.getCurrentPath
  (_missing, rawHistoricalNames) <-
    liftIO $
      Branch.findHistoricalRefs
        deps
        root'
  basicNames <- basicPrettyPrintNamesA
  pure $ NamesWithHistory basicNames (fixupNamesRelative curPath rawHistoricalNames)

getTermsIncludingHistorical :: Monad m => Path.HQSplit -> Branch0 m -> Cli r (Set Referent)
getTermsIncludingHistorical (p, hq) b = case Set.toList refs of
  [] -> case hq of
    HQ'.HashQualified n hs -> do
      names <- findHistoricalHQs (Set.fromList [HQ.HashQualified (Name.fromSegment n) hs])
      pure . R.ran $ Names.terms names
    _ -> pure Set.empty
  _ -> pure refs
  where
    refs = BranchUtil.getTerm (p, hq) b

-- discards inputs that aren't hashqualified;
-- I'd enforce it with finer-grained types if we had them.
findHistoricalHQs :: Set (HQ.HashQualified Name) -> Cli r Names
findHistoricalHQs lexedHQs0 = do
  root' <- Cli.getRootBranch
  curPath <- Cli.getCurrentPath
  let -- omg this nightmare name-to-path parsing code is littered everywhere.
      -- We need to refactor so that the absolute-ness of a name isn't represented
      -- by magical text combinations.
      -- Anyway, this function takes a name, tries to determine whether it is
      -- relative or absolute, and tries to return the corresponding name that is
      -- /relative/ to the Command.root.
      preprocess n = case Name.toString n of
        -- some absolute name that isn't just "."
        '.' : t@(_ : _) -> Name.unsafeFromString t
        -- something in current path
        _ ->
          case curPath of
            p@(_ :> _) -> Name.joinDot (Path.unsafeToName . Path.unabsolute $ p) n
            _ -> n

      lexedHQs = Set.map (fmap preprocess) . Set.filter HQ.hasHash $ lexedHQs0
  (_missing, rawHistoricalNames) <- liftIO $ Branch.findHistoricalHQs lexedHQs root'
  pure rawHistoricalNames

basicPrettyPrintNamesA :: Cli r Names
basicPrettyPrintNamesA = snd <$> basicNames' Backend.AllNames

makeShadowedPrintNamesFromHQ :: Set (HQ.HashQualified Name) -> Names -> Cli r NamesWithHistory
makeShadowedPrintNamesFromHQ lexedHQs shadowing = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- basicPrettyPrintNamesA
  currentPath <- Cli.getCurrentPath
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    NamesWithHistory.shadowing
      shadowing
      (NamesWithHistory basicNames (fixupNamesRelative currentPath rawHistoricalNames))

basicParseNames :: Cli r Names
basicParseNames =
  fst <$> basicNames' Backend.Within

-- implementation detail of basicParseNames and basicPrettyPrintNames
basicNames' :: (Path -> Backend.NameScoping) -> Cli r (Names, Names)
basicNames' nameScoping = do
  root' <- Cli.getRootBranch
  currentPath' <- Cli.getCurrentPath
  let (parse, pretty, _local) = Backend.namesForBranch root' (nameScoping $ Path.unabsolute currentPath')
  pure (parse, pretty)

data GetTermResult
  = NoTermWithThatName
  | TermHasBadType (Type Symbol Ann)
  | GetTermSuccess (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)

-- Adds a watch expression of the given name to the file, if
-- it would resolve to a TLD in the file. Returns the freshened
-- variable name and the new typechecked file.
--
-- Otherwise, returns `Nothing`.
addWatch ::
  Var v =>
  String ->
  Maybe (TypecheckedUnisonFile v Ann) ->
  Maybe (v, TypecheckedUnisonFile v Ann)
addWatch _watchName Nothing = Nothing
addWatch watchName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == watchName) . view _1) components
  case mainComponent of
    [(v, tm, ty)] ->
      Just $
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

addSavedTermToUnisonFile :: Name -> Cli r (TypecheckedUnisonFile Symbol Ann)
addSavedTermToUnisonFile resultName = do
  let resultSymbol = Name.toVar resultName
  (trm, typ, uf) <-
    use #lastRunResult >>= \case
      Nothing -> Cli.returnEarly NoLastRunResult
      Just x -> pure x
  case Map.lookup resultSymbol (UF.hashTermsId uf) of
    Just _ -> Cli.returnEarly (SaveTermNameConflict resultName)
    Nothing -> pure ()
  pure $
    UF.typecheckedUnisonFile
      (UF.dataDeclarationsId' uf)
      (UF.effectDeclarationsId' uf)
      ([(resultSymbol, trm, typ)] : UF.topLevelComponents' uf)
      (UF.watchComponents uf)

-- | Look up runnable term with the given name in the codebase or
-- latest typechecked unison file. Return its symbol, term, type, and
-- the type of the evaluated term.
getTerm :: String -> Cli r (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)
getTerm main =
  getTerm' main >>= \case
    NoTermWithThatName -> do
      mainType <- Runtime.mainType <$> view #runtime
      basicPrettyPrintNames <- getBasicPrettyPrintNames
      ppe <- suffixifiedPPE (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
      Cli.returnEarly $ NoMainFunction main ppe [mainType]
    TermHasBadType ty -> do
      mainType <- Runtime.mainType <$> view #runtime
      basicPrettyPrintNames <- getBasicPrettyPrintNames
      ppe <- suffixifiedPPE (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
      Cli.returnEarly $ BadMainFunction main ty ppe [mainType]
    GetTermSuccess x -> pure x

getTerm' :: String -> Cli r GetTermResult
getTerm' mainName =
  let getFromCodebase = do
        Cli.Env {codebase, runtime} <- ask

        parseNames <- basicParseNames
        let loadTypeOfTerm ref = liftIO (Codebase.getTypeOfTerm codebase ref)
        mainToFile
          =<< MainTerm.getMainTerm loadTypeOfTerm parseNames mainName (Runtime.mainType runtime)
        where
          mainToFile (MainTerm.NotAFunctionName _) = pure NoTermWithThatName
          mainToFile (MainTerm.NotFound _) = pure NoTermWithThatName
          mainToFile (MainTerm.BadType _ ty) = pure $ maybe NoTermWithThatName TermHasBadType ty
          mainToFile (MainTerm.Success hq tm typ) =
            let v = Var.named (HQ.toText hq)
             in checkType typ \otyp ->
                  pure (GetTermSuccess (v, tm, typ, otyp))
      getFromFile uf = do
        let components = join $ UF.topLevelComponents uf
        let mainComponent = filter ((\v -> Var.nameStr v == mainName) . view _1) components
        case mainComponent of
          [(v, tm, ty)] ->
            checkType ty \otyp ->
              let runMain = DD.forceTerm a a (Term.var a v)
                  v2 = Var.freshIn (Set.fromList [v]) v
                  a = ABT.annotation tm
               in pure (GetTermSuccess (v2, runMain, ty, otyp))
          _ -> getFromCodebase
      checkType :: Type Symbol Ann -> (Type Symbol Ann -> Cli r GetTermResult) -> Cli r GetTermResult
      checkType ty f = do
        Cli.Env {runtime} <- ask
        case Typechecker.fitsScheme ty (Runtime.mainType runtime) of
          True -> f (synthesizeForce ty)
          False -> pure (TermHasBadType ty)
   in Cli.getLatestTypecheckedFile >>= \case
        Nothing -> getFromCodebase
        Just uf -> getFromFile uf

-- | Produce a typechecked unison file where the given term is the
-- only watcher, with the watch type set to 'magicMainWatcherString'.
createWatcherFile :: Symbol -> Term Symbol Ann -> Type Symbol Ann -> Cli r (TypecheckedUnisonFile Symbol Ann)
createWatcherFile v tm typ =
  Cli.getLatestTypecheckedFile >>= \case
    Nothing -> pure (UF.typecheckedUnisonFile mempty mempty mempty [(magicMainWatcherString, [(v, tm, typ)])])
    Just uf ->
      let v2 = Var.freshIn (Set.fromList [v]) v
       in pure $
            UF.typecheckedUnisonFile
              (UF.dataDeclarationsId' uf)
              (UF.effectDeclarationsId' uf)
              (UF.topLevelComponents' uf)
              -- what about main's component? we have dropped them if they existed.
              [(magicMainWatcherString, [(v2, tm, typ)])]

executePPE ::
  Var v =>
  TypecheckedUnisonFile v a ->
  Cli r PPE.PrettyPrintEnv
executePPE unisonFile =
  suffixifiedPPE =<< displayNames unisonFile

-- Produce a `Names` needed to display all the hashes used in the given file.
displayNames ::
  Var v =>
  TypecheckedUnisonFile v a ->
  Cli r NamesWithHistory
displayNames unisonFile =
  -- voodoo
  makeShadowedPrintNamesFromLabeled
    (UF.termSignatureExternalLabeledDependencies unisonFile)
    (UF.typecheckedToNames unisonFile)

diffHelper ::
  Branch0 IO ->
  Branch0 IO ->
  Cli r (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffHelper before after =
  Cli.time "diffHelper" do
    Cli.Env {codebase} <- ask
    rootBranch <- Cli.getRootBranch
    currentPath <- Cli.getCurrentPath
    hqLength <- liftIO (Codebase.hashLength codebase)
    diff <- liftIO (BranchDiff.diff0 before after)
    let (_parseNames, prettyNames0, _local) = Backend.namesForBranch rootBranch (Backend.AllNames $ Path.unabsolute currentPath)
    ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl (NamesWithHistory prettyNames0 mempty)
    liftIO do
      fmap (ppe,) do
        OBranchDiff.toOutput
          (loadTypeOfTerm codebase)
          (declOrBuiltin codebase)
          hqLength
          (Branch.toNames before)
          (Branch.toNames after)
          ppe
          diff

loadTypeOfTerm :: Monad m => Codebase m Symbol Ann -> Referent -> m (Maybe (Type Symbol Ann))
loadTypeOfTerm codebase (Referent.Ref r) = Codebase.getTypeOfTerm codebase r
loadTypeOfTerm codebase (Referent.Con (ConstructorReference (Reference.DerivedId r) cid) _) = do
  decl <- Codebase.getTypeDeclaration codebase r
  case decl of
    Just (either DD.toDataDecl id -> dd) -> pure $ DD.typeOfConstructor dd cid
    Nothing -> pure Nothing
loadTypeOfTerm _ Referent.Con {} =
  error $
    reportBug "924628772" "Attempt to load a type declaration which is a builtin!"

declOrBuiltin :: Applicative m => Codebase m Symbol Ann -> Reference -> m (Maybe (DD.DeclOrBuiltin Symbol Ann))
declOrBuiltin codebase r = case r of
  Reference.Builtin {} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> Codebase.getTypeDeclaration codebase id

hqNameQuery :: [HQ.HashQualified Name] -> Cli r QueryResult
hqNameQuery query = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  hqLength <- liftIO (Codebase.hashLength codebase)
  let parseNames = Backend.parseNamesForBranch root' (Backend.AllNames (Path.unabsolute currentPath))
  let nameSearch = Backend.makeNameSearch hqLength (NamesWithHistory.fromCurrentNames parseNames)
  liftIO (Backend.hqNameQuery codebase nameSearch query)

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
fuzzySelectDefinition :: MonadIO m => Position -> Branch0 m0 -> m (Maybe [HQ.HashQualified Name])
fuzzySelectDefinition pos searchBranch0 = liftIO do
  let termsAndTypes =
        Relation.dom (Names.hashQualifyTermsRelation (Relation.swap $ Branch.deepTerms searchBranch0))
          <> Relation.dom (Names.hashQualifyTypesRelation (Relation.swap $ Branch.deepTypes searchBranch0))
  let inputs :: [HQ.HashQualified Name]
      inputs =
        termsAndTypes
          & Set.toList
          & map (fmap (Name.setPosition pos))
  Fuzzy.fuzzySelect Fuzzy.defaultOptions HQ.toText inputs

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
fuzzySelectNamespace :: MonadIO m => Position -> Branch0 m0 -> m (Maybe [Path'])
fuzzySelectNamespace pos searchBranch0 = liftIO do
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
  Fuzzy.fuzzySelect
    Fuzzy.defaultOptions {Fuzzy.allowMultiSelect = False}
    tShow
    inputs

-- | synthesize the type of forcing a term
--
-- precondition: @fitsScheme typeOfFunc Runtime.mainType@ is satisfied
synthesizeForce :: Type Symbol Ann -> Type Symbol Ann
synthesizeForce typeOfFunc = do
  let term :: Term Symbol Ann
      term = Term.ref External ref
      ref = Reference.DerivedId (Reference.Id (Hash.fromByteString "deadbeef") 0)
      env =
        Typechecker.Env
          { Typechecker._ambientAbilities = [DD.exceptionType External, Type.builtinIO External],
            Typechecker._typeLookup = tl <> Builtin.typeLookup,
            Typechecker._termsByShortname = Map.empty
          }
      tl =
        TypeLookup.TypeLookup
          { TypeLookup.typeOfTerms = Map.singleton ref typeOfFunc,
            TypeLookup.dataDecls = Map.empty,
            TypeLookup.effectDecls = Map.empty
          }
  case Result.runResultT (Typechecker.synthesize env (DD.forceTerm External External term)) of
    Identity (Nothing, notes) ->
      error
        ( unlines
            [ "synthesizeForce fails although fitsScheme passed",
              "Input Type:",
              show typeOfFunc,
              "Notes:",
              show notes
            ]
        )
    Identity (Just typ, _) -> typ

typecheck ::
  [Type Symbol Ann] ->
  NamesWithHistory ->
  Text ->
  (Text, [L.Token L.Lexeme]) ->
  Cli
    r
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either (UF.UnisonFile Symbol Ann) (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheck ambient names sourceName source =
  Cli.time "typecheck" do
    Cli.Env {codebase, generateUniqueName} <- ask
    uniqueName <- liftIO generateUniqueName
    (liftIO . Result.getResult) $
      parseAndSynthesizeFile
        ambient
        (((<> Builtin.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
        (Parser.ParsingEnv uniqueName names)
        (Text.unpack sourceName)
        (fst source)

-- | Evaluate all watched expressions in a UnisonFile and return
-- their results, keyed by the name of the watch variable. The tuple returned
-- has the form:
--   (hash, (ann, sourceTerm, evaluatedTerm, isCacheHit))
--
-- where
--   `hash` is the hash of the original watch expression definition
--   `ann` gives the location of the watch expression
--   `sourceTerm` is a closed term (no free vars) for the watch expression
--   `evaluatedTerm` is the result of evaluating that `sourceTerm`
--   `isCacheHit` is True if the result was computed by just looking up
--   in a cache
--
-- It's expected that the user of this action might add the
-- `(hash, evaluatedTerm)` mapping to a cache to make future evaluations
-- of the same watches instantaneous.
evalUnisonFile ::
  Bool ->
  PPE.PrettyPrintEnv ->
  TypecheckedUnisonFile Symbol Ann ->
  [String] ->
  Cli
    r
    ( [(Symbol, Term Symbol ())],
      Map Symbol (Ann, WK.WatchKind, Reference.Id, Term Symbol (), Term Symbol (), Bool)
    )
evalUnisonFile sandbox ppe unisonFile args = do
  Cli.Env {codebase, runtime, sandboxedRuntime} <- ask
  let theRuntime = if sandbox then sandboxedRuntime else runtime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.lookupWatchCache codebase ref
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  Cli.with_ (withArgs args) do
    rs@(_, map) <-
      Cli.ioE (Runtime.evaluateWatches (Codebase.toCodeLookup codebase) ppe watchCache theRuntime unisonFile) \err -> do
        Cli.returnEarly (EvaluationFailure err)
    for_ (Map.elems map) \(_loc, kind, hash, _src, value, isHit) ->
      when (not isHit) do
        let value' = Term.amap (\() -> Ann.External) value
        liftIO (Codebase.putWatch codebase kind hash value')
    pure rs

-- | Evaluate a single closed definition.
evalUnisonTermE ::
  Bool ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli r (Either Runtime.Error (Term Symbol Ann))
evalUnisonTermE sandbox ppe useCache tm = do
  Cli.Env {codebase, runtime, sandboxedRuntime} <- ask
  let theRuntime = if sandbox then sandboxedRuntime else runtime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.lookupWatchCache codebase ref
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  let cache = if useCache then watchCache else Runtime.noCache
  r <- liftIO (Runtime.evaluateTerm' (Codebase.toCodeLookup codebase) cache ppe theRuntime tm)
  when useCache do
    case r of
      Right tmr ->
        liftIO $
          Codebase.putWatch
            codebase
            WK.RegularWatch
            (Hashing.hashClosedTerm tm)
            (Term.amap (const Ann.External) tmr)
      Left _ -> pure ()
  pure $ r <&> Term.amap (\() -> Ann.External)

-- | Evaluate a single closed definition.
evalUnisonTerm ::
  Bool ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli r (Term Symbol Ann)
evalUnisonTerm sandbox ppe useCache tm =
  evalUnisonTermE sandbox ppe useCache tm & onLeftM \err ->
    Cli.returnEarly (EvaluationFailure err)

getBasicPrettyPrintNames :: Cli r Names
getBasicPrettyPrintNames = do
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  pure (Backend.prettyNamesForBranch rootBranch (Backend.AllNames (Path.unabsolute currentPath)))

-- Hack alert
--
-- After we evaluate a term all vars are transformed into references,
-- but we want to feed this result into 'slurpFile' which won't add
-- dependencies that are referenced by hash. The hacky solution for
-- now is to convert all references that match a variable defined
-- within the unison file to variable references. This is hacky both
-- because we needlessly flip-flopping between var and reference
-- representations, and because we might unexpectedly add a term from
-- the local file if it has the same hash as a term in the codebase.
stripUnisonFileReferences :: TypecheckedUnisonFile Symbol a -> Term Symbol () -> Term Symbol ()
stripUnisonFileReferences unisonFile term =
  let refMap :: Map Reference.Id Symbol
      refMap = Map.fromList . map (\(sym, (refId, _, _, _)) -> (refId, sym)) . Map.toList . UF.hashTermsId $ unisonFile
      alg () = \case
        ABT.Var x -> ABT.var x
        ABT.Cycle x -> ABT.cycle x
        ABT.Abs v x -> ABT.abs v x
        ABT.Tm t -> case t of
          Term.Ref ref
            | Just var <- (\k -> Map.lookup k refMap) =<< Reference.toId ref -> ABT.var var
          x -> ABT.tm x
   in ABT.cata alg term

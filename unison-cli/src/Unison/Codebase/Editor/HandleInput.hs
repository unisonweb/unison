{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Unison.Codebase.Editor.HandleInput
  ( loop,
  )
where

-- TODO: Don't import backend

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import qualified Control.Error.Util as ErrorUtil
import Control.Exception (catch)
import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Writer (WriterT (..))
import Data.Bifunctor (first, second)
import qualified Data.Foldable as Foldable
import qualified Data.Foldable.Extra as Foldable
import qualified Data.List as List
import Data.List.Extra (nubOrd)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Tuple.Extra (uncurry3)
import qualified System.Console.Regions as Console.Regions
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
  )
import System.Environment (withArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process
  ( callProcess,
    readCreateProcess,
    readCreateProcessWithExitCode,
    shell,
  )
import qualified Text.Megaparsec as P
import qualified U.Codebase.Branch.Diff as V2Branch
import qualified U.Codebase.Causal as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import qualified U.Codebase.Reference as V2 (Reference)
import qualified U.Codebase.Reflog as Reflog
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Queries
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as Builtin
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Builtin.Terms as Builtin
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.NamesUtils (basicParseNames, displayNames, findHistoricalHQs, getBasicPrettyPrintNames, makeHistoricalParsingNames, makePrintNamesFromLabeled', makeShadowedPrintNamesFromHQ)
import Unison.Cli.PrettyPrintUtils (currentPrettyPrintEnvDecl, prettyPrintEnvDecl)
import Unison.Cli.TypeCheck (typecheck, typecheckTerm)
import Unison.Cli.UnisonConfigUtils (gitUrlKey, remoteMappingKey)
import Unison.Codebase (Codebase, Preprocessing (..), PushGitBranchOpts (..))
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchUtil as BranchUtil
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.AuthorInfo (AuthorInfo (..))
import qualified Unison.Codebase.Editor.AuthorInfo as AuthorInfo
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin, ensureAuthenticatedWithCodeserver)
import Unison.Codebase.Editor.HandleInput.MetadataUtils (addDefaultMetadata, manageLinks)
import Unison.Codebase.Editor.HandleInput.MoveBranch (doMoveBranch)
import qualified Unison.Codebase.Editor.HandleInput.NamespaceDependencies as NamespaceDependencies
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.HandleInput.TermResolution
  ( resolveCon,
    resolveMainRef,
    resolveTermRef,
  )
import Unison.Codebase.Editor.HandleInput.Update (doSlurpAdds, handleUpdate)
import Unison.Codebase.Editor.Input
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Editor.Output.DumpNamespace as Output.DN
import Unison.Codebase.Editor.Output.PushPull (PushPull (Pull, Push))
import qualified Unison.Codebase.Editor.Propagate as Propagate
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadRemoteNamespace (..),
    ReadShareRemoteNamespace (..),
    ShareUserHandle (..),
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
import qualified Unison.Codebase.Editor.SlurpResult as SlurpResult
import qualified Unison.Codebase.Editor.TodoOutput as TO
import qualified Unison.Codebase.Editor.UriParser as UriParser
import qualified Unison.Codebase.IntegrityCheck as IntegrityCheck (integrityCheckFullCodebase)
import qualified Unison.Codebase.MainTerm as MainTerm
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path, Path' (..))
import qualified Unison.Codebase.Path as HQSplit'
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Codebase.ShortCausalHash as SCH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Conversions
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.TermEdit (TermEdit (..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TermEdit.Typing as TermEdit
import Unison.Codebase.Type (GitPushBehavior (..))
import Unison.Codebase.TypeEdit (TypeEdit)
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
import qualified Unison.Hash as Hash
import Unison.Hash32 (Hash32)
import qualified Unison.Hash32 as Hash32
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.HashQualified' as HashQualified
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
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo, empty)
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPED
import Unison.Reference (Reference (..), TermReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Result as Result
import qualified Unison.Runtime.IOSource as IOSource
import Unison.Server.Backend (ShallowListEntry (..))
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.CodebaseServer as Server
import qualified Unison.Server.Doc.Markdown.Render as Md
import qualified Unison.Server.Doc.Markdown.Types as Md
import Unison.Server.QueryResult
import Unison.Server.SearchResult (SearchResult)
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.Share.Codeserver as Codeserver
import qualified Unison.Share.Sync as Share
import qualified Unison.Share.Sync.Types as Share
import Unison.Share.Types (codeserverBaseURL)
import qualified Unison.ShortHash as SH
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import qualified Unison.Sync.Types as Share
import qualified Unison.Syntax.HashQualified as HQ (fromString, toString, toText, unsafeFromString)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Name as Name (toString, toText, toVar, unsafeFromString, unsafeFromVar)
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Syntax.TermPrinter as TP
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
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Set as Set
import qualified Unison.Util.Star3 as Star3
import Unison.Util.TransitiveClosure (transitiveClosure)
import Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.WatchKind as WK
import qualified UnliftIO.STM as STM
import Web.Browser (openBrowser)

------------------------------------------------------------------------------------------------------------------------
-- Main loop

loop :: Either Event Input -> Cli ()
loop e = do
  let withFile ::
        -- ambient abilities
        [Type Symbol Ann] ->
        Text ->
        (Text, [L.Token L.Lexeme]) ->
        Cli (TypecheckedUnisonFile Symbol Ann)
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

      loadUnisonFile :: Text -> Text -> Cli ()
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
        #latestTypecheckedFile .= Just unisonFile

  case e of
    Left (IncomingRootBranch hashes) -> Cli.time "IncomingRootBranch" do
      schLength <- Cli.runTransaction Codebase.branchHashLength
      rootBranch <- Cli.getRootBranch
      Cli.respond $
        WarnIncomingRootBranch
          (SCH.fromHash schLength $ Branch.headHash rootBranch)
          (Set.map (SCH.fromHash schLength) hashes)
    Left (UnisonFileChanged sourceName text) -> Cli.time "UnisonFileChanged" do
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
          doRemoveReplacement :: HQ.HashQualified Name -> Maybe PatchPath -> Bool -> Cli ()
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
                go :: Text -> Reference -> Cli ()
                go description fr = do
                  let termPatch = over Patch.termEdits (R.deleteDom fr) patch
                      typePatch = over Patch.typeEdits (R.deleteDom fr) patch
                  (patchPath'', patchName) <- Cli.resolveSplit' patchPath'
                  -- Save the modified patch
                  Cli.stepAtM
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
          saveAndApplyPatch :: Path -> NameSegment -> Patch -> Cli ()
          saveAndApplyPatch patchPath'' patchName patch' = do
            description <- inputDescription input
            Cli.stepAtM
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
       in Cli.time "InputPattern" case input of
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
              let numEntriesToShow = 500
              entries <-
                Cli.runTransaction do
                  schLength <- Codebase.branchHashLength
                  Codebase.getReflog numEntriesToShow <&> fmap (first $ SCH.fromHash schLength)
              let moreEntriesToLoad = length entries == numEntriesToShow
              let expandedEntries = List.unfoldr expandEntries (entries, Nothing, moreEntriesToLoad)
              let numberedEntries = expandedEntries <&> \(_time, hash, _reason) -> "#" <> SCH.toString hash
              #numberedArgs .= numberedEntries
              Cli.respond $ ShowReflog expandedEntries
              where
                expandEntries ::
                  ([Reflog.Entry SCH.ShortCausalHash Text], Maybe SCH.ShortCausalHash, Bool) ->
                  Maybe ((Maybe UTCTime, SCH.ShortCausalHash, Text), ([Reflog.Entry SCH.ShortCausalHash Text], Maybe SCH.ShortCausalHash, Bool))
                expandEntries ([], Just expectedHash, moreEntriesToLoad) =
                  if moreEntriesToLoad
                    then Nothing
                    else Just ((Nothing, expectedHash, "history starts here"), ([], Nothing, moreEntriesToLoad))
                expandEntries ([], Nothing, _moreEntriesToLoad) = Nothing
                expandEntries (entries@(Reflog.Entry {time, fromRootCausalHash, toRootCausalHash, reason} : rest), mayExpectedHash, moreEntriesToLoad) =
                  Just $
                    case mayExpectedHash of
                      Just expectedHash
                        | expectedHash == toRootCausalHash -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))
                        -- Historical discontinuity, insert a synthetic entry
                        | otherwise -> ((Nothing, toRootCausalHash, "(external change)"), (entries, Nothing, moreEntriesToLoad))
                      -- No expectation, either because this is the most recent entry or
                      -- because we're recovering from a discontinuity
                      Nothing -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))
            ResetRootI src0 ->
              Cli.time "reset-root" do
                newRoot <-
                  case src0 of
                    Left hash -> Cli.resolveShortCausalHash hash
                    Right path' -> Cli.expectBranchAtPath' path'
                description <- inputDescription input
                Cli.updateRoot newRoot description
                Cli.respond Success
            ForkLocalBranchI src0 dest0 -> do
              srcb <-
                case src0 of
                  Left hash -> Cli.resolveShortCausalHash hash
                  Right path' -> Cli.expectBranchAtPath' path'
              Cli.assertNoBranchAtPath' dest0
              description <- inputDescription input
              dest <- Cli.resolvePath' dest0
              ok <- Cli.updateAtM description dest (const $ pure srcb)
              Cli.respond
                if ok
                  then Success
                  else BranchEmpty case src0 of
                    Left hash -> WhichBranchEmptyHash hash
                    Right path -> WhichBranchEmptyPath path
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
              Cli.updateAt description destAbs $ Branch.step \destBranch0 ->
                destBranch0
                  & Branch.children
                    %~ ( \childMap ->
                           childMap
                             & at "base" ?~ baseb
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
              Cli.stepManyAt
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
              Cli.stepAt
                description
                (BranchUtil.makeReplacePatch (Path.convert dest) p)
              Cli.respond Success
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
              branchExists <- Cli.branchExistsAtPath' path'
              when (not branchExists) (Cli.respond $ CreatedNewBranch path)
              #currentPathStack %= Nel.cons path
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
                  Left hash -> Cli.resolveShortCausalHash hash
                  Right path' -> do
                    path <- Cli.resolvePath' path'
                    Cli.getMaybeBranchAt path & onNothingM (Cli.returnEarly (CreatedNewBranch path))
              schLength <- Cli.runTransaction Codebase.branchHashLength
              history <- liftIO (doHistory schLength 0 branch [])
              Cli.respondNumbered history
              where
                doHistory :: Int -> Int -> Branch IO -> [(CausalHash, NamesWithHistory.Diff)] -> IO NumberedOutput
                doHistory schLength !n b acc =
                  if maybe False (n >=) resultsCap
                    then pure (History diffCap schLength acc (PageEnd (Branch.headHash b) n))
                    else case Branch._history b of
                      Causal.One {} -> pure (History diffCap schLength acc (EndOfLog $ Branch.headHash b))
                      Causal.Merge _ _ _ tails ->
                        pure (History diffCap schLength acc (MergeTail (Branch.headHash b) $ Map.keys tails))
                      Causal.Cons _ _ _ tail -> do
                        b' <- fmap Branch.Branch $ snd tail
                        let elem = (Branch.headHash b, Branch.namesDiff b' b)
                        doHistory schLength (n + 1) b' (elem : acc)
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
            DocToMarkdownI docName -> do
              basicPrettyPrintNames <- getBasicPrettyPrintNames
              hqLength <- Cli.runTransaction Codebase.hashLength
              let pped = PPED.fromNamesDecl hqLength (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
              basicPrettyPrintNames <- basicParseNames
              let nameSearch = Backend.makeNameSearch hqLength (NamesWithHistory.fromCurrentNames basicPrettyPrintNames)
              Cli.Env {codebase, runtime} <- ask
              mdText <- liftIO $ do
                docRefs <- Backend.docsForDefinitionName codebase nameSearch docName
                for docRefs $ \docRef -> do
                  (_, _, doc) <- Backend.renderDoc pped (Pretty.Width 80) runtime codebase docRef
                  pure . Md.toText $ Md.toMarkdown doc
              Cli.respond $ Output.MarkdownOut (Text.intercalate "\n---\n" mdText)
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
                  (Cli.runTransaction . Backend.termReferentsByShortHash codebase)
                  Cli.getTermsAt
                  src
              srcTerm <-
                Set.asSingleton srcTerms & onNothing do
                  Cli.returnEarly =<< case (Set.null srcTerms, src') of
                    (True, Left hash) -> pure (TermNotFound' hash)
                    (True, Right name) -> pure (TermNotFound name)
                    (False, Left hash) -> pure (HashAmbiguous hash srcTerms)
                    (False, Right name) -> do
                      hqLength <- Cli.runTransaction Codebase.hashLength
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
              Cli.stepAt
                description
                (BranchUtil.makeAddTermName (Path.convert dest) srcTerm srcMetadata)
              Cli.respond Success
            AliasTypeI src' dest' -> do
              src <- traverseOf _Right Cli.resolveSplit' src'
              srcTypes <-
                either
                  (Cli.runTransaction . Backend.typeReferencesByShortHash)
                  Cli.getTypesAt
                  src
              srcType <-
                Set.asSingleton srcTypes & onNothing do
                  Cli.returnEarly =<< case (Set.null srcTypes, src') of
                    (True, Left hash) -> pure (TypeNotFound' hash)
                    (True, Right name) -> pure (TypeNotFound name)
                    (False, Left hash) -> pure (HashAmbiguous hash (Set.map Referent.Ref srcTypes))
                    (False, Right name) -> do
                      hqLength <- Cli.runTransaction Codebase.hashLength
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
              Cli.stepAt
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
              Cli.stepManyAt description actions
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
              currentPath' <- Path.unabsolute <$> Cli.getCurrentPath
              hqLength <- Cli.runTransaction Codebase.hashLength
              root <- Cli.getRootBranch
              (names, pped) <-
                if global || any Name.isAbsolute query
                  then do
                    let root0 = Branch.head root
                    let names = NamesWithHistory.fromCurrentNames . Names.makeAbsolute $ Branch.toNames root0
                    -- Use an absolutely qualified ppe for view.global
                    let pped = PPED.fromNamesDecl hqLength names
                    pure (names, pped)
                  else do
                    currentBranch <- Cli.getCurrentBranch0
                    let currentNames = NamesWithHistory.fromCurrentNames $ Branch.toNames currentBranch
                    let pped = Backend.getCurrentPrettyNames hqLength (Backend.Within currentPath') root
                    pure (currentNames, pped)

              let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
                  terms = NamesWithHistory.lookupHQTerm query names
                  types = NamesWithHistory.lookupHQType query names
                  terms' :: [(Referent, [HQ'.HashQualified Name])]
                  terms' = map (\r -> (r, PPE.allTermNames unsuffixifiedPPE r)) (Set.toList terms)
                  types' :: [(Reference, [HQ'.HashQualified Name])]
                  types' = map (\r -> (r, PPE.allTypeNames unsuffixifiedPPE r)) (Set.toList types)
              Cli.respond $ ListNames global hqLength types' terms'
            LinkI mdValue srcs -> do
              description <- inputDescription input
              manageLinks False srcs [mdValue] Metadata.insert
              Cli.syncRoot description
            UnlinkI mdValue srcs -> do
              description <- inputDescription input
              manageLinks False srcs [mdValue] Metadata.delete
              Cli.syncRoot description

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
              Cli.runTransaction (traverse_ (uncurry3 (Codebase.putTerm codebase)) [guid, author, copyrightHolder])
              authorPath <- Cli.resolveSplit' authorPath'
              copyrightHolderPath <- Cli.resolveSplit' (base |> "copyrightHolders" |> authorNameSegment)
              guidPath <- Cli.resolveSplit' (authorPath' |> "guid")
              Cli.stepManyAt
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
                      hqLength <- Cli.runTransaction Codebase.hashLength
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
              Cli.stepManyAt
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
                      hqLength <- Cli.runTransaction Codebase.hashLength
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
              Cli.stepManyAt
                description
                [ -- Mitchell: throwing away any hash-qualification here seems wrong!
                  BranchUtil.makeDeleteTypeName (over _2 HQ'.toName p) srcType,
                  BranchUtil.makeAddTypeName (Path.convert dest) srcType srcMetadata
                ]
              Cli.respond Success
            DeleteI dtarget -> case dtarget of
              DeleteTarget'TermOrType doutput hqs -> delete input doutput Cli.getTermsAt Cli.getTypesAt hqs
              DeleteTarget'Type doutput hqs -> delete input doutput (const (pure Set.empty)) Cli.getTypesAt hqs
              DeleteTarget'Term doutput hqs -> delete input doutput Cli.getTermsAt (const (pure Set.empty)) hqs
              DeleteTarget'Patch src' -> do
                _ <- Cli.expectPatchAt src'
                description <- inputDescription input
                src <- Cli.resolveSplit' src'
                Cli.stepAt
                  description
                  (BranchUtil.makeDeletePatch (Path.convert src))
                Cli.respond Success
              DeleteTarget'Branch insistence Nothing -> do
                hasConfirmed <- confirmedCommand input
                if hasConfirmed || insistence == Force
                  then do
                    description <- inputDescription input
                    Cli.updateRoot Branch.empty description
                    Cli.respond DeletedEverything
                  else Cli.respond DeleteEverythingConfirmation
              DeleteTarget'Branch insistence (Just p@(parentPath, childName)) -> do
                branch <- Cli.expectBranchAtPath' (Path.unsplit' p)
                description <- inputDescription input
                absPath <- Cli.resolveSplit' p
                let toDelete =
                      Names.prefix0
                        (Path.unsafeToName (Path.unsplit (Path.convert absPath)))
                        (Branch.toNames (Branch.head branch))
                afterDelete <- do
                  rootNames <- Branch.toNames <$> Cli.getRootBranch0
                  endangerments <- Cli.runTransaction (getEndangeredDependents toDelete Set.empty rootNames)
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
                parentPathAbs <- Cli.resolvePath' parentPath
                -- We have to modify the parent in order to also wipe out the history at the
                -- child.
                Cli.updateAt description parentPathAbs \parentBranch ->
                  parentBranch
                    & Branch.modifyAt (Path.singleton childName) \_ -> Branch.empty
                afterDelete
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

              pathArgAbs <- Cli.resolvePath' pathArg
              entries <- liftIO (Backend.lsAtPath codebase Nothing pathArgAbs)
              -- caching the result as an absolute path, for easier jumping around
              #numberedArgs .= fmap entryToHQString entries
              getRoot <- atomically . STM.readTMVar <$> use #root
              let buildPPE = do
                    schLength <- Codebase.runTransaction codebase Codebase.branchHashLength
                    rootBranch <- getRoot
                    pure $
                      Backend.basicSuffixifiedNames
                        schLength
                        rootBranch
                        (Backend.AllNames (Path.unabsolute pathArgAbs))
              Cli.respond $ ListShallow buildPPE entries
              where
                entryToHQString :: ShallowListEntry v Ann -> String
                entryToHQString e =
                  fixup case e of
                    ShallowTypeEntry te -> Text.unpack $ Backend.typeEntryDisplayName te
                    ShallowTermEntry te -> Text.unpack $ Backend.termEntryDisplayName te
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
                      hqLength <- Cli.runTransaction Codebase.hashLength
                      Cli.returnEarly (DeleteNameAmbiguous hqLength path' Set.empty types)
              Cli.stepAt
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
                        hqLength <- Cli.runTransaction Codebase.hashLength
                        Cli.returnEarly (DeleteNameAmbiguous hqLength path' terms Set.empty)
              description <- inputDescription input
              terms <- Cli.getTermsAt path
              terms
                & Set.delete term
                & Set.toList
                -- Mitchell: throwing away HQ seems wrong
                & map (BranchUtil.makeDeleteTermName (Path.convert (over _2 HQ'.toName path)))
                & Cli.stepManyAt description
            ReplaceI from to patchPath -> do
              Cli.Env {codebase} <- ask
              hqLength <- Cli.runTransaction Codebase.hashLength

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

                  replaceTerms :: Reference -> Reference -> Cli ()
                  replaceTerms fr tr = do
                    (mft, mtt) <-
                      Cli.runTransaction do
                        mft <- Codebase.getTypeOfTerm codebase fr
                        mtt <- Codebase.getTypeOfTerm codebase tr
                        pure (mft, mtt)
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

                  replaceTypes :: Reference -> Reference -> Cli ()
                  replaceTypes fr tr = do
                    let patch' =
                          -- The modified patch
                          over
                            Patch.typeEdits
                            (R.insert fr (TypeEdit.Replace tr) . R.deleteDom fr)
                            patch
                    (patchPath'', patchName) <- Cli.resolveSplit' patchPath'
                    saveAndApplyPatch (Path.convert patchPath'') patchName patch'

                  ambiguous :: HQ.HashQualified Name -> [TermReference] -> Cli a
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
            ClearI -> Cli.respond ClearScreen
            AddI requestedNames -> do
              description <- inputDescription input
              let vars = Set.map Name.toVar requestedNames
              uf <- Cli.expectLatestTypecheckedFile
              Cli.Env {codebase} <- ask
              currentPath <- Cli.getCurrentPath
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.AddOp currentNames
              let adds = SlurpResult.adds sr
              Cli.stepAtNoSync (Path.unabsolute currentPath, doSlurpAdds adds uf)
              Cli.runTransaction . Codebase.addDefsToCodebase codebase . SlurpResult.filterUnisonFile sr $ uf
              ppe <- prettyPrintEnvDecl =<< displayNames uf
              Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
              addDefaultMetadata adds
              Cli.syncRoot description
            SaveExecuteResultI resultName -> do
              description <- inputDescription input
              let resultVar = Name.toVar resultName
              uf <- addSavedTermToUnisonFile resultName
              Cli.Env {codebase} <- ask
              currentPath <- Cli.getCurrentPath
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf (Set.singleton resultVar) Slurp.AddOp currentNames
              let adds = SlurpResult.adds sr
              Cli.stepAtNoSync (Path.unabsolute currentPath, doSlurpAdds adds uf)
              Cli.runTransaction . Codebase.addDefsToCodebase codebase . SlurpResult.filterUnisonFile sr $ uf
              ppe <- prettyPrintEnvDecl =<< displayNames uf
              addDefaultMetadata adds
              Cli.syncRoot description
              Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
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
              (ref, ppe) <- resolveMainRef main
              let codeLookup = () <$ Codebase.toCodeLookup codebase
              whenJustM (liftIO (Runtime.compileTo runtime codeLookup ppe ref (output <> ".uc"))) \err ->
                Cli.returnEarly (EvaluationFailure err)
            CompileSchemeI output main -> doCompileScheme output main
            ExecuteSchemeI main args -> doRunAsScheme main args
            GenSchemeLibsI -> doGenerateSchemeBoot True Nothing
            FetchSchemeCompilerI name -> doFetchCompiler name
            IOTestI main -> handleIOTest main
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
              Cli.runTransaction (Codebase.addDefsToCodebase codebase uf)
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
              Cli.runTransaction do
                Codebase.addDefsToCodebase codebase uf
                -- these have not necessarily been added yet
                Codebase.addDefsToCodebase codebase IOSource.typecheckedFile'

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
              ppe <- suffixifiedPPE =<< makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
              Cli.respondNumbered $ ListEdits patch ppe
            PullRemoteBranchI mRepo path sMode pMode verbosity ->
              inputDescription input
                >>= doPullRemoteBranch mRepo path sMode pMode verbosity
            PushRemoteBranchI pushRemoteBranchInput -> handlePushRemoteBranch pushRemoteBranchInput
            ListDependentsI hq -> handleDependents hq
            ListDependenciesI hq -> do
              Cli.Env {codebase} <- ask
              hqLength <- Cli.runTransaction Codebase.hashLength
              -- todo: add flag to handle transitive efficiently
              lds <- resolveHQToLabeledDependencies hq
              when (null lds) do
                Cli.returnEarly (LabeledReferenceNotFound hq)
              rootBranch <- Cli.getRootBranch
              for_ lds \ld -> do
                dependencies :: Set Reference <-
                  Cli.runTransaction do
                    let tp r@(Reference.DerivedId i) =
                          Codebase.getTypeDeclaration codebase i <&> \case
                            Nothing -> error $ "What happened to " ++ show i ++ "?"
                            Just decl -> Set.delete r . DD.dependencies $ DD.asDataDecl decl
                        tp _ = pure mempty
                        tm (Referent.Ref r@(Reference.DerivedId i)) =
                          Codebase.getTerm codebase i <&> \case
                            Nothing -> error $ "What happened to " ++ show i ++ "?"
                            Just tm -> Set.delete r $ Term.dependencies tm
                        tm con@(Referent.Con (ConstructorReference (Reference.DerivedId i) cid) _ct) =
                          Codebase.getTypeDeclaration codebase i <&> \case
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
              Cli.Env {codebase} <- ask
              path <- maybe Cli.getCurrentPath Cli.resolvePath' namespacePath'
              Cli.getMaybeBranchAt path >>= \case
                Nothing -> Cli.respond $ BranchEmpty (WhichBranchEmptyPath (Path.absoluteToPath' path))
                Just b -> do
                  externalDependencies <-
                    Cli.runTransaction (NamespaceDependencies.namespaceDependencies codebase (Branch.head b))
                  ppe <- PPE.unsuffixifiedPPE <$> currentPrettyPrintEnvDecl Backend.Within
                  Cli.respond $ ListNamespaceDependencies ppe path externalDependencies
            DebugNumberedArgsI -> do
              numArgs <- use #numberedArgs
              Cli.respond (DumpNumberedArgs numArgs)
            DebugTypecheckedUnisonFileI -> do
              hqLength <- Cli.runTransaction Codebase.hashLength
              uf <- Cli.expectLatestTypecheckedFile
              let datas, effects, terms :: [(Name, Reference.Id)]
                  datas = [(Name.unsafeFromVar v, r) | (v, (r, _d)) <- Map.toList $ UF.dataDeclarationsId' uf]
                  effects = [(Name.unsafeFromVar v, r) | (v, (r, _e)) <- Map.toList $ UF.effectDeclarationsId' uf]
                  terms = [(Name.unsafeFromVar v, r) | (v, (r, _wk, _tm, _tp)) <- Map.toList $ UF.hashTermsId uf]
              Cli.respond $ DumpUnisonFileHashes hqLength datas effects terms
            DebugTabCompletionI inputs -> do
              Cli.Env {authHTTPClient, codebase} <- ask
              currentPath <- Cli.getCurrentPath
              let completionFunc = Completion.haskelineTabComplete IP.patternMap codebase authHTTPClient currentPath
              (_, completions) <- liftIO $ completionFunc (reverse (unwords inputs), "")
              Cli.respond (DisplayDebugCompletions completions)
            DebugDumpNamespacesI -> do
              let seen h = State.gets (Set.member h)
                  set h = State.modify (Set.insert h)
                  getCausal b = (Branch.headHash b, pure $ Branch._history b)
                  goCausal :: forall m. (Monad m) => [(CausalHash, m (Branch.UnwrappedBranch m))] -> StateT (Set CausalHash) m ()
                  goCausal [] = pure ()
                  goCausal ((h, mc) : queue) = do
                    ifM (seen h) (goCausal queue) do
                      lift mc >>= \case
                        Causal.One h _bh b -> goBranch h b mempty queue
                        Causal.Cons h _bh b tail -> goBranch h b [fst tail] (tail : queue)
                        Causal.Merge h _bh b (Map.toList -> tails) -> goBranch h b (map fst tails) (tails ++ queue)
                  goBranch :: forall m. (Monad m) => CausalHash -> Branch0 m -> [CausalHash] -> [(CausalHash, m (Branch.UnwrappedBranch m))] -> StateT (Set CausalHash) m ()
                  goBranch h b (Set.fromList -> causalParents) queue = case b of
                    Branch0 terms0 types0 children0 patches0 _ _ _ _ _ _ _ ->
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
                    P.lit "Namespace "
                      <> P.shown h
                      <> P.newline
                      <> ( P.indentN 2 $
                             P.linesNonEmpty
                               [ Monoid.unlessM (null causalParents) $ P.lit "Causal Parents:" <> P.newline <> P.indentN 2 (P.lines (map P.shown $ Set.toList causalParents)),
                                 Monoid.unlessM (null terms) $ P.lit "Terms:" <> P.newline <> P.indentN 2 (P.lines (map (prettyDefn Referent.toText) $ Map.toList terms)),
                                 Monoid.unlessM (null types) $ P.lit "Types:" <> P.newline <> P.indentN 2 (P.lines (map (prettyDefn Reference.toText) $ Map.toList types)),
                                 Monoid.unlessM (null patches) $ P.lit "Patches:" <> P.newline <> P.indentN 2 (P.column2 (map (bimap (P.text . NameSegment.toText) P.shown) $ Map.toList patches)),
                                 Monoid.unlessM (null children) $ P.lit "Children:" <> P.newline <> P.indentN 2 (P.column2 (map (bimap (P.text . NameSegment.toText) P.shown) $ Map.toList children))
                               ]
                         )
                    where
                      prettyLinks renderR r [] = P.indentN 2 $ P.text (renderR r)
                      prettyLinks renderR r links = P.indentN 2 (P.lines (P.text (renderR r) : (links <&> \r -> "+ " <> P.text (Reference.toText r))))
                      prettyDefn renderR (r, (Foldable.toList -> names, Foldable.toList -> links)) =
                        P.lines (P.text . NameSegment.toText <$> if null names then [NameSegment "<unnamed>"] else names) <> P.newline <> prettyLinks renderR r links
              rootBranch <- Cli.getRootBranch
              void . liftIO . flip State.execStateT mempty $ goCausal [getCausal rootBranch]
            DebugDumpNamespaceSimpleI -> do
              rootBranch0 <- Cli.getRootBranch0
              for_ (Relation.toList . Branch.deepTypes $ rootBranch0) \(r, name) ->
                traceM $ show name ++ ",Type," ++ Text.unpack (Reference.toText r)
              for_ (Relation.toList . Branch.deepTerms $ rootBranch0) \(r, name) ->
                traceM $ show name ++ ",Term," ++ Text.unpack (Referent.toText r)
            DebugClearWatchI {} ->
              Cli.runTransaction Codebase.clearWatches
            DebugDoctorI {} -> do
              r <- Cli.runTransaction IntegrityCheck.integrityCheckFullCodebase
              Cli.respond (IntegrityCheck r)
            DebugNameDiffI fromSCH toSCH -> do
              (schLen, fromCHs, toCHs) <-
                Cli.runTransaction do
                  schLen <- Codebase.branchHashLength
                  fromCHs <- Codebase.causalHashesByPrefix fromSCH
                  toCHs <- Codebase.causalHashesByPrefix toSCH
                  pure (schLen, fromCHs, toCHs)
              (fromCH, toCH) <- case (Set.toList fromCHs, Set.toList toCHs) of
                ((_ : _ : _), _) -> Cli.returnEarly $ Output.BranchHashAmbiguous fromSCH (Set.map (SCH.fromHash schLen) fromCHs)
                ([], _) -> Cli.returnEarly $ Output.NoBranchWithHash fromSCH
                (_, []) -> Cli.returnEarly $ Output.NoBranchWithHash toSCH
                (_, (_ : _ : _)) -> Cli.returnEarly $ Output.BranchHashAmbiguous toSCH (Set.map (SCH.fromHash schLen) toCHs)
                ([fromCH], [toCH]) -> pure (fromCH, toCH)
              output <-
                Cli.runTransaction do
                  fromBranch <- Codebase.expectCausalBranchByCausalHash fromCH >>= V2Causal.value
                  toBranch <- Codebase.expectCausalBranchByCausalHash toCH >>= V2Causal.value
                  treeDiff <- V2Branch.diffBranches fromBranch toBranch
                  let nameChanges = V2Branch.nameChanges Nothing treeDiff
                  pure (DisplayDebugNameDiff nameChanges)
              Cli.respond output
            DeprecateTermI {} -> Cli.respond NotImplemented
            DeprecateTypeI {} -> Cli.respond NotImplemented
            RemoveTermReplacementI from patchPath -> doRemoveReplacement from patchPath True
            RemoveTypeReplacementI from patchPath -> doRemoveReplacement from patchPath False
            ShowDefinitionByPrefixI {} -> Cli.respond NotImplemented
            UpdateBuiltinsI -> Cli.respond NotImplemented
            QuitI -> Cli.haltRepl
            GistI input -> handleGist input
            AuthLoginI -> void $ authLogin (Codeserver.resolveCodeserver RemoteRepo.DefaultCodeserver)
            VersionI -> do
              Cli.Env {ucmVersion} <- ask
              Cli.respond $ PrintVersion ucmVersion
            DiffNamespaceToPatchI diffNamespaceToPatchInput -> do
              description <- inputDescription input
              handleDiffNamespaceToPatch description diffNamespaceToPatchInput

magicMainWatcherString :: String
magicMainWatcherString = "main"

inputDescription :: Input -> Cli Text
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
    DeleteI dtarget -> do
      case dtarget of
        DeleteTarget'TermOrType DeleteOutput'NoDiff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete " <> Text.intercalate " " thing)
        DeleteTarget'TermOrType DeleteOutput'Diff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete.verbose " <> Text.intercalate " " thing)
        DeleteTarget'Term DeleteOutput'NoDiff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete.term " <> Text.intercalate " " thing)
        DeleteTarget'Term DeleteOutput'Diff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete.term.verbose " <> Text.intercalate " " thing)
        DeleteTarget'Type DeleteOutput'NoDiff thing0 -> do
          thing <- traverse hqs' thing0
          pure ("delete.type " <> Text.intercalate " " thing)
        DeleteTarget'Type DeleteOutput'Diff thing0 -> do
          thing <- traverse hqs' thing0
          pure ("delete.type.verbose " <> Text.intercalate " " thing)
        DeleteTarget'Branch Try opath0 -> do
          opath <- ops' opath0
          pure ("delete.namespace " <> opath)
        DeleteTarget'Branch Force opath0 -> do
          opath <- ops' opath0
          pure ("delete.namespace.force " <> opath)
        DeleteTarget'Patch path0 -> do
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
    ExecuteSchemeI nm args ->
      pure $
        "run.native "
          <> HQ.toText nm
          <> " "
          <> Text.unwords (fmap Text.pack args)
    CompileSchemeI fi nm -> pure ("compile.native " <> HQ.toText nm <> " " <> Text.pack fi)
    GenSchemeLibsI -> pure "compile.native.genlibs"
    FetchSchemeCompilerI name -> pure ("compile.native.fetch" <> Text.pack name)
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
    DiffNamespaceToPatchI input -> do
      branchId1 <- hp' (input ^. #branchId1)
      branchId2 <- hp' (input ^. #branchId2)
      patch <- ps' (input ^. #patch)
      pure (Text.unwords ["diff.namespace.to-patch", branchId1, branchId2, patch])
    --
    ApiI -> wat
    AuthLoginI {} -> wat
    CreateMessage {} -> wat
    CreatePullRequestI {} -> wat
    DebugClearWatchI {} -> wat
    DebugDoctorI {} -> wat
    DebugNameDiffI {} -> wat
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
    ClearI {} -> pure "clear"
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
    DocToMarkdownI name -> pure ("debug.doc-to-markdown " <> Name.toText name)
  where
    hp' :: Either SCH.ShortCausalHash Path' -> Cli Text
    hp' = either (pure . Text.pack . show) p'
    p' :: Path' -> Cli Text
    p' = fmap tShow . Cli.resolvePath'
    ops' :: Maybe Path.Split' -> Cli Text
    ops' = maybe (pure ".") ps'
    opatch :: Maybe Path.Split' -> Cli Text
    opatch = ps' . fromMaybe Cli.defaultPatchPath
    wat = error $ show input ++ " is not expected to alter the branch"
    hhqs' :: Either SH.ShortHash Path.HQSplit' -> Cli Text
    hhqs' = \case
      Left sh -> pure (SH.toText sh)
      Right x -> hqs' x
    hqs' :: Path.HQSplit' -> Cli Text
    hqs' (p0, hq) = do
      p <- if Path.isRoot' p0 then pure mempty else p' p0
      pure (p <> "." <> HQ'.toTextWith NameSegment.toText hq)
    hqs (p, hq) = hqs' (Path' . Right . Path.Relative $ p, hq)
    ps' = p' . Path.unsplit'

handleCreatePullRequest :: ReadRemoteNamespace -> ReadRemoteNamespace -> Cli ()
handleCreatePullRequest baseRepo0 headRepo0 = do
  Cli.Env {codebase} <- ask

  let withBranch :: ReadRemoteNamespace -> (forall x. (Branch IO -> Cli x) -> Cli x)
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
  Cli ()
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
  let getResults :: Names -> Cli [SearchResult]
      getResults names =
        case ws of
          [] -> pure (List.sortBy SR.compareByName (SR.fromNames names))
          -- type query
          ":" : ws -> do
            typ <- parseSearchType (show input) (unwords ws)
            let keepNamed = Set.intersection (Branch.deepReferents currentBranch0)
            (noExactTypeMatches, matches) <- do
              Cli.runTransaction do
                matches <- keepNamed <$> Codebase.termsOfType codebase typ
                if null matches
                  then (True,) . keepNamed <$> Codebase.termsMentioningType codebase typ
                  else pure (False, matches)
            when noExactTypeMatches (Cli.respond NoExactTypeMatches)
            pure $
              -- in verbose mode, aliases are shown, so we collapse all
              -- aliases to a single search result; in non-verbose mode,
              -- a separate result may be shown for each alias
              (if isVerbose then uniqueBy SR.toReferent else id) $
                searchResultsFor names (Set.toList matches) []

          -- name query
          (map HQ.unsafeFromString -> qs) -> do
            let srs = searchBranchScored names fuzzyNameDistance qs
            pure $ uniqueBy SR.toReferent srs
  let respondResults results = do
        #numberedArgs .= fmap searchResultToHQString results
        results' <- Cli.runTransaction (Backend.loadSearchResults codebase results)
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

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  hqLength <- Cli.runTransaction Codebase.hashLength
  -- todo: add flag to handle transitive efficiently
  lds <- resolveHQToLabeledDependencies hq

  when (null lds) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  for_ lds \ld -> do
    -- The full set of dependent references, any number of which may not have names in the current namespace.
    dependents <-
      let tp r = Codebase.dependents Queries.ExcludeOwnComponent r
          tm = \case
            Referent.Ref r -> Codebase.dependents Queries.ExcludeOwnComponent r
            Referent.Con (ConstructorReference r _cid) _ct ->
              Codebase.dependents Queries.ExcludeOwnComponent r
       in Cli.runTransaction (LD.fold tp tm ld)
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
handleGist :: GistInput -> Cli ()
handleGist (GistInput repo) =
  doPushRemoteBranch (GistyPush repo) Path.relativeEmpty' SyncMode.ShortCircuit

handleDiffNamespaceToPatch :: Text -> DiffNamespaceToPatchInput -> Cli ()
handleDiffNamespaceToPatch description input = do
  Cli.Env {codebase} <- ask

  absBranchId1 <- Cli.resolveBranchIdToAbsBranchId (input ^. #branchId1)
  absBranchId2 <- Cli.resolveBranchIdToAbsBranchId (input ^. #branchId2)

  patch <- do
    Cli.runEitherTransaction do
      runExceptT do
        branch1 <- ExceptT (Cli.resolveAbsBranchIdV2 absBranchId1)
        branch2 <- ExceptT (Cli.resolveAbsBranchIdV2 absBranchId2)
        lift do
          branchDiff <- V2Branch.nameBasedDiff <$> V2Branch.diffBranches branch1 branch2
          termEdits <-
            (branchDiff ^. #terms)
              & Relation.domain
              & Map.toList
              & traverse \(oldRef, newRefs) -> makeTermEdit codebase oldRef newRefs
          pure
            Patch
              { _termEdits =
                  termEdits
                    & catMaybes
                    & Relation.fromList,
                _typeEdits =
                  (branchDiff ^. #types)
                    & Relation.domain
                    & Map.toList
                    & mapMaybe (\(oldRef, newRefs) -> makeTypeEdit oldRef newRefs)
                    & Relation.fromList
              }

  -- Display the patch that we are about to create.
  ppe <- suffixifiedPPE =<< makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
  Cli.respondNumbered (ListEdits patch ppe)

  (patchPath, patchName) <- Cli.resolveSplit' (input ^. #patch)

  -- Add the patch to the in-memory root branch and flush it all to SQLite.
  -- If there's already a patch at the given path, overwrite it.
  Cli.stepAtM
    description
    (Path.unabsolute patchPath, Branch.modifyPatches patchName (const patch))
  where
    -- Given {old reference} and {new references}, create term edit patch entries as follows:
    --
    --   * If the {new references} is a singleton set {new reference}, proceed. (Otherwise, the patch we might create
    --     would not be a function, which is a bogus/conflicted patch).
    --   * Look up {old reference} and {new reference} types in the codebase (which can technically fail, due to
    --     non-transactionality of this command, though we don't typically delete anything from SQLite), and create a
    --     patch entry that maps {old reference} to {new reference} with the typing relationship.
    makeTermEdit ::
      Codebase m Symbol Ann ->
      V2.Reference ->
      Set V2.Reference ->
      Sqlite.Transaction (Maybe (Reference, TermEdit))
    makeTermEdit codebase (Conversions.reference2to1 -> oldRef) newRefs =
      runMaybeT do
        newRef <- Conversions.reference2to1 <$> MaybeT (pure (Set.asSingleton newRefs))
        oldRefType <- MaybeT (Codebase.getTypeOfTerm codebase oldRef)
        newRefType <- MaybeT (Codebase.getTypeOfTerm codebase newRef)
        pure (oldRef, TermEdit.Replace newRef (TermEdit.typing oldRefType newRefType))

    -- Same idea as 'makeTermEdit', but simpler, because there's nothing to look up in the database.
    makeTypeEdit :: V2.Reference -> Set V2.Reference -> Maybe (Reference, TypeEdit)
    makeTypeEdit (Conversions.reference2to1 -> oldRef) newRefs =
      Set.asSingleton newRefs <&> \newRef -> (oldRef, TypeEdit.Replace (Conversions.reference2to1 newRef))

handleIOTest :: HQ.HashQualified Name -> Cli ()
handleIOTest main = do
  Cli.Env {codebase, runtime} <- ask

  let testType = Runtime.ioTestType runtime
  parseNames <- (`NamesWithHistory.NamesWithHistory` mempty) <$> basicParseNames
  -- use suffixed names for resolving the argument to display
  ppe <- suffixifiedPPE parseNames
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

  matches <-
    Cli.label \returnMatches -> do
      -- First, look at the terms in the latest typechecked file for a name-match.
      whenJustM Cli.getLatestTypecheckedFile \typecheckedFile -> do
        whenJust (HQ.toName main) \mainName ->
          whenJust (Map.lookup (Name.toVar mainName) (UF.hashTermsId typecheckedFile)) \(ref, _wk, _term, typ) ->
            returnMatches [(Reference.fromId ref, typ)]

      -- Then, if we get here (because nothing in the scratch file matched), look at the terms in the codebase.
      Cli.runTransaction do
        forMaybe (Set.toList (NamesWithHistory.lookupHQTerm main parseNames)) \ref0 ->
          runMaybeT do
            ref <- MaybeT (pure (Referent.toTermReference ref0))
            typ <- MaybeT (loadTypeOfTerm codebase (Referent.Ref ref))
            pure (ref, typ)

  ref <-
    case matches of
      [] -> Cli.returnEarly (NoMainFunction (HQ.toString main) ppe [testType])
      [(ref, typ)] ->
        if Typechecker.isSubtype typ testType
          then pure ref
          else Cli.returnEarly (BadMainFunction "io.test" (HQ.toString main) typ ppe [testType])
      _ -> do
        hashLength <- Cli.runTransaction Codebase.hashLength
        let labeledDependencies =
              matches
                & map (\(ref, _typ) -> LD.termRef ref)
                & Set.fromList
        Cli.returnEarly (LabeledReferenceAmbiguous hashLength main labeledDependencies)

  let a = ABT.annotation tm
      tm = DD.forceTerm a a (Term.ref a ref)
  -- Don't cache IO tests
  tm' <- evalUnisonTerm False ppe False tm
  Cli.respond $ TestResults Output.NewlyComputed ppe True True (oks [(ref, tm')]) (fails [(ref, tm')])

-- | Handle a @push@ command.
handlePushRemoteBranch :: PushRemoteBranchInput -> Cli ()
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
  Cli ()
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
      schLength <- Cli.runTransaction Codebase.branchHashLength
      Cli.respond $
        GistCreated
          ( ReadRemoteNamespaceGit
              ReadGitRemoteNamespace
                { repo = writeToReadGit repo,
                  sch = Just (SCH.fromHash schLength (Branch.headHash sourceBranch)),
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

handlePushToUnisonShare :: WriteShareRemotePath -> Path.Absolute -> PushBehavior -> Cli ()
handlePushToUnisonShare remote@WriteShareRemotePath {server, repo, path = remotePath} localPath behavior = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  let sharePath = Share.Path (shareUserHandleToText repo Nel.:| pathToSegments remotePath)
  _userInfo <- ensureAuthenticatedWithCodeserver codeserver

  -- doesn't handle the case where a non-existent path is supplied
  localCausalHash <-
    Cli.runTransaction (Ops.loadCausalHashAtPath (pathToSegments (Path.unabsolute localPath))) & onNothingM do
      Cli.returnEarly (EmptyPush . Path.absoluteToPath' $ localPath)

  let checkAndSetPush :: Maybe Hash32 -> Cli ()
      checkAndSetPush remoteHash =
        when (Just (Hash32.fromHash (unCausalHash localCausalHash)) /= remoteHash) do
          let push =
                Cli.with withEntitiesUploadedProgressCallback \uploadedCallback -> do
                  Share.checkAndSetPush
                    baseURL
                    sharePath
                    remoteHash
                    localCausalHash
                    uploadedCallback
          push & onLeftM (pushError ShareErrorCheckAndSetPush)

  case behavior of
    PushBehavior.ForcePush -> do
      maybeHashJwt <-
        Share.getCausalHashByPath baseURL sharePath & onLeftM \err0 ->
          (Cli.returnEarly . Output.ShareError) case err0 of
            Share.SyncError err -> ShareErrorGetCausalHashByPath err
            Share.TransportError err -> ShareErrorTransport err
      checkAndSetPush (Share.hashJWTHash <$> maybeHashJwt)
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireEmpty -> do
      checkAndSetPush Nothing
      Cli.respond (ViewOnShare remote)
    PushBehavior.RequireNonEmpty -> do
      let push :: Cli (Either (Share.SyncError Share.FastForwardPushError) ())
          push =
            Cli.with withEntitiesUploadedProgressCallback \uploadedCallback ->
              Share.fastForwardPush
                baseURL
                sharePath
                localCausalHash
                uploadedCallback
      push & onLeftM (pushError ShareErrorFastForwardPush)
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

    pushError :: (a -> Output.ShareError) -> Share.SyncError a -> Cli b
    pushError f err0 = do
      Cli.returnEarly case err0 of
        Share.SyncError err -> Output.ShareError (f err)
        Share.TransportError err -> Output.ShareError (ShareErrorTransport err)

-- | Handle a @ShowDefinitionI@ input command, i.e. `view` or `edit`.
handleShowDefinition :: OutputLocation -> ShowDefinitionScope -> [HQ.HashQualified Name] -> Cli ()
handleShowDefinition outputLoc showDefinitionScope inputQuery = do
  Cli.Env {codebase} <- ask
  hqLength <- Cli.runTransaction Codebase.hashLength
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
      let ppe = PPED.fromNamesDecl hqLength names
      pure (names, ppe)
    (_, ShowDefinitionLocal) -> do
      currentBranch <- Cli.getCurrentBranch0
      let currentNames = NamesWithHistory.fromCurrentNames $ Branch.toNames currentBranch
      let ppe = Backend.getCurrentPrettyNames hqLength (Backend.Within currentPath') root
      pure (currentNames, ppe)
  Backend.DefinitionResults terms types misses <- do
    let nameSearch = Backend.makeNameSearch hqLength names
    Cli.runTransaction (Backend.definitionsBySuffixes codebase nameSearch includeCycles query)
  outputPath <- getOutputPath
  when (not (null types && null terms)) do
    -- We need an 'isTest' check in the output layer, so it can prepend "test>" to tests in a scratch file. Since we
    -- currently have the whole branch in memory, we just use that to make our predicate, but this could/should get this
    -- information from the database instead, once it's efficient to do so.
    isTest <- do
      branch <- Cli.getCurrentBranch0
      pure \ref ->
        branch
          & Branch.deepTermMetadata
          & Metadata.hasMetadataWithType' (Referent.fromTermReference ref) IOSource.isTestReference
    Cli.respond $
      DisplayDefinitions
        DisplayDefinitionsOutput
          { isTest,
            outputFile = outputPath,
            prettyPrintEnv = PPED.biasTo (mapMaybe HQ.toName inputQuery) unbiasedPPE,
            terms,
            types
          }
  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  for_ outputPath \p -> do
    -- We set latestFile to be programmatically generated, if we
    -- are viewing these definitions to a file - this will skip the
    -- next update for that file (which will happen immediately)
    #latestFile ?= (p, True)
  where
    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ -> Backend.IncludeCycles
        LatestFileLocation -> Backend.IncludeCycles

    -- Get the file path to send the definition(s) to. `Nothing` means the terminal.
    getOutputPath :: Cli (Maybe FilePath)
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
handleTest :: TestInput -> Cli ()
handleTest TestInput {includeLibNamespace, showFailures, showSuccesses} = do
  Cli.Env {codebase} <- ask

  testTerms <- do
    branch <- Cli.getCurrentBranch0
    branch
      & Branch.deepTermMetadata
      & R4.restrict34d12 IOSource.isTest
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
        r@(Reference.DerivedId rid) -> fmap (r,) <$> Cli.runTransaction (Codebase.getWatch codebase WK.TestWatch rid)
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
          Cli.runTransaction (Codebase.getTerm codebase rid) >>= \case
            Nothing -> do
              hqLength <- Cli.runTransaction Codebase.hashLength
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
                  Cli.runTransaction (Codebase.putWatch WK.TestWatch rid tm')
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

-- Takes a maybe (namespace address triple); returns it as-is if `Just`;
-- otherwise, tries to load a value from .unisonConfig, and complains
-- if needed.
resolveConfiguredUrl :: PushPull -> Path' -> Cli WriteRemotePath
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
remoteMappingForPath :: PushPull -> Path.Absolute -> Cli (Maybe WriteRemotePath)
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

importRemoteShareBranch :: ReadShareRemoteNamespace -> Cli (Branch IO)
importRemoteShareBranch rrn@(ReadShareRemoteNamespace {server, repo, path}) = do
  let codeserver = Codeserver.resolveCodeserver server
  let baseURL = codeserverBaseURL codeserver
  -- Auto-login to share if pulling from a non-public path
  when (not $ RemoteRepo.isPublic rrn) . void $ ensureAuthenticatedWithCodeserver codeserver
  let shareFlavoredPath = Share.Path (shareUserHandleToText repo Nel.:| coerce @[NameSegment] @[Text] (Path.toList path))
  Cli.Env {codebase} <- ask
  causalHash <-
    Cli.with withEntitiesDownloadedProgressCallback \downloadedCallback ->
      Share.pull baseURL shareFlavoredPath downloadedCallback & onLeftM \err0 ->
        (Cli.returnEarly . Output.ShareError) case err0 of
          Share.SyncError err -> Output.ShareErrorPull err
          Share.TransportError err -> Output.ShareErrorTransport err
  liftIO (Codebase.getBranchForHash codebase causalHash) & onNothingM do
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
resolveHQToLabeledDependencies :: HQ.HashQualified Name -> Cli (Set LabeledDependency)
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
      (terms, types) <-
        Cli.runTransaction do
          terms <- Backend.termReferentsByShortHash codebase sh
          types <- Backend.typeReferencesByShortHash sh
          pure (terms, types)
      pure $ Set.map LD.referent terms <> Set.map LD.typeRef types

doDisplay :: OutputLocation -> NamesWithHistory -> Term Symbol () -> Cli ()
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
        Nothing -> fmap (fmap Term.unannotate) $ Cli.runTransaction (Codebase.getTerm codebase r)
        Just (tm, _) -> pure (Just $ Term.unannotate tm)
      loadTerm _ = pure Nothing
      loadDecl (Reference.DerivedId r) = case Map.lookup r typs of
        Nothing -> fmap (fmap $ DD.amap (const ())) $ Cli.runTransaction $ Codebase.getTypeDeclaration codebase r
        Just decl -> pure (Just $ DD.amap (const ()) decl)
      loadDecl _ = pure Nothing
      loadTypeOfTerm' (Referent.Ref (Reference.DerivedId r))
        | Just (_, ty) <- Map.lookup r tms = pure $ Just (void ty)
      loadTypeOfTerm' r = fmap (fmap void) . Cli.runTransaction . loadTypeOfTerm codebase $ r
  rendered <-
    DisplayValues.displayTerm
      ppe
      loadTerm
      loadTypeOfTerm'
      evalTerm
      loadDecl
      tm
  Cli.respond $ DisplayRendered loc rendered

getLinks ::
  SrcLoc ->
  Path.HQSplit' ->
  Either (Set Reference) (Maybe String) ->
  Cli
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
  sigs <- Cli.runTransaction (for (toList allRefs) (loadTypeOfTerm codebase . Referent.Ref))
  let deps =
        Set.map LD.termRef allRefs
          <> Set.unions [Set.map LD.typeRef . Type.dependencies $ t | Just t <- sigs]
  ppe <- prettyPrintEnvDecl =<< makePrintNamesFromLabeled' deps
  let ppeDecl = PPE.unsuffixifiedPPE ppe
  let sortedSigs = sortOn snd (toList allRefs `zip` sigs)
  let out = [(PPE.termName ppeDecl (Referent.Ref r), r, t) | (r, t) <- sortedSigs]
  pure (PPE.suffixifiedPPE ppe, out)

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatch ::
  Text ->
  Patch ->
  Path.Absolute ->
  Cli Bool
propagatePatch inputDescription patch scopePath = do
  Cli.time "propagatePatch" do
    Cli.stepAt'
      (inputDescription <> " (applying patch)")
      (Path.unabsolute scopePath, Propagate.propagateAndApply patch)

-- | Show todo output if there are any conflicts or edits.
doShowTodoOutput :: Patch -> Path.Absolute -> Cli ()
doShowTodoOutput patch scopePath = do
  Cli.Env {codebase} <- ask
  names0 <- Branch.toNames <$> Cli.getBranch0At scopePath
  todo <- Cli.runTransaction (checkTodo codebase patch names0)
  if TO.noConflicts todo && TO.noEdits todo
    then Cli.respond NoConflictsOrEdits
    else do
      #numberedArgs
        .= ( Text.unpack . Reference.toText . view _2
               <$> fst (TO.todoFrontierDependents todo)
           )
      -- only needs the local references to check for obsolete defs
      ppe <- do
        names <- makePrintNamesFromLabeled' (Patch.labeledDependencies patch)
        prettyPrintEnvDecl names
      Cli.respondNumbered $ TodoOutput ppe todo

checkTodo :: Codebase m Symbol Ann -> Patch -> Names -> Sqlite.Transaction (TO.TodoOutput Symbol Ann)
checkTodo codebase patch names0 = do
  let -- Get the dependents of a reference which:
      --   1. Don't appear on the LHS of this patch
      --   2. Have a name in this namespace
      getDependents :: Reference -> Sqlite.Transaction (Set Reference)
      getDependents ref = do
        dependents <- Codebase.dependents Queries.ExcludeSelf ref
        pure (dependents & removeEditedThings & removeNamelessThings)
  -- (r,r2) ∈ dependsOn if r depends on r2, excluding self-references (i.e. (r,r))
  dependsOn <- Monoid.foldMapM (\ref -> R.fromManyDom <$> getDependents ref <*> pure ref) edited
  let dirty = R.dom dependsOn
  transitiveDirty <- transitiveClosure getDependents dirty
  (frontierTerms, frontierTypes) <- loadDisplayInfo codebase (R.ran dependsOn)
  (dirtyTerms, dirtyTypes) <- loadDisplayInfo codebase dirty
  pure $
    TO.TodoOutput
      (Set.size transitiveDirty)
      (frontierTerms, frontierTypes)
      (score dirtyTerms, score dirtyTypes)
      (Names.conflicts names0)
      (Patch.conflicts patch)
  where
    -- Remove from a all references that were edited, i.e. appear on the LHS of this patch.
    removeEditedThings :: Set Reference -> Set Reference
    removeEditedThings =
      (`Set.difference` edited)
    -- Remove all references that don't have a name in the given namespace
    removeNamelessThings :: Set Reference -> Set Reference
    removeNamelessThings =
      Set.filter (Names.contains names0)
    -- todo: something more intelligent here?
    score :: [(a, b)] -> [(TO.Score, a, b)]
    score = map (\(x, y) -> (1, x, y))
    edited :: Set Reference
    edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)

confirmedCommand :: Input -> Cli Bool
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
  nubOrd
    . fmap snd
    . List.sortBy (\(s0, r0) (s1, r1) -> compare s0 s1 <> SR.compareByName r0 r1)
    $ searchTermNamespace <> searchTypeNamespace
  where
    searchTermNamespace = queries >>= do1query
      where
        do1query :: HQ.HashQualified Name -> [(Maybe score, SearchResult)]
        do1query q = mapMaybe (score1hq q) (R.toList . Names.terms $ names0)
        score1hq :: HQ.HashQualified Name -> (Name, Referent) -> Maybe (Maybe score, SearchResult)
        score1hq query (name, ref) = case query of
          HQ.NameOnly qn ->
            pair qn
          HQ.HashQualified qn h
            | h `SH.isPrefixOf` Referent.toShortHash ref ->
                pair qn
          HQ.HashOnly h
            | h `SH.isPrefixOf` Referent.toShortHash ref ->
                Just (Nothing, result)
          _ -> Nothing
          where
            result = SR.termSearchResult names0 name ref
            pair qn =
              (\score -> (Just score, result)) <$> score qn name
    searchTypeNamespace = queries >>= do1query
      where
        do1query :: HQ.HashQualified Name -> [(Maybe score, SearchResult)]
        do1query q = mapMaybe (score1hq q) (R.toList . Names.types $ names0)
        score1hq :: HQ.HashQualified Name -> (Name, Reference) -> Maybe (Maybe score, SearchResult)
        score1hq query (name, ref) = case query of
          HQ.NameOnly qn ->
            pair qn
          HQ.HashQualified qn h
            | h `SH.isPrefixOf` Reference.toShortHash ref ->
                pair qn
          HQ.HashOnly h
            | h `SH.isPrefixOf` Reference.toShortHash ref ->
                Just (Nothing, result)
          _ -> Nothing
          where
            result = SR.typeSearchResult names0 name ref
            pair qn =
              (\score -> (Just score, result)) <$> score qn name

-- | supply `dest0` if you want to print diff messages
--   supply unchangedMessage if you want to display it if merge had no effect
mergeBranchAndPropagateDefaultPatch ::
  Branch.MergeMode ->
  Text ->
  Maybe Output ->
  Branch IO ->
  Maybe Path.Path' ->
  Path.Absolute ->
  Cli ()
mergeBranchAndPropagateDefaultPatch mode inputDescription unchangedMessage srcb maybeDest0 dest =
  ifM
    mergeBranch
    (loadPropagateDiffDefaultPatch inputDescription maybeDest0 dest)
    (for_ unchangedMessage Cli.respond)
  where
    mergeBranch :: Cli Bool
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

basicPPE :: Cli PPE.PrettyPrintEnv
basicPPE = do
  parseNames <-
    flip NamesWithHistory.NamesWithHistory mempty
      <$> basicParseNames
  suffixifiedPPE parseNames

compilerPath :: Path.Path'
compilerPath = Path.Path' {Path.unPath' = Left abs}
  where
    segs = NameSegment <$> ["unison", "internal"]
    rootPath = Path.Path {Path.toSeq = Seq.fromList segs}
    abs = Path.Absolute {Path.unabsolute = rootPath}

doFetchCompiler :: String -> Cli ()
doFetchCompiler username =
  inputDescription pullInput
    >>= doPullRemoteBranch
      repo
      compilerPath
      SyncMode.Complete
      Input.PullWithoutHistory
      Verbosity.Silent
  where
    -- fetching info
    ns =
      ReadShareRemoteNamespace
        { server = RemoteRepo.DefaultCodeserver,
          repo = ShareUserHandle (Text.pack username),
          path =
            Path.fromList $ NameSegment <$> ["public", "internal", "trunk"]
        }
    repo = Just $ ReadRemoteNamespaceShare ns

    pullInput =
      PullRemoteBranchI
        repo
        compilerPath
        SyncMode.Complete
        Input.PullWithoutHistory
        Verbosity.Silent

ensureCompilerExists :: Cli ()
ensureCompilerExists =
  Cli.branchExistsAtPath' compilerPath
    >>= flip unless (doFetchCompiler "unison")

getCacheDir :: Cli String
getCacheDir = liftIO $ getXdgDirectory XdgCache "unisonlanguage"

getSchemeGenLibDir :: Cli String
getSchemeGenLibDir =
  Cli.getConfig "SchemeLibs.Generated" >>= \case
    Just dir -> pure dir
    Nothing -> (</> "scheme-libs") <$> getCacheDir

getSchemeStaticLibDir :: Cli String
getSchemeStaticLibDir =
  Cli.getConfig "SchemeLibs.Static" >>= \case
    Just dir -> pure dir
    Nothing ->
      liftIO $
        getXdgDirectory XdgData ("unisonlanguage" </> "scheme-libs")

doGenerateSchemeBoot :: Bool -> Maybe PPE.PrettyPrintEnv -> Cli ()
doGenerateSchemeBoot force mppe = do
  ppe <- maybe basicPPE pure mppe
  dir <- getSchemeGenLibDir
  let bootf = dir </> "unison" </> "boot-generated.ss"
      binf = dir </> "unison" </> "builtin-generated.ss"
      dirTm = Term.text a (Text.pack dir)
  liftIO $ createDirectoryIfMissing True dir
  saveBase <- Term.ref a <$> resolveTermRef sbName
  gen ppe saveBase bootf dirTm bootName
  gen ppe saveBase binf dirTm builtinName
  where
    a = External
    hq nm
      | Just hqn <- HQ.fromString nm = hqn
      | otherwise = error $ "internal error: cannot hash qualify: " ++ nm

    sbName = hq ".unison.internal.compiler.scheme.saveBaseFile"
    bootName = hq ".unison.internal.compiler.scheme.bootSpec"
    builtinName = hq ".unison.internal.compiler.scheme.builtinSpec"

    gen ppe save file dir nm =
      liftIO (doesFileExist file) >>= \b -> when (not b || force) do
        spec <- Term.ref a <$> resolveTermRef nm
        let make = Term.apps' save [dir, spec]
        typecheckAndEval ppe make

typecheckAndEval :: PPE.PrettyPrintEnv -> Term Symbol Ann -> Cli ()
typecheckAndEval ppe tm = do
  Cli.Env {runtime} <- ask
  let mty = Runtime.mainType runtime
  typecheckTerm (Term.delay a tm) >>= \case
    -- Type checking succeeded
    Result.Result _ (Just ty)
      | Typechecker.fitsScheme ty mty ->
          () <$ evalUnisonTerm False ppe False tm
      | otherwise ->
          Cli.returnEarly $ BadMainFunction "run" rendered ty ppe [mty]
    Result.Result notes Nothing -> do
      currentPath <- Cli.getCurrentPath
      let tes = [err | Result.TypeError err <- toList notes]
      Cli.returnEarly (TypeErrors currentPath (Text.pack rendered) ppe tes)
  where
    a = External
    rendered = P.toPlainUnbroken $ TP.pretty ppe tm

ensureSchemeExists :: SchemeBackend -> Cli ()
ensureSchemeExists bk =
  liftIO callScheme >>= \case
    True -> pure ()
    False -> Cli.returnEarly (PrintMessage msg)
  where
    msg = case bk of
      Racket ->
        P.lines
          [ "I can't seem to call racket. See",
            "",
            P.indentN
              2
              "https://download.racket-lang.org/",
            "",
            "for how to install Racket."
          ]
      Chez ->
        P.lines
          [ "I can't seem to call scheme. See",
            "",
            P.indentN
              2
              "https://github.com/cisco/ChezScheme/blob/main/BUILDING",
            "",
            "for how to install Chez Scheme."
          ]

    cmd = case bk of
      Racket -> "racket -l- raco help"
      Chez -> "scheme -q"
    callScheme =
      readCreateProcessWithExitCode (shell cmd) "" >>= \case
        (ExitSuccess, _, _) -> pure True
        (ExitFailure _, _, _) -> pure False

racketOpts :: FilePath -> FilePath -> [String] -> [String]
racketOpts gendir statdir args = libs ++ args
  where
    includes = [gendir, statdir </> "common", statdir </> "racket"]
    libs = concatMap (\dir -> ["-S", dir]) includes

chezOpts :: FilePath -> FilePath -> [String] -> [String]
chezOpts gendir statdir args =
  "-q" : opt ++ libs ++ ["--script"] ++ args
  where
    includes = [gendir, statdir </> "common", statdir </> "chez"]
    libs = ["--libdirs", List.intercalate ":" includes]
    opt = ["--optimize-level", "3"]

data SchemeBackend = Racket | Chez

runScheme :: SchemeBackend -> String -> [String] -> Cli ()
runScheme bk file args = do
  ensureSchemeExists bk
  gendir <- getSchemeGenLibDir
  statdir <- getSchemeStaticLibDir
  let cmd = case bk of Racket -> "racket"; Chez -> "scheme"
      opts = case bk of
        Racket -> racketOpts gendir statdir (file : args)
        Chez -> chezOpts gendir statdir (file : args)
  success <-
    liftIO $
      (True <$ callProcess cmd opts)
        `catch` \(_ :: IOException) -> pure False
  unless success $
    Cli.returnEarly (PrintMessage "Scheme evaluation failed.")

buildScheme :: SchemeBackend -> String -> String -> Cli ()
buildScheme bk main file = do
  ensureSchemeExists bk
  statDir <- getSchemeStaticLibDir
  genDir <- getSchemeGenLibDir
  build genDir statDir main file
  where
    build
      | Racket <- bk = buildRacket
      | Chez <- bk = buildChez

buildRacket :: String -> String -> String -> String -> Cli ()
buildRacket genDir statDir main file =
  let args = ["-l", "raco", "--", "exe", "-o", main, file]
      opts = racketOpts genDir statDir args
   in void . liftIO $
        catch
          (True <$ callProcess "racket" opts)
          (\(_ :: IOException) -> pure False)

buildChez :: String -> String -> String -> String -> Cli ()
buildChez genDir statDir main file = do
  let cmd = shell "scheme -q --optimize-level 3"
  void . liftIO $ readCreateProcess cmd (build statDir genDir)
  where
    surround s = '"' : s ++ "\""
    parens s = '(' : s ++ ")"
    lns dir nms = surround . ln dir <$> nms
    ln dir nm = dir </> "unison" </> (nm ++ ".ss")

    static = ["core", "cont", "bytevector", "string", "primops", "boot"]
    gen = ["boot-generated", "builtin-generated"]

    bootf = surround $ main ++ ".boot"
    base = "'(\"scheme\" \"petite\")"

    build sd gd =
      parens . List.intercalate " " $
        ["make-boot-file", bootf, base]
          ++ lns sd static
          ++ lns gd gen
          ++ [surround file]

doRunAsScheme :: HQ.HashQualified Name -> [String] -> Cli ()
doRunAsScheme main args = do
  fullpath <- generateSchemeFile True (HQ.toString main) main
  runScheme Racket fullpath args

doCompileScheme :: String -> HQ.HashQualified Name -> Cli ()
doCompileScheme out main =
  generateSchemeFile True out main >>= buildScheme Racket out

generateSchemeFile :: Bool -> String -> HQ.HashQualified Name -> Cli String
generateSchemeFile exec out main = do
  (comp, ppe) <- resolveMainRef main
  ensureCompilerExists
  doGenerateSchemeBoot False $ Just ppe
  cacheDir <- getCacheDir
  liftIO $ createDirectoryIfMissing True (cacheDir </> "scheme-tmp")
  let scratch = out ++ ".scm"
      fullpath = cacheDir </> "scheme-tmp" </> scratch
      output = Text.pack fullpath
  sscm <- Term.ref a <$> resolveTermRef saveNm
  fprf <- resolveCon filePathNm
  let toCmp = Term.termLink a (Referent.Ref comp)
      outTm = Term.text a output
      fpc = Term.constructor a fprf
      fp = Term.app a fpc outTm
      tm :: Term Symbol Ann
      tm = Term.apps' sscm [Term.boolean a exec, toCmp, fp]
  typecheckAndEval ppe tm
  pure fullpath
  where
    a = External
    hq nm
      | Just hqn <- HQ.fromString nm = hqn
      | otherwise = error $ "internal error: cannot hash qualify: " ++ nm

    saveNm = hq ".unison.internal.compiler.saveScheme"
    filePathNm = hq "FilePath.FilePath"

doPullRemoteBranch ::
  Maybe ReadRemoteNamespace ->
  Path' ->
  SyncMode.SyncMode ->
  PullMode ->
  Verbosity.Verbosity ->
  Text ->
  Cli ()
doPullRemoteBranch mayRepo path syncMode pullMode verbosity description = do
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
  when (Branch.isEmpty0 (Branch.head remoteBranch)) do
    Cli.respond (PulledEmptyBranch ns)
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

loadPropagateDiffDefaultPatch ::
  Text ->
  Maybe Path.Path' ->
  Path.Absolute ->
  Cli ()
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

delete ::
  Input ->
  DeleteOutput ->
  ((Path.Absolute, HQ'.HQSegment) -> Cli (Set Referent)) -> -- compute matching terms
  ((Path.Absolute, HQ'.HQSegment) -> Cli (Set Reference)) -> -- compute matching types
  [Path.HQSplit'] -> -- targets for deletion
  Cli ()
delete input doutput getTerms getTypes hqs' = do
  -- persists the original hash qualified entity for error reporting
  typesTermsTuple <-
    traverse
      ( \hq -> do
          absolute <- Cli.resolveSplit' hq
          types <- getTypes absolute
          terms <- getTerms absolute
          return (hq, types, terms)
      )
      hqs'
  let notFounds = List.filter (\(_, types, terms) -> Set.null terms && Set.null types) typesTermsTuple
  -- if there are any entities which cannot be deleted because they don't exist, short circuit.
  if not $ null notFounds
    then do
      let toName :: [(Path.HQSplit', Set Reference, Set referent)] -> [Name]
          toName notFounds =
            mapMaybe (\(split, _, _) -> Path.toName' $ HashQualified.toName (HQSplit'.unsplitHQ' split)) notFounds
      Cli.returnEarly $ NamesNotFound (toName notFounds)
    else do
      checkDeletes typesTermsTuple doutput input

checkDeletes :: [(Path.HQSplit', Set Reference, Set Referent)] -> DeleteOutput -> Input -> Cli ()
checkDeletes typesTermsTuples doutput inputs = do
  let toSplitName ::
        (Path.HQSplit', Set Reference, Set Referent) ->
        Cli (Path.Split, Name, Set Reference, Set Referent)
      toSplitName hq = do
        resolvedPath <- Path.convert <$> Cli.resolveSplit' (HQ'.toName <$> hq ^. _1)
        return (resolvedPath, Path.unsafeToName (Path.unsplit resolvedPath), hq ^. _2, hq ^. _3)
  -- get the splits and names with terms and types
  splitsNames <- traverse toSplitName typesTermsTuples
  let toRel :: (Ord ref) => Set ref -> Name -> R.Relation Name ref
      toRel setRef name = R.fromList (fmap (name,) (toList setRef))
  let toDelete = fmap (\(_, names, types, terms) -> Names (toRel terms names) (toRel types names)) splitsNames
  -- make sure endangered is compeletely contained in paths
  rootNames <- Branch.toNames <$> Cli.getRootBranch0
  -- get only once for the entire deletion set
  let allTermsToDelete :: Set LabeledDependency
      allTermsToDelete = Set.unions (fmap Names.labeledReferences toDelete)
  -- get the endangered dependencies for each entity to delete
  endangered <-
    Cli.runTransaction $
      traverse
        ( \targetToDelete ->
            getEndangeredDependents targetToDelete (allTermsToDelete) rootNames
        )
        toDelete
  -- If the overall dependency map is not completely empty, abort deletion
  let endangeredDeletions = List.filter (\m -> not $ null m || Map.foldr (\s b -> null s || b) False m) endangered
  if null endangeredDeletions
    then do
      let deleteTypesTerms =
            splitsNames
              >>= ( \(split, _, types, terms) ->
                      (map (BranchUtil.makeDeleteTypeName split) . Set.toList $ types)
                        ++ (map (BranchUtil.makeDeleteTermName split) . Set.toList $ terms)
                  )
      before <- Cli.getRootBranch0
      description <- inputDescription inputs
      Cli.stepManyAt description deleteTypesTerms
      case doutput of
        DeleteOutput'Diff -> do
          after <- Cli.getRootBranch0
          (ppe, diff) <- diffHelper before after
          Cli.respondNumbered (ShowDiffAfterDeleteDefinitions ppe diff)
        DeleteOutput'NoDiff -> do
          Cli.respond Success
    else do
      ppeDecl <- currentPrettyPrintEnvDecl Backend.Within
      let combineRefs = List.foldl (Map.unionWith NESet.union) Map.empty endangeredDeletions
      Cli.respondNumbered (CantDeleteDefinitions ppeDecl combineRefs)

-- | Goal: When deleting, we might be removing the last name of a given definition (i.e. the
-- definition is going "extinct"). In this case we may wish to take some action or warn the
-- user about these "endangered" definitions which would now contain unnamed references.
-- The argument `otherDesiredDeletions` is included in this function because the user might want to
-- delete a term and all its dependencies in one command, so we give this function access to
-- the full set of entities that the user wishes to delete.
getEndangeredDependents ::
  -- | Prospective target for deletion
  Names ->
  -- | All entities we want to delete (including the target)
  Set LabeledDependency ->
  -- | All names from the root branch
  Names ->
  -- | map from references going extinct to the set of endangered dependents
  Sqlite.Transaction (Map LabeledDependency (NESet LabeledDependency))
getEndangeredDependents targetToDelete otherDesiredDeletions rootNames = do
  -- names of terms left over after target deletion
  let remainingNames :: Names
      remainingNames = rootNames `Names.difference` targetToDelete
  -- target refs for deletion
  let refsToDelete :: Set LabeledDependency
      refsToDelete = Names.labeledReferences targetToDelete
  -- refs left over after deleting target
  let remainingRefs :: Set LabeledDependency
      remainingRefs = Names.labeledReferences remainingNames
  -- remove the other targets for deletion from the remaining terms
  let remainingRefsWithoutOtherTargets :: Set LabeledDependency
      remainingRefsWithoutOtherTargets = Set.difference remainingRefs otherDesiredDeletions
  -- deleting and not left over
  let extinct :: Set LabeledDependency
      extinct = refsToDelete `Set.difference` remainingRefs
  let accumulateDependents :: LabeledDependency -> Sqlite.Transaction (Map LabeledDependency (Set LabeledDependency))
      accumulateDependents ld =
        let ref = LD.fold id Referent.toReference ld
         in Map.singleton ld . Set.map LD.termRef <$> Codebase.dependents Queries.ExcludeOwnComponent ref
  -- All dependents of extinct, including terms which might themselves be in the process of being deleted.
  allDependentsOfExtinct :: Map LabeledDependency (Set LabeledDependency) <-
    Map.unionsWith (<>) <$> for (Set.toList extinct) accumulateDependents

  -- Filtered to only include dependencies which are not being deleted, but depend one which
  -- is going extinct.
  let extinctToEndangered :: Map LabeledDependency (NESet LabeledDependency)
      extinctToEndangered =
        allDependentsOfExtinct & Map.mapMaybe \endangeredDeps ->
          let remainingEndangered = endangeredDeps `Set.intersection` remainingRefsWithoutOtherTargets
           in NESet.nonEmptySet remainingEndangered
  pure extinctToEndangered

displayI ::
  Names ->
  OutputLocation ->
  HQ.HashQualified Name ->
  Cli ()
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

docsI :: SrcLoc -> Names -> Path.HQSplit' -> Cli ()
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

    fileByName :: Cli ()
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

    codebaseByMetadata :: Cli ()
    codebaseByMetadata = do
      (ppe, out) <- getLinks srcLoc src (Left $ Set.fromList [DD.docRef, IOSource.doc2Ref])
      case out of
        [] -> codebaseByName
        [(_name, ref, _tm)] -> do
          len <- Cli.runTransaction Codebase.branchHashLength
          let names = NamesWithHistory.NamesWithHistory prettyPrintNames mempty
          let tm = Term.ref External ref
          tm <- evalUnisonTerm True (PPE.fromNames len names) True tm
          doDisplay ConsoleLocation names (Term.unannotate tm)
        out -> do
          #numberedArgs .= fmap (HQ.toString . view _1) out
          Cli.respond $ ListOfLinks ppe out

    codebaseByName :: Cli ()
    codebaseByName = do
      parseNames <- basicParseNames
      case NamesWithHistory.lookupHQTerm dotDoc (NamesWithHistory.NamesWithHistory parseNames mempty) of
        s
          | Set.size s == 1 -> displayI prettyPrintNames ConsoleLocation dotDoc
          | Set.size s == 0 -> Cli.respond $ ListOfLinks PPE.empty []
          -- todo: return a list of links here too
          | otherwise -> Cli.respond $ ListOfLinks PPE.empty []

loadDisplayInfo ::
  Codebase m Symbol Ann ->
  Set Reference ->
  Sqlite.Transaction
    ( [(Reference, Maybe (Type Symbol Ann))],
      [(Reference, DisplayObject () (DD.Decl Symbol Ann))]
    )
loadDisplayInfo codebase refs = do
  termRefs <- filterM (Codebase.isTerm codebase) (toList refs)
  typeRefs <- filterM (Codebase.isType codebase) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> Codebase.getTypeOfTerm codebase r
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayObject codebase r
  pure (terms, types)

loadTypeDisplayObject :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (DisplayObject () (DD.Decl Symbol Ann))
loadTypeDisplayObject codebase = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> Codebase.getTypeDeclaration codebase id

lexedSource :: Text -> Text -> Cli (NamesWithHistory, (Text, [L.Token L.Lexeme]))
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

suffixifiedPPE :: NamesWithHistory -> Cli PPE.PrettyPrintEnv
suffixifiedPPE ns =
  Cli.runTransaction Codebase.hashLength <&> (`PPE.fromSuffixNames` ns)

fqnPPE :: NamesWithHistory -> Cli PPE.PrettyPrintEnv
fqnPPE ns =
  Cli.runTransaction Codebase.hashLength <&> (`PPE.fromNames` ns)

parseSearchType :: SrcLoc -> String -> Cli (Type Symbol Ann)
parseSearchType srcLoc typ = Type.removeAllEffectVars <$> parseType srcLoc typ

-- | A description of where the given parse was triggered from, for error messaging purposes.
type SrcLoc = String

parseType :: SrcLoc -> String -> Cli (Type Symbol Ann)
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

  Type.bindNames Name.unsafeFromVar mempty (NamesWithHistory.currentNames names) (Type.generalizeLowercase mempty typ) & onLeft \errs ->
    Cli.returnEarly (ParseResolutionFailures src (toList errs))

getTermsIncludingHistorical :: (Monad m) => Path.HQSplit -> Branch0 m -> Cli (Set Referent)
getTermsIncludingHistorical (p, hq) b = case Set.toList refs of
  [] -> case hq of
    HQ'.HashQualified n hs -> do
      names <- findHistoricalHQs (Set.fromList [HQ.HashQualified (Name.fromSegment n) hs])
      pure . R.ran $ Names.terms names
    _ -> pure Set.empty
  _ -> pure refs
  where
    refs = BranchUtil.getTerm (p, hq) b

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
  (Var v) =>
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

addSavedTermToUnisonFile :: Name -> Cli (TypecheckedUnisonFile Symbol Ann)
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
getTerm :: String -> Cli (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)
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
      Cli.returnEarly $ BadMainFunction "run" main ty ppe [mainType]
    GetTermSuccess x -> pure x

getTerm' :: String -> Cli GetTermResult
getTerm' mainName =
  let getFromCodebase = do
        Cli.Env {codebase, runtime} <- ask

        parseNames <- basicParseNames
        let loadTypeOfTerm ref = Cli.runTransaction (Codebase.getTypeOfTerm codebase ref)
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
      checkType :: Type Symbol Ann -> (Type Symbol Ann -> Cli GetTermResult) -> Cli GetTermResult
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
createWatcherFile :: Symbol -> Term Symbol Ann -> Type Symbol Ann -> Cli (TypecheckedUnisonFile Symbol Ann)
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
  (Var v) =>
  TypecheckedUnisonFile v a ->
  Cli PPE.PrettyPrintEnv
executePPE unisonFile =
  suffixifiedPPE =<< displayNames unisonFile

loadTypeOfTerm :: Codebase m Symbol Ann -> Referent -> Sqlite.Transaction (Maybe (Type Symbol Ann))
loadTypeOfTerm codebase (Referent.Ref r) = Codebase.getTypeOfTerm codebase r
loadTypeOfTerm codebase (Referent.Con (ConstructorReference (Reference.DerivedId r) cid) _) = do
  decl <- Codebase.getTypeDeclaration codebase r
  case decl of
    Just (either DD.toDataDecl id -> dd) -> pure $ DD.typeOfConstructor dd cid
    Nothing -> pure Nothing
loadTypeOfTerm _ Referent.Con {} =
  error $
    reportBug "924628772" "Attempt to load a type declaration which is a builtin!"

hqNameQuery :: [HQ.HashQualified Name] -> Cli QueryResult
hqNameQuery query = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  Cli.runTransaction do
    hqLength <- Codebase.hashLength
    let parseNames = Backend.parseNamesForBranch root' (Backend.AllNames (Path.unabsolute currentPath))
    let nameSearch = Backend.makeNameSearch hqLength (NamesWithHistory.fromCurrentNames parseNames)
    Backend.hqNameQuery codebase nameSearch query

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
fuzzySelectDefinition :: (MonadIO m) => Position -> Branch0 m0 -> m (Maybe [HQ.HashQualified Name])
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
fuzzySelectNamespace :: (MonadIO m) => Position -> Branch0 m0 -> m (Maybe [Path'])
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
    ( [(Symbol, Term Symbol ())],
      Map Symbol (Ann, WK.WatchKind, Reference.Id, Term Symbol (), Term Symbol (), Bool)
    )
evalUnisonFile sandbox ppe unisonFile args = do
  Cli.Env {codebase, runtime, sandboxedRuntime} <- ask
  let theRuntime = if sandbox then sandboxedRuntime else runtime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  Cli.with_ (withArgs args) do
    rs@(_, map) <-
      Cli.ioE (Runtime.evaluateWatches (Codebase.toCodeLookup codebase) ppe watchCache theRuntime unisonFile) \err -> do
        Cli.returnEarly (EvaluationFailure err)
    for_ (Map.elems map) \(_loc, kind, hash, _src, value, isHit) ->
      when (not isHit) do
        let value' = Term.amap (\() -> Ann.External) value
        Cli.runTransaction (Codebase.putWatch kind hash value')
    pure rs

-- | Evaluate a single closed definition.
evalUnisonTermE ::
  Bool ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Runtime.Error (Term Symbol Ann))
evalUnisonTermE sandbox ppe useCache tm = do
  Cli.Env {codebase, runtime, sandboxedRuntime} <- ask
  let theRuntime = if sandbox then sandboxedRuntime else runtime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  let cache = if useCache then watchCache else Runtime.noCache
  r <- liftIO (Runtime.evaluateTerm' (Codebase.toCodeLookup codebase) cache ppe theRuntime tm)
  when useCache do
    case r of
      Right tmr ->
        Cli.runTransaction do
          Codebase.putWatch
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
  Cli (Term Symbol Ann)
evalUnisonTerm sandbox ppe useCache tm =
  evalUnisonTermE sandbox ppe useCache tm & onLeftM \err ->
    Cli.returnEarly (EvaluationFailure err)

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

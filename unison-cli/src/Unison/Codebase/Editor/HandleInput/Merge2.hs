-- | @merge@ input handler.
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,

    -- * API exported for @pull@
    MergeInfo (..),
    AliceMergeInfo (..),
    BobMergeInfo (..),
    LcaMergeInfo (..),
    doMerge,
    doMergeLocalBranch,

    -- * API exported for @todo@
    hasDefnsInLib,
  )
where

import Control.Lens (mapped, _1)
import Control.Monad.Reader (ask)
import Data.Algorithm.Diff qualified as Diff
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Semialign (zipWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp qualified as Temporary
import System.Process qualified as Process
import Text.ANSI qualified as Text
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.HashTags (CausalHash, unCausalHash)
import U.Codebase.Reference (TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.MergeTypes (MergeSource (..), MergeSourceAndTarget (..), MergeSourceOrTarget (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Cli.UpdateUtils (hydrateRefs)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadShareLooseCode (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Debug qualified as Debug
import Unison.DeclCoherencyCheck (asOneRandomIncoherentDeclReason)
import Unison.Hash qualified as Hash
import Unison.Merge qualified as Merge
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoOrThreeWay qualified as TwoOrThreeWay
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup qualified as PartialDeclNameLookup
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchName,
    ProjectName,
    projectBranchNameToValidProjectBranchNameText,
  )
import Unison.Reference (TermReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, defnsAreEmpty)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Nametree (Nametree (..))
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.WatchKind qualified as WatchKind
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip, zipWith)

handleMerge :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleMerge (ProjectAndBranch maybeBobProjectName bobBranchName) = do
  -- Assert that Alice (us) is on a project branch, and grab the causal hash.
  ProjectPath aliceProject aliceProjectBranch _path <- Cli.getCurrentProjectPath
  let aliceProjectAndBranch = ProjectAndBranch aliceProject aliceProjectBranch

  -- Resolve Bob's maybe-project-name + branch-name to the info the merge algorithm needs: the project name, branch
  -- name, and causal hash.
  bobProject <-
    case maybeBobProjectName of
      Nothing -> pure aliceProjectAndBranch.project
      Just bobProjectName
        | bobProjectName == aliceProjectAndBranch.project.name -> pure aliceProjectAndBranch.project
        | otherwise -> do
            Cli.runTransaction (Queries.loadProjectByName bobProjectName)
              & onNothingM (Cli.returnEarly (Output.LocalProjectDoesntExist bobProjectName))
  bobProjectBranch <- ProjectUtils.expectProjectBranchByName bobProject bobBranchName
  let bobProjectAndBranch = ProjectAndBranch bobProject bobProjectBranch

  doMergeLocalBranch
    Merge.TwoWay
      { alice = aliceProjectAndBranch,
        bob = bobProjectAndBranch
      }

data MergeInfo = MergeInfo
  { alice :: !AliceMergeInfo,
    bob :: !BobMergeInfo,
    lca :: !LcaMergeInfo,
    -- | How should we describe this merge in the reflog?
    description :: !Text
  }

data AliceMergeInfo = AliceMergeInfo
  { causalHash :: !CausalHash,
    projectAndBranch :: !(ProjectAndBranch Project ProjectBranch)
  }

data BobMergeInfo = BobMergeInfo
  { causalHash :: !CausalHash,
    source :: !MergeSource
  }

newtype LcaMergeInfo = LcaMergeInfo
  { causalHash :: Maybe CausalHash
  }

doMerge :: MergeInfo -> Cli ()
doMerge info = do
  let debugFunctions =
        if Debug.shouldDebug Debug.Merge
          then realDebugFunctions
          else fakeDebugFunctions

  -- When debugging, don't bother with progress messages, so debug output is cleaner and doesn't disappear
  let withRespondRegion :: ((Output -> IO ()) -> Cli a) -> Cli a
      withRespondRegion =
        if Debug.shouldDebug Debug.Merge
          then \f -> f \_output -> pure ()
          else Cli.withRespondRegionIO

  let aliceBranchNames = ProjectUtils.justTheNames info.alice.projectAndBranch
  let mergeSource = MergeSourceOrTarget'Source info.bob.source
  let mergeTarget = MergeSourceOrTarget'Target aliceBranchNames
  let mergeSourceAndTarget = MergeSourceAndTarget {alice = aliceBranchNames, bob = info.bob.source}

  env <- ask

  finalOutput <-
    Cli.label \done -> do
      -- If alice == bob, or LCA == bob (so alice is ahead of bob), then we are done.
      when (info.alice.causalHash == info.bob.causalHash || info.lca.causalHash == Just info.bob.causalHash) do
        done (Output.MergeAlreadyUpToDate2 mergeSourceAndTarget)

      -- Otherwise, if LCA == alice (so alice is behind bob), then we could fast forward to bob, so we're done.
      when (info.lca.causalHash == Just info.alice.causalHash) do
        bobBranch <- liftIO (Codebase.expectBranchForHash env.codebase info.bob.causalHash)
        _ <- Cli.updateAt info.description (PP.projectBranchRoot info.alice.projectAndBranch) (\_aliceBranch -> bobBranch)
        done (Output.MergeSuccessFastForward mergeSourceAndTarget)

      withRespondRegion \respondRegion -> do
        liftIO (respondRegion (Output.Literal "Loading namespaces..."))

        -- Load Alice/Bob/LCA branches
        branches <-
          Cli.runTransaction do
            traverse
              (Codebase.expectBranchForHashTx env.codebase)
              Merge.TwoOrThreeWay
                { alice = info.alice.causalHash,
                  bob = info.bob.causalHash,
                  lca = info.lca.causalHash
                }

        -- Assert that neither Alice nor Bob have defns in lib
        for_ [(mergeTarget, Branch.head branches.alice), (mergeSource, Branch.head branches.bob)] \(who, branch) -> do
          when (Branch.hasDefnsInLib branch) do
            done (Output.MergeDefnsInLib who)

        -- Derive unconflicted defns views
        --
        -- FIXME: Oops, if this fails due to a conflicted name, we don't actually say where the conflicted name came from.
        -- We should have a better error message (even though you can't do anything about conflicted names in the LCA).
        defns <- do
          let asUnconflicted branch = Branch.asUnconflicted branch & onLeft (done . Output.ConflictedDefn "merge")
          lca <-
            case branches.lca of
              Just lca -> asUnconflicted (Branch.head lca)
              Nothing ->
                pure
                  Branch.UnconflictedBranchView
                    { defns = Defns BiMultimap.empty BiMultimap.empty,
                      nametree = Nametree (Defns Map.empty Map.empty) Map.empty,
                      names = Names Relation.empty Relation.empty
                    }
          alice <- asUnconflicted (Branch.head branches.alice)
          bob <- asUnconflicted (Branch.head branches.bob)
          pure Merge.ThreeWay {lca, alice, bob}

        -- Load decl name lookups
        declNameLookups <- do
          onLeftM done do
            Cli.runTransactionWithRollbackE \rollback -> do
              lca <-
                case branches.lca of
                  Just lca -> Codebase.getBranchPartialDeclNameLookup env.codebase (Branch.namespaceHash lca) defns.lca
                  Nothing -> pure PartialDeclNameLookup.empty
              aliceAndBob <-
                sequence $
                  ( \x y z ->
                      Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash x) y
                        & onLeftM \reasons ->
                          rollback (Output.IncoherentDeclDuringMerge z (asOneRandomIncoherentDeclReason reasons))
                  )
                    <$> TwoOrThreeWay.forgetLca branches
                    <*> ThreeWay.forgetLca defns
                    <*> Merge.TwoWay {alice = mergeTarget, bob = mergeSource}
              pure (ThreeWay.gfromTwoWay lca aliceAndBob)

        (mergeblob, libdepsBranches) <- do
          let hydrate message refs
                | defnsAreEmpty refs = pure (Defns Map.empty Map.empty)
                | otherwise = do
                    Sqlite.unsafeIO (respondRegion (Output.Literal message))
                    hydrateRefs
                      (Codebase.unsafeGetTermComponent env.codebase)
                      Operations.expectDeclComponent
                      refs

          onLeftM done do
            Cli.runTransactionWithRollbackE \rollback -> do
              Sqlite.unsafeIO (respondRegion (Output.Literal "Computing diff..."))

              diffblob <-
                Merge.makeDiffblob
                  Merge.DiffblobLog
                    { logDefns =
                        -- Sqlite.unsafeIO . debugFunctions.debugDefns
                        mempty,
                      logNarrowedDefns = Sqlite.unsafeIO . debugFunctions.debugNarrowedDefns,
                      logSynhashedNarrowedDefns = Sqlite.unsafeIO . debugFunctions.debugSynhashedNarrowedDefns,
                      logDiffsFromLCA = Sqlite.unsafeIO . debugFunctions.debugDiffs,
                      logDiff = Sqlite.unsafeIO . debugFunctions.debugCombinedDiff
                    }
                  (hydrate "Loading definitions...")
                  (TwoOrThreeWay.toThreeWay Names.empty (Branch.toNames . view Branch.head_ <$> branches))
                  defns
                  ( let f = view (Branch.head_ . Branch.libdeps_)
                     in Merge.ThreeWay
                          { lca = maybe Map.empty f branches.lca,
                            alice = f branches.alice,
                            bob = f branches.bob
                          }
                  )
                  declNameLookups

              let libdepsBranches =
                    diffblob.libdeps & Updated.map \libdeps ->
                      Branch.empty0 & Branch.children_ .~ libdeps

              Sqlite.unsafeIO (respondRegion (Output.Literal "Computing merge..."))

              mergeblob <- do
                let handleMergeblobError err =
                      rollback case err of
                        Merge.MergeblobError'ConflictedAlias defn0 ->
                          case defn0 of
                            Merge.Alice defn -> Output.MergeConflictedAliases mergeTarget defn
                            Merge.Bob defn -> Output.MergeConflictedAliases mergeSource defn
                        Merge.MergeblobError'ConflictedBuiltin defn -> Output.MergeConflictInvolvingBuiltin defn
                onLeftM handleMergeblobError do
                  Merge.makeMergeblob
                    (hydrate "Loading more definitions...")
                    Operations.transitiveDependentsWithinScope
                    (pure (Updated.map Branch.toNames libdepsBranches))
                    (Codebase.typeLookupForDependencies env.codebase)
                    diffblob
                    Merge.TwoWay
                      { alice = into @Text aliceBranchNames,
                        bob =
                          case info.bob.source of
                            MergeSource'LocalProjectBranch bobBranch -> into @Text (ProjectUtils.justTheNames bobBranch)
                            MergeSource'RemoteProjectBranch bobBranch
                              | aliceBranchNames == bobBranchNames -> "remote " <> into @Text bobBranchNames
                              | otherwise -> into @Text bobBranchNames
                              where
                                bobBranchNames =
                                  ProjectAndBranch bobBranch.projectName bobBranch.branchName
                            MergeSource'RemoteLooseCode info ->
                              case Path.toName info.path of
                                Nothing -> "<root>"
                                Just name -> Name.toText name
                      }

              pure (mergeblob, libdepsBranches)

        let makeMergeNode :: (Branch0 Transaction -> Branch0 Transaction) -> Branch Transaction
            makeMergeNode =
              let unconflictedBranch =
                    Branch.fromUnconflictedDefns mergeblob.unconflictedDefns
                      & Branch.setLibdeps libdepsBranches.new
               in \f ->
                    Branch.mergeNode
                      (f unconflictedBranch)
                      (Branch.headHash branches.alice, pure branches.alice)
                      (Branch.headHash branches.bob, pure branches.bob)

        typecheckedFile <-
          mergeblob.typecheckedFile & onNothing do
            env <- ask
            (_temporaryBranchId, temporaryBranchName) <-
              HandleInput.Branch.createBranch
                info.description
                ( let sourceStuff =
                        ( case info.bob.source of
                            MergeSource'LocalProjectBranch bobBranch ->
                              HandleInput.Branch.CreateFromMergeSource'Local bobBranch.branch
                            MergeSource'RemoteProjectBranch bobBranch ->
                              HandleInput.Branch.CreateFromMergeSource'Remote bobBranch Share.hardCodedUri
                            MergeSource'RemoteLooseCode _ -> HandleInput.Branch.CreateFromMergeSource'LooseCode,
                          info.bob.causalHash,
                          mergeblob.uniqueTypeGuids.bob
                        )
                      targetStuff =
                        ( info.alice.projectAndBranch.branch,
                          info.alice.causalHash,
                          mergeblob.uniqueTypeGuids.alice
                        )
                      mergeStuff = makeMergeNode id
                   in HandleInput.Branch.CreateFrom'MergeParents sourceStuff targetStuff mergeStuff
                )
                info.alice.projectAndBranch.project
                (findTemporaryBranchName info.alice.projectAndBranch.project.projectId mergeSourceAndTarget)

            --   Merge conflicts?    Have UCM_MERGETOOL?    Result
            --   ----------------    -------------------    ------------------------------------------------------------
            --                 No                     No           Put code that doesn't parse or typecheck in scratch.u
            --                 No                    Yes           Put code that doesn't parse or typecheck in scratch.u
            --                Yes                     No    Put code that doesn't parse (because conflicts) in scratch.u
            --                Yes                    Yes                                              Run that cool tool

            maybeMergetool <-
              if not (defnsAreEmpty mergeblob.conflicts.alice)
                then liftIO (lookupEnv "UCM_MERGETOOL")
                else pure Nothing

            case maybeMergetool of
              Nothing -> do
                scratchFilePath <-
                  Cli.getLatestFile <&> \case
                    Nothing -> "scratch.u"
                    Just (file, _) -> file
                liftIO $
                  env.writeSource
                    (Text.pack scratchFilePath)
                    (Text.pack $ Pretty.toPlain 80 mergeblob.unparsedFile)
                    True
                done (Output.MergeFailure scratchFilePath mergeSourceAndTarget temporaryBranchName)
              Just mergetool0 -> do
                let aliceFilenameSlug = projectBranchNameToValidProjectBranchNameText mergeSourceAndTarget.alice.branch
                let bobFilenameSlug = mangleMergeSource mergeSourceAndTarget.bob
                makeTempFilename <-
                  liftIO do
                    tmpdir0 <- getTemporaryDirectory
                    tmpdir1 <- canonicalizePath tmpdir0
                    tmpdir2 <- Temporary.createTempDirectory tmpdir1 "unison-merge"
                    pure \filename -> Text.pack (tmpdir2 </> Text.unpack (Text.Builder.run filename))
                let filenames =
                      fmap
                        makeTempFilename
                        Merge.ThreeWay
                          { lca = aliceFilenameSlug <> "-" <> bobFilenameSlug <> "-base.u",
                            alice = aliceFilenameSlug <> ".u",
                            bob = bobFilenameSlug <> ".u"
                          }
                let mergedFilename = Text.Builder.run (aliceFilenameSlug <> "-" <> bobFilenameSlug <> "-merged.u")
                let mergetool =
                      mergetool0
                        & Text.pack
                        & Text.replace "$BASE" filenames.lca
                        & Text.replace "$LOCAL" filenames.alice
                        & Text.replace "$MERGED" mergedFilename
                        & Text.replace "$REMOTE" filenames.bob
                exitCode <-
                  liftIO do
                    let fileContents = Text.pack . Pretty.toPlain 80 <$> mergeblob.unparsedSoloFiles
                    removeFile (Text.unpack mergedFilename) <|> pure ()
                    for_ ((,) <$> filenames <*> fileContents) \(name, contents) ->
                      env.writeSource name contents True
                    env.writeSource
                      mergedFilename
                      ( makeMergedFileContents
                          mergeSourceAndTarget
                          fileContents.alice
                          fileContents.bob
                      )
                      True
                    let createProcess = (Process.shell (Text.unpack mergetool)) {Process.delegate_ctlc = True}
                    Process.withCreateProcess createProcess \_ _ _ -> Process.waitForProcess
                done (Output.MergeFailureWithMergetool mergeSourceAndTarget temporaryBranchName mergetool exitCode)

        Cli.runTransaction (Codebase.addDefsToCodebase env.codebase typecheckedFile)
        Cli.updateProjectBranchRoot_
          info.alice.projectAndBranch.branch
          info.description
          \_aliceBranch ->
            typecheckedFile
              & typecheckedUnisonFileToBranchAdds
              & Branch.batchUpdates
              & makeMergeNode
              -- Awkward: we have a Branch Transaction but we need a Branch IO (because reasons)
              & Branch.transform (Codebase.runTransaction env.codebase)
        pure (Output.MergeSuccess mergeSourceAndTarget)

  Cli.respond finalOutput

doMergeLocalBranch :: Merge.TwoWay (ProjectAndBranch Project ProjectBranch) -> Cli ()
doMergeLocalBranch branches = do
  (aliceCausalHash, bobCausalHash, lcaCausalHash) <-
    Cli.runTransaction do
      aliceCausalHash <- ProjectUtils.getProjectBranchCausalHash (branches.alice ^. #branch)
      bobCausalHash <- ProjectUtils.getProjectBranchCausalHash (branches.bob ^. #branch)
      -- Using Alice and Bob's causal hashes, find the LCA (if it exists)
      lcaCausalHash <- Operations.lca aliceCausalHash bobCausalHash
      pure (aliceCausalHash, bobCausalHash, lcaCausalHash)

  -- Do the merge!
  doMerge
    MergeInfo
      { alice =
          AliceMergeInfo
            { causalHash = aliceCausalHash,
              projectAndBranch = branches.alice
            },
        bob =
          BobMergeInfo
            { causalHash = bobCausalHash,
              source = MergeSource'LocalProjectBranch branches.bob
            },
        lca =
          LcaMergeInfo
            { causalHash = lcaCausalHash
            },
        description = "merge " <> into @Text (ProjectUtils.justTheNames branches.bob)
      }

------------------------------------------------------------------------------------------------------------------------
-- Merge precondition violation checks

hasDefnsInLib :: (Applicative m) => V2.Branch m -> m Bool
hasDefnsInLib branch = do
  ( case Map.lookup NameSegment.libSegment branch.children of
      Nothing -> pure V2.Branch.empty
      Just libdeps -> libdeps.value
    )
    <&> \libdeps -> not (Map.null libdeps.terms) || not (Map.null libdeps.types)

------------------------------------------------------------------------------------------------------------------------
--

findTemporaryBranchName :: ProjectId -> MergeSourceAndTarget -> Transaction ProjectBranchName
findTemporaryBranchName projectId mergeSourceAndTarget = do
  ProjectUtils.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        Text.Builder.run $
          "merge-"
            <> mangleMergeSource mergeSourceAndTarget.bob
            <> "-into-"
            <> projectBranchNameToValidProjectBranchNameText mergeSourceAndTarget.alice.branch

mangleMergeSource :: MergeSource -> Text.Builder
mangleMergeSource = \case
  MergeSource'LocalProjectBranch (ProjectAndBranch _project branch) -> projectBranchNameToValidProjectBranchNameText branch.name
  MergeSource'RemoteProjectBranch remoteBranch -> "remote-" <> projectBranchNameToValidProjectBranchNameText remoteBranch.branchName
  MergeSource'RemoteLooseCode info -> manglePath info.path
  where
    manglePath :: Path -> Text.Builder
    manglePath =
      Monoid.intercalateMap "-" (Text.Builder.text . NameSegment.toUnescapedText) . Path.toList

typecheckedUnisonFileToBranchAdds :: TypecheckedUnisonFile Symbol Ann -> [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchAdds tuf = do
  declAdds ++ termAdds
  where
    declAdds :: [(Path, Branch0 m -> Branch0 m)]
    declAdds = do
      foldMap makeDataDeclAdds (Map.toList (UnisonFile.dataDeclarationsId' tuf))
        ++ foldMap makeEffectDeclUpdates (Map.toList (UnisonFile.effectDeclarationsId' tuf))
      where
        makeDataDeclAdds (symbol, (typeRefId, dataDecl)) = makeDeclAdds (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclAdds (symbol, (typeRefId, Left effectDecl))

        makeDeclAdds :: (Symbol, (TypeReferenceId, Decl Symbol Ann)) -> [(Path, Branch0 m -> Branch0 m)]
        makeDeclAdds (symbol, (typeRefId, decl)) =
          let insertTypeAction = BranchUtil.makeAddTypeName (splitVar symbol) (Reference.fromId typeRefId)
              insertTypeConstructorActions =
                zipWith
                  (\sym rid -> BranchUtil.makeAddTermName (splitVar sym) (Reference.fromId <$> rid))
                  (DataDeclaration.constructorVars (DataDeclaration.asDataDecl decl))
                  (DataDeclaration.declConstructorReferents typeRefId decl)
           in insertTypeAction : insertTypeConstructorActions

    termAdds :: [(Path, Branch0 m -> Branch0 m)]
    termAdds =
      tuf
        & UnisonFile.hashTermsId
        & Map.toList
        & mapMaybe \(var, (_, ref, wk, _, _)) -> do
          guard (WatchKind.watchKindShouldBeStoredInDatabase wk)
          Just (BranchUtil.makeAddTermName (splitVar var) (Referent.fromTermReferenceId ref))

    splitVar :: Symbol -> Path.Split Path
    splitVar = Path.splitFromName . Name.unsafeParseVar

------------------------------------------------------------------------------------------------------------------------
-- Making file with conflict markers

makeMergedFileContents :: MergeSourceAndTarget -> Text -> Text -> Text
makeMergedFileContents sourceAndTarget aliceContents bobContents =
  let f :: (Text.Builder, Diff.Diff Text) -> Diff.Diff Text -> (Text.Builder, Diff.Diff Text)
      f (acc, previous) line =
        case (previous, line) of
          (Diff.Both {}, Diff.Both bothLine _) -> go (Text.Builder.text bothLine)
          (Diff.Both {}, Diff.First aliceLine) -> go (aliceSlug <> Text.Builder.text aliceLine)
          (Diff.Both {}, Diff.Second bobLine) -> go (aliceSlug <> middleSlug <> Text.Builder.text bobLine)
          (Diff.First {}, Diff.Both bothLine _) -> go (middleSlug <> bobSlug <> Text.Builder.text bothLine)
          (Diff.First {}, Diff.First aliceLine) -> go (Text.Builder.text aliceLine)
          (Diff.First {}, Diff.Second bobLine) -> go (middleSlug <> Text.Builder.text bobLine)
          (Diff.Second {}, Diff.Both bothLine _) -> go (bobSlug <> Text.Builder.text bothLine)
          (Diff.Second {}, Diff.First aliceLine) -> go (bobSlug <> aliceSlug <> Text.Builder.text aliceLine)
          (Diff.Second {}, Diff.Second bobLine) -> go (Text.Builder.text bobLine)
        where
          go content =
            let !acc1 = acc <> content <> newline
             in (acc1, line)
   in Diff.getDiff (Text.lines aliceContents) (Text.lines bobContents)
        & List.foldl' f (mempty @Text.Builder, Diff.Both Text.empty Text.empty)
        & fst
        & Text.Builder.run
  where
    aliceSlug :: Text.Builder
    aliceSlug =
      "<<<<<<< " <> Text.Builder.text (into @Text sourceAndTarget.alice.branch) <> newline

    middleSlug :: Text.Builder
    middleSlug = "=======\n"

    bobSlug :: Text.Builder
    bobSlug =
      ">>>>>>> "
        <> ( case sourceAndTarget.bob of
               MergeSource'LocalProjectBranch bobProjectAndBranch ->
                 Text.Builder.text (into @Text bobProjectAndBranch.branch.name)
               MergeSource'RemoteProjectBranch bobRemoteBranch ->
                 "remote " <> Text.Builder.text (into @Text bobRemoteBranch.branchName)
               MergeSource'RemoteLooseCode info ->
                 case Path.toName info.path of
                   Nothing -> "<root>"
                   Just name -> Text.Builder.text (Name.toText name)
           )
        <> newline

    newline :: Text.Builder
    newline = "\n"

------------------------------------------------------------------------------------------------------------------------
-- Debugging by printing a bunch of stuff out

data DebugFunctions = DebugFunctions
  { debugCausals :: Merge.TwoOrThreeWay (V2.CausalBranch Transaction) -> IO (),
    debugCoreDependencies ::
      Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
      Merge.TwoWay (DefnsF Set TermReference TypeReference) ->
      IO (),
    debugDefns :: Merge.ThreeWay (DefnsF (Map Name) Referent TypeReference) -> IO (),
    debugDiffs :: Merge.TwoWay (DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference) -> IO (),
    debugCombinedDiff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference -> IO (),
    debugHumanDiffs :: Merge.TwoWay (DefnsF2 (Map Name) Merge.HumanDiffOp Referent TypeReference) -> IO (),
    debugInitialDependents ::
      Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
      Merge.TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
      IO (),
    debugNarrowedDefns :: Merge.TwoWay (Merge.Updated (DefnsF (Map Name) Referent TypeReference)) -> IO (),
    debugPartitionedDiff ::
      Merge.TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
      DefnsF Merge.Unconflicts Referent TypeReference ->
      IO (),
    debugRenames :: Merge.TwoWay (DefnsF [] Merge.Rename Merge.Rename) -> IO (),
    debugSimpleRenames :: Merge.TwoWay (Defns Merge.SimpleRenames Merge.SimpleRenames) -> IO (),
    debugSynhashedNarrowedDefns ::
      Merge.TwoWay (Merge.Updated (DefnsF2 (Map Name) Merge.Synhashed Referent TypeReference)) ->
      IO ()
  }

realDebugFunctions :: DebugFunctions
realDebugFunctions =
  DebugFunctions
    { debugCausals = realDebugCausals,
      debugCoreDependencies = realDebugCoreDependencies,
      debugDefns = realDebugDefns,
      debugDiffs = realDebugDiffs,
      debugCombinedDiff = realDebugCombinedDiff,
      debugHumanDiffs = realDebugHumanDiffs,
      debugInitialDependents = realDebugInitialDependents,
      debugNarrowedDefns = realDebugNarrowedDefns,
      debugPartitionedDiff = realDebugPartitionedDiff,
      debugRenames = realDebugRenames,
      debugSimpleRenames = realDebugSimpleRenames,
      debugSynhashedNarrowedDefns = realDebugSynhashedNarrowedDefns
    }

fakeDebugFunctions :: DebugFunctions
fakeDebugFunctions =
  DebugFunctions
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

realDebugCausals :: Merge.TwoOrThreeWay (V2.CausalBranch Transaction) -> IO ()
realDebugCausals causals = do
  Text.putStrLn (Text.bold "\n=== Alice causal hash ===")
  Text.putStrLn (Hash.toBase32HexText (unCausalHash causals.alice.causalHash))
  Text.putStrLn (Text.bold "\n=== Bob causal hash ===")
  Text.putStrLn (Hash.toBase32HexText (unCausalHash causals.bob.causalHash))
  Text.putStrLn (Text.bold "\n=== LCA causal hash ===")
  Text.putStrLn case causals.lca of
    Nothing -> "Nothing"
    Just causal -> "Just " <> Hash.toBase32HexText (unCausalHash causal.causalHash)

realDebugDefns :: Merge.ThreeWay (DefnsF (Map Name) Referent TypeReference) -> IO ()
realDebugDefns defns = do
  Text.putStrLn (Text.bold "\n=== LCA defns ===")
  renderDefns defns.lca
  Text.putStrLn (Text.bold "\n=== Alice defns ===")
  renderDefns defns.alice
  Text.putStrLn (Text.bold "\n=== Bob defns ===")
  renderDefns defns.bob
  where
    renderDefns :: DefnsF (Map Name) Referent TypeReference -> IO ()
    renderDefns defns = do
      renderThings referentLabel Referent.toText defns.terms
      renderThings (const "type") Reference.toText defns.types

    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name ref -> IO ()
    renderThings label render =
      Map.toList
        >>> over (mapped . _1) Name.toText
        >>> sortAlphabeticallyOn fst
        >>> traverse_ \(name, ref) ->
          Text.putStrLn (Text.italic (label ref) <> " " <> name <> " " <> render ref)

realDebugDiffs :: Merge.TwoWay (DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference) -> IO ()
realDebugDiffs diffs = do
  Text.putStrLn (Text.bold "\n=== LCA→Alice diff ===")
  renderDiff diffs.alice
  Text.putStrLn (Text.bold "\n=== LCA→Bob diff ===")
  renderDiff diffs.bob
  where
    renderDiff :: DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference -> IO ()
    renderDiff diff = do
      renderThings referentLabel diff.terms
      renderThings (const "type") diff.types

    renderThings :: (ref -> Text) -> Map Name (Merge.DiffOp (Merge.Synhashed ref)) -> IO ()
    renderThings label =
      Map.toList
        >>> over (mapped . _1) Name.toText
        >>> sortAlphabeticallyOn fst
        >>> traverse_ \(name, op) ->
          let go color action x =
                color $
                  action
                    <> " "
                    <> Text.italic (label (Synhashed.value x))
                    <> " "
                    <> name
                    <> " #"
                    <> Hash.toBase32HexText (Synhashed.hash x)
           in Text.putStrLn case op of
                Merge.DiffOp'Add x -> go Text.green "+" x
                Merge.DiffOp'Delete x -> go Text.red "-" x
                Merge.DiffOp'Update x -> go Text.yellow "%" x.new

realDebugHumanDiffs :: Merge.TwoWay (DefnsF2 (Map Name) Merge.HumanDiffOp Referent TypeReference) -> IO ()
realDebugHumanDiffs diffs = do
  Text.putStrLn (Text.bold "\n=== LCA→Alice diff (humanized) ===")
  renderDiff diffs.alice
  Text.putStrLn (Text.bold "\n=== LCA→Bob diff (humanized) ===")
  renderDiff diffs.bob
  where
    renderDiff :: DefnsF2 (Map Name) Merge.HumanDiffOp Referent TypeReference -> IO ()
    renderDiff diff = do
      renderThings referentLabel Referent.toText diff.terms
      renderThings (const "type") Reference.toText diff.types

    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name (Merge.HumanDiffOp ref) -> IO ()
    renderThings label textify things =
      for_ (Map.toList things) \(name, op) ->
        Text.putStrLn case op of
          Merge.HumanDiffOp'Add x ->
            Text.green $
              "add "
                <> Text.italic (label x)
                <> " "
                <> Name.toText name
                <> " "
                <> textify x
          Merge.HumanDiffOp'Delete x ->
            Text.red $
              "delete "
                <> Text.italic (label x)
                <> " "
                <> Name.toText name
                <> " "
                <> textify x
          Merge.HumanDiffOp'Update x ->
            Text.yellow $
              "update "
                <> Text.italic (label x.old)
                <> " "
                <> Name.toText name
                <> "\n  "
                <> textify x.old
                <> "\n  → "
                <> textify x.new
          Merge.HumanDiffOp'PropagatedUpdate x ->
            Text.brightBlack $
              "update (propagated) "
                <> Text.italic (label x.old)
                <> " "
                <> Name.toText name
                <> "\n  "
                <> textify x.old
                <> "\n  → "
                <> textify x.new
          Merge.HumanDiffOp'AliasOf x _ ->
            Text.brightBlack $
              "add (alias) "
                <> Text.italic (label x)
                <> " "
                <> Name.toText name
                <> " "
                <> textify x
          Merge.HumanDiffOp'RenamedFrom x oldNames ->
            Text.magenta $
              "rename "
                <> Text.italic (label x)
                <> " "
                <> textify x
                <> "\n  "
                <> Name.toText name
                <> " ← "
                <> Text.unwords (map Name.toText (Foldable.toList oldNames))
          Merge.HumanDiffOp'RenamedTo x newNames ->
            Text.magenta $
              "rename "
                <> Text.italic (label x)
                <> " "
                <> textify x
                <> "\n  "
                <> Name.toText name
                <> " → "
                <> Text.unwords (map Name.toText (Foldable.toList newNames))

realDebugCombinedDiff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference -> IO ()
realDebugCombinedDiff diff = do
  Text.putStrLn (Text.bold "\n=== Combined diff ===")
  renderThings referentLabel Referent.toText diff.terms
  renderThings (const "type") Reference.toText diff.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name (Merge.CombinedDiffOp ref) -> IO ()
    renderThings label renderRef things =
      things
        & Map.toList
        & over (mapped . _1) Name.toText
        & sortAlphabeticallyOn fst
        & traverse_ \(name, op) ->
          Text.putStrLn case op of
            Merge.CombinedDiffOp'Add who ->
              Text.green $
                "+ "
                  <> Text.italic (label (EitherWayI.value who))
                  <> " "
                  <> name
                  <> " "
                  <> renderRef (EitherWayI.value who)
                  <> " ("
                  <> renderWho who
                  <> ")"
            Merge.CombinedDiffOp'Delete who ->
              Text.red $
                "- "
                  <> Text.italic (label (EitherWayI.value who))
                  <> " "
                  <> name
                  <> " "
                  <> renderRef (EitherWayI.value who)
                  <> " ("
                  <> renderWho who
                  <> ")"
            Merge.CombinedDiffOp'Update who ->
              Text.yellow $
                "% "
                  <> Text.italic (label (EitherWayI.value who).new)
                  <> " "
                  <> name
                  <> " "
                  <> renderRef (EitherWayI.value who).new
                  <> " ("
                  <> renderWho who
                  <> ")"
            Merge.CombinedDiffOp'Conflict ref ->
              Text.magenta $
                "! "
                  <> Text.italic (label ref.alice)
                  <> "/"
                  <> Text.italic (label ref.bob)
                  <> " "
                  <> name
                  <> " "
                  <> renderRef ref.alice
                  <> "/"
                  <> renderRef ref.bob

    renderWho :: Merge.EitherWayI v -> Text
    renderWho = \case
      Merge.OnlyAlice _ -> "Alice"
      Merge.OnlyBob _ -> "Bob"
      Merge.AliceAndBob _ -> "Alice and Bob"

realDebugCoreDependencies ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (DefnsF Set TermReference TypeReference) ->
  IO ()
realDebugCoreDependencies defns dependencies = do
  Text.putStrLn (Text.bold "\n=== Alice core dependencies ===")
  renderDependencies defns.alice dependencies.alice

  Text.putStrLn (Text.bold "\n=== Bob core dependencies ===")
  renderDependencies defns.bob dependencies.bob
  where
    renderDependencies ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      DefnsF Set TermReference TypeReference ->
      IO ()
    renderDependencies defns dependencies = do
      dependencies.terms
        & Set.toList
        & map (\dep -> (dep, termNames dep))
        & sortAlphabeticallyOn snd
        & traverse_ \(dep, names) ->
          Text.putStrLn (Text.italic "term" <> " " <> names <> Reference.toText dep)
      dependencies.types
        & Set.toList
        & map (\dep -> (dep, typeNames dep))
        & sortAlphabeticallyOn snd
        & traverse_ \(dep, names) ->
          Text.putStrLn (Text.italic "type" <> " " <> names <> Reference.toText dep)
      where
        termNames :: TermReference -> Text
        termNames ref =
          foldMap
            (\name -> Name.toText name <> " ")
            (BiMultimap.lookupDom (Referent.fromTermReference ref) defns.terms)

        typeNames :: TypeReference -> Text
        typeNames ref =
          foldMap
            (\name -> Name.toText name <> " ")
            (BiMultimap.lookupDom ref defns.types)

realDebugInitialDependents ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  IO ()
realDebugInitialDependents defns dependents = do
  Text.putStrLn (Text.bold "\n=== Alice initial dependents ===")
  renderDependents defns.alice dependents.alice

  Text.putStrLn (Text.bold "\n=== Bob initial dependents ===")
  renderDependents defns.bob dependents.bob
  where
    renderDependents ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      DefnsF Set TermReferenceId TypeReferenceId ->
      IO ()
    renderDependents defns dependents = do
      dependents.terms
        & Set.toList
        & map (\dep -> (dep, termNames dep))
        & sortAlphabeticallyOn snd
        & traverse_ \(dep, names) ->
          Text.putStrLn (Text.italic "term" <> " " <> names <> Reference.idToText dep)
      dependents.types
        & Set.toList
        & map (\dep -> (dep, typeNames dep))
        & sortAlphabeticallyOn snd
        & traverse_ \(dep, names) ->
          Text.putStrLn (Text.italic "type" <> " " <> names <> Reference.idToText dep)
      where
        termNames :: TermReferenceId -> Text
        termNames ref =
          foldMap
            (\name -> Name.toText name <> " ")
            (BiMultimap.lookupDom (Referent.fromTermReferenceId ref) defns.terms)

        typeNames :: TypeReferenceId -> Text
        typeNames ref =
          foldMap
            (\name -> Name.toText name <> " ")
            (BiMultimap.lookupDom (Reference.fromId ref) defns.types)

realDebugNarrowedDefns :: Merge.TwoWay (Merge.Updated (DefnsF (Map Name) Referent TypeReference)) -> IO ()
realDebugNarrowedDefns defns = do
  Text.putStrLn (Text.bold "\n=== Narrowed LCA→Alice defns (LCA) ===")
  renderDefns defns.alice.old
  Text.putStrLn (Text.bold "\n=== Narrowed LCA→Alice defns (Alice) ===")
  renderDefns defns.alice.new
  Text.putStrLn (Text.bold "\n=== Narrowed LCA→Bob defns (LCA) ===")
  renderDefns defns.bob.old
  Text.putStrLn (Text.bold "\n=== Narrowed LCA→Bob defns (Bob) ===")
  renderDefns defns.bob.new
  where
    renderDefns :: DefnsF (Map Name) Referent TypeReference -> IO ()
    renderDefns defns = do
      renderThings referentLabel Referent.toText defns.terms
      renderThings (const "type") Reference.toText defns.types

    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name ref -> IO ()
    renderThings label render =
      Map.toList
        >>> over (mapped . _1) Name.toText
        >>> sortAlphabeticallyOn fst
        >>> traverse_ \(name, ref) ->
          Text.putStrLn (Text.italic (label ref) <> " " <> name <> " " <> render ref)

realDebugPartitionedDiff ::
  Merge.TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  DefnsF Merge.Unconflicts Referent TypeReference ->
  IO ()
realDebugPartitionedDiff conflicts unconflicts = do
  Text.putStrLn (Text.bold "\n=== Alice conflicts ===")
  renderConflicts "termid" conflicts.alice.terms (Merge.Alice ())
  renderConflicts "typeid" conflicts.alice.types (Merge.Alice ())

  Text.putStrLn (Text.bold "\n=== Bob conflicts ===")
  renderConflicts "termid" conflicts.bob.terms (Merge.Bob ())
  renderConflicts "typeid" conflicts.bob.types (Merge.Bob ())

  Text.putStrLn (Text.bold "\n=== Alice unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.alice
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.alice
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.alice
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.alice
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.alice
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.alice

  Text.putStrLn (Text.bold "\n=== Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.bob
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.bob
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.bob
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.bob
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.bob
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.bob

  Text.putStrLn (Text.bold "\n=== Alice-and-Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.both
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.both
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.both
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.both
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.both
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.both
  where
    renderConflicts :: Text -> Map Name Reference.Id -> Merge.EitherWay () -> IO ()
    renderConflicts label conflicts who =
      conflicts
        & Map.toList
        & over (mapped . _1) Name.toText
        & sortAlphabeticallyOn fst
        & traverse_ \(name, ref) ->
          Text.putStrLn $
            Text.magenta $
              "! "
                <> Text.italic label
                <> " "
                <> name
                <> " "
                <> Reference.idToText ref
                <> " ("
                <> (case who of Merge.Alice () -> "Alice"; Merge.Bob () -> "Bob")
                <> ")"

    renderUnconflicts ::
      (Text -> Text) ->
      Text ->
      (ref -> Text) ->
      (ref -> Text) ->
      Map Name ref ->
      IO ()
    renderUnconflicts color action label renderRef unconflicts =
      unconflicts
        & Map.toList
        & over (mapped . _1) Name.toText
        & sortAlphabeticallyOn fst
        & traverse_ \(name, ref) ->
          Text.putStrLn $
            color $
              action
                <> " "
                <> Text.italic (label ref)
                <> " "
                <> name
                <> " "
                <> renderRef ref

realDebugRenames :: Merge.TwoWay (DefnsF [] Merge.Rename Merge.Rename) -> IO ()
realDebugRenames renames = do
  Text.putStrLn (Text.bold "\n=== Alice renames ===")
  renderRenames renames.alice
  Text.putStrLn (Text.bold "\n=== Bob renames ===")
  renderRenames renames.bob
  where
    renderRenames :: DefnsF [] Merge.Rename Merge.Rename -> IO ()
    renderRenames renames = do
      for_ renames.terms \rename ->
        Text.putStrLn (Text.italic "term" <> " " <> renderRename rename)
      for_ renames.types \rename ->
        Text.putStrLn (Text.italic "type" <> " " <> renderRename rename)

    renderRename :: Merge.Rename -> Text
    renderRename rename =
      Text.unwords $
        catMaybes
          [ case Set.toList rename.unchanged of
              [] -> Nothing
              unchanged -> Just (Text.unwords (map Name.toText unchanged)),
            case Set.toList rename.deletes of
              [] -> Nothing
              deletes -> Just (Text.unwords (map (\name -> Text.red ("-" <> Name.toText name)) deletes)),
            case Set.toList rename.adds of
              [] -> Nothing
              adds -> Just (Text.unwords (map (\name -> Text.green ("+" <> Name.toText name)) adds))
          ]

realDebugSimpleRenames :: Merge.TwoWay (Defns Merge.SimpleRenames Merge.SimpleRenames) -> IO ()
realDebugSimpleRenames renames = do
  Text.putStrLn (Text.bold "\n=== Alice simple renames ===")
  renderRenames renames.alice
  Text.putStrLn (Text.bold "\n=== Bob simple renames ===")
  renderRenames renames.bob
  where
    renderRenames :: Defns Merge.SimpleRenames Merge.SimpleRenames -> IO ()
    renderRenames renames = do
      renames.terms.forwards
        & Map.toList
        & map (\(old, new) -> Text.italic "term" <> " " <> Name.toText old <> " → " <> Name.toText new)
        & Text.unlines
        & Text.putStr
      renames.types.forwards
        & Map.toList
        & map (\(old, new) -> Text.italic "type" <> " " <> Name.toText old <> " → " <> Name.toText new)
        & Text.unlines
        & Text.putStr

realDebugSynhashedNarrowedDefns :: Merge.TwoWay (Merge.Updated (DefnsF2 (Map Name) Merge.Synhashed Referent TypeReference)) -> IO ()
realDebugSynhashedNarrowedDefns defns = do
  Text.putStrLn (Text.bold "\n=== Synhashed narrowed LCA→Alice defns (LCA) ===")
  renderDefns defns.alice.old
  Text.putStrLn (Text.bold "\n=== Synhashed narrowed LCA→Alice defns (Alice) ===")
  renderDefns defns.alice.new
  Text.putStrLn (Text.bold "\n=== Synhashed narrowed LCA→Bob defns (LCA) ===")
  renderDefns defns.bob.old
  Text.putStrLn (Text.bold "\n=== Synhashed narrowed LCA→Bob defns (Bob) ===")
  renderDefns defns.bob.new
  where
    renderDefns :: DefnsF2 (Map Name) Merge.Synhashed Referent TypeReference -> IO ()
    renderDefns defns = do
      renderThings referentLabel defns.terms
      renderThings (const "type") defns.types

    renderThings :: (ref -> Text) -> Map Name (Merge.Synhashed ref) -> IO ()
    renderThings label =
      Map.toList
        >>> over (mapped . _1) Name.toText
        >>> sortAlphabeticallyOn fst
        >>> traverse_ \(name, ref) ->
          Text.putStrLn $
            Text.italic (label (Synhashed.value ref))
              <> " "
              <> name
              <> " #"
              <> Hash.toBase32HexText (Synhashed.hash ref)

referentLabel :: Referent -> Text
referentLabel ref
  | Referent'.isConstructor ref = "ctor"
  | otherwise = "term"

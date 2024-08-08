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

import Control.Lens (mapped)
import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Semialign (align, unzip, zipWith)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Text.ANSI qualified as Text
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.HashTags (CausalHash, unCausalHash)
import U.Codebase.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
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
import Unison.Cli.UpdateUtils
  ( getNamespaceDependentsOf3,
    hydrateDefns,
    loadNamespaceDefinitions,
    renderDefnsForUnisonFile,
  )
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadShareLooseCode (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Debug qualified as Debug
import Unison.FileParsers qualified as FileParsers
import Unison.Hash qualified as Hash
import Unison.Merge qualified as Merge
import Unison.Merge.DeclNameLookup (expectConstructorNames)
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchName,
    ProjectBranchNameKind (..),
    ProjectName,
    Semver (..),
    classifyProjectBranchName,
  )
import Unison.Reference (Reference' (..), TermReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser (ParsingEnv (..), UniqueName)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Conflicted (Conflicted)
import Unison.Util.Defn (Defn)
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, alignDefnsWith, defnsAreEmpty, zipDefnsWith, zipDefnsWith3)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Nametree (Nametree (..), flattenNametrees, unflattenNametree)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2
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

      -- Load Alice/Bob/LCA causals
      causals <-
        Cli.runTransaction do
          traverse
            Operations.expectCausalBranchByCausalHash
            Merge.TwoOrThreeWay
              { alice = info.alice.causalHash,
                bob = info.bob.causalHash,
                lca = info.lca.causalHash
              }

      liftIO (debugFunctions.debugCausals causals)

      -- Load Alice/Bob/LCA branches
      branches <-
        Cli.runTransaction do
          alice <- causals.alice.value
          bob <- causals.bob.value
          lca <- for causals.lca \causal -> causal.value
          pure Merge.TwoOrThreeWay {lca, alice, bob}

      -- Assert that neither Alice nor Bob have defns in lib
      for_ [(mergeTarget, branches.alice), (mergeSource, branches.bob)] \(who, branch) -> do
        whenM (Cli.runTransaction (hasDefnsInLib branch)) do
          done (Output.MergeDefnsInLib who)

      -- Load Alice/Bob/LCA definitions
      --
      -- FIXME: Oops, if this fails due to a conflicted name, we don't actually say where the conflicted name came from.
      -- We should have a better error message (even though you can't do anything about conflicted names in the LCA).
      nametrees3 :: Merge.ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) <- do
        let referent2to1 = Conversions.referent2to1 (Codebase.getDeclType env.codebase)
        let action ::
              (forall a. Defn (Conflicted Name Referent) (Conflicted Name TypeReference) -> Transaction a) ->
              Transaction (Merge.ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)))
            action rollback = do
              alice <- loadNamespaceDefinitions referent2to1 branches.alice & onLeftM rollback
              bob <- loadNamespaceDefinitions referent2to1 branches.bob & onLeftM rollback
              lca <-
                case branches.lca of
                  Nothing -> pure Nametree {value = Defns Map.empty Map.empty, children = Map.empty}
                  Just lca -> loadNamespaceDefinitions referent2to1 lca & onLeftM rollback
              pure Merge.ThreeWay {alice, bob, lca}
        Cli.runTransactionWithRollback2 (\rollback -> Right <$> action (rollback . Left))
          & onLeftM (done . Output.ConflictedDefn "merge")

      libdeps3 <- Cli.runTransaction (loadLibdeps branches)

      let blob0 = makeMergeblob0 nametrees3 libdeps3

      -- Hydrate
      hydratedDefns ::
        Merge.ThreeWay
          ( DefnsF
              (Map Name)
              (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
              (TypeReferenceId, Decl Symbol Ann)
          ) <-
        Cli.runTransaction $
          traverse
            ( hydrateDefns
                (Codebase.unsafeGetTermComponent env.codebase)
                Operations.expectDeclComponent
            )
            ( let f = Map.mapMaybe Referent.toTermReferenceId . BiMultimap.range
                  g = Map.mapMaybe Reference.toId . BiMultimap.range
               in bimap f g <$> blob0.defns
            )

      blob1 <-
        makeMergeblob1 blob0 hydratedDefns & onLeft \case
          Merge.Alice reason -> done (Output.IncoherentDeclDuringMerge mergeTarget reason)
          Merge.Bob reason -> done (Output.IncoherentDeclDuringMerge mergeSource reason)

      liftIO (debugFunctions.debugDiffs blob1.diffs)

      liftIO (debugFunctions.debugCombinedDiff blob1.diff)

      blob2 <-
        makeMergeblob2 blob1 & onLeft \err ->
          done case err of
            Mergeblob2Error'ConflictedAlias defn0 ->
              case defn0 of
                Merge.Alice defn -> Output.MergeConflictedAliases mergeTarget defn
                Merge.Bob defn -> Output.MergeConflictedAliases mergeSource defn
            Mergeblob2Error'ConflictedBuiltin defn -> Output.MergeConflictInvolvingBuiltin defn

      liftIO (debugFunctions.debugPartitionedDiff blob2.conflicts blob2.unconflicts)

      dependents0 <-
        Cli.runTransaction $
          for ((,) <$> ThreeWay.forgetLca blob2.defns <*> blob2.coreDependencies) \(defns, deps) ->
            getNamespaceDependentsOf3 defns deps

      -- Load and merge Alice's and Bob's libdeps
      mergedLibdeps <-
        Cli.runTransaction (libdepsToBranch0 (Codebase.getDeclType env.codebase) blob2.libdeps)

      uniqueName <- liftIO env.generateUniqueName

      let hasConflicts =
            blob2.hasConflicts

      let blob3 =
            makeMergeblob3
              blob2
              dependents0
              (Branch.toNames mergedLibdeps)
              Merge.TwoWay
                { alice = into @Text aliceBranchNames,
                  bob =
                    case info.bob.source of
                      MergeSource'LocalProjectBranch bobBranchNames -> into @Text bobBranchNames
                      MergeSource'RemoteProjectBranch bobBranchNames
                        | aliceBranchNames == bobBranchNames -> "remote " <> into @Text bobBranchNames
                        | otherwise -> into @Text bobBranchNames
                      MergeSource'RemoteLooseCode info ->
                        case Path.toName info.path of
                          Nothing -> "<root>"
                          Just name -> Name.toText name
                }

      maybeBlob5 <-
        if hasConflicts
          then pure Nothing
          else case makeMergeblob4 blob3 uniqueName of
            Left _parseErr -> pure Nothing
            Right blob4 -> do
              typeLookup <- Cli.runTransaction (Codebase.typeLookupForDependencies env.codebase blob4.dependencies)
              pure case makeMergeblob5 blob4 typeLookup of
                Left _typecheckErr -> Nothing
                Right blob5 -> Just blob5

      let stageOneBranch = defnsAndLibdepsToBranch0 env.codebase blob3.stageOne mergedLibdeps

      let parents =
            causals <&> \causal -> (causal.causalHash, Codebase.expectBranchForHash env.codebase causal.causalHash)

      blob5 <-
        maybeBlob5 & onNothing do
          Cli.Env {writeSource} <- ask
          (_temporaryBranchId, temporaryBranchName) <-
            HandleInput.Branch.createBranch
              info.description
              ( HandleInput.Branch.CreateFrom'NamespaceWithParent
                  info.alice.projectAndBranch.branch
                  (Branch.mergeNode stageOneBranch parents.alice parents.bob)
              )
              info.alice.projectAndBranch.project
              (findTemporaryBranchName info.alice.projectAndBranch.project.projectId mergeSourceAndTarget)
          scratchFilePath <-
            Cli.getLatestFile <&> \case
              Nothing -> "scratch.u"
              Just (file, _) -> file
          liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 blob3.unparsedFile)
          done (Output.MergeFailure scratchFilePath mergeSourceAndTarget temporaryBranchName)

      Cli.runTransaction (Codebase.addDefsToCodebase env.codebase blob5.file)
      let stageTwoBranch = Branch.batchUpdates (typecheckedUnisonFileToBranchAdds blob5.file) stageOneBranch
      Cli.updateProjectBranchRoot_
        info.alice.projectAndBranch.branch
        info.description
        (\_aliceBranch -> Branch.mergeNode stageTwoBranch parents.alice parents.bob)
      pure (Output.MergeSuccess mergeSourceAndTarget)

  Cli.respond finalOutput

data Mergeblob0 libdep = Mergeblob0
  { defns :: Merge.ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    libdeps :: Merge.ThreeWay (Map NameSegment libdep),
    nametrees :: Merge.ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
  }

makeMergeblob0 ::
  Merge.ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
  Merge.ThreeWay (Map NameSegment libdep) ->
  Mergeblob0 libdep
makeMergeblob0 nametrees libdeps =
  Mergeblob0
    { defns = flattenNametrees <$> nametrees,
      libdeps,
      nametrees
    }

data Mergeblob1 libdep = Mergeblob1
  { conflicts :: Merge.TwoWay (DefnsF (Map Name) TermReference TypeReference),
    declNameLookups :: Merge.TwoWay Merge.DeclNameLookup,
    defns :: Merge.ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    diff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference,
    diffs :: Merge.TwoWay (DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference),
    hydratedDefns ::
      Merge.ThreeWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        ),
    lcaDeclNameLookup :: Merge.PartialDeclNameLookup,
    libdeps :: Map NameSegment libdep,
    libdepsDiff :: Map NameSegment (Merge.LibdepDiffOp libdep),
    unconflicts :: DefnsF Merge.Unconflicts Referent TypeReference
  }

makeMergeblob1 ::
  forall libdep.
  (Eq libdep) =>
  Mergeblob0 libdep ->
  Merge.ThreeWay
    ( DefnsF
        (Map Name)
        (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        (TypeReferenceId, Decl Symbol Ann)
    ) ->
  Either (Merge.EitherWay Merge.IncoherentDeclReason) (Mergeblob1 libdep)
makeMergeblob1 blob hydratedDefns = do
  -- Make one big constructor count lookup for all type decls
  let numConstructors =
        Map.empty
          & f (Map.elems hydratedDefns.alice.types)
          & f (Map.elems hydratedDefns.bob.types)
          & f (Map.elems hydratedDefns.lca.types)
        where
          f :: [(TypeReferenceId, Decl Symbol Ann)] -> Map TypeReferenceId Int -> Map TypeReferenceId Int
          f types acc =
            List.foldl'
              ( \acc (ref, decl) ->
                  Map.insert ref (DataDeclaration.constructorCount (DataDeclaration.asDataDecl decl)) acc
              )
              acc
              types

  -- Make Alice/Bob decl name lookups, which can fail if either have an incoherent decl
  declNameLookups <- do
    alice <- Merge.checkDeclCoherency blob.nametrees.alice numConstructors & mapLeft Merge.Alice
    bob <- Merge.checkDeclCoherency blob.nametrees.bob numConstructors & mapLeft Merge.Bob
    pure Merge.TwoWay {alice, bob}

  -- Make LCA decl name lookup
  let lcaDeclNameLookup =
        Merge.lenientCheckDeclCoherency blob.nametrees.lca numConstructors

  -- Diff LCA->Alice and LCA->Bob
  let diffs =
        Merge.nameBasedNamespaceDiff
          declNameLookups
          lcaDeclNameLookup
          blob.defns
          Defns
            { terms =
                foldMap
                  (List.foldl' (\acc (ref, (term, _)) -> Map.insert ref term acc) Map.empty . Map.elems . (.terms))
                  hydratedDefns,
              types =
                foldMap
                  (List.foldl' (\acc (ref, typ) -> Map.insert ref typ acc) Map.empty . Map.elems . (.types))
                  hydratedDefns
            }

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff =
        Merge.combineDiffs diffs

  -- Partition the combined diff into the conflicted things and the unconflicted things
  let (conflicts, unconflicts) =
        Merge.partitionCombinedDiffs (ThreeWay.forgetLca blob.defns) declNameLookups diff

  -- Diff and merge libdeps
  let libdepsDiff :: Map NameSegment (Merge.LibdepDiffOp libdep)
      libdepsDiff =
        Merge.diffLibdeps blob.libdeps

  let libdeps :: Map NameSegment libdep
      libdeps =
        Merge.applyLibdepsDiff Merge.getTwoFreshLibdepNames blob.libdeps libdepsDiff

  pure
    Mergeblob1
      { conflicts,
        declNameLookups,
        defns = blob.defns,
        diff,
        diffs,
        hydratedDefns,
        lcaDeclNameLookup,
        libdeps,
        libdepsDiff,
        unconflicts
      }

data Mergeblob2 libdep = Mergeblob2
  { conflicts :: Merge.TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
    conflictsIds :: Merge.TwoWay (DefnsF Set TermReferenceId TypeReferenceId),
    conflictsNames :: Merge.TwoWay (DefnsF Set Name Name),
    coreDependencies :: Merge.TwoWay (DefnsF Set TermReference TypeReference),
    declNameLookups :: Merge.TwoWay Merge.DeclNameLookup,
    defns :: Merge.ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    hasConflicts :: Bool,
    hydratedDefns ::
      Merge.TwoWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        ),
    lcaDeclNameLookup :: Merge.PartialDeclNameLookup,
    libdeps :: Map NameSegment libdep,
    soloUpdatesAndDeletes :: Merge.TwoWay (DefnsF Set Name Name),
    unconflicts :: DefnsF Merge.Unconflicts Referent TypeReference
  }

data Mergeblob2Error
  = Mergeblob2Error'ConflictedAlias (Merge.EitherWay (Defn (Name, Name) (Name, Name)))
  | Mergeblob2Error'ConflictedBuiltin (Defn Name Name)

makeMergeblob2 :: Mergeblob1 libdep -> Either Mergeblob2Error (Mergeblob2 libdep)
makeMergeblob2 blob = do
  -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
  for_ ((,) <$> Merge.TwoWay Merge.Alice Merge.Bob <*> blob.diffs) \(who, diff) ->
    whenJust (Merge.findConflictedAlias blob.defns.lca diff) $
      Left . Mergeblob2Error'ConflictedAlias . who

  conflicts <- Merge.narrowConflictsToNonBuiltins blob.conflicts & mapLeft Mergeblob2Error'ConflictedBuiltin
  let conflictsIds = bimap (Set.fromList . Map.elems) (Set.fromList . Map.elems) <$> conflicts
  let conflictsNames = bimap Map.keysSet Map.keysSet <$> conflicts

  let soloUpdatesAndDeletes = Unconflicts.soloUpdatesAndDeletes blob.unconflicts
  let coreDependencies = identifyCoreDependencies (ThreeWay.forgetLca blob.defns) conflictsIds soloUpdatesAndDeletes

  pure
    Mergeblob2
      { conflicts,
        conflictsIds,
        conflictsNames,
        coreDependencies,
        declNameLookups = blob.declNameLookups,
        defns = blob.defns,
        -- Eh, they'd either both be null, or neither, but just check both maps anyway
        hasConflicts = not (defnsAreEmpty conflicts.alice) || not (defnsAreEmpty conflicts.bob),
        hydratedDefns = ThreeWay.forgetLca blob.hydratedDefns,
        lcaDeclNameLookup = blob.lcaDeclNameLookup,
        libdeps = blob.libdeps,
        soloUpdatesAndDeletes,
        unconflicts = blob.unconflicts
      }

data Mergeblob3 = Mergeblob3
  { libdeps :: Names,
    stageOne :: DefnsF (Map Name) Referent TypeReference,
    unparsedFile :: Pretty ColorText
  }

makeMergeblob3 ::
  Mergeblob2 libdep ->
  Merge.TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  Names ->
  Merge.TwoWay Text ->
  Mergeblob3
makeMergeblob3 blob dependents0 libdeps authors =
  -- Identify the unconflicted dependents we need to pull into the Unison file (either first for typechecking, if
  -- there aren't conflicts, or else for manual conflict resolution without a typechecking step, if there are)
  let dependents =
        filterDependents
          blob.conflictsNames
          blob.soloUpdatesAndDeletes
          ( let f :: Set TermReferenceId -> Referent -> NESet Name -> Set Name
                f deps defn0 names
                  | Just defn <- Referent.toTermReferenceId defn0,
                    Set.member defn deps =
                      Set.NonEmpty.toSet names
                  | otherwise = Set.empty
                g :: Set TypeReferenceId -> TypeReference -> NESet Name -> Set Name
                g deps defn0 names
                  | ReferenceDerived defn <- defn0,
                    Set.member defn deps =
                      Set.NonEmpty.toSet names
                  | otherwise = Set.empty
             in zipDefnsWith
                  (\defns deps -> Map.foldMapWithKey (f deps) (BiMultimap.domain defns))
                  (\defns deps -> Map.foldMapWithKey (g deps) (BiMultimap.domain defns))
                  <$> ThreeWay.forgetLca blob.defns
                  <*> dependents0
          )

      (renderedConflicts, renderedDependents) =
        renderConflictsAndDependents
          blob.declNameLookups
          blob.hydratedDefns
          blob.conflictsNames
          dependents
          (defnsToNames <$> ThreeWay.forgetLca blob.defns)
          libdeps
   in Mergeblob3
        { libdeps,
          stageOne =
            makeStageOne
              blob.declNameLookups
              blob.conflictsNames
              blob.unconflicts
              dependents
              (bimap BiMultimap.range BiMultimap.range blob.defns.lca),
          unparsedFile = makePrettyUnisonFile authors renderedConflicts renderedDependents
        }

data Mergeblob4 = Mergeblob4
  { dependencies :: Set Reference,
    file :: UnisonFile Symbol Ann
  }

makeMergeblob4 :: Mergeblob3 -> UniqueName -> Either (Parser.Err Symbol) Mergeblob4
makeMergeblob4 blob uniqueName = do
  let stageOneNames =
        Names (Relation.fromMap blob.stageOne.terms) (Relation.fromMap blob.stageOne.types) <> blob.libdeps

      parsingEnv =
        ParsingEnv
          { uniqueNames = uniqueName,
            -- The codebase names are disjoint from the file names, i.e. there aren't any things that
            -- would be classified as an update upon parsing. So, there's no need to try to look up any
            -- existing unique type GUIDs to reuse.
            uniqueTypeGuid = \_ -> Identity Nothing,
            names = stageOneNames
          }
  file <- runIdentity (Parsers.parseFile "<merge>" (Pretty.toPlain 80 blob.unparsedFile) parsingEnv)
  Right
    Mergeblob4
      { dependencies = UnisonFile.dependencies file,
        file
      }

data Mergeblob5 = Mergeblob5
  { file :: TypecheckedUnisonFile Symbol Ann
  }

makeMergeblob5 :: Mergeblob4 -> TypeLookup Symbol Ann -> Either (Seq (Result.Note Symbol Ann)) Mergeblob5
makeMergeblob5 blob typeLookup =
  let typecheckingEnv =
        Typechecker.Env
          { ambientAbilities = [],
            termsByShortname = Map.empty,
            typeLookup
          }
   in case runIdentity (Result.runResultT (FileParsers.synthesizeFile typecheckingEnv blob.file)) of
        (Nothing, notes) -> Left notes
        (Just file, _) -> Right Mergeblob5 {file}

renderConflictsAndDependents ::
  Merge.TwoWay Merge.DeclNameLookup ->
  Merge.TwoWay (DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)) ->
  Merge.TwoWay (DefnsF Set Name Name) ->
  Merge.TwoWay (DefnsF Set Name Name) ->
  Merge.TwoWay Names ->
  Names ->
  ( Merge.TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)),
    Merge.TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
  )
renderConflictsAndDependents declNameLookups hydratedDefns conflicts dependents names libdepsNames =
  unzip $
    ( \declNameLookup (conflicts, dependents) ppe ->
        let render = renderDefnsForUnisonFile declNameLookup ppe . over (#terms . mapped) snd
         in (render conflicts, render dependents)
    )
      <$> declNameLookups
      <*> hydratedConflictsAndDependents
      <*> Merge.makePrettyPrintEnvs names libdepsNames
  where
    hydratedConflictsAndDependents ::
      Merge.TwoWay
        ( DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann),
          DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)
        )
    hydratedConflictsAndDependents =
      ( \as bs cs ->
          ( zipDefnsWith Map.restrictKeys Map.restrictKeys as bs,
            zipDefnsWith Map.restrictKeys Map.restrictKeys as cs
          )
      )
        <$> hydratedDefns
        <*> conflicts
        <*> dependents

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
              source = MergeSource'LocalProjectBranch (ProjectUtils.justTheNames branches.bob)
            },
        lca =
          LcaMergeInfo
            { causalHash = lcaCausalHash
            },
        description = "merge " <> into @Text (ProjectUtils.justTheNames branches.bob)
      }

------------------------------------------------------------------------------------------------------------------------
-- Loading basic info out of the database

loadLibdeps ::
  Merge.TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction (Merge.ThreeWay (Map NameSegment (V2.CausalBranch Transaction)))
loadLibdeps branches = do
  lca <-
    case branches.lca of
      Nothing -> pure Map.empty
      Just lcaBranch -> load lcaBranch
  alice <- load branches.alice
  bob <- load branches.bob
  pure Merge.ThreeWay {lca, alice, bob}
  where
    load :: V2.Branch Transaction -> Transaction (Map NameSegment (V2.CausalBranch Transaction))
    load branch =
      case Map.lookup NameSegment.libSegment branch.children of
        Nothing -> pure Map.empty
        Just libdepsCausal -> do
          libdepsBranch <- libdepsCausal.value
          pure libdepsBranch.children

------------------------------------------------------------------------------------------------------------------------
-- Merge precondition violation checks

hasDefnsInLib :: (Applicative m) => V2.Branch m -> m Bool
hasDefnsInLib branch = do
  libdeps <-
    case Map.lookup NameSegment.libSegment branch.children of
      Nothing -> pure V2.Branch.empty
      Just libdeps -> libdeps.value
  pure (not (Map.null libdeps.terms) || not (Map.null libdeps.types))

------------------------------------------------------------------------------------------------------------------------
-- Creating Unison files

makePrettyUnisonFile ::
  Merge.TwoWay Text ->
  Merge.TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Merge.TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettyUnisonFile authors conflicts dependents =
  fold
    [ conflicts
        -- Merge the two maps together into one, remembering who authored what
        & TwoWay.twoWay (zipDefnsWith align align)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each conflict, types then terms (even though a type can conflict with a term, in which case they
        -- would not be adjacent in the file), with an author comment above each conflicted thing
        & ( let f =
                  foldMap \case
                    This x -> alice x
                    That y -> bob y
                    These x y -> alice x <> bob y
                  where
                    alice = prettyBinding (Just (Pretty.text authors.alice))
                    bob = prettyBinding (Just (Pretty.text authors.bob))
             in bifoldMap f f
          ),
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      let thereAre defns = TwoWay.or (not . defnsAreEmpty <$> defns)
       in if thereAre conflicts && thereAre dependents
            then
              fold
                [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
                  "-- conflicted definitions above.\n\n"
                ]
            else mempty,
      dependents
        -- Merge dependents together into one map (they are disjoint)
        & TwoWay.twoWay (zipDefnsWith Map.union Map.union)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each dependent, types then terms, without bothering to comment attribution
        & (let f = foldMap (prettyBinding Nothing) in bifoldMap f f)
    ]
  where
    prettyBinding maybeComment binding =
      fold
        [ case maybeComment of
            Nothing -> mempty
            Just comment -> "-- " <> comment <> "\n",
          binding,
          "\n",
          "\n"
        ]

    inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
    inAlphabeticalOrder =
      bimap f f
      where
        f = map snd . List.sortOn (Name.toText . fst) . Map.toList

------------------------------------------------------------------------------------------------------------------------
--

-- Given just named term/type reference ids, fill out all names that occupy the term and type namespaces. This is simply
-- the given names plus all of the types' constructors.
--
-- For example, if the input is
--
--   declNameLookup = {
--     "Maybe" => ["Maybe.Nothing", "Maybe.Just"]
--   }
--   defns = {
--     terms = { "foo" => #foo }
--     types = { "Maybe" => #Maybe }
--   }
--
-- then the output is
--
--   defns = {
--     terms = { "foo", "Maybe.Nothing", "Maybe.Just" }
--     types = { "Maybe" }
--   }
refIdsToNames :: Merge.DeclNameLookup -> DefnsF Set Name Name -> DefnsF Set Name Name
refIdsToNames declNameLookup =
  bifoldMap goTerms goTypes
  where
    goTerms :: Set Name -> DefnsF Set Name Name
    goTerms terms =
      Defns {terms, types = Set.empty}

    goTypes :: Set Name -> DefnsF Set Name Name
    goTypes types =
      Defns
        { terms = foldMap (Set.fromList . expectConstructorNames declNameLookup) types,
          types
        }

defnsAndLibdepsToBranch0 ::
  Codebase IO v a ->
  DefnsF (Map Name) Referent TypeReference ->
  Branch0 Transaction ->
  Branch0 IO
defnsAndLibdepsToBranch0 codebase defns libdeps =
  let -- Unflatten the collection of terms into tree, ditto for types
      nametrees :: DefnsF2 Nametree (Map NameSegment) Referent TypeReference
      nametrees =
        bimap unflattenNametree unflattenNametree defns

      -- Align the tree of terms and tree of types into one tree
      nametree :: Nametree (DefnsF (Map NameSegment) Referent TypeReference)
      nametree =
        nametrees & alignDefnsWith \case
          This terms -> Defns {terms, types = Map.empty}
          That types -> Defns {terms = Map.empty, types}
          These terms types -> Defns terms types

      -- Convert the tree to a branch0
      branch0 = nametreeToBranch0 nametree

      -- Add back the libdeps branch at path "lib"
      branch1 = Branch.setChildBranch NameSegment.libSegment (Branch.one libdeps) branch0

      -- Awkward: we have a Branch Transaction but we need a Branch IO (because reasons)
      branch2 = Branch.transform0 (Codebase.runTransaction codebase) branch1
   in branch2

nametreeToBranch0 :: Nametree (DefnsF (Map NameSegment) Referent TypeReference) -> Branch0 m
nametreeToBranch0 nametree =
  Branch.branch0
    (rel2star defns.terms)
    (rel2star defns.types)
    (Branch.one . nametreeToBranch0 <$> nametree.children)
    Map.empty
  where
    defns :: Defns (Relation Referent NameSegment) (Relation TypeReference NameSegment)
    defns =
      bimap (Relation.swap . Relation.fromMap) (Relation.swap . Relation.fromMap) nametree.value

    rel2star :: Relation ref name -> Star2 ref name metadata
    rel2star rel =
      Star2.Star2 {fact = Relation.dom rel, d1 = rel, d2 = Relation.empty}

identifyCoreDependencies ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  Merge.TwoWay (DefnsF Set Name Name) ->
  Merge.TwoWay (DefnsF Set TermReference TypeReference)
identifyCoreDependencies defns conflicts soloUpdatesAndDeletes = do
  fold
    [ -- One source of dependencies: Alice's versions of Bob's unconflicted deletes and updates, and vice-versa.
      --
      -- This is name-based: if Bob updates the *name* "foo", then we go find the thing that Alice calls "foo" (if
      -- anything), no matter what its hash is.
      defnsReferences
        <$> ( zipDefnsWith BiMultimap.restrictRan BiMultimap.restrictRan
                <$> TwoWay.swap soloUpdatesAndDeletes
                <*> defns
            ),
      -- The other source of dependencies: Alice's own conflicted things, and ditto for Bob.
      --
      -- An example: suppose Alice has foo#alice and Bob has foo#bob, so foo is conflicted. Furthermore, suppose
      -- Alice has bar#bar that depends on foo#alice.
      --
      -- We want Alice's #alice to be considered a dependency, so that when we go off and find dependents of these
      -- dependencies to put in the scratch file for type checking and propagation, we find bar#bar.
      --
      -- Note that this is necessary even if bar#bar is unconflicted! We don't want bar#bar to be put directly
      -- into the namespace / parsing context for the conflicted merge, because it has an unnamed reference on
      -- foo#alice. It rather ought to be in the scratchfile alongside the conflicted foo#alice and foo#bob, so
      -- that when that conflict is resolved, it will propagate to bar.
      bimap (Set.map Reference.DerivedId) (Set.map Reference.DerivedId) <$> conflicts
    ]

filterDependents ::
  (Ord name) =>
  Merge.TwoWay (DefnsF Set name name) ->
  Merge.TwoWay (DefnsF Set name name) ->
  Merge.TwoWay (DefnsF Set name name) ->
  Merge.TwoWay (DefnsF Set name name)
filterDependents conflicts soloUpdatesAndDeletes dependents0 =
  -- There is some subset of Alice's dependents (and ditto for Bob of course) that we don't ultimately want/need to put
  -- into the scratch file: those for which any of the following are true:
  --
  --   1. It is Alice-conflicted (since we only want to return *unconflicted* things).
  --   2. It was deleted by Bob.
  --   3. It was updated by Bob and not updated by Alice.
  let dependents1 =
        zipDefnsWith Set.difference Set.difference
          <$> dependents0
          <*> (conflicts <> TwoWay.swap soloUpdatesAndDeletes)

      -- Of the remaining dependents, it's still possible that the maps are not disjoint. But whenever the same name key
      -- exists in Alice's and Bob's dependents, the value will either be equal (by Unison hash)...
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {"foo" => #alice} } }
      --
      -- ...or synhash-equal (i.e. the term or type received different auto-propagated updates)...
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {"foo" => #bob}   } }
      --
      -- So, we can arbitrarily keep Alice's, because they will render the same.
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {}                } }
      dependents2 =
        dependents1 & over #bob \bob ->
          zipDefnsWith Set.difference Set.difference bob dependents1.alice
   in dependents2

makeStageOne ::
  Merge.TwoWay Merge.DeclNameLookup ->
  Merge.TwoWay (DefnsF Set Name Name) ->
  DefnsF Merge.Unconflicts term typ ->
  Merge.TwoWay (DefnsF Set Name Name) ->
  DefnsF (Map Name) term typ ->
  DefnsF (Map Name) term typ
makeStageOne declNameLookups conflicts unconflicts dependents =
  zipDefnsWith3 makeStageOneV makeStageOneV unconflicts (f conflicts <> f dependents)
  where
    f :: Merge.TwoWay (DefnsF Set Name Name) -> DefnsF Set Name Name
    f defns =
      fold (refIdsToNames <$> declNameLookups <*> defns)

makeStageOneV :: Merge.Unconflicts v -> Set Name -> Map Name v -> Map Name v
makeStageOneV unconflicts namesToDelete =
  (`Map.withoutKeys` namesToDelete) . Unconflicts.apply unconflicts

defnsReferences ::
  Defns (BiMultimap Referent name) (BiMultimap TypeReference name) ->
  DefnsF Set TermReference TypeReference
defnsReferences defns =
  List.foldl' f Defns {terms = Set.empty, types = BiMultimap.dom defns.types} (Set.toList (BiMultimap.dom defns.terms))
  where
    f :: DefnsF Set TermReference TypeReference -> Referent -> DefnsF Set TermReference TypeReference
    f acc = \case
      Referent.Con (ConstructorReference ref _) _ ->
        let !types = Set.insert ref acc.types
         in Defns {terms = acc.terms, types}
      Referent.Ref ref ->
        let !terms = Set.insert ref acc.terms
         in Defns {terms, types = acc.types}

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames defns =
  Names.Names
    { terms = Relation.fromMap (BiMultimap.range defns.terms),
      types = Relation.fromMap (BiMultimap.range defns.types)
    }

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
            <> mangleBranchName mergeSourceAndTarget.alice.branch

    mangleMergeSource :: MergeSource -> Text.Builder
    mangleMergeSource = \case
      MergeSource'LocalProjectBranch (ProjectAndBranch _project branch) -> mangleBranchName branch
      MergeSource'RemoteProjectBranch (ProjectAndBranch _project branch) -> "remote-" <> mangleBranchName branch
      MergeSource'RemoteLooseCode info -> manglePath info.path
    mangleBranchName :: ProjectBranchName -> Text.Builder
    mangleBranchName name =
      case classifyProjectBranchName name of
        ProjectBranchNameKind'Contributor user name1 ->
          Text.Builder.text user
            <> Text.Builder.char '-'
            <> mangleBranchName name1
        ProjectBranchNameKind'DraftRelease semver -> "releases-drafts-" <> mangleSemver semver
        ProjectBranchNameKind'Release semver -> "releases-" <> mangleSemver semver
        ProjectBranchNameKind'NothingSpecial -> Text.Builder.text (into @Text name)

    manglePath :: Path -> Text.Builder
    manglePath =
      Monoid.intercalateMap "-" (Text.Builder.text . NameSegment.toUnescapedText) . Path.toList

    mangleSemver :: Semver -> Text.Builder
    mangleSemver (Semver x y z) =
      Text.Builder.decimal x
        <> Text.Builder.char '.'
        <> Text.Builder.decimal y
        <> Text.Builder.char '.'
        <> Text.Builder.decimal z

libdepsToBranch0 ::
  (Reference -> Transaction ConstructorType) ->
  Map NameSegment (V2.CausalBranch Transaction) ->
  Transaction (Branch0 Transaction)
libdepsToBranch0 loadDeclType libdeps = do
  let branch :: V2.Branch Transaction
      branch =
        V2.Branch
          { terms = Map.empty,
            types = Map.empty,
            patches = Map.empty,
            children = libdeps
          }

  -- We make a fresh branch cache to load the branch of libdeps.
  -- It would probably be better to reuse the codebase's branch cache.
  -- FIXME how slow/bad is this without that branch cache?
  branchCache <- Sqlite.unsafeIO newBranchCache
  Conversions.branch2to1 branchCache loadDeclType branch

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

    splitVar :: Symbol -> Path.Split
    splitVar = Path.splitFromName . Name.unsafeParseVar

------------------------------------------------------------------------------------------------------------------------
-- Debugging by printing a bunch of stuff out

data DebugFunctions = DebugFunctions
  { debugCausals :: Merge.TwoOrThreeWay (V2.CausalBranch Transaction) -> IO (),
    debugDiffs :: Merge.TwoWay (DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference) -> IO (),
    debugCombinedDiff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference -> IO (),
    debugPartitionedDiff ::
      Merge.TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
      DefnsF Merge.Unconflicts Referent TypeReference ->
      IO ()
  }

realDebugFunctions :: DebugFunctions
realDebugFunctions =
  DebugFunctions
    { debugCausals = realDebugCausals,
      debugDiffs = realDebugDiffs,
      debugCombinedDiff = realDebugCombinedDiff,
      debugPartitionedDiff = realDebugPartitionedDiff
    }

fakeDebugFunctions :: DebugFunctions
fakeDebugFunctions =
  DebugFunctions mempty mempty mempty mempty

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
    renderThings label things =
      for_ (Map.toList things) \(name, op) ->
        let go color action x =
              color $
                action
                  <> " "
                  <> Text.italic (label (Synhashed.value x))
                  <> " "
                  <> Name.toText name
                  <> " #"
                  <> Hash.toBase32HexText (Synhashed.hash x)
         in Text.putStrLn case op of
              Merge.DiffOp'Add x -> go Text.green "+" x
              Merge.DiffOp'Delete x -> go Text.red "-" x
              Merge.DiffOp'Update x -> go Text.yellow "%" x.new

realDebugCombinedDiff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference -> IO ()
realDebugCombinedDiff diff = do
  Text.putStrLn (Text.bold "\n=== Combined diff ===")
  renderThings referentLabel Referent.toText diff.terms
  renderThings (const "type") Reference.toText diff.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name (Merge.CombinedDiffOp ref) -> IO ()
    renderThings label renderRef things =
      for_ (Map.toList things) \(name, op) ->
        Text.putStrLn case op of
          Merge.CombinedDiffOp'Add who ->
            Text.green $
              "+ "
                <> Text.italic (label (EitherWayI.value who))
                <> " "
                <> Name.toText name
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
                <> Name.toText name
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
                <> Name.toText name
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
                <> Name.toText name
                <> " "
                <> renderRef ref.alice
                <> "/"
                <> renderRef ref.bob

    renderWho :: Merge.EitherWayI v -> Text
    renderWho = \case
      Merge.OnlyAlice _ -> "Alice"
      Merge.OnlyBob _ -> "Bob"
      Merge.AliceAndBob _ -> "Alice and Bob"

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
      for_ (Map.toList conflicts) \(name, ref) ->
        Text.putStrLn $
          Text.magenta $
            "! "
              <> Text.italic label
              <> " "
              <> Name.toText name
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
      for_ (Map.toList unconflicts) \(name, ref) ->
        Text.putStrLn $
          color $
            action
              <> " "
              <> Text.italic (label ref)
              <> " "
              <> Name.toText name
              <> " "
              <> renderRef ref

referentLabel :: Referent -> Text
referentLabel ref
  | Referent'.isConstructor ref = "constructor"
  | otherwise = "term"

module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (over, view)
import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (unzip)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Text.ANSI qualified as Text
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2
  ( getNamespaceDependentsOf2,
    makeParsingEnv,
    prettyParseTypecheck2,
    typecheckedUnisonFileToBranchAdds,
  )
import Unison.Codebase.Editor.HandleInput.Update2 qualified as Update2
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Debug qualified as Debug
import Unison.Hash qualified as Hash
import Unison.Merge.CombineDiffs (CombinedDiffOp (..), combineDiffs)
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.DeclCoherencyCheck (IncoherentDeclReason (..), checkDeclCoherency)
import Unison.Merge.DeclNameLookup (DeclNameLookup (..), expectConstructorNames)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.TwoWayI qualified as TwoWayI
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Merge.Updated (Updated (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Referent' qualified as Referent'
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile (UnisonFile')
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, alignDefnsWith, defnsAreEmpty, zipDefnsWith, zipDefnsWith3)
import Unison.Util.Nametree (Nametree (..), flattenNametree, traverseNametreeWithName, unflattenNametree)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip, zipWith)

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  let debugFunctions =
        if Debug.shouldDebug Debug.Merge
          then realDebugFunctions
          else fakeDebugFunctions

  Cli.Env {codebase} <- ask

  -- Create a bunch of cached database lookup functions
  db <- makeMergeDatabase codebase

  -- Load the current project branch ("Alice"), and the branch from the same project to merge in ("Bob")
  info <- loadMergeInfo bobBranchName

  -- Load Alice/Bob/LCA branches
  branches <-
    Cli.runTransactionWithRollback \abort -> do
      loadV2Branches =<< loadV2Causals abort db info

  -- Load Alice/Bob/LCA definitions and decl name lookups
  (defns, declNameLookups) <-
    Cli.runTransactionWithRollback \abort -> do
      loadDefns abort db info.projectBranches branches

  liftIO (debugFunctions.debugDefns defns declNameLookups)

  -- Diff LCA->Alice and LCA->Bob
  diffs <-
    Cli.runTransaction do
      Merge.nameBasedNamespaceDiff db declNameLookups defns

  liftIO (debugFunctions.debugDiffs diffs)

  -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
  whenJust (findOneConflictedAlias info.projectBranches defns.lca diffs) \violation ->
    Cli.returnEarly (mergePreconditionViolationToOutput violation)

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff = combineDiffs diffs

  liftIO (debugFunctions.debugCombinedDiff diff)

  -- Partition the combined diff into the conflicted things and the unconflicted things
  (conflicts, unconflicts) <-
    partitionCombinedDiffs (ThreeWay.forgetLca defns) (ThreeWay.forgetLca declNameLookups) diff & onLeft \name ->
      Cli.returnEarly (mergePreconditionViolationToOutput (Merge.ConflictInvolvingBuiltin name))

  liftIO (debugFunctions.debugPartitionedDiff conflicts unconflicts)

  -- Identify the unconflicted dependents we need to pull into the Unison file (either first for typechecking, if there
  -- aren't conflicts, or else for manual conflict resolution without a typechecking step, if there are)
  dependents <-
    Cli.runTransaction do
      identifyDependents (ThreeWay.forgetLca defns) conflicts unconflicts

  liftIO (debugFunctions.debugDependents dependents)

  let stageOne :: DefnsF (Map Name) Referent TypeReference
      stageOne =
        makeStageOne
          (ThreeWay.forgetLca declNameLookups)
          conflicts
          unconflicts
          dependents
          (bimap BiMultimap.range BiMultimap.range defns.lca)

  liftIO (debugFunctions.debugBumpedDefns stageOne)

  -- Create the Unison file, which may have conflicts, in which case we don't bother trying to parse and typecheck it.
  unisonFile <-
    Cli.runTransactionWithRollback \abort ->
      let toFile defns = fold (defnsToUnisonFile abort codebase <$> ThreeWay.forgetLca declNameLookups <*> defns)
       in toFile conflicts <> toFile dependents

  -- Load and merge Alice's and Bob's libdeps
  mergedLibdeps <-
    Cli.runTransaction do
      libdeps <- loadLibdeps branches
      libdepsToBranch0 db (Merge.mergeLibdeps getTwoFreshNames libdeps)

  let stageOneBranch = defnsAndLibdepsToBranch0 codebase stageOne mergedLibdeps

  let prettyUnisonFile =
        let names = defnsToNames defns.alice <> defnsToNames defns.bob <> Branch.toNames mergedLibdeps
            ppe = PPED.makePPED (PPE.namer names) (PPE.suffixifyByName names)
         in Pretty.prettyUnisonFile ppe unisonFile

  maybeTypecheckedUnisonFile <-
    let thisMergeHasConflicts =
          -- Eh, they'd either both be null, or neither, but just check both maps anyway
          not (defnsAreEmpty conflicts.alice) || not (defnsAreEmpty conflicts.bob)
     in if thisMergeHasConflicts
          then pure Nothing
          else do
            currentPath <- Cli.getCurrentPath
            parsingEnv <- makeParsingEnv currentPath (Branch.toNames stageOneBranch)
            prettyParseTypecheck2 prettyUnisonFile parsingEnv <&> eitherToMaybe

  case maybeTypecheckedUnisonFile of
    Nothing -> do
      Cli.Env {writeSource} <- ask
      branch <- Cli.getBranchAt info.paths.alice
      _temporaryBranchId <-
        HandleInput.Branch.doCreateBranch'
          (Branch.cons stageOneBranch branch)
          Nothing
          info.project
          (findTemporaryBranchName info)
          (textualDescriptionOfMerge info)
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
      Cli.respond $
        Output.MergeFailure
          scratchFilePath
          (aliceProjectAndBranchName info)
          (bobProjectAndBranchName info)
    Just tuf -> do
      Cli.runTransaction (Codebase.addDefsToCodebase codebase tuf)
      let stageTwoBranch = Branch.batchUpdates (typecheckedUnisonFileToBranchAdds tuf) stageOneBranch
      Cli.stepAt (textualDescriptionOfMerge info) (Path.unabsolute info.paths.alice, const stageTwoBranch)
      Cli.respond (Output.MergeSuccess (aliceProjectAndBranchName info) (bobProjectAndBranchName info))

aliceProjectAndBranchName :: MergeInfo -> ProjectAndBranch ProjectName ProjectBranchName
aliceProjectAndBranchName mergeInfo =
  ProjectAndBranch
    { project = mergeInfo.project.name,
      branch = mergeInfo.projectBranches.alice.name
    }

bobProjectAndBranchName :: MergeInfo -> ProjectAndBranch ProjectName ProjectBranchName
bobProjectAndBranchName mergeInfo =
  ProjectAndBranch
    { project = mergeInfo.project.name,
      branch = mergeInfo.projectBranches.bob.name
    }

------------------------------------------------------------------------------------------------------------------------
-- Loading basic info out of the database

loadMergeInfo :: ProjectBranchName -> Cli MergeInfo
loadMergeInfo bobBranchName = do
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let alicePath = Cli.projectBranchPath (ProjectAndBranch project.projectId aliceProjectBranch.branchId)
  let bobPath = Cli.projectBranchPath (ProjectAndBranch project.projectId bobProjectBranch.branchId)
  pure
    MergeInfo
      { paths = TwoWay alicePath bobPath,
        projectBranches = TwoWay aliceProjectBranch bobProjectBranch,
        project
      }

loadV2Causals ::
  (forall a. Output -> Transaction a) ->
  MergeDatabase ->
  MergeInfo ->
  Transaction (TwoOrThreeWay (V2.CausalBranch Transaction))
loadV2Causals abort db info = do
  alice <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.alice)
  bob <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.bob)
  lca <-
    Operations.lca alice.causalHash bob.causalHash >>= \case
      Nothing -> pure Nothing
      Just lcaCausalHash -> do
        -- If LCA == bob, then we are at or ahead of bob, so the merge is done.
        when (lcaCausalHash == bob.causalHash) do
          abort $
            Output.MergeAlreadyUpToDate
              (Right (ProjectAndBranch info.project info.projectBranches.bob))
              (Right (ProjectAndBranch info.project info.projectBranches.alice))
        Just <$> db.loadCausal lcaCausalHash
  pure TwoOrThreeWay {lca, alice, bob}

loadV2Branches :: TwoOrThreeWay (V2.CausalBranch Transaction) -> Transaction (TwoOrThreeWay (V2.Branch Transaction))
loadV2Branches causals = do
  alice <- causals.alice.value
  bob <- causals.bob.value
  lca <- for causals.lca \causal -> causal.value
  pure TwoOrThreeWay {lca, alice, bob}

loadDefns ::
  (forall a. Output -> Transaction a) ->
  MergeDatabase ->
  TwoWay ProjectBranch ->
  TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction
    ( ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
      ThreeWay DeclNameLookup
    )
loadDefns abort0 db projectBranches branches = do
  lcaDefns0 <-
    case branches.lca of
      Nothing -> pure Nametree {value = Defns Map.empty Map.empty, children = Map.empty}
      Just lcaBranch -> loadNamespaceInfo abort db lcaBranch
  aliceDefns0 <- loadNamespaceInfo abort db branches.alice
  bobDefns0 <- loadNamespaceInfo abort db branches.bob

  lca <- assertNamespaceSatisfiesPreconditions db abort Nothing (fromMaybe V2.Branch.empty branches.lca) lcaDefns0
  alice <- assertNamespaceSatisfiesPreconditions db abort (Just projectBranches.alice.name) branches.alice aliceDefns0
  bob <- assertNamespaceSatisfiesPreconditions db abort (Just projectBranches.bob.name) branches.bob bobDefns0

  pure (unzip ThreeWay {lca, alice, bob})
  where
    abort :: Merge.PreconditionViolation -> Transaction void
    abort =
      abort0 . mergePreconditionViolationToOutput

loadLibdeps ::
  TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction (ThreeWay (Map NameSegment (V2.CausalBranch Transaction)))
loadLibdeps branches = do
  lca <-
    case branches.lca of
      Nothing -> pure Map.empty
      Just lcaBranch -> load lcaBranch
  alice <- load branches.alice
  bob <- load branches.bob
  pure ThreeWay {lca, alice, bob}
  where
    load :: V2.Branch Transaction -> Transaction (Map NameSegment (V2.CausalBranch Transaction))
    load branch =
      case Map.lookup NameSegment.libSegment branch.children of
        Nothing -> pure Map.empty
        Just libdepsCausal -> do
          libdepsBranch <- libdepsCausal.value
          pure libdepsBranch.children

------------------------------------------------------------------------------------------------------------------------
-- Creating Unison files

defnsToUnisonFile ::
  (forall void. Output -> Transaction void) ->
  Codebase IO Symbol Ann ->
  DeclNameLookup ->
  DefnsF (Map Name) TermReferenceId TypeReferenceId ->
  Transaction (UnisonFile' [] Symbol Ann)
defnsToUnisonFile abort codebase declNameLookup defns =
  Update2.makeUnisonFile
    abort
    codebase
    (\_ name -> Right (expectConstructorNames declNameLookup name))
    (bimap Relation.fromMap Relation.fromMap defns)

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
refIdsToNames :: DeclNameLookup -> DefnsF (Map Name) term typ -> DefnsF Set Name Name
refIdsToNames declNameLookup =
  bifoldMap goTerms goTypes
  where
    goTerms :: Map Name term -> DefnsF Set Name Name
    goTerms terms =
      Defns {terms = Map.keysSet terms, types = Set.empty}

    goTypes :: Map Name typ -> DefnsF Set Name Name
    goTypes types =
      Defns
        { terms = foldMap (Set.fromList . expectConstructorNames declNameLookup) names,
          types = names
        }
      where
        names = Map.keysSet types

defnsAndLibdepsToBranch0 ::
  Codebase IO v a ->
  DefnsF (Map Name) Referent TypeReference ->
  Branch0 Transaction ->
  Branch0 IO
defnsAndLibdepsToBranch0 codebase defns libdeps =
  let -- Unflatten the collection of terms into tree, ditto for types
      nametrees :: DefnsF2 Nametree (Map NameSegment) Referent TypeReference
      nametrees =
        bimap go go defns

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
  where
    go :: Ord v => Map Name v -> Nametree (Map NameSegment v)
    go =
      unflattenNametree . BiMultimap.fromRange

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

data MergeInfo = MergeInfo
  { paths :: !(TwoWay Path.Absolute),
    projectBranches :: !(TwoWay ProjectBranch),
    project :: !Project
  }
  deriving stock (Generic)

textualDescriptionOfMerge :: MergeInfo -> Text
textualDescriptionOfMerge mergeInfo =
  let bobBranchText = into @Text (ProjectAndBranch mergeInfo.project.name mergeInfo.projectBranches.bob.name)
   in "merge-" <> bobBranchText

-- FIXME: let's come up with a better term for "dependencies" in the implementation of this function
identifyDependents ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  DefnsF Unconflicts Referent TypeReference ->
  Transaction (TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId))
identifyDependents defns conflicts unconflicts = do
  let -- The other person's (i.e. with "Alice" and "Bob" swapped) solo-deleted and solo-updated names
      theirSoloUpdatesAndDeletes :: TwoWay (DefnsF Set Name Name)
      theirSoloUpdatesAndDeletes =
        TwoWay.swap (unconflictedSoloDeletedNames <> unconflictedSoloUpdatedNames)
        where
          unconflictedSoloDeletedNames :: TwoWay (DefnsF Set Name Name)
          unconflictedSoloDeletedNames =
            bitraverse Unconflicts.soloDeletedNames Unconflicts.soloDeletedNames unconflicts

          unconflictedSoloUpdatedNames :: TwoWay (DefnsF Set Name Name)
          unconflictedSoloUpdatedNames =
            bitraverse Unconflicts.soloUpdatedNames Unconflicts.soloUpdatedNames unconflicts

  let dependencies :: TwoWay (Set Reference)
      dependencies =
        fold
          [ -- One source of dependencies: Alice's versions of Bob's unconflicted deletes and updates, and vice-versa.
            --
            -- This is name-based: if Bob updates the *name* "foo", then we go find the thing that Alice calls "foo" (if
            -- anything), no matter what its hash is.
            defnsReferences
              <$> ( zipDefnsWith BiMultimap.restrictRan BiMultimap.restrictRan
                      <$> theirSoloUpdatesAndDeletes
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
            let f :: Foldable t => t Reference.Id -> Set Reference
                f =
                  List.foldl' (\acc ref -> Set.insert (Reference.DerivedId ref) acc) Set.empty . Foldable.toList
             in bifoldMap f f <$> conflicts
          ]

  dependents0 <-
    for ((,) <$> defns <*> dependencies) \(defns1, dependencies1) ->
      getNamespaceDependentsOf2 defns1 dependencies1

  -- For each of Alice's dependents "foo" (and ditto for Bob, of course), throw the dependent away if any of the
  -- following are true:
  --
  --   1. "foo" is Alice-conflicted (since we only want to return *unconflicted* things.
  --   2. "foo" was deleted by Bob.
  --   3. "foo" was updated by Bob and not updated by Alice.
  let dependents1 :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId)
      dependents1 =
        zipDefnsWith Map.withoutKeys Map.withoutKeys
          <$> dependents0
          <*> ((bimap Map.keysSet Map.keysSet <$> conflicts) <> theirSoloUpdatesAndDeletes)

  -- Of the remaining dependents, it's still possible that the maps are not disjoint. But whenever the same name key
  -- exists in Alice's and Bob's dependents, the value will either be equal (by Unison hash), or synhash-equal (i.e.
  -- the term or type received different auto-propagated updates). So, we can just arbitrarily keep Alice's.
  let dependents2 :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId)
      dependents2 =
        dependents1 & over #bob \bob ->
          zipDefnsWith Map.difference Map.difference bob dependents1.alice

  pure dependents2

makeStageOne ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) termid typeid) ->
  DefnsF Unconflicts term typ ->
  TwoWay (DefnsF (Map Name) termid typeid) ->
  DefnsF (Map Name) term typ ->
  DefnsF (Map Name) term typ
makeStageOne declNameLookups conflicts unconflicts dependents =
  zipDefnsWith3 makeStageOneV makeStageOneV unconflicts (f conflicts <> f dependents)
  where
    f :: TwoWay (DefnsF (Map Name) term typ) -> DefnsF Set Name Name
    f defns =
      fold (refIdsToNames <$> declNameLookups <*> defns)

makeStageOneV :: Unconflicts v -> Set Name -> Map Name v -> Map Name v
makeStageOneV unconflicts namesToDelete =
  (`Map.withoutKeys` namesToDelete) . Unconflicts.apply unconflicts

defnsReferences :: Defns (BiMultimap Referent name) (BiMultimap TypeReference name) -> Set Reference
defnsReferences =
  bifoldMap (Set.map Referent.toReference . BiMultimap.dom) BiMultimap.dom

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames defns =
  Names.Names
    { terms = Relation.fromMap (BiMultimap.range defns.terms),
      types = Relation.fromMap (BiMultimap.range defns.types)
    }

findTemporaryBranchName :: MergeInfo -> Transaction ProjectBranchName
findTemporaryBranchName info = do
  -- let currentProjectId = mergeInfo.project.projectId
  Cli.findTemporaryBranchName info.project.projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        "merge-"
          <> into @Text info.projectBranches.bob.name
          <> "-into-"
          <> into @Text info.projectBranches.alice.name

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  MergeDatabase ->
  V2.Branch Transaction ->
  Transaction (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
loadNamespaceInfo abort db branch = do
  defns <- loadNamespaceInfo0 (referent2to1 db) branch
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  Monad m =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (DefnsF2 (Map NameSegment) Set Referent TypeReference))
loadNamespaceInfo0 referent2to1 branch = do
  terms <-
    branch.terms
      & Map.map Map.keysSet
      & traverse (Set.traverse referent2to1)
  let types = Map.map Map.keysSet branch.types
  children <-
    for (Map.delete NameSegment.libSegment branch.children) \childCausal -> do
      childBranch <- childCausal.value
      loadNamespaceInfo0_ referent2to1 childBranch
  pure Nametree {value = Defns {terms, types}, children}

loadNamespaceInfo0_ ::
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (DefnsF2 (Map NameSegment) Set Referent TypeReference))
loadNamespaceInfo0_ referent2to1 branch = do
  terms <-
    branch.terms
      & Map.map Map.keysSet
      & traverse (Set.traverse referent2to1)
  let types = Map.map Map.keysSet branch.types
  children <-
    for branch.children \childCausal -> do
      childBranch <- childCausal.value
      loadNamespaceInfo0_ referent2to1 childBranch
  pure Nametree {value = Defns {terms, types}, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree (DefnsF2 (Map NameSegment) Set Referent TypeReference) ->
  Either Merge.PreconditionViolation (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names defns -> do
    terms <-
      defns.terms & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTermName (Name.fromReverseSegments (name :| names)))
    types <-
      defns.types & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTypeName (Name.fromReverseSegments (name :| names)))
    pure Defns {terms, types}
  where
    assertUnconflicted :: (Set ref -> Merge.PreconditionViolation) -> Set ref -> Either Merge.PreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- Convert a merge precondition violation to an output message.
mergePreconditionViolationToOutput :: Merge.PreconditionViolation -> Output.Output
mergePreconditionViolationToOutput = \case
  Merge.ConflictedAliases branch name1 name2 -> Output.MergeConflictedAliases branch name1 name2
  Merge.ConflictedTermName name refs -> Output.MergeConflictedTermName name refs
  Merge.ConflictedTypeName name refs -> Output.MergeConflictedTypeName name refs
  Merge.ConflictInvolvingBuiltin name -> Output.MergeConflictInvolvingBuiltin name
  Merge.ConstructorAlias maybeBranch name1 name2 -> Output.MergeConstructorAlias maybeBranch name1 name2
  Merge.DefnsInLib -> Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> Output.MergeMissingConstructorName name
  Merge.NestedDeclAlias name -> Output.MergeNestedDeclAlias name
  Merge.StrayConstructor name -> Output.MergeStrayConstructor name

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  Maybe ProjectBranchName ->
  V2.Branch Transaction ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  Transaction (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name), DeclNameLookup)
assertNamespaceSatisfiesPreconditions db abort maybeBranchName branch defns = do
  whenJust (Map.lookup NameSegment.libSegment branch.children) \libdepsCausal -> do
    libdepsBranch <- libdepsCausal.value
    when (not (Map.null libdepsBranch.terms) || not (Map.null libdepsBranch.types)) do
      abort Merge.DefnsInLib

  declNameLookup <-
    checkDeclCoherency db.loadDeclNumConstructors defns
      & onLeftM (abort . incoherentDeclReasonToMergePreconditionViolation)

  pure
    ( Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        },
      declNameLookup
    )
  where
    incoherentDeclReasonToMergePreconditionViolation :: IncoherentDeclReason -> Merge.PreconditionViolation
    incoherentDeclReasonToMergePreconditionViolation = \case
      IncoherentDeclReason'ConstructorAlias firstName secondName ->
        Merge.ConstructorAlias maybeBranchName firstName secondName
      IncoherentDeclReason'MissingConstructorName name -> Merge.MissingConstructorName name
      IncoherentDeclReason'NestedDeclAlias name -> Merge.NestedDeclAlias name
      IncoherentDeclReason'StrayConstructor name -> Merge.StrayConstructor name

findOneConflictedAlias ::
  TwoWay ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  Maybe Merge.PreconditionViolation
findOneConflictedAlias projectBranchNames lcaDefns diffs =
  aliceConflictedAliases <|> bobConflictedAliases
  where
    aliceConflictedAliases =
      findConflictedAlias lcaDefns diffs.alice <&> \(name1, name2) ->
        Merge.ConflictedAliases projectBranchNames.alice.name name1 name2

    bobConflictedAliases =
      findConflictedAlias lcaDefns diffs.bob <&> \(name1, name2) ->
        Merge.ConflictedAliases projectBranchNames.bob.name name1 name2

-- @findConflictedAlias namespace diff@, given an old namespace and a diff to a new namespace, will return the first
-- "conflicted alias" encountered (if any), where a "conflicted alias" is a pair of names that referred to the same
-- thing in the old namespace, but different things in the new one.
--
-- For example, if the old namespace was
--
--   foo = #foo
--   bar = #foo
--
-- and the new namespace is
--
--   foo = #baz
--   bar = #qux
--
-- then (foo, bar) is a conflicted alias.
--
-- This function currently doesn't return whether the conflicted alias is a decl or a term, but it certainly could.
findConflictedAlias ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum [go defns.terms diff.terms, go defns.types diff.types]
  where
    go :: forall ref. (Ord ref) => BiMultimap ref Name -> Map Name (DiffOp (Synhashed ref)) -> Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, DiffOp (Synhashed ref)) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            DiffOp'Add _ -> Nothing
            DiffOp'Delete _ -> Nothing
            DiffOp'Update hashed1 ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hashed1.new)
                & asum
          where
            g :: Synhashed ref -> Name -> Maybe (Name, Name)
            g hashed1 alias =
              case Map.lookup alias diff of
                Just (DiffOp'Update hashed2) | hashed1 == hashed2.new -> Nothing
                _ -> Just (name, alias)

-- Given a name like "base", try "base__1", then "base__2", etc, until we find a name that doesn't
-- clash with any existing dependencies.
getTwoFreshNames :: Set NameSegment -> NameSegment -> (NameSegment, NameSegment)
getTwoFreshNames names name0 =
  go2 0
  where
    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go2 4 = ("base__4", "base__7")
    go2 :: Integer -> (NameSegment, NameSegment)
    go2 !i
      | Set.member name names = go2 (i + 1)
      | otherwise = (name, go1 (i + 1))
      where
        name = mangled i

    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go1 5 = "base__7"
    go1 :: Integer -> NameSegment
    go1 !i
      | Set.member name names = go1 (i + 1)
      | otherwise = name
      where
        name = mangled i

    mangled :: Integer -> NameSegment
    mangled i =
      NameSegment (NameSegment.toUnescapedText name0 <> "__" <> tShow i)

libdepsToBranch0 :: MergeDatabase -> Map NameSegment (V2.CausalBranch Transaction) -> Transaction (Branch0 Transaction)
libdepsToBranch0 db libdeps = do
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
  Conversions.branch2to1 branchCache db.loadDeclType branch

------------------------------------------------------------------------------------------------------------------------
-- Debugging by printing a bunch of stuff out

data DebugFunctions = DebugFunctions
  { debugDefns ::
      ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
      ThreeWay DeclNameLookup ->
      IO (),
    debugDiffs :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) -> IO (),
    debugCombinedDiff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> IO (),
    debugPartitionedDiff ::
      TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
      DefnsF Unconflicts Referent TypeReference ->
      IO (),
    debugDependents :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) -> IO (),
    debugBumpedDefns :: DefnsF (Map Name) Referent TypeReference -> IO ()
  }

realDebugFunctions :: DebugFunctions
realDebugFunctions =
  DebugFunctions
    { debugDefns = realDebugDefns,
      debugDiffs = realDebugDiffs,
      debugCombinedDiff = realDebugCombinedDiff,
      debugPartitionedDiff = realDebugPartitionedDiff,
      debugDependents = realDebugDependents,
      debugBumpedDefns = realDebugBumpedDefns
    }

fakeDebugFunctions :: DebugFunctions
fakeDebugFunctions =
  DebugFunctions mempty mempty mempty mempty mempty mempty

realDebugDefns ::
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  ThreeWay DeclNameLookup ->
  IO ()
realDebugDefns defns declNameLookups = do
  Text.putStrLn (Text.bold "\n=== Alice's definitions ===")
  debugDefns1 (bimap BiMultimap.range BiMultimap.range defns.alice)

  Text.putStrLn (Text.bold "\n=== Bob's definitions ===")
  debugDefns1 (bimap BiMultimap.range BiMultimap.range defns.bob)

  Text.putStrLn (Text.bold "\n=== Alice's constructor names ===")
  debugConstructorNames declNameLookups.alice.declToConstructors

  Text.putStrLn (Text.bold "\n=== Bob's constructor names ===")
  debugConstructorNames declNameLookups.bob.declToConstructors

realDebugDiffs :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) -> IO ()
realDebugDiffs diffs = do
  Text.putStrLn (Text.bold "\n=== Alice's diff ===")
  renderDiff diffs.alice
  Text.putStrLn (Text.bold "\n=== Bob's diff ===")
  renderDiff diffs.bob
  where
    renderDiff :: DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference -> IO ()
    renderDiff diff = do
      renderThings referentLabel diff.terms
      renderThings (const "type") diff.types

    renderThings :: (ref -> Text) -> Map Name (DiffOp (Synhashed ref)) -> IO ()
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
              DiffOp'Add x -> go Text.green "+" x
              DiffOp'Delete x -> go Text.red "-" x
              DiffOp'Update x -> go Text.yellow "%" x.new

realDebugCombinedDiff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> IO ()
realDebugCombinedDiff diff = do
  Text.putStrLn (Text.bold "\n=== Combined diff ===")
  renderThings referentLabel Referent.toText diff.terms
  renderThings (const "type") Reference.toText diff.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name (CombinedDiffOp ref) -> IO ()
    renderThings label renderRef things =
      for_ (Map.toList things) \(name, op) ->
        Text.putStrLn case op of
          CombinedDiffOp'Add who ->
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
          CombinedDiffOp'Delete who ->
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
          CombinedDiffOp'Update who ->
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
          CombinedDiffOp'Conflict ref ->
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

    renderWho :: EitherWayI v -> Text
    renderWho = \case
      OnlyAlice _ -> "Alice"
      OnlyBob _ -> "Bob"
      AliceAndBob _ -> "Alice and Bob"

realDebugPartitionedDiff ::
  TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  DefnsF Unconflicts Referent TypeReference ->
  IO ()
realDebugPartitionedDiff conflicts unconflicts = do
  Text.putStrLn (Text.bold "\n=== Conflicts ===")
  renderConflicts "termid" conflicts.alice.terms (Alice ())
  renderConflicts "termid" conflicts.bob.terms (Bob ())
  renderConflicts "typeid" conflicts.alice.types (Alice ())
  renderConflicts "typeid" conflicts.bob.types (Bob ())

  Text.putStrLn (Text.bold "\n=== Unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.alice (OnlyAlice ())
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.bob (OnlyBob ())
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.both (AliceAndBob ())
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.alice (OnlyAlice ())
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.bob (OnlyBob ())
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.both (AliceAndBob ())
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.alice (OnlyAlice ())
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.bob (OnlyBob ())
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.both (AliceAndBob ())
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.alice (OnlyAlice ())
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.bob (OnlyBob ())
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.both (AliceAndBob ())
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.alice (OnlyAlice ())
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.bob (OnlyBob ())
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.both (AliceAndBob ())
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.alice (OnlyAlice ())
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.bob (OnlyBob ())
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.both (AliceAndBob ())
  where
    renderConflicts :: Text -> Map Name Reference.Id -> EitherWay () -> IO ()
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
              <> (case who of Alice () -> "Alice"; Bob () -> "Bob")
              <> ")"

    renderUnconflicts ::
      (Text -> Text) ->
      Text ->
      (ref -> Text) ->
      (ref -> Text) ->
      Map Name ref ->
      EitherWayI () ->
      IO ()
    renderUnconflicts color action label renderRef unconflicts who =
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
              <> " ("
              <> (case who of OnlyAlice () -> "Alice"; OnlyBob () -> "Bob"; AliceAndBob () -> "Alice and Bob")
              <> ")"

realDebugDependents :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) -> IO ()
realDebugDependents dependents = do
  Text.putStrLn (Text.bold "\n=== Alice's dependents of deletes, updates, and conflicts ===")
  renderThings "termid" dependents.alice.terms
  renderThings "typeid" dependents.alice.types
  Text.putStrLn (Text.bold "\n=== Bob's dependents of deletes, updates, and conflicts ===")
  renderThings "termid" dependents.bob.terms
  renderThings "typeid" dependents.bob.types
  where
    renderThings :: Text -> Map Name Reference.Id -> IO ()
    renderThings label things =
      for_ (Map.toList things) \(name, ref) ->
        Text.putStrLn $
          Text.italic label
            <> " "
            <> Name.toText name
            <> " "
            <> Reference.idToText ref

realDebugBumpedDefns :: DefnsF (Map Name) Referent TypeReference -> IO ()
realDebugBumpedDefns defns = do
  Text.putStrLn (Text.bold "\n=== Bumped definitions ===")
  debugDefns1 defns

debugConstructorNames :: Map Name [Name] -> IO ()
debugConstructorNames names =
  for_ (Map.toList names) \(typeName, conNames) ->
    Text.putStrLn (Name.toText typeName <> " => " <> Text.intercalate ", " (map Name.toText conNames))

debugDefns1 :: DefnsF (Map Name) Referent TypeReference -> IO ()
debugDefns1 defns = do
  renderThings referentLabel Referent.toText defns.terms
  renderThings (const "type") Reference.toText defns.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name ref -> IO ()
    renderThings label renderRef things =
      for_ (Map.toList things) \(name, ref) ->
        Text.putStrLn (Text.italic (label ref) <> " " <> Name.toText name <> " " <> renderRef ref)

referentLabel :: Referent -> Text
referentLabel ref
  | Referent'.isConstructor ref = "constructor"
  | otherwise = "term"

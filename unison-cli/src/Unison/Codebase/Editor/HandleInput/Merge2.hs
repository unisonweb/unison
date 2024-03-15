{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (Lens', view, (%~))
import Control.Monad.Reader (ask)
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (Semialign, alignWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Data.Zip (Zip, zipWith)
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Sqlite.DbId (ProjectId)
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
import Unison.Codebase.Branch.DeclCoherencyCheck (IncoherentDeclReason (..), checkDeclCoherency)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2
  ( addDefinitionsToUnisonFile,
    getExistingReferencesNamed,
    getNamespaceDependentsOf,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Merge.CombineDiffs (AliceIorBob (..), CombinedDiffsOp (..), combineDiffs)
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.DiffOp qualified as Merge
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
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
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Referent' qualified as Referent'
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), alignDefnsWith, bifoldMapDefns, bimapDefns, bitraverseDefns, unzipDefns, unzipDefnsWith)
import Unison.Util.Nametree (Nametree (..), flattenNametree, traverseNametreeWithName, unflattenNametree)
import Unison.Util.Pretty (ColorText, Pretty)
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
  Cli.Env {codebase} <- ask

  -- Create a bunch of cached database lookup functions
  db <- makeMergeDatabase codebase

  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  mergeInfo <- loadMergeInfo bobBranchName

  -- Load alice, bob, and LCA branches
  branches <-
    Cli.runTransactionWithRollback \abort -> do
      loadV2Branches =<< loadV2Causals abort db mergeInfo

  -- Load alice, bob, and LCA definitions + decl names
  (declNames, defns) <-
    Cli.runTransactionWithRollback \abort -> do
      loadDefns abort db mergeInfo branches

  -- Diff LCA->Alice and LCA->Bob
  diffs <-
    Cli.runTransaction do
      Merge.nameBasedNamespaceDiff db defns

  -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
  whenJust (findOneConflictedAlias mergeInfo.projectBranches defns.lca diffs) \violation ->
    Cli.returnEarly (mergePreconditionViolationToOutput violation)

  -- Combine the LCA->Alice and LCA->Bob diffs together into a combined diff
  let diff = combineDiffs diffs

  -- Partition the combined diff into "flicts" - the CON-flicts (conflicted things) and the UN-CON-flicts (unconflicted
  -- things)
  let flicts = partitionDiffIntoFlicts diff

  -- Identify the dependents we need to pull into the scratch file (either first for typechecking, if there aren't
  -- conflicts, or else for manual conflict resolution without a typechecking step, if there are)
  dependents <-
    Cli.runTransaction do
      identifyDependentsOfUnconflicts (ThreeWay.forgetLca defns) flicts.unconflicts

  -- Create the scratch file
  unisonFile <-
    Cli.runTransactionWithRollback \abort -> do
      absoluteHonk abort codebase declNames dependents flicts.conflicts

  let (newDefns, droppedDefns) = bumpLca defns.lca flicts.unconflicts dependents

  -- Load and merge Alice's and Bob's libdeps
  libdeps <-
    Cli.runTransaction do
      libdeps <- loadLibdeps branches
      libdepsToBranch0 db (Merge.mergeLibdeps getTwoFreshNames libdeps)

  let newBranchIO = defnsAndLibdepsToBranch0 codebase newDefns libdeps

  let mergedNames = Branch.toNames newBranchIO

  let ppedNames = mergedNames <> defnsRangeToNames droppedDefns

  let pped = PPED.makePPED (PPE.namer ppedNames) (PPE.suffixifyByName ppedNames)

  let prettyUf = Pretty.prettyUnisonFile pped unisonFile

  currentPath <- Cli.getCurrentPath
  parsingEnv <- makeParsingEnv currentPath mergedNames
  prettyParseTypecheck unisonFile pped parsingEnv >>= \case
    Left prettyError -> do
      promptUser mergeInfo (Pretty.prettyUnisonFile pped unisonFile) newBranchIO
    Right tuf -> do
      mergedBranchPlusTuf <-
        Cli.runTransactionWithRollback \abort -> do
          updates <- typecheckedUnisonFileToBranchUpdates abort undefined tuf
          pure (Branch.batchUpdates updates newBranchIO)
      Cli.stepAt
        (textualDescriptionOfMerge mergeInfo)
        ( Path.unabsolute mergeInfo.paths.alice,
          const mergedBranchPlusTuf
        )

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
  MergeInfo ->
  (TwoOrThreeWay (V2.Branch Transaction)) ->
  Transaction (TwoWay (Map Name [Name]), ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)))
loadDefns abort0 db info branches = do
  lcaDefns <-
    case branches.lca of
      Nothing -> pure (Defns BiMultimap.empty BiMultimap.empty)
      Just lcaBranch -> loadLcaDefinitions abort (referent2to1 db) lcaBranch
  aliceDefns0 <- loadNamespaceInfo abort db branches.alice
  (aliceDeclNames, aliceDefns1) <-
    assertNamespaceSatisfiesPreconditions db abort info.projectBranches.alice.name branches.alice aliceDefns0
  bobDefns0 <- loadNamespaceInfo abort db branches.bob
  (bobDeclNames, bobDefns1) <-
    assertNamespaceSatisfiesPreconditions db abort info.projectBranches.bob.name branches.bob bobDefns0
  pure
    ( TwoWay {alice = aliceDeclNames, bob = bobDeclNames},
      ThreeWay {lca = lcaDefns, alice = aliceDefns1, bob = bobDefns1}
    )
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

-- absolutely honk it up
absoluteHonk ::
  (forall x. Output -> Transaction x) ->
  Codebase IO Symbol Ann ->
  TwoWay (Map Name [Name]) ->
  Defns (Map Name (Set TermReferenceId)) (Map Name (Set TypeReferenceId)) ->
  Defns (Map Name (TwoWay Referent)) (Map Name (TwoWay TypeReference)) ->
  Transaction (UnisonFile Symbol Ann)
absoluteHonk abort codebase declNames dependents conflicts =
  case Map.null conflicts.terms && Map.null conflicts.types of
    False -> do
      conflicts1 <- weewoo <$> assertConflictsSatisfyPreconditions abort conflicts
      makeUnisonFile2 abort codebase declNames dependents conflicts1
    True -> makeUnisonFile abort codebase (declNames.alice <> declNames.bob) dependents

bumpLca ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Unconflicts (Map Name Referent)) (Unconflicts (Map Name TypeReference)) ->
  Defns (Map Name (Set TermReferenceId)) (Map Name (Set TypeReferenceId)) ->
  ( Defns (Map Name Referent) (Map Name TypeReference),
    Dropped (Defns (Map Name Referent) (Map Name TypeReference))
  )
bumpLca lca unconflicts dependents =
  let -- Compute the adds to apply to the LCA
      adds =
        bimapDefns f f unconflicts
        where
          f :: Unconflicts (Map Name v) -> NamespaceUpdate (Map Name v)
          f x = performAdds (fold (x.adds <> x.updates) Map.\\ dependents.terms)

      -- Compute the deletes to apply to the LCA
      deletes =
        bimapDefns f f unconflicts
        where
          f :: Unconflicts (Map Name v) -> NamespaceUpdate (Map Name v)
          f = performDeletes . foldMap Map.keysSet . view #deletes
   in (adds <> deletes)
        -- Combine the separate term/type namespace updates into one
        & combineNamespaceUpdates
        -- Apply the updates to the LCA
        & runNamespaceUpdate (defnsRangeOnly lca)

weewoo ::
  Defns (Map Name (TwoWay Referent.Id)) (Map Name (TwoWay TypeReferenceId)) ->
  TwoWay (Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId))
weewoo =
  bifoldMapDefns
    (Map.foldMapWithKey clonk)
    (fmap typesToDefns . Map.foldMapWithKey honk)
  where
    clonk ::
      Name ->
      TwoWay Referent.Id ->
      TwoWay (Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId))
    clonk name = fmap f
      where
        f :: Referent.Id -> Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId)
        f = \case
          Referent'.Ref' ref -> termsToDefns (Relation.singleton name ref)
          Referent'.Con' (ConstructorReference ref _) _ -> typesToDefns (Relation.singleton name ref)
    honk :: Name -> TwoWay TypeReferenceId -> TwoWay (Relation Name TypeReferenceId)
    honk name = fmap (Relation.singleton name)
    termsToDefns terms = Defns {terms, types = Relation.empty}
    typesToDefns types = Defns {terms = Relation.empty, types}

type Dropped a = a

newtype NamespaceUpdate a
  = NamespaceUpdate (a -> (a, Dropped a))

instance Semigroup a => Semigroup (NamespaceUpdate a) where
  NamespaceUpdate update1 <> NamespaceUpdate update2 =
    NamespaceUpdate \refs0 ->
      let (refs1, dropped1) = update1 refs0
          (refs2, dropped2) = update2 refs1
       in (refs2, dropped1 <> dropped2)

runNamespaceUpdate :: a -> NamespaceUpdate a -> (a, Dropped a)
runNamespaceUpdate refs (NamespaceUpdate update) =
  update refs

-- Combine separate term and type updates together.
combineNamespaceUpdates :: Defns (NamespaceUpdate tm) (NamespaceUpdate ty) -> NamespaceUpdate (Defns tm ty)
combineNamespaceUpdates updates =
  NamespaceUpdate \defns ->
    let (newTerms, droppedTerms) = runNamespaceUpdate defns.terms updates.terms
        (newTypes, droppedTypes) = runNamespaceUpdate defns.types updates.types
     in (Defns newTerms newTypes, Defns droppedTerms droppedTypes)

performAdds :: Map Name v -> NamespaceUpdate (Map Name v)
performAdds adds =
  NamespaceUpdate \refs ->
    ( Map.unionWith (\new _old -> new) adds refs,
      Map.intersectionWith (\_new old -> old) adds refs
    )

performDeletes :: Set Name -> NamespaceUpdate (Map Name v)
performDeletes deletions =
  NamespaceUpdate (Map.partitionWithKey (\name _ -> Set.notMember name deletions))

defnsAndLibdepsToBranch0 ::
  Codebase IO v a ->
  Defns (Map Name Referent) (Map Name TypeReference) ->
  Branch0 Transaction ->
  Branch0 IO
defnsAndLibdepsToBranch0 codebase defns libdeps =
  let -- Unflatten the collection of terms into tree, ditto for types
      nametrees :: Defns (Nametree (Map NameSegment Referent)) (Nametree (Map NameSegment TypeReference))
      nametrees =
        bimapDefns go go defns

      -- Align the tree of terms and tree of types into one tree
      nametree :: Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
      nametree =
        nametrees & alignDefnsWith \case
          This x -> Defns x Map.empty
          That y -> Defns Map.empty y
          These x y -> Defns x y

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

nametreeToBranch0 :: forall m. Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)) -> Branch0 m
nametreeToBranch0 nametree =
  Branch.branch0
    (rel2star defns.terms)
    (rel2star defns.types)
    (Branch.one . nametreeToBranch0 <$> nametree.children)
    Map.empty
  where
    defns =
      bimapDefns (Relation.swap . Relation.fromMap) (Relation.swap . Relation.fromMap) nametree.value

    rel2star :: Relation ref name -> Star2 ref name metadata
    rel2star rel =
      Star2.Star2 {fact = Relation.dom rel, d1 = rel, d2 = Relation.empty}

makeUnisonFile ::
  (forall x. Output -> Transaction x) ->
  Codebase IO Symbol Ann ->
  Map Name [Name] ->
  Defns (Map Name (Set TermReferenceId)) (Map Name (Set TypeReferenceId)) ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile abort codebase declNames defns = do
  let lookupCons k = case Map.lookup k declNames of
        Nothing -> Left (error ("failed to find: " <> show k <> " in the declMap"))
        Just x -> Right x
  addDefinitionsToUnisonFile
    abort
    codebase
    -- todo: fix output
    (const lookupCons)
    (Relation.fromMultimap defns.terms, Relation.fromMultimap defns.types)
    UnisonFile.emptyUnisonFile

makeUnisonFile2 ::
  (forall x. Output -> Transaction x) ->
  Codebase IO Symbol Ann ->
  TwoWay (Map Name [Name]) ->
  Defns (Map Name (Set TermReferenceId)) (Map Name (Set TypeReferenceId)) ->
  TwoWay (Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId)) ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile2 abort codebase declNames unconflicts conflicts = do
  unconflictedFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      (\_ declName -> Right (lookupCons declName (declNames.alice <> declNames.bob)))
      (Relation.fromMultimap unconflicts.terms, Relation.fromMultimap unconflicts.types)
      UnisonFile.emptyUnisonFile
  aliceFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      (\_ declName -> Right (lookupCons declName declNames.alice))
      (conflicts.alice.terms, conflicts.alice.types)
      UnisonFile.emptyUnisonFile
  bobFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      (\_ declName -> Right (lookupCons declName declNames.bob))
      (conflicts.bob.terms, conflicts.bob.types)
      UnisonFile.emptyUnisonFile
  pure wundefined
  where
    lookupCons declName m =
      case Map.lookup declName m of
        Nothing -> error (reportBug "E077058" ("Expected decl name " <> show declName <> " in constructor name map"))
        Just x -> x

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

-- "Two way inclusive"
data TwoWayI a = TwoWayI
  { alice :: !a,
    bob :: !a,
    both :: !a
  }
  deriving stock (Foldable, Functor, Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid (TwoWayI a))

instance Semialign TwoWayI where
  alignWith :: (These a b -> c) -> TwoWayI a -> TwoWayI b -> TwoWayI c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Zip TwoWayI where
  zipWith :: (a -> b -> c) -> TwoWayI a -> TwoWayI b -> TwoWayI c
  zipWith f (TwoWayI x1 x2 x3) (TwoWayI y1 y2 y3) =
    TwoWayI (f x1 y1) (f x2 y2) (f x3 y3)

identifyDependentsOfUnconflicts ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Defns (Unconflicts (Map Name Referent)) (Unconflicts (Map Name TypeReference)) ->
  Transaction (Defns (Map Name (Set TermReferenceId)) (Map Name (Set TypeReferenceId)))
identifyDependentsOfUnconflicts defns unconflicts = do
  let deletesAndUpdates = getSoloDeletesAndUpdates unconflicts

  let names = defnsToNames <$> defns

  let dependencies =
        TwoWay
          { alice = getExistingReferencesNamed deletesAndUpdates.bob names.alice,
            bob = getExistingReferencesNamed deletesAndUpdates.alice names.bob
          }

  let getPersonDependents ::
        (forall a. TwoWay a -> a) ->
        Transaction (Defns (Map Name (Set TermReferenceId)) (Map Name (Set TypeReferenceId)))
      getPersonDependents who =
        bimapDefns Relation.domain Relation.domain <$> getNamespaceDependentsOf (who names) (who dependencies)

  -- FIXME document why left-biased merge is ok (seems like it is)
  (<>)
    <$> getPersonDependents (view #alice)
    <*> getPersonDependents (view #bob)

getSoloDeletesAndUpdates ::
  Defns (Unconflicts (Map Name Referent)) (Unconflicts (Map Name TypeReference)) ->
  TwoWay (Defns (Set Name) (Set Name))
getSoloDeletesAndUpdates unconflicts =
  TwoWay
    { alice =
        Defns
          (Map.keysSet unconflicts.terms.deletes.alice <> Map.keysSet unconflicts.terms.updates.alice)
          (Map.keysSet unconflicts.types.deletes.alice <> Map.keysSet unconflicts.types.updates.alice),
      bob =
        Defns
          (Map.keysSet unconflicts.terms.deletes.bob <> Map.keysSet unconflicts.terms.updates.bob)
          (Map.keysSet unconflicts.types.deletes.bob <> Map.keysSet unconflicts.types.updates.bob)
    }

getSoloDeletesAndUpdates2 ::
  Defns (Unconflicts (Map Name Referent)) (Unconflicts (Map Name TypeReference)) ->
  TwoWay (Defns (Set Name) (Set Name))
getSoloDeletesAndUpdates2 unconflicts =
  -- Defns (Unconflicts (Map Name Referent)) (Unconflicts (Map Name TypeReference)) ->
  --         -> terms               :: Unconflicts (Map Name Referent)
  --         -> terms.deletes       :: TwoWayI (Map Name Referent)
  --         -> terms.deletes.alice :: Map Name Referent
  TwoWay
    { alice =
        Defns
          (Map.keysSet unconflicts.terms.deletes.alice <> Map.keysSet unconflicts.terms.updates.alice)
          (Map.keysSet unconflicts.types.deletes.alice <> Map.keysSet unconflicts.types.updates.alice),
      bob =
        Defns
          (Map.keysSet unconflicts.terms.deletes.bob <> Map.keysSet unconflicts.terms.updates.bob)
          (Map.keysSet unconflicts.types.deletes.bob <> Map.keysSet unconflicts.types.updates.bob)
    }

defnsRangeOnly :: Defns (BiMultimap term name) (BiMultimap typ name) -> Defns (Map name term) (Map name typ)
defnsRangeOnly =
  bimapDefns BiMultimap.range BiMultimap.range

data PartitionedDiff = Flicts
  { conflicts :: !(Defns (Map Name (TwoWay Referent)) (Map Name (TwoWay TypeReference))),
    unconflicts :: !(Defns (Unconflicts (Map Name Referent)) (Unconflicts (Map Name TypeReference)))
  }
  deriving stock (Generic)

data FlictsV v = FlictsV
  { unconflicts :: !(Unconflicts (Map Name v)),
    conflicts :: !(Map Name (TwoWay v))
  }
  deriving stock (Generic)

data Changes a = Changes
  { adds :: !a,
    deletes :: !a,
    updates :: !a
  }
  deriving stock (Foldable, Functor, Generic)

instance Semialign Changes where
  alignWith :: (These a b -> c) -> Changes a -> Changes b -> Changes c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Zip Changes where
  zipWith :: (a -> b -> c) -> Changes a -> Changes b -> Changes c
  zipWith f (Changes x1 x2 x3) (Changes y1 y2 y3) =
    Changes (f x1 y1) (f x2 y2) (f x3 y3)

data Unconflicts a = Unconflicts
  { adds :: !(TwoWayI a),
    deletes :: !(TwoWayI a),
    updates :: !(TwoWayI a)
  }
  deriving stock (Foldable, Functor, Generic)

instance Semialign Unconflicts where
  alignWith :: (These a b -> c) -> Unconflicts a -> Unconflicts b -> Unconflicts c
  alignWith f =
    zipWith \x y -> f (These x y)

instance Zip Unconflicts where
  zipWith :: (a -> b -> c) -> Unconflicts a -> Unconflicts b -> Unconflicts c
  zipWith f (Unconflicts x1 x2 x3) (Unconflicts y1 y2 y3) =
    Unconflicts (zipWith f x1 y1) (zipWith f x2 y2) (zipWith f x3 y3)

emptyFlictsV :: FlictsV v
emptyFlictsV =
  FlictsV
    { unconflicts =
        Unconflicts
          { adds = TwoWayI Map.empty Map.empty Map.empty,
            deletes = TwoWayI Map.empty Map.empty Map.empty,
            updates = TwoWayI Map.empty Map.empty Map.empty
          },
      conflicts = Map.empty
    }

makeFlictsV :: Map Name (CombinedDiffsOp v) -> FlictsV v
makeFlictsV =
  Map.foldlWithKey' (\s k v -> insert k v s) emptyFlictsV
  where
    insert :: Name -> CombinedDiffsOp v -> FlictsV v -> FlictsV v
    insert k = \case
      Added2 who v -> #unconflicts . #adds . whoL who %~ Map.insert k v
      Deleted2 who v -> #unconflicts . #deletes . whoL who %~ Map.insert k v
      Updated2 who v -> #unconflicts . #updates . whoL who %~ Map.insert k v
      Conflict v -> #conflicts %~ Map.insert k v
      where
        whoL :: forall x. AliceIorBob -> Lens' (TwoWayI x) x
        whoL = \case
          OnlyAlice -> #alice
          OnlyBob -> #bob
          AliceAndBob -> #both

-- Partition definitions into conflicted and unconflicted.
partitionDiffIntoFlicts ::
  Defns (Map Name (CombinedDiffsOp Referent)) (Map Name (CombinedDiffsOp TypeReference)) ->
  PartitionedDiff
partitionDiffIntoFlicts =
  makeFlicts . bimapDefns makeFlictsV makeFlictsV

makeFlicts :: Defns (FlictsV Referent) (FlictsV TypeReference) -> PartitionedDiff
makeFlicts defns =
  let (conflicts, unconflicts) =
        unzipDefnsWith (\v -> (v.conflicts, v.unconflicts)) (\v -> (v.conflicts, v.unconflicts)) defns
   in Flicts {conflicts, unconflicts}

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames =
  defnsRangeToNames . defnsRangeOnly

defnsRangeToNames :: Defns (Map Name Referent) (Map Name TypeReference) -> Names
defnsRangeToNames Defns {terms, types} =
  Names.Names
    { terms = Relation.fromMap terms,
      types = Relation.fromMap types
    }

promptUser ::
  MergeInfo ->
  Pretty ColorText ->
  Branch0 IO ->
  Cli a
promptUser mergeInfo prettyUnisonFile newBranch = do
  Cli.Env {writeSource} <- ask
  let currentProjectId = mergeInfo.project.projectId
  let targetBranchName = mergeInfo.projectBranches.bob.name
  let selfBranchName = mergeInfo.projectBranches.alice.name
  -- Small race condition: since picking a branch name and creating the branch happen in different
  -- transactions, creating could fail.
  temporaryBranchName <- Cli.runTransaction (findTemporaryBranchName currentProjectId targetBranchName selfBranchName)
  _temporaryBranchId <-
    HandleInput.Branch.doCreateBranch'
      (Branch.one newBranch)
      Nothing
      mergeInfo.project
      temporaryBranchName
      (textualDescriptionOfMerge mergeInfo)
  scratchFilePath <-
    Cli.getLatestFile <&> \case
      Nothing -> "scratch.u"
      Just (file, _) -> file
  liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
  -- todo: respond with some message
  Cli.returnEarlyWithoutOutput

findTemporaryBranchName :: ProjectId -> ProjectBranchName -> ProjectBranchName -> Transaction ProjectBranchName
findTemporaryBranchName projectId other self = do
  Cli.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        "merge-"
          <> into @Text other
          <> "-into-"
          <> into @Text self

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  MergeDatabase ->
  V2.Branch Transaction ->
  Transaction (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)))
loadNamespaceInfo abort db branch = do
  defns <- loadNamespaceInfo0 (referent2to1 db) branch
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))))
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
  m (Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))))
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
  Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))) ->
  Either Merge.PreconditionViolation (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)))
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
  Merge.ConstructorAlias branch name1 name2 -> Output.MergeConstructorAlias branch name1 name2
  Merge.DefnsInLib -> Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> Output.MergeMissingConstructorName name
  Merge.NestedDeclAlias name -> Output.MergeNestedDeclAlias name
  Merge.NoConstructorNames name -> Output.MergeNoConstructorNames name
  Merge.StrayConstructor name -> Output.MergeStrayConstructor name

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  ProjectBranchName ->
  V2.Branch Transaction ->
  (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))) ->
  Transaction (Map Name [Name], Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
assertNamespaceSatisfiesPreconditions db abort branchName branch defns = do
  Map.lookup NameSegment.libSegment branch.children `whenJust` \libdepsCausal -> do
    libdepsBranch <- libdepsCausal.value
    when (not (Map.null libdepsBranch.terms) || not (Map.null libdepsBranch.types)) do
      abort Merge.DefnsInLib
  declNames <-
    checkDeclCoherency db.loadDeclNumConstructors defns
      & onLeftM (abort . incoherentDeclReasonToMergePreconditionViolation)
  pure
    ( declNames,
      Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        }
    )
  where
    incoherentDeclReasonToMergePreconditionViolation :: IncoherentDeclReason -> Merge.PreconditionViolation
    incoherentDeclReasonToMergePreconditionViolation = \case
      IncoherentDeclReason'ConstructorAlias firstName secondName ->
        Merge.ConstructorAlias branchName firstName secondName
      IncoherentDeclReason'MissingConstructorName name -> Merge.MissingConstructorName name
      IncoherentDeclReason'NestedDeclAlias name -> Merge.NestedDeclAlias name
      IncoherentDeclReason'NoConstructorNames name -> Merge.NoConstructorNames name
      IncoherentDeclReason'StrayConstructor name -> Merge.StrayConstructor name

assertConflictsSatisfyPreconditions ::
  (forall void. Output -> Transaction void) ->
  Defns (Map Name (TwoWay Referent)) (Map Name (TwoWay TypeReference)) ->
  Transaction (Defns (Map Name (TwoWay Referent.Id)) (Map Name (TwoWay TypeReferenceId)))
assertConflictsSatisfyPreconditions abort =
  bitraverseDefns
    (Map.traverseWithKey assertTermIsntBuiltin)
    (Map.traverseWithKey assertTypeIsntBuiltin)
  where
    assertTermIsntBuiltin :: Name -> TwoWay Referent -> Transaction (TwoWay Referent.Id)
    assertTermIsntBuiltin name =
      traverse \ref ->
        case Referent.toId ref of
          Nothing -> abort (mergePreconditionViolationToOutput (Merge.ConflictInvolvingBuiltin name))
          Just refId -> pure refId

    assertTypeIsntBuiltin :: Name -> TwoWay TypeReference -> Transaction (TwoWay TypeReferenceId)
    assertTypeIsntBuiltin name =
      traverse \ref ->
        case Reference.toId ref of
          Nothing -> abort (mergePreconditionViolationToOutput (Merge.ConflictInvolvingBuiltin name))
          Just refId -> pure refId

-- Like `loadNamespaceInfo`, but for loading the LCA, which has fewer preconditions.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadLcaDefinitions ::
  (Monad m) =>
  (forall void. Merge.PreconditionViolation -> m void) ->
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
loadLcaDefinitions abort referent2to1 branch = do
  defns0 <- loadNamespaceInfo0 referent2to1 branch
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  pure
    Defns
      { terms = flattenNametree (view #terms) defns1,
        types = flattenNametree (view #types) defns1
      }

findOneConflictedAlias ::
  TwoWay ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  TwoWay (Defns (Map Name (DiffOp (Synhashed Referent))) (Map Name (DiffOp (Synhashed TypeReference)))) ->
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
  Defns (Map Name (DiffOp (Synhashed Referent))) (Map Name (DiffOp (Synhashed TypeReference))) ->
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
            Merge.Added _ -> Nothing
            Merge.Deleted _ -> Nothing
            Merge.Updated _ hashed1 ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hashed1)
                & asum
          where
            g :: Synhashed ref -> Name -> Maybe (Name, Name)
            g hashed1 alias =
              case Map.lookup alias diff of
                Just (Merge.Updated _ hashed2) | hashed1 == hashed2 -> Nothing
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

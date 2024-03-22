{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (Lens', view)
import Control.Monad.Reader (ask)
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.Reference (TermReferenceId, TypeReference, TypeReferenceId)
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
  ( getExistingReferencesNamed,
    getNamespaceDependentsOf,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
import Unison.Codebase.Editor.HandleInput.Update2 qualified as Update2
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Merge.CombineDiffs (AliceIorBob (..), Unconflicts (..), combineDiffs)
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
import Unison.Merge.TwoWayI (TwoWayI)
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
import Unison.UnisonFile (UnisonFile')
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, alignDefnsWith, bifoldMapDefns, bimapDefns, bitraverseDefns, defnsAreEmpty, unzipDefnsWith)
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

  -- Combine the LCA->Alice and LCA->Bob diffs together into the conflicted things and the unconflicted things
  let diff = combineDiffs diffs
  conflicts <-
    Cli.runTransactionWithRollback \abort -> do
      assertConflictsSatisfyPreconditions abort (bimapDefns (view #conflicts) (view #conflicts) diff)
  let unconflicts = bimapDefns (view #unconflicts) (view #unconflicts) diff

  -- Identify the dependents we need to pull into the Unison file (either first for typechecking, if there aren't
  -- conflicts, or else for manual conflict resolution without a typechecking step, if there are)
  dependents <-
    Cli.runTransaction do
      identifyDependentsOfUnconflicts (ThreeWay.forgetLca defns) unconflicts

  -- Create the Unison file (which may have conflicts)
  unisonFile <- makeUnisonFile declNames conflicts dependents

  let (newDefns, droppedDefns) = bumpLca defns.lca unconflicts dependents

  -- Load and merge Alice's and Bob's libdeps
  libdeps <-
    Cli.runTransaction do
      libdeps <- loadLibdeps branches
      libdepsToBranch0 db (Merge.mergeLibdeps getTwoFreshNames libdeps)

  let newBranchIO = defnsAndLibdepsToBranch0 codebase newDefns libdeps

  let mergedNames = Branch.toNames newBranchIO

  let ppedNames = mergedNames <> defnsRangeToNames droppedDefns

  let pped = PPED.makePPED (PPE.namer ppedNames) (PPE.suffixifyByName ppedNames)

  currentPath <- Cli.getCurrentPath
  parsingEnv <- makeParsingEnv currentPath mergedNames

  maybeTypecheckedUnisonFile <-
    if defnsAreEmpty conflicts
      then prettyParseTypecheck unisonFile pped parsingEnv <&> eitherToMaybe
      else pure Nothing

  case maybeTypecheckedUnisonFile of
    Nothing -> promptUser mergeInfo (Pretty.prettyUnisonFile pped unisonFile) newBranchIO
    Just tuf -> do
      mergedBranchPlusTuf <-
        Cli.runTransactionWithRollback \abort -> do
          updates <- typecheckedUnisonFileToBranchUpdates abort undefined tuf
          pure (Branch.batchUpdates updates newBranchIO)
      Cli.stepAt
        (textualDescriptionOfMerge mergeInfo)
        ( Path.unabsolute mergeInfo.paths.alice,
          const mergedBranchPlusTuf
        )
      Cli.respond (Output.MergeSuccess (aliceProjectAndBranchName mergeInfo) (bobProjectAndBranchName mergeInfo))

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
-- Creating Unison files

makeUnisonFile ::
  TwoWay (Map Name [Name]) ->
  DefnsF (Map Name) (TwoWay Referent.Id) (TwoWay TypeReferenceId) ->
  DefnsF (Map Name) (Set TermReferenceId) (Set TypeReferenceId) ->
  Cli (UnisonFile' [] Symbol Ann)
makeUnisonFile declNames conflicts dependents
  | defnsAreEmpty conflicts = makeUnconflictedUnisonFile (declNames.alice <> declNames.bob) dependents
  | otherwise = makeConflictedUnisonFile declNames dependents conflicts

makeUnconflictedUnisonFile ::
  Map Name [Name] ->
  DefnsF (Map Name) (Set TermReferenceId) (Set TypeReferenceId) ->
  Cli (UnisonFile' [] Symbol Ann)
makeUnconflictedUnisonFile declNames defns = do
  Cli.Env {codebase} <- ask
  Cli.runTransactionWithRollback \abort ->
    Update2.makeUnisonFile
      abort
      codebase
      (\_ name -> Right (lookupConstructorName declNames name))
      (bimapDefns Relation.fromMultimap Relation.fromMultimap defns)

makeConflictedUnisonFile ::
  TwoWay (Map Name [Name]) ->
  DefnsF (Map Name) (Set TermReferenceId) (Set TypeReferenceId) ->
  DefnsF (Map Name) (TwoWay Referent.Id) (TwoWay TypeReferenceId) ->
  Cli (UnisonFile' [] Symbol Ann)
makeConflictedUnisonFile declNames unconflicts conflicts = do
  Cli.Env {codebase} <- ask
  Cli.runTransactionWithRollback \abort -> do
    unconflictedFile <-
      Update2.makeUnisonFile
        abort
        codebase
        (\_ name -> Right (lookupConstructorName (Map.union declNames.alice declNames.bob) name))
        (bimapDefns Relation.fromMultimap Relation.fromMultimap unconflicts)
    aliceFile <-
      Update2.makeUnisonFile
        abort
        codebase
        (\_ name -> Right (lookupConstructorName declNames.alice name))
        conflicts1.alice
    bobFile <-
      Update2.makeUnisonFile
        abort
        codebase
        (\_ name -> Right (lookupConstructorName declNames.bob name))
        conflicts1.bob
    pure (foldr UnisonFile.semigroupMerge UnisonFile.emptyUnisonFile [unconflictedFile, aliceFile, bobFile])
  where
    conflicts1 :: TwoWay (DefnsF (Relation Name) TermReferenceId TypeReferenceId)
    conflicts1 =
      identifyConflictedReferences conflicts

lookupConstructorName :: Map Name [Name] -> Name -> [Name]
lookupConstructorName m x =
  case Map.lookup x m of
    Nothing -> error (reportBug "E077058" ("Expected decl name " <> show x <> " in constructor name map"))
    Just y -> y

-- As input, we have a conflicted namespace laid out like
--
--   {
--     terms =
--       "foo"     => { alice = #fooAlice,   bob = #fooBob }
--       "Bar.Baz" => { alice = #BarAlice.0, bob = #BarBob.0 }
--     types =
--       "Qux"     => { alice = #QuxAlice,   bob = #QuxBob }
--   }
--
-- however, when a term (a referent) is conflicted, we want to extract the corresponding *reference* (i.e. if the
-- conflicted thing is actually a constructor, then we want to go fetch the type decl it belongs to). We also want to
-- repartition the conflicts by Alice's stuff and Bob's stuff. So,
--
--   {
--     alice =
--       terms =
--         "foo" <=> #fooAlice
--       types =
--         "Bar" <=> #BarAlice
--         "Qux" <=> #QuxAlice
--
--     bob =
--       terms =
--         "foo" <=> #fooBob
--       types =
--         "Bar" <=> #BarBob
--         "Qux" <=> #QuxBob
--  }
--
-- Note that the conflicted constructor Bar.Baz has "jumped" from the terms half to the types half (because, again, we
-- want to go off and fetch the type decls #BarAlice and #BarBob given that one of their constructors is conflicted).
identifyConflictedReferences ::
  DefnsF (Map Name) (TwoWay Referent.Id) (TwoWay TypeReferenceId) ->
  TwoWay (DefnsF (Relation Name) TermReferenceId TypeReferenceId)
identifyConflictedReferences =
  bifoldMapDefns
    (Map.foldMapWithKey conflictedTerms)
    (fmap typesToDefns . Map.foldMapWithKey conflictedTypes)
  where
    conflictedTerms ::
      Name ->
      TwoWay Referent.Id ->
      TwoWay (DefnsF (Relation Name) TermReferenceId TypeReferenceId)
    conflictedTerms name = fmap f
      where
        f :: Referent.Id -> DefnsF (Relation Name) TermReferenceId TypeReferenceId
        f = \case
          Referent'.Ref' ref -> termsToDefns (Relation.singleton name ref)
          Referent'.Con' (ConstructorReference ref _) _ -> typesToDefns (Relation.singleton name ref)

    conflictedTypes :: Name -> TwoWay TypeReferenceId -> TwoWay (Relation Name TypeReferenceId)
    conflictedTypes name =
      fmap (Relation.singleton name)

    termsToDefns terms = Defns {terms, types = Relation.empty}
    typesToDefns types = Defns {terms = Relation.empty, types}

------------------------------------------------------------------------------------------------------------------------
--

-- Given the LCA, apply all of the changes we can, resulting in the new "pre-typecheck" / "pre-propagation" version of
-- the merged branch.
--
-- To first approximation, we just want to take all of Alice and Bob's unconflicted adds and unconflicted updates, and
-- lay them over the LCA (shadowing whatever was there before, in case of update).
--
-- ... but we also need to perform propagation from Alice's updates to Bob's dependents that she didn't know about, and
-- vice versa!
--
-- For example, suppose in the LCA foo#foo depends on bar#bar which depends on baz#baz.
--
-- Alice updates foo#foo to foo#foo' (which still happens to depend on bar#bar), and Bob updates bar#bar to bar#bar'.
-- These are both unconflicted updates, but note that we want to propagate Bob's change to bar#bar' to all of Alice's
-- dependents of whatever she calls bar (namely, #bar), namely foo#foo'!
--
-- The final merged branch (if everything typechecks) will contain
--
--   {foo#foo'', bar#bar', baz#baz}
--
-- where #foo'' is the result of propagating Bob's #bar' to Alice's #foo', but the "pre-typecheck" / "pre-propagation"
-- merged branch that *this* function computes will only contain
--
--   {bar#bar', baz#baz}
--
-- To get there, we simply want to start from our "first approximation" above, but then delete unconflicted adds and
-- updates that are themselves dependents updates the other person made (because we need to propagate).
bumpLca ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF Unconflicts Referent TypeReference ->
  DefnsF (Map Name) (Set TermReferenceId) (Set TypeReferenceId) ->
  ( DefnsF (Map Name) Referent TypeReference,
    Dropped (DefnsF (Map Name) Referent TypeReference)
  )
bumpLca lca unconflicts dependents =
  (adds <> deletes)
    -- Combine the separate term/type namespace updates into one
    & combineNamespaceUpdates
    -- Apply the updates to the LCA
    & runNamespaceUpdate (defnsRangeOnly lca)
  where
    -- Compute the adds to apply to the LCA
    adds :: DefnsF NamespaceUpdate (Map Name Referent) (Map Name TypeReference)
    adds =
      Defns
        { terms = performAdds (addsAndUpdates unconflicts.terms Map.\\ dependents.terms),
          types = performAdds (addsAndUpdates unconflicts.types Map.\\ dependents.types)
        }

    -- Compute the deletes to apply to the LCA
    deletes :: DefnsF NamespaceUpdate (Map Name Referent) (Map Name TypeReference)
    deletes =
      bimapDefns deletes1 deletes1 unconflicts
      where
        deletes1 :: Unconflicts v -> NamespaceUpdate (Map Name v)
        deletes1 =
          performDeletes . foldMap Map.keysSet . view #deletes

    addsAndUpdates :: Unconflicts v -> Map Name v
    addsAndUpdates x =
      fold (x.adds <> x.updates)

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
combineNamespaceUpdates :: DefnsF NamespaceUpdate tm ty -> NamespaceUpdate (Defns tm ty)
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
  DefnsF (Map Name) Referent TypeReference ->
  Branch0 Transaction ->
  Branch0 IO
defnsAndLibdepsToBranch0 codebase defns libdeps =
  let -- Unflatten the collection of terms into tree, ditto for types
      nametrees :: DefnsF Nametree (Map NameSegment Referent) (Map NameSegment TypeReference)
      nametrees =
        bimapDefns go go defns

      -- Align the tree of terms and tree of types into one tree
      nametree :: Nametree (DefnsF (Map NameSegment) Referent TypeReference)
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

nametreeToBranch0 :: Nametree (DefnsF (Map NameSegment) Referent TypeReference) -> Branch0 m
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

identifyDependentsOfUnconflicts ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  DefnsF Unconflicts Referent TypeReference ->
  Transaction (DefnsF (Map Name) (Set TermReferenceId) (Set TypeReferenceId))
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
        Transaction (DefnsF (Map Name) (Set TermReferenceId) (Set TypeReferenceId))
      getPersonDependents who =
        bimapDefns Relation.domain Relation.domain <$> getNamespaceDependentsOf (who names) (who dependencies)

  -- FIXME document why left-biased merge is ok (seems like it is)
  (<>)
    <$> getPersonDependents (view #alice)
    <*> getPersonDependents (view #bob)

getSoloDeletesAndUpdates ::
  DefnsF Unconflicts Referent TypeReference ->
  TwoWay (DefnsF Set Name Name)
getSoloDeletesAndUpdates unconflicts =
  let (alice, bob) =
        unzipDefnsWith
          ( \terms ->
              ( namesOfUnconflictedDeletesAndUpdates OnlyAlice terms,
                namesOfUnconflictedDeletesAndUpdates OnlyBob terms
              )
          )
          ( \types ->
              ( namesOfUnconflictedDeletesAndUpdates OnlyAlice types,
                namesOfUnconflictedDeletesAndUpdates OnlyBob types
              )
          )
          unconflicts
   in TwoWay {alice, bob}

defnsRangeOnly :: Defns (BiMultimap term name) (BiMultimap typ name) -> DefnsF (Map name) term typ
defnsRangeOnly =
  bimapDefns BiMultimap.range BiMultimap.range

-- Names of the given person's deletes and updates.
namesOfUnconflictedDeletesAndUpdates :: AliceIorBob -> Unconflicts v -> Set Name
namesOfUnconflictedDeletesAndUpdates who unconflicts =
  Map.keysSet (view (whoL who) unconflicts.deletes) <> Map.keysSet (view (whoL who) unconflicts.updates)

whoL :: AliceIorBob -> Lens' (TwoWayI a) a
whoL = \case
  OnlyAlice -> #alice
  OnlyBob -> #bob
  AliceAndBob -> #both

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames =
  defnsRangeToNames . defnsRangeOnly

defnsRangeToNames :: DefnsF (Map Name) Referent TypeReference -> Names
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
  Cli.returnEarly (Output.MergeFailure scratchFilePath (aliceProjectAndBranchName mergeInfo) (bobProjectAndBranchName mergeInfo))

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
  Transaction (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
loadNamespaceInfo abort db branch = do
  defns <- loadNamespaceInfo0 (referent2to1 db) branch
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (DefnsF (Map NameSegment) (Set Referent) (Set TypeReference)))
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
  m (Nametree (DefnsF (Map NameSegment) (Set Referent) (Set TypeReference)))
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
  Nametree (DefnsF (Map NameSegment) (Set Referent) (Set TypeReference)) ->
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
  Merge.ConstructorAlias branch name1 name2 -> Output.MergeConstructorAlias branch name1 name2
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
  ProjectBranchName ->
  V2.Branch Transaction ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
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
      IncoherentDeclReason'StrayConstructor name -> Merge.StrayConstructor name

assertConflictsSatisfyPreconditions ::
  (forall void. Output -> Transaction void) ->
  DefnsF (Map Name) (TwoWay Referent) (TwoWay TypeReference) ->
  Transaction (DefnsF (Map Name) (TwoWay Referent.Id) (TwoWay TypeReferenceId))
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
  TwoWay (DefnsF (Map Name) (DiffOp (Synhashed Referent)) (DiffOp (Synhashed TypeReference))) ->
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
  DefnsF (Map Name) (DiffOp (Synhashed Referent)) (DiffOp (Synhashed TypeReference)) ->
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

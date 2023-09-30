-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (Lens', mapped, over, traverseOf, (.~), (^.), (^?), _1)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Writer.CPS (execWriter)
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Data.Text.Lazy.Builder.Int qualified as Text.Builder
import Data.These (These (..))
import Data.Tuple.Strict
import GHC.Clock (getMonotonicTime)
import System.Process qualified as Process
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch, CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Branch.Diff (DefinitionDiffs (DefinitionDiffs), Diff (..), TreeDiff (TreeDiff), pattern DiffIsAdd)
import U.Codebase.Branch.Diff qualified as Diff
import U.Codebase.Causal qualified as Causal
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference, Reference' (..), TermReference, TypeReference, TypeReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Queries qualified as Queries
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Term (Term)
import U.Codebase.Term qualified as Term
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Operations qualified as SqliteCodebase.Operations
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Core.ConstructorId (ConstructorId)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashQualified' qualified as HQ'
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.SyntacticHash qualified as SyntacticHash
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Term qualified as V1 (Term)
import Unison.Type qualified as V1 (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.List qualified as List
import Unison.Util.Monoid (foldMapM, intercalateMap)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Relation3 (Relation3)
import Unison.Util.Relation3 qualified as Relation3
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Witch (unsafeFrom)
import Witherable (catMaybes)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

handleMerge :: Path' -> Path' -> Path' -> Cli ()
handleMerge alicePath0 bobPath0 _resultPath = do
  -- FIXME we want to cache some of these, right?
  let mergeDatabase =
        Merge.Database
          { loadConstructorTypeSignature =
              -- FIXME would be smarter to fetch entire component, and cache them all
              \typeRef conId -> do
                decl <- Operations.expectDeclByReference typeRef
                pure (Decl.constructorType decl conId),
            loadConstructorType = SqliteCodebase.Operations.getDeclType,
            loadTerm = Operations.expectTermByReference,
            loadType = Operations.expectDeclByReference
          }

  alicePath <- Cli.resolvePath' alicePath0
  bobPath <- Cli.resolvePath' bobPath0

  Cli.runTransaction do
    aliceCausal <- step "load alice causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
    bobCausal <- step "load bob causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

    let aliceCausalHash = Causal.causalHash aliceCausal
    let bobCausalHash = Causal.causalHash bobCausal
    maybeLcaCausalHash <- step "compute lca" $ Operations.lca aliceCausalHash bobCausalHash

    -- Read the (shallow) branches out of the database.
    aliceBranch <- step "load shallow alice branch" $ Causal.value aliceCausal
    bobBranch <- step "load shallow bob branch" $ Causal.value bobCausal

    T3 aliceTypeNames0 aliceDataconNames aliceTermNames0 <- step "load alice names" $ loadBranchDefinitionNames aliceBranch
    T3 bobTypeNames0 bobDataconNames bobTermNames0 <- step "load bob names" $ loadBranchDefinitionNames bobBranch

    -- Assert that these branches don't have any conflicted names anywhere, as we'd rather like to not deal with some
    -- of the annoying complexity those cases bring, wrt. classifying things as conflicted adds/updates.

    aliceTypeNames :: BiMultimap TypeReference Name <-
      relationToLuniqRelation (Relation.swap aliceTypeNames0) & onLeft \names ->
        werror ("can't merge; conflicted type names in first namespace: " ++ show names)

    bobTypeNames :: BiMultimap TypeReference Name <-
      relationToLuniqRelation (Relation.swap bobTypeNames0) & onLeft \names ->
        werror ("can't merge; conflicted type names in second namespace: " ++ show names)

    aliceTermNames :: BiMultimap TermReference Name <-
      relationToLuniqRelation (Relation.swap aliceTermNames0) & onLeft \names ->
        werror ("can't merge; conflicted term names in first namespace: " ++ show names)

    bobTermNames :: BiMultimap TermReference Name <-
      relationToLuniqRelation (Relation.swap bobTermNames0) & onLeft \names ->
        werror ("can't merge; conflicted term names in second namespace: " ++ show names)

    let conflictedDataconNames :: Relation3 a b c -> Set a
        conflictedDataconNames =
          Relation3.d1 >>> Map.filter (\r -> Relation.size r > 1) >>> Map.keysSet

    let conflictedAliceDataconNames = conflictedDataconNames aliceDataconNames
    when (not (Set.null conflictedAliceDataconNames)) do
      werror ("can't merge; conflicted constructor names in first namespace: " ++ show conflictedAliceDataconNames)

    let conflictedBobDataconNames = conflictedDataconNames bobDataconNames
    when (not (Set.null conflictedBobDataconNames)) do
      werror ("can't merge; conflicted constructor names in second namespace: " ++ show conflictedBobDataconNames)

    case maybeLcaCausalHash of
      -- TODO: go down 2-way merge code paths
      Nothing -> pure ()
      Just lcaCausalHash -> do
        lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
        lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
        T3 lcaTypeNames lcaDataconNames lcaTermNames <- step "load lca names" $ loadBranchDefinitionNames lcaBranch

        -- Compute and load the (deep) definition diffs (everything but lib.*)
        aliceDefinitionsDiff <- step "load alice definitions diff" do
          aliceBranchDiff <- Diff.diffBranches lcaBranch aliceBranch
          loadDefinitionsDiff aliceBranchDiff
        bobDefinitionsDiff <- step "load bob definitions diff" do
          bobBranchDiff <- Diff.diffBranches lcaBranch bobBranch
          loadDefinitionsDiff bobBranchDiff

        -- Special diff for `lib`
        aliceDependenciesDiff <- step "load alice dependencies diff" $ loadDependenciesDiff lcaBranch aliceBranch
        bobDependenciesDiff <- step "load alice dependencies diff" $ loadDependenciesDiff lcaBranch bobBranch

        -- Collect all the conflicted adds and updates
        let T2 typeChanges termChanges = definitionsDiffsToChanges aliceDefinitionsDiff bobDefinitionsDiff
        let aliceTypeUpdates = typeChanges ^. #aliceUpdates
        let aliceTermUpdates = termChanges ^. #aliceUpdates
        let bobTypeUpdates = typeChanges ^. #bobUpdates
        let bobTermUpdates = termChanges ^. #bobUpdates
        let typeConflictedAdds = typeChanges ^. #conflictedAdds
        let termConflictedAdds = termChanges ^. #conflictedAdds
        let typeUpdates = aliceTypeUpdates <> bobTypeUpdates
        let termUpdates = aliceTermUpdates <> bobTermUpdates

        -- The canonicalizer is just an implementation detail of the isUser{Type,Term}Update classifiers
        let canonicalizer = Merge.makeCanonicalizer v2HashHandle mergeDatabase typeUpdates termUpdates

        -- Classify the type updates
        let isUserTypeUpdate dataconNames =
              Merge.isUserTypeUpdate
                mergeDatabase
                canonicalizer
                (\ref1 decl1 ref2 decl2 -> computeConstructorMapping lcaDataconNames ref1 decl1 dataconNames ref2 decl2)
        aliceTypeUserUpdates <- step "classify alice user type updates" $ Relation.filterM (isUserTypeUpdate aliceDataconNames) aliceTypeUpdates
        bobTypeUserUpdates <- step "classify bob user type updates" $ Relation.filterM (isUserTypeUpdate bobDataconNames) bobTypeUpdates
        let typeUserUpdates = aliceTypeUserUpdates <> bobTypeUserUpdates

        -- Classify the term updates
        let isUserTermUpdate = Merge.isUserTermUpdate mergeDatabase canonicalizer
        termUserUpdates <- step "classify user term updates" $ Relation.filterM isUserTermUpdate termUpdates

        let changes :: Merge.Changes TypeReference Referent
            changes =
              Merge.Changes
                { termConflictedAdds,
                  typeConflictedAdds,
                  termUpdates,
                  typeUpdates,
                  termUserUpdates,
                  typeUserUpdates
                }

        -- Build the core ecs from the changes
        let coreEcs = Merge.makeCoreEcs changes

        coreEcDependencies <- step "compute core EC dependencies" do
          Merge.makeCoreEcDependencies
            loadTypeConstructorTerms
            loadTypeDependencies
            loadTermDependencies
            changes
            coreEcs

        Sqlite.unsafeIO do
          Text.putStrLn ""
          Text.putStrLn "===== hashes ====="
          Text.putStrLn ("alice causal hash = " <> showCausalHash aliceCausalHash)
          Text.putStrLn ("alice namespace hash = " <> showNamespaceHash (Causal.valueHash aliceCausal))
          Text.putStrLn ("bob causal hash = " <> showCausalHash bobCausalHash)
          Text.putStrLn ("bob namespace hash = " <> showNamespaceHash (Causal.valueHash bobCausal))
          Text.putStrLn ("lca causal hash = " <> showCausalHash lcaCausalHash)
          Text.putStrLn ""
          Text.putStrLn "===== lca->alice diff ====="
          printDefinitionsDiff Nothing aliceDefinitionsDiff
          printDependenciesDiff aliceDependenciesDiff
          Text.putStrLn ""
          Text.putStrLn "===== lca->bob diff ====="
          printDefinitionsDiff Nothing bobDefinitionsDiff
          printDependenciesDiff bobDependenciesDiff
          Text.putStrLn ""
          Text.putStrLn "===== conflicted adds ====="
          printTypeConflictedAdds typeConflictedAdds
          printTermConflictedAdds termConflictedAdds
          Text.putStrLn ""
          Text.putStrLn ("===== updates (" <> Text.magenta "magenta" <> " means user-update) =====")
          printTypeUpdates typeUpdates typeUserUpdates
          printTermUpdates termUpdates termUserUpdates
          Text.putStrLn ""
          Text.putStrLn "===== core ecs ====="
          printEcs coreEcs
          Text.putStrLn ""
          Text.putStrLn "===== core ec dependencies ====="
          printEcDependencies coreEcDependencies

          Text.putStrLn ""

          Text.writeFile
            "ec-graph.dot"
            ( ecDependenciesToDot
                (luniqRelationToRelation aliceTypeNames <> luniqRelationToRelation bobTypeNames)
                (aliceDataconNames <> bobDataconNames)
                (luniqRelationToRelation aliceTermNames <> luniqRelationToRelation bobTermNames)
                (Relation.ran typeUserUpdates)
                (Relation.ran termUserUpdates)
                coreEcs
                coreEcDependencies
            )
          Process.callCommand "dot -Tpdf ec-graph.dot > ec-graph.pdf && open ec-graph.pdf && rm ec-graph.dot"

-- Given a type reference, load its constructors' referents.
--
-- For example,
--
--   loadTypeConstructorTerms #Maybe = [Con #Maybe 0, Con #Maybe 1]
loadTypeConstructorTerms :: TypeReference -> Transaction [Referent]
loadTypeConstructorTerms ref =
  case ref of
    ReferenceBuiltin _ -> pure []
    ReferenceDerived refId -> do
      decl <- Operations.expectDeclByReference refId
      pure (List.imap (\i _ -> Referent.Con ref (unsafeFrom @Int i)) (Decl.constructorTypes decl))

-- TODO we probably want to pass down a version of this that caches
-- FIXME use ConstructorReference when there's only one
loadTypeDependencies :: TypeReference -> Transaction (Set TypeReference)
loadTypeDependencies = \case
  ReferenceBuiltin _ -> pure Set.empty
  ReferenceDerived ref0 -> do
    ref1 <- traverseOf Reference.idH Queries.expectObjectIdForPrimaryHash ref0
    dependencies0 <- Queries.getDependenciesForDependent ref1
    dependencies1 <-
      traverse
        (bitraverse Queries.expectText Queries.expectPrimaryHashByObjectId)
        dependencies0
    pure (Set.fromList dependencies1)

-- TODO we probably want to pass down a version of this that caches
loadTermDependencies :: Referent -> Transaction (Set (Either TypeReference Referent))
loadTermDependencies = \case
  Referent.Ref (ReferenceBuiltin _) -> pure Set.empty
  Referent.Ref (ReferenceDerived ref) -> do
    term <- Operations.expectTermByReference ref
    pure (termDependencies term)
  Referent.Con typeRef _conId ->
    pure (Set.singleton (Left typeRef))

computeConstructorMapping ::
  Relation3 Name TypeReference ConstructorId ->
  TypeReferenceId ->
  Decl v ->
  Relation3 Name TypeReference ConstructorId ->
  TypeReferenceId ->
  Decl v ->
  Maybe ([a] -> [a])
computeConstructorMapping allNames1 ref1 decl1 allNames2 ref2 decl2 = do
  -- Basic checks: there's no constructor mapping if these are clearly different types
  guard (Decl.declType decl1 == Decl.declType decl2)
  guard (Decl.modifier decl1 == Decl.modifier decl2)
  guard (length (Decl.bound decl1) == length (Decl.bound decl2))
  let numConstructors = length (Decl.constructorTypes decl1)
  guard (numConstructors == length (Decl.constructorTypes decl2))

  let constructorNames1 = Relation3.lookupD2 (ReferenceDerived ref1) allNames1
  let constructorNames2 = Relation3.lookupD2 (ReferenceDerived ref2) allNames2

  let constructorName1 :: ConstructorId -> Maybe Name
      constructorName1 i =
        Set.asSingleton (Relation.lookupRan i constructorNames1)

  let constructorId2 :: Name -> Maybe ConstructorId
      constructorId2 name =
        Set.asSingleton (Relation.lookupDom name constructorNames2)

  let constructorIdsInOrder :: [ConstructorId]
      constructorIdsInOrder =
        map (unsafeFrom @Int) [0 .. numConstructors - 1]

  let step :: Maybe (Map ConstructorId ConstructorId) -> ConstructorId -> Maybe (Map ConstructorId ConstructorId)
      step maybeAcc i = do
        acc <- maybeAcc
        name <- constructorName1 i
        j <- constructorId2 name
        Just (Map.insert j i acc)

  -- It all looks good so far; let's see if the data constructors' names match.
  --
  -- For simplicity, though there is probably a slightly more correct way to implement this, we only care to find
  -- constructor mappings for types whose old and new constructors all have *exactly one* name in their respective
  -- namespaces. If any constructor has either 0 or more than 1 names, we conservatively conclude that there is no
  -- constructor mapping.
  mapping <- foldl' step (Just Map.empty) constructorIdsInOrder
  -- Sanity check that may not be totally necessary because names or something: this bimap should be injective.
  -- Without this check, we only know that it's a function.
  guard (Map.keys mapping == constructorIdsInOrder)

  let reorder constructors =
        constructors
          -- Pair each new constructor with its constructor id
          & zip constructorIdsInOrder
          -- Replace each new constructor id with the corresponding old constructor id
          & over (mapped . _1) (mapping Map.!)
          -- Sort by the old constructor ids and return the new constructors in that order
          & sortOn fst
          & map snd

  Just reorder

-- lol bad name
-- where/when do we care about normal unconflicted adds? may make sense to include them here too?
data TwoSetsOfChanges ref = TwoSetsOfChanges
  { -- All of the (alice, bob) conflicts due to the same name being used for two different refs.
    -- TODO make this a Set (ref, ref); no need for the indexing of a Relation
    -- Invariant: irreflexive (i.e. no x is related to itself)
    conflictedAdds :: !(Relation ref ref),
    -- All of the (old, new) updates from each set of changes.
    -- Invariant: irreflexive (i.e. no x is related to itself)
    aliceUpdates :: !(Relation ref ref),
    bobUpdates :: !(Relation ref ref)
  }
  deriving stock (Generic)

instance Ord ref => Monoid (TwoSetsOfChanges ref) where
  mempty = TwoSetsOfChanges Relation.empty Relation.empty Relation.empty

instance Ord ref => Semigroup (TwoSetsOfChanges ref) where
  TwoSetsOfChanges a0 b0 c0 <> TwoSetsOfChanges a1 b1 c1 =
    TwoSetsOfChanges (a0 <> a1) (b0 <> b1) (c0 <> c1)

makeTwoSetsOfChanges :: Ord ref => These (Diff ref) (Diff ref) -> TwoSetsOfChanges ref
makeTwoSetsOfChanges = \case
  This aliceDiff ->
    TwoSetsOfChanges
      { conflictedAdds = Relation.empty,
        aliceUpdates = diffToUpdates aliceDiff,
        bobUpdates = Relation.empty
      }
  That bobDiff ->
    TwoSetsOfChanges
      { conflictedAdds = Relation.empty,
        aliceUpdates = Relation.empty,
        bobUpdates = diffToUpdates bobDiff
      }
  These aliceDiff bobDiff ->
    -- Two cases to handle:
    case (aliceDiff, bobDiff) of
      -- 1. Neither Alice nor Bob have any deletions on this name. That means both Alice and Bob have added
      --    a new definition; it's conflicted if the refs are different.
      (DiffIsAdd aliceRef, DiffIsAdd bobRef) ->
        TwoSetsOfChanges
          { conflictedAdds =
              if aliceRef == bobRef
                then Relation.empty
                else Relation.singleton aliceRef bobRef,
            aliceUpdates = Relation.empty,
            bobUpdates = Relation.empty
          }
      -- 2. Alice inclusive-or bob have a deletion on this name. That means this definitely *isn't* a
      --    "conflicted add", as there was at least one reference associated with this name in the LCA.
      _ ->
        TwoSetsOfChanges
          { conflictedAdds = Relation.empty,
            aliceUpdates = diffToUpdates aliceDiff,
            bobUpdates = diffToUpdates bobDiff
          }

definitionsDiffsToChanges ::
  Cofree (Map NameSegment) DefinitionDiffs ->
  Cofree (Map NameSegment) DefinitionDiffs ->
  (T2 (TwoSetsOfChanges Reference) (TwoSetsOfChanges Referent))
definitionsDiffsToChanges aliceDiffs bobDiffs =
  T2 typeChanges termChanges <> childrenChanges
  where
    DefinitionDiffs aliceTermDiffs aliceTypeDiffs :< aliceChildren = aliceDiffs
    DefinitionDiffs bobTermDiffs bobTypeDiffs :< bobChildren = bobDiffs

    typeChanges :: TwoSetsOfChanges Reference
    typeChanges =
      fold (alignWith makeTwoSetsOfChanges aliceTypeDiffs bobTypeDiffs)

    termChanges :: TwoSetsOfChanges Referent
    termChanges =
      fold (alignWith makeTwoSetsOfChanges aliceTermDiffs bobTermDiffs)

    childrenChanges :: T2 (TwoSetsOfChanges Reference) (TwoSetsOfChanges Referent)
    childrenChanges =
      fold (alignWith f aliceChildren bobChildren)
      where
        f ::
          These (Cofree (Map NameSegment) DefinitionDiffs) (Cofree (Map NameSegment) DefinitionDiffs) ->
          T2 (TwoSetsOfChanges Reference) (TwoSetsOfChanges Referent)
        f = \case
          This aliceDiff -> oneSided #aliceUpdates aliceDiff
          That bobDiff -> oneSided #bobUpdates bobDiff
          These aliceDiff bobDiff -> definitionsDiffsToChanges aliceDiff bobDiff
          where
            oneSided ::
              (forall ref. Lens' (TwoSetsOfChanges ref) (Relation ref ref)) ->
              Cofree (Map NameSegment) DefinitionDiffs ->
              T2 (TwoSetsOfChanges Reference) (TwoSetsOfChanges Referent)
            oneSided updatesLens diff =
              T2 (mempty & updatesLens .~ typeUpdates) (mempty & updatesLens .~ termUpdates)
              where
                T2 typeUpdates termUpdates = definitionsDiffToUpdates diff

definitionsDiffToUpdates ::
  Cofree (Map NameSegment) DefinitionDiffs ->
  T2 (Relation Reference Reference) (Relation Referent Referent)
definitionsDiffToUpdates (DefinitionDiffs {termDiffs, typeDiffs} :< children) =
  T2 (foldMap diffToUpdates typeDiffs) (foldMap diffToUpdates termDiffs) <> foldMap definitionsDiffToUpdates children

diffToUpdates :: Ord ref => Diff ref -> Relation ref ref
diffToUpdates Diff {adds, removals} =
  -- Per the invariant that neither Alice nor Bob's namespace may contain a conflicted name, we know that
  -- `length adds` is at most 1.
  case Set.lookupMin adds of
    Nothing -> Relation.empty
    Just ref -> Relation.swap (Relation.fromMultimap (Map.singleton ref removals))

-- | Load all term and type names from a branch (excluding dependencies) into memory.
loadBranchDefinitionNames ::
  forall m.
  Monad m =>
  Branch m ->
  m
    ( T3
        (Relation Name TypeReference)
        (Relation3 Name TypeReference ConstructorId)
        (Relation Name TermReference)
    )
loadBranchDefinitionNames =
  go []
  where
    go ::
      [NameSegment] ->
      Branch m ->
      m
        ( T3
            (Relation Name TypeReference)
            (Relation3 Name TypeReference ConstructorId)
            (Relation Name TermReference)
        )
    go reversePrefix branch = do
      let types = branchTypeNames reversePrefix branch
          T2 datacons terms = branchTermNames reversePrefix branch

      childrenNames <-
        Branch.children branch
          & Map.toList
          & foldMapM \(childName, childCausal) -> do
            childBranch <- Causal.value childCausal
            go (childName : reversePrefix) childBranch

      pure (T3 types datacons terms <> childrenNames)

    branchTypeNames :: [NameSegment] -> Branch m -> Relation Name TypeReference
    branchTypeNames reversePrefix =
      Branch.types >>> Map.toList >>> map f >>> Map.fromList >>> Relation.fromMultimap
      where
        f (segment, xs) =
          (Name.fromReverseSegments (segment :| reversePrefix), Map.keysSet xs)

    branchTermNames :: [NameSegment] -> Branch m -> T2 (Relation3 Name TypeReference ConstructorId) (Relation Name TermReference)
    branchTermNames reversePrefix =
      Branch.terms >>> Map.toList >>> foldl' f (T2 Relation3.empty Relation.empty)
      where
        f ::
          T2 (Relation3 Name TypeReference ConstructorId) (Relation Name TermReference) ->
          (NameSegment, Map Referent metadata) ->
          T2 (Relation3 Name TypeReference ConstructorId) (Relation Name TermReference)
        f acc (segment, refs) =
          foldl' (g (Name.fromReverseSegments (segment :| reversePrefix))) acc (Map.keys refs)

        g ::
          Name ->
          T2 (Relation3 Name TypeReference ConstructorId) (Relation Name TermReference) ->
          Referent ->
          T2 (Relation3 Name TypeReference ConstructorId) (Relation Name TermReference)
        g name (T2 accDatacons accTerms) = \case
          Referent.Ref ref -> T2 accDatacons (Relation.insert name ref accTerms)
          Referent.Con ref cid -> T2 (Relation3.insert name ref cid accDatacons) accTerms

data DependencyDiff
  = AddDependency !CausalHash
  | DeleteDependency !CausalHash
  | UpdateDependency !CausalHash !CausalHash

-- | Diff the "dependencies" ("lib" sub-namespace) of two namespaces.
loadDependenciesDiff :: Branch Transaction -> Branch Transaction -> Transaction (Map NameSegment DependencyDiff)
loadDependenciesDiff branch1 branch2 = do
  dependencies1 <- namespaceDependencies branch1
  dependencies2 <- namespaceDependencies branch2
  pure (catMaybes (alignWith f dependencies1 dependencies2))
  where
    f = \case
      This dep1 -> Just (DeleteDependency (Causal.causalHash dep1))
      That dep2 -> Just (AddDependency (Causal.causalHash dep2))
      These (Causal.causalHash -> dep1) (Causal.causalHash -> dep2) ->
        if dep1 == dep2
          then Nothing
          else Just (UpdateDependency dep1 dep2)

-- | Extract just the "dependencies" (sub-namespaces of "lib") of a branch.
namespaceDependencies :: Branch Transaction -> Transaction (Map NameSegment (CausalBranch Transaction))
namespaceDependencies branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Map.empty
    Just dependenciesCausal -> Branch.children <$> Causal.value dependenciesCausal

loadDefinitionsDiff :: TreeDiff Transaction -> Transaction (Cofree (Map NameSegment) DefinitionDiffs)
loadDefinitionsDiff (TreeDiff (diff :< Compose children0)) = do
  children <-
    children0 & Map.traverseWithKey \name loadChildDiff -> do
      childDiff :< grandchildren0 <- loadChildDiff
      -- Weird stuff: we don't want to consider lib.* (e.g. lib.base.*, lib.http.*) as part of this "definitions diff",
      -- because lib is supposed to contain only dependencies. However, it is of course possible that there is some
      -- non-namespace type-or-term in lib, such as lib.SomeType. We've decided (for now) we do indeed want to treat
      -- these like ordinary definitions defined in the non-lib part of the namespace. Therefore, we load the child
      -- diff regardless of its name, but if its name is "lib", then we throw away its grandchildren (e.g. lib.base,
      -- lib.http, etc), leaving behind only the weirdo lib.SomeType things (which we hope is empty).
      let grandchildren =
            if name == Name.libSegment
              then Compose Map.empty
              else grandchildren0
      hoistM getCompose (childDiff :< grandchildren)
  pure (diff :< children)

hoistM :: (Traversable g, Monad m) => (forall x. f x -> g (m x)) -> Cofree f a -> m (Cofree g a)
hoistM f (x :< xs) =
  (x :<) <$> traverse (>>= hoistM f) (f xs)

-----------------------------------------------------------------------------------------------------------------------
-- Term dependencies

termDependencies :: Term Symbol -> Set (Either TypeReference Referent)
termDependencies =
  execWriter . ABT.visit_ (Writer.tell . termFDependencies)

termFDependencies :: Term.F Symbol term -> Set (Either TypeReference Referent)
termFDependencies = \case
  Term.Ann _term ty -> Set.map Left (Type.dependencies ty)
  Term.Constructor typeRef conId -> Set.singleton (Right (Referent.Con typeRef conId))
  Term.Match _term cases -> Set.map Left (foldMap termMatchCaseDependencies cases)
  Term.Ref rref ->
    case rref ^? Reference._RReferenceReference of
      Nothing -> Set.empty
      Just ref -> Set.singleton (Right (Referent.Ref ref))
  Term.Request typeRef conId -> Set.singleton (Right (Referent.Con typeRef conId))
  Term.TermLink rref ->
    case rref ^? Referent._ReferentHReferent of
      Nothing -> Set.empty
      Just ref -> Set.singleton (Right ref)
  Term.TypeLink typeRef -> Set.singleton (Left typeRef)
  -- No top-level dependencies: these either have inner terms (like App), or no dependencies (like Nat)
  Term.And {} -> Set.empty
  Term.App {} -> Set.empty
  Term.Boolean {} -> Set.empty
  Term.Char {} -> Set.empty
  Term.Float {} -> Set.empty
  Term.Handle {} -> Set.empty
  Term.If {} -> Set.empty
  Term.Int {} -> Set.empty
  Term.Lam {} -> Set.empty
  Term.Let {} -> Set.empty
  Term.LetRec {} -> Set.empty
  Term.List {} -> Set.empty
  Term.Nat {} -> Set.empty
  Term.Or {} -> Set.empty
  Term.Text {} -> Set.empty

termMatchCaseDependencies :: Term.MatchCase Text TypeReference term -> Set TypeReference
termMatchCaseDependencies (Term.MatchCase pat _guard _body) =
  termPatternDependencies pat

termPatternDependencies :: Term.Pattern Text TypeReference -> Set TypeReference
termPatternDependencies = \case
  Term.PAs pat -> termPatternDependencies pat
  Term.PConstructor typeRef _conId fields -> Set.insert typeRef (foldMap termPatternDependencies fields)
  Term.PEffectBind typeRef _conId fields k -> Set.insert typeRef (foldMap termPatternDependencies (k : fields))
  Term.PEffectPure pat -> termPatternDependencies pat
  Term.PSequenceLiteral pats -> foldMap termPatternDependencies pats
  Term.PSequenceOp lpat _op rpat -> Set.union (termPatternDependencies lpat) (termPatternDependencies rpat)
  --
  Term.PBoolean {} -> Set.empty
  Term.PChar {} -> Set.empty
  Term.PFloat {} -> Set.empty
  Term.PInt {} -> Set.empty
  Term.PNat {} -> Set.empty
  Term.PText {} -> Set.empty
  Term.PUnbound {} -> Set.empty
  Term.PVar {} -> Set.empty

-- | Try to view a relation as a left-unique relation.
--
-- If the relation is not left-unique, returns the set of elements of the range that were each related to more than
-- element in the domain.
--
-- TODO move this helper to some other module
relationToLuniqRelation :: forall a b. (Ord a, Ord b) => Relation a b -> Either (Set b) (BiMultimap a b)
relationToLuniqRelation relation =
  domain
    & Map.toList
    & foldr f (Just (T2 BiMultimap.empty Set.empty))
    & \case
      Nothing -> Left (duplicates (Map.elems domain))
      Just (T2 rel _range) -> Right rel
  where
    domain :: Map a (Set b)
    domain =
      Relation.domain relation

    -- Accumulator: the left-unique relation and its range. Its range is kept separately because it can't be derived
    -- from the relation in O(1)
    f ::
      (a, Set b) ->
      Maybe (T2 (BiMultimap a b) (Set b)) ->
      Maybe (T2 (BiMultimap a b) (Set b))
    f (x, ys) = \case
      Nothing -> Nothing
      Just (T2 rel0 range0) ->
        if Set.disjoint ys range0
          then
            let rel1 = foldl' (\rel1 name -> BiMultimap.unsafeInsert x name rel1) rel0 ys
                range1 = Set.union ys range0
             in Just $! T2 rel1 range1
          else Nothing

-- | View a left-unique relation as a relation.
--
-- TODO move this helper to some other module
luniqRelationToRelation :: forall a b. (Ord a, Ord b) => BiMultimap a b -> Relation a b
luniqRelationToRelation =
  Relation.fromMultimap . Map.map Set.NonEmpty.toSet . BiMultimap.domain

-- | Return the set of elements that appear in at least two of the given sets.
duplicates :: forall a. Ord a => [Set a] -> Set a
duplicates =
  foldl' f (T2 Set.empty Set.empty) >>> (\(T2 _ xs) -> xs)
  where
    f :: T2 (Set a) (Set a) -> Set a -> T2 (Set a) (Set a)
    f (T2 everything dupes) xs =
      T2 (Set.union xs everything) (Set.union (Set.intersection xs everything) dupes)

-----------------------------------------------------------------------------------------------------------------------
-- Debug show/print utils

showCausalHash :: CausalHash -> Text
showCausalHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unCausalHash

showNamespaceHash :: BranchHash -> Text
showNamespaceHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unBranchHash

showReference :: Reference -> Text
showReference =
  showShortHash . Reference.toShortHash

showReferent :: Referent -> Text
showReferent =
  showShortHash . Referent.toShortHash

showShortHash :: ShortHash -> Text
showShortHash =
  ShortHash.toText . ShortHash.shortenTo 4

ecDependenciesToDot ::
  Relation TypeReference Name ->
  Relation3 Name TypeReference ConstructorId ->
  Relation TermReference Name ->
  Set TypeReference ->
  Set Referent ->
  Bimap Merge.EC (Merge.Node Referent TypeReference) ->
  Relation Merge.EC Merge.EC ->
  Text
ecDependenciesToDot typeNames constructorNames termNames typeUserUpdates termUserUpdates coreEcs coreEcDependencies =
  Text.Lazy.toStrict . Text.Builder.toLazyText $
    fold
      [ "digraph {",
        newline,
        "edge [arrowhead = vee; arrowsize = 0.5]",
        newline,
        coreEcs
          & Bimap.toList
          & intercalateMap
            newline
            ( \(Merge.EC ec, node) ->
                fold
                  [ "  ",
                    Text.Builder.decimal ec,
                    " [color = ",
                    case node of
                      Merge.Node'Term _ -> "blue"
                      Merge.Node'Terms _ -> "blue"
                      Merge.Node'Type _ -> "green"
                      Merge.Node'Types _ -> "green",
                    "; label = <",
                    nodeToTable node,
                    ">]"
                  ]
            ),
        newline,
        coreEcDependencies
          & Relation.toList
          & intercalateMap
            newline
            ( \(Merge.EC ec0, Merge.EC ec1) ->
                fold
                  [ "  ",
                    Text.Builder.decimal ec0,
                    " -> ",
                    Text.Builder.decimal ec1
                  ]
            ),
        newline,
        "}"
      ]
  where
    nodeToTable :: Merge.Node Referent TypeReference -> Text.Builder
    nodeToTable node =
      "<table>" <> nodeToRows node <> "</table>"

    nodeToRows :: Merge.Node Referent TypeReference -> Text.Builder
    nodeToRows = \case
      Merge.NodeTms tms ->
        tms & foldMap \ref ->
          let name =
                fromMaybe "" . Set.lookupMin $
                  case ref of
                    Referent.Con typeRef conId -> Relation3.lookupD23 typeRef conId constructorNames
                    Referent.Ref termRef -> Relation.lookupDom termRef termNames
           in refToRow (Name.toText name <> showReferent ref) (Set.member ref termUserUpdates)
      Merge.NodeTys tys ->
        tys & foldMap \ref ->
          let name = fromMaybe "" (Set.lookupMin (Relation.lookupDom ref typeNames))
           in refToRow (Name.toText name <> showReference ref) (Set.member ref typeUserUpdates)

    refToRow :: Text -> Bool -> Text.Builder
    refToRow ref isUserUpdate =
      fold
        [ "<tr><td>",
          if isUserUpdate then "<b>" else "",
          Text.Builder.fromText ref,
          if isUserUpdate then "</b>" else "",
          "</td></tr>"
        ]

    newline :: Text.Builder
    newline = "\n"

printDiff :: Maybe Name -> DefinitionDiffs -> IO ()
printDiff prefix DefinitionDiffs {termDiffs, typeDiffs} = do
  for_ (Map.toList termDiffs) \(segment, Diff.Diff {adds, removals}) -> do
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.snoc prefix1 segment
    Text.putStrLn $
      "term "
        <> Name.toText name
        <> " "
        <> Text.unwords
          ( map (Text.red . ("-" <>) . showReferent) (Set.toList removals)
              ++ map (Text.green . ("+" <>) . showReferent) (Set.toList adds)
          )
  for_ (Map.toList typeDiffs) \(segment, Diff.Diff {adds, removals}) -> do
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.snoc prefix1 segment
    Text.putStrLn $
      "type "
        <> Name.toText name
        <> " "
        <> Text.unwords
          ( map (Text.red . ("-" <>) . showReference) (Set.toList removals)
              ++ map (Text.green . ("+" <>) . showReference) (Set.toList adds)
          )

printDefinitionsDiff :: Maybe Name -> Cofree (Map NameSegment) DefinitionDiffs -> IO ()
printDefinitionsDiff prefix (diff :< children) = do
  printDiff prefix diff
  for_ (Map.toList children) \(segment, child) ->
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
     in printDefinitionsDiff (Just name) child

printDependenciesDiff :: Map NameSegment DependencyDiff -> IO ()
printDependenciesDiff =
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f (name, diff) =
      let prefix = "dep " <> NameSegment.toText name <> " "
       in case diff of
            AddDependency hash -> prefix <> plus hash
            DeleteDependency hash -> prefix <> minus hash
            UpdateDependency oldHash newHash -> prefix <> minus oldHash <> " " <> plus newHash

    plus = Text.green . ("+" <>) . showCausalHash
    minus = Text.red . ("-" <>) . showCausalHash

printEcs :: Bimap Merge.EC (Merge.Node Referent TypeReference) -> IO ()
printEcs =
  Text.putStr . Text.unlines . map (uncurry f) . Bimap.toList
  where
    f (Merge.EC ec) node =
      tShow ec <> ": " <> case node of
        Merge.NodeTms tms -> "terms " <> Text.unwords (map showReferent (Set.toList tms))
        Merge.NodeTys tys -> "types " <> Text.unwords (map showReference (Set.toList tys))

printEcDependencies :: Relation Merge.EC Merge.EC -> IO ()
printEcDependencies =
  Text.putStr . Text.unlines . map (uncurry f) . Relation.toList
  where
    f (Merge.EC ec0) (Merge.EC ec1) =
      tShow ec0 <> " -> " <> tShow ec1

printTypeConflictedAdds :: Relation Reference Reference -> IO ()
printTypeConflictedAdds =
  Text.putStr . Text.unlines . map f . Relation.toList
  where
    f (alice, bob) =
      Text.magenta (showReference alice <> " " <> showReference bob)

printTermConflictedAdds :: Relation Referent Referent -> IO ()
printTermConflictedAdds =
  Text.putStr . Text.unlines . map f . Relation.toList
  where
    f (alice, bob) =
      Text.magenta (showReferent alice <> " " <> showReferent bob)

printTypeUpdates :: Relation Reference Reference -> Relation Reference Reference -> IO ()
printTypeUpdates allUpdates userUpdates =
  Text.putStr (Text.unlines (map f (Relation.toList allUpdates)))
  where
    f (old, new) =
      (if Relation.member old new userUpdates then Text.magenta else id) $
        "type "
          <> showReference old
          <> " => "
          <> showReference new

printTermUpdates :: Relation Referent Referent -> Relation Referent Referent -> IO ()
printTermUpdates allUpdates userUpdates =
  Text.putStr (Text.unlines (map f (Relation.toList allUpdates)))
  where
    f (old, new) =
      (if Relation.member old new userUpdates then Text.magenta else id) $
        "term " <> showReferent old <> " => " <> showReferent new

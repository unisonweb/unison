-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (Lens', mapped, over, traverseOf, (.~), (^.), (^?), _1)
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
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import GHC.Clock (getMonotonicTime)
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
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Hash qualified as Hash
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Relation3 (Relation3)
import Unison.Util.Relation3 qualified as Relation3
import Unison.Util.Set qualified as Set
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

    -- Read the (shallow) branches out of the database
    aliceBranch <- step "load shallow alice branch" $ Causal.value aliceCausal
    bobBranch <- step "load shallow bob branch" $ Causal.value bobCausal

    -- TODO assert somehow that these branches don't have any conflicted names anywhere, as we'd rather like to not
    -- deal with some of the annoying complexity those cases bring, wrt. classifying things as conflicted adds/updates.

    (aliceTypeNames, aliceDataconNames, aliceTermNames) <- step "load alice names" $ loadBranchDefinitionNames aliceBranch
    (bobTypeNames, bobDataconNames, bobTermNames) <- step "load bob names" $ loadBranchDefinitionNames bobBranch

    case maybeLcaCausalHash of
      -- TODO: go down 2-way merge code paths
      Nothing -> pure ()
      Just lcaCausalHash -> do
        lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
        lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
        (lcaTypeNames, lcaDataconNames, lcaTermNames) <- step "load lca names" $ loadBranchDefinitionNames lcaBranch

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
        let (typeChanges, termChanges) = definitionsDiffsToChanges aliceDefinitionsDiff bobDefinitionsDiff
        let aliceTypeUpdates = typeChanges ^. #aliceUpdates
        let aliceTermUpdates = termChanges ^. #aliceUpdates
        let bobTypeUpdates = typeChanges ^. #bobUpdates
        let bobTermUpdates = termChanges ^. #bobUpdates

        -- FIXME actually use these somewhere around here
        let typeConflictedAdds = typeChanges ^. #conflictedAdds
        let termConflictedAdds = termChanges ^. #conflictedAdds

        let typeUpdates = aliceTypeUpdates <> bobTypeUpdates
        let termUpdates = aliceTermUpdates <> bobTermUpdates

        -- The canonicalizer is just an implementation detail of the isUser{Type,Term}Update classifiers
        let canonicalizer = Merge.makeCanonicalizer v2HashHandle mergeDatabase typeUpdates termUpdates

        -- Classify the updates
        aliceUserTypeUpdates <- step "classify alice user type updates" do
          let isUserTypeUpdate =
                Merge.isUserTypeUpdate
                  mergeDatabase
                  canonicalizer
                  (\ref1 decl1 ref2 decl2 -> computeConstructorMapping lcaDataconNames ref1 decl1 aliceDataconNames ref2 decl2)
          Relation.filterM isUserTypeUpdate aliceTypeUpdates
        bobUserTypeUpdates <- step "classify bob user type updates" do
          let isUserTypeUpdate =
                Merge.isUserTypeUpdate
                  mergeDatabase
                  canonicalizer
                  (\ref1 decl1 ref2 decl2 -> computeConstructorMapping lcaDataconNames ref1 decl1 bobDataconNames ref2 decl2)
          Relation.filterM isUserTypeUpdate bobTypeUpdates
        let userTypeUpdates = aliceUserTypeUpdates <> bobUserTypeUpdates

        userTermUpdates <- step "classify user term updates" do
          let isUserTermUpdate = Merge.isUserTermUpdate mergeDatabase canonicalizer
          Relation.filterM isUserTermUpdate termUpdates

        let updates :: Merge.Updates TypeReference Referent
            updates =
              Merge.Updates
                { terms = termUpdates,
                  types = typeUpdates,
                  userTerms = userTermUpdates,
                  userTypes = userTypeUpdates
                }

        -- Build the core ecs from the updates
        let coreEcs = Merge.makeCoreEcs updates

        let getTypeConstructorTerms :: TypeReference -> Transaction [Referent]
            getTypeConstructorTerms ref =
              case ref of
                ReferenceBuiltin _ -> pure []
                ReferenceDerived refId -> do
                  cycleLen <- Operations.expectCycleLen (refId ^. Reference.idH)
                  pure (map (Referent.Con ref) [0 .. cycleLen - 1])

        -- TODO we probably want to pass down a version of this that caches
        -- FIXME use ConstructorReference when there's only one
        let typeDependsOn :: TypeReference -> Transaction (Set TypeReference)
            typeDependsOn = \case
              ReferenceBuiltin _ -> pure Set.empty
              ReferenceDerived typeRefId0 -> do
                typeRefId1 <- traverseOf Reference.idH Queries.expectObjectIdForPrimaryHash typeRefId0
                dependencies0 <- Queries.getDependenciesForDependent typeRefId1
                dependencies1 <-
                  traverse
                    (bitraverse Queries.expectText Queries.expectPrimaryHashByObjectId)
                    dependencies0
                pure (Set.fromList dependencies1)

        -- TODO we probably want to pass down a version of this that caches
        let termDependsOn :: Referent -> Transaction (Set (Either TypeReference Referent))
            termDependsOn = \case
              Referent.Ref (ReferenceBuiltin _) -> pure Set.empty
              Referent.Ref (ReferenceDerived termRef0) -> do
                term <- Operations.expectTermByReference termRef0
                pure (termDependencies term)
              Referent.Con typeRef _conId ->
                pure (Set.singleton (Left typeRef))

        coreEcDependencies <- step "compute core EC dependencies" do
          Merge.makeCoreEcDependencies
            getTypeConstructorTerms
            typeDependsOn
            termDependsOn
            updates
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
          Text.putStrLn "===== updates ====="
          printTypeUpdates typeUpdates userTypeUpdates
          printTermUpdates termUpdates userTermUpdates
          Text.putStrLn ""
          Text.putStrLn "===== core ecs ====="
          printEcs coreEcs

          Text.putStrLn ""

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

  let oink1 = Relation3.lookupD2 (ReferenceDerived ref1) allNames1
  let oink2 = Relation3.lookupD2 (ReferenceDerived ref2) allNames2

  let constructorIdsInOrder = map (unsafeFrom @Int) [0 .. numConstructors - 1]

  let step :: Maybe (Map ConstructorId ConstructorId) -> ConstructorId -> Maybe (Map ConstructorId ConstructorId)
      step maybeAcc i = do
        acc <- maybeAcc
        name <- Set.asSingleton (Relation.lookupRan i oink1)
        j <- Set.asSingleton (Relation.lookupDom name oink2)
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
  mappend = (<>)

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
  ( TwoSetsOfChanges Reference,
    TwoSetsOfChanges Referent
  )
definitionsDiffsToChanges
  (DefinitionDiffs aliceTermDiffs aliceTypeDiffs :< aliceChildren)
  (DefinitionDiffs bobTermDiffs bobTypeDiffs :< bobChildren) =
    (typeChanges, termChanges) <> childrenChanges
    where
      typeChanges :: TwoSetsOfChanges Reference
      typeChanges =
        fold (alignWith makeTwoSetsOfChanges aliceTypeDiffs bobTypeDiffs)

      termChanges :: TwoSetsOfChanges Referent
      termChanges =
        fold (alignWith makeTwoSetsOfChanges aliceTermDiffs bobTermDiffs)

      childrenChanges :: (TwoSetsOfChanges Reference, TwoSetsOfChanges Referent)
      childrenChanges =
        fold (alignWith f aliceChildren bobChildren)
        where
          f ::
            These (Cofree (Map NameSegment) DefinitionDiffs) (Cofree (Map NameSegment) DefinitionDiffs) ->
            (TwoSetsOfChanges Reference, TwoSetsOfChanges Referent)
          f = \case
            This aliceDiff -> oneSided #aliceUpdates aliceDiff
            That bobDiff -> oneSided #bobUpdates bobDiff
            These aliceDiff bobDiff -> definitionsDiffsToChanges aliceDiff bobDiff
            where
              oneSided ::
                (forall ref. Lens' (TwoSetsOfChanges ref) (Relation ref ref)) ->
                Cofree (Map NameSegment) DefinitionDiffs ->
                (TwoSetsOfChanges Reference, TwoSetsOfChanges Referent)
              oneSided updatesLens diff =
                let (typeUpdates, termUpdates) = definitionsDiffToUpdates diff
                 in ( mempty & updatesLens .~ typeUpdates,
                      mempty & updatesLens .~ termUpdates
                    )

definitionsDiffToUpdates ::
  Cofree (Map NameSegment) DefinitionDiffs ->
  (Relation Reference Reference, Relation Referent Referent)
definitionsDiffToUpdates (DefinitionDiffs {termDiffs, typeDiffs} :< children) =
  (typeUpdates, termUpdates) <> childrenUpdates
  where
    typeUpdates = diffsToUpdates typeDiffs
    termUpdates = diffsToUpdates termDiffs
    childrenUpdates = foldMap definitionsDiffToUpdates children

diffsToUpdates :: Ord ref => Map NameSegment (Diff ref) -> Relation ref ref
diffsToUpdates =
  foldMap diffToUpdates

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
    ( Relation Name TypeReference,
      Relation3 Name TypeReference ConstructorId,
      Relation Name TermReference
    )
loadBranchDefinitionNames =
  go []
  where
    go ::
      [NameSegment] ->
      Branch m ->
      m
        ( Relation Name TypeReference,
          Relation3 Name TypeReference ConstructorId,
          Relation Name TermReference
        )
    go reversePrefix branch = do
      let types :: Relation Name TypeReference
          types =
            Relation.fromMultimap (Map.fromList (map f (Map.toList (Branch.types branch))))
            where
              f (segment, xs) =
                (Name.fromReverseSegments (segment :| reversePrefix), Map.keysSet xs)

      let datacons :: Relation3 Name TypeReference ConstructorId
          terms :: Relation Name TermReference
          (datacons, terms) =
            Branch.terms branch
              & Map.toList
              & foldl' f (Relation3.empty, Relation.empty)
            where
              f ::
                (Relation3 Name TypeReference ConstructorId, Relation Name TermReference) ->
                (NameSegment, Map Referent metadata) ->
                (Relation3 Name TypeReference ConstructorId, Relation Name TermReference)
              f acc (!segment, !refs) =
                foldl' (g (Name.fromReverseSegments (segment :| reversePrefix))) acc (Map.keys refs)

              g ::
                Name ->
                (Relation3 Name TypeReference ConstructorId, Relation Name TermReference) ->
                Referent ->
                (Relation3 Name TypeReference ConstructorId, Relation Name TermReference)
              g name (!accDatacons, !accTerms) = \case
                Referent.Ref ref -> (accDatacons, Relation.insert name ref accTerms)
                Referent.Con ref cid -> (Relation3.insert name ref cid accDatacons, accTerms)

      (childrenTypes, childrenDatacons, childrenTerms) <-
        Branch.children branch
          & Map.toList
          & foldMapM \(childName, childCausal) -> do
            childBranch <- Causal.value childCausal
            go (childName : reversePrefix) childBranch

      let !allTypes = types <> childrenTypes
      let !allDatacons = datacons <> childrenDatacons
      let !allTerms = terms <> childrenTerms
      pure (allTypes, allDatacons, allTerms)

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
      "(" <> tShow ec <> ") " <> case node of
        Merge.NodeTms tms -> "{" <> Text.intercalate ", " (map showReferent (Set.toList tms)) <> "}"
        Merge.NodeTys tys -> "{" <> Text.intercalate ", " (map showReference (Set.toList tys)) <> "}"

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

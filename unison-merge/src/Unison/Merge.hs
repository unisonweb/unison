{-# LANGUAGE RecordWildCards #-}

module Unison.Merge
  ( Database (..),
    TypeBloboid (..),
    makeTypeBloboid,
    TermBloboid (..),
    makeTermBloboid,
    computeTypeUserUpdates,
    computeTermUserUpdates,
  )
where

import Control.Lens ((%~), (^.))
import Control.Lens qualified as Lens
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Bit (Bit (Bit, unBit))
import Data.Generics.Labels ()
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Vector.Unboxed qualified as UVector
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Reference (Reference' (..), TermRReference, TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle (HashHandle)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Term (ResolvedTerm, Term)
import U.Codebase.Term qualified as Term
import U.Codebase.Type as Type
import U.Core.ABT qualified as ABT
import Unison.ABT qualified as ABT
import Unison.ABT qualified as V1.ABT
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as ConstructorType
import Unison.Core.ConstructorId (ConstructorId)
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Hash (Hash)
import Unison.Hashing.V2 qualified as Hashing.V2
import Unison.Hashing.V2.Convert qualified as V2.Convert
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.PatternMatchCoverage.UFMap (UFMap)
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.Reference qualified as V1
import Unison.Reference qualified as V1.Reference
import Unison.Referent qualified as V1
import Unison.Referent qualified as V1.Referent
import Unison.Term qualified as V1
import Unison.Term qualified as V1.Term
import Unison.Type qualified as V1
import Unison.Type qualified as V1.Type
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Maybe qualified as Maybe
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.TransitiveClosure (transitiveClosure1')
import Unison.Var (Var)
import Unison.Var qualified as V1.Var

data NamespaceDiff reference referent = NamespaceDiff
  { -- Mapping from old term to new term.
    termUpdates :: Map referent referent,
    -- Mapping from old type to new type.
    typeUpdates :: Map reference reference
  }

-- | An abstract interface to the bits of a code database that we need for performing a merge.
data Database m = Database
  { loadConstructorType :: TypeReference -> m ConstructorType,
    loadTerm :: TermReferenceId -> m (Term Symbol),
    loadType :: TypeReferenceId -> m (Decl Symbol)
  }
  deriving stock (Generic)

data TypeBloboid = TypeBloboid
  { canonicalize :: TypeReference -> TypeReference,
    equivalenceClasses :: UFMap TypeReference TypeReference,
    updates :: Relation TypeReference TypeReference
  }
  deriving stock (Generic)

makeTypeBloboid :: Relation TypeReference TypeReference -> TypeBloboid
makeTypeBloboid updates =
  let canonicalizeMap = UFMap.freeze equivalenceClasses
      canonicalize r = Map.findWithDefault r r canonicalizeMap
      equivalenceClasses = computeEquivalenceClasses updates
   in TypeBloboid {canonicalize, equivalenceClasses, updates}

data TermBloboid = TermBloboid
  { canonicalize :: Referent -> Referent,
    equivalenceClasses :: UFMap Referent Referent,
    updates :: Relation Referent Referent
  }
  deriving stock (Generic)

makeTermBloboid :: Relation Referent Referent -> TermBloboid
makeTermBloboid updates =
  let canonicalizeMap = UFMap.freeze equivalenceClasses
      canonicalize r = Map.findWithDefault r r canonicalizeMap
      equivalenceClasses = computeEquivalenceClasses updates
   in TermBloboid {canonicalize, equivalenceClasses, updates}

-- | Compute equivalence classes from updates.
computeEquivalenceClasses :: forall x. Ord x => Relation x x -> UFMap x x
computeEquivalenceClasses updates =
  let nodes :: Set x
      nodes = Relation.dom updates `Set.union` Relation.ran updates

      edges :: [(x, x)]
      edges = Relation.toList updates

      nodesOnly :: UFMap x x
      nodesOnly = foldl' (\b a -> UFMap.insert a a b) UFMap.empty nodes

      addEdge :: (x, x) -> UFMap x x -> UFMap x x
      addEdge (a, b) m0 = fromMaybe m0 $ runIdentity $ UFMap.union a b m0 \canonk nonCanonV m -> do
        let m' =
              UFMap.alter
                canonk
                (error "impossible")
                (\_ equivClassSize canonV -> UFMap.Canonical equivClassSize (min canonV nonCanonV))
                m
        Identity (Just m')
   in foldl' (\b a -> addEdge a b) nodesOnly edges

computeTermUserUpdates ::
  forall m.
  Monad m =>
  HashHandle ->
  Database m ->
  TypeBloboid ->
  TermBloboid ->
  m (Relation Referent Referent)
computeTermUserUpdates hashHandle database typeBloboid termBloboid =
  Relation.fromList <$> filterM isUserUpdate (Relation.toList (termBloboid ^. #updates))
  where
    isUserUpdate :: (Referent, Referent) -> m Bool
    isUserUpdate = \case
      (Referent.Ref {}, Referent.Con {}) -> pure True
      (Referent.Con {}, Referent.Ref {}) -> pure True
      (Referent.Con typeRef0 cid0, Referent.Con typeRef1 cid1) -> wundefined
      (Referent.Ref (Reference.ReferenceDerived ref0), Referent.Ref (Reference.ReferenceDerived ref1)) -> do
        term0 <- loadCanonicalizedTerm ref0
        term1 <- loadCanonicalizedTerm ref1
        pure (HashHandle.hashTerm hashHandle term0 /= HashHandle.hashTerm hashHandle term1)
      -- Builtin-to-derived, derived-to-builtin, and builtin-to-builtin are all clearly user updates
      (Referent.Ref {}, Referent.Ref {}) -> pure True

    loadCanonicalizedTerm :: TermReferenceId -> m (ResolvedTerm Symbol)
    loadCanonicalizedTerm ref = do
      term <- (database ^. #loadTerm) ref
      canonicalize (ref ^. Reference.idH) term

    canonicalize :: Hash -> Term Symbol -> m (ResolvedTerm Symbol)
    canonicalize =
      canonicalizeTerm
        database
        (termBloboid ^. #canonicalize)
        (typeBloboid ^. #canonicalize)

computeTypeUserUpdates ::
  forall m.
  (Monad m) =>
  HashHandle ->
  Database m ->
  -- | A function that, given two types, returns a function if the decls are thought to "match", which means the two
  -- decls' decl types, modifiers, number of bound variables, and number of data constructors are equal, and the data
  -- constructors all have the same names. The returned function ought to be applied to the second type's constructors,
  -- which will put them in the order of the first.
  ( TypeReferenceId ->
    Decl Symbol ->
    TypeReferenceId ->
    Decl Symbol ->
    Maybe ([Decl.Type Symbol] -> [Decl.Type Symbol])
  ) ->
  TypeBloboid ->
  m (Relation TypeReference TypeReference)
computeTypeUserUpdates hashHandle database getConstructorMapping bloboid =
  Relation.fromList <$> filterM isUserUpdate0 (Relation.toList (bloboid ^. #updates))
  where
    isUserUpdate0 :: (TypeReference, TypeReference) -> m Bool
    isUserUpdate0 = \case
      (ReferenceBuiltin _, ReferenceBuiltin _) -> pure True
      (ReferenceBuiltin _, ReferenceDerived _) -> pure True
      (ReferenceDerived _, ReferenceBuiltin _) -> pure True
      (ReferenceDerived oldRef, ReferenceDerived newRef) -> do
        oldDecl <- (database ^. #loadType) oldRef
        newDecl <- (database ^. #loadType) newRef
        pure
          case Decl.declType oldDecl == Decl.declType newDecl of
            True -> isUserUpdateDecl oldRef oldDecl newRef newDecl
            False -> True

    isUserUpdateDecl :: TypeReferenceId -> Decl Symbol -> TypeReferenceId -> Decl Symbol -> Bool
    isUserUpdateDecl oldRef oldDecl newRef newDecl =
      case getConstructorMapping oldRef oldDecl newRef newDecl of
        Nothing -> True
        Just mapping ->
          let oldHash = oldRef ^. Reference.idH
              newHash = newRef ^. Reference.idH
           in any
                (\(oldCon, newCon) -> not (alphaEquivalentTypesModCandidateRefs oldHash newHash oldCon newCon))
                (zip (Decl.constructorTypes oldDecl) (mapping (Decl.constructorTypes newDecl)))

    alphaEquivalentTypesModCandidateRefs :: Hash -> Hash -> TypeD Symbol -> TypeD Symbol -> Bool
    alphaEquivalentTypesModCandidateRefs hlhs hrhs lhs rhs =
      let hashCanon :: Hash -> TypeD Symbol -> Reference.Reference
          hashCanon h x = HashHandle.toReference hashHandle (canonicalize h x)

          canonicalize :: Hash -> TypeD Symbol -> TypeT Symbol
          canonicalize selfHash x = Type.rmap ((bloboid ^. #canonicalize) . subSelfReferences) x
            where
              subSelfReferences :: Reference.TypeRReference -> TypeReference
              subSelfReferences =
                Reference.h_ %~ \case
                  Nothing -> selfHash
                  Just r -> r
       in hashCanon hlhs lhs == hashCanon hrhs rhs

canonicalizeTerm ::
  forall m.
  Monad m =>
  Database m ->
  (Referent -> Referent) ->
  (TypeReference -> TypeReference) ->
  Hash ->
  Term Symbol ->
  m (ResolvedTerm Symbol)
canonicalizeTerm database lookupCanonTerm lookupCanonType selfHash =
  ABT.transformM \case
    Term.Ann a typ -> pure $ Term.Ann a (Type.rmap lookupCanonType typ)
    Term.Constructor r cid -> lookupCanonReferent (Referent.Con r cid)
    Term.Match s cs -> pure $ Term.Match s (canonicalizeCase <$> cs)
    Term.Ref r -> lookupCanonTermRReference r
    Term.Request r cid -> lookupCanonReferent (Referent.Con r cid)
    Term.TermLink r -> pure $ Term.TermLink (lookupTermLink r)
    Term.TypeLink r -> pure $ Term.TypeLink (lookupCanonType r)
    -- Boring no-ops
    Term.And p q -> pure $ Term.And p q
    Term.App f a -> pure $ Term.App f a
    Term.Boolean b -> pure $ Term.Boolean b
    Term.Char c -> pure $ Term.Char c
    Term.Float d -> pure $ Term.Float d
    Term.Handle e h -> pure $ Term.Handle e h
    Term.If c t f -> pure $ Term.If c t f
    Term.Int i -> pure $ Term.Int i
    Term.Lam b -> pure $ Term.Lam b
    Term.Let a b -> pure $ Term.Let a b
    Term.LetRec bs b -> pure $ Term.LetRec bs b
    Term.List s -> pure $ Term.List s
    Term.Nat n -> pure $ Term.Nat n
    Term.Or p q -> pure $ Term.Or p q
    Term.Text t -> pure $ Term.Text t
  where
    canonicalizeCase :: forall t a. Term.MatchCase t TypeReference a -> Term.MatchCase t TypeReference a
    canonicalizeCase (Term.MatchCase pat g body) =
      Term.MatchCase (canonicalizePattern pat) g body

    canonicalizePattern :: Term.Pattern t TypeReference -> Term.Pattern t TypeReference
    canonicalizePattern pat =
      case pat of
        -- Actual canonicalization happening here. If a datacon gets canonicalized to a term reference, that's ok -
        -- just stuff it back into the pattern where the *type* reference should go (with a bogus constructor id). This
        -- makes the term invalid, but we only care about its hash.
        Term.PConstructor ref cid fields ->
          let (cref, ccid) = canonicalizeConstructorPattern ref cid
           in Term.PConstructor cref ccid (map canonicalizePattern fields)
        Term.PEffectBind ref cid fields k ->
          let (cref, ccid) = canonicalizeConstructorPattern ref cid
           in Term.PEffectBind cref ccid (map canonicalizePattern fields) (canonicalizePattern k)
        -- Boring recursive cases
        Term.PAs x -> Term.PAs (canonicalizePattern x)
        Term.PEffectPure x -> Term.PEffectPure (canonicalizePattern x)
        Term.PSequenceLiteral xs -> Term.PSequenceLiteral (map canonicalizePattern xs)
        Term.PSequenceOp x y z -> Term.PSequenceOp (canonicalizePattern x) y (canonicalizePattern z)
        -- Boring no-ops
        Term.PBoolean {} -> pat
        Term.PChar {} -> pat
        Term.PFloat {} -> pat
        Term.PInt {} -> pat
        Term.PNat {} -> pat
        Term.PText {} -> pat
        Term.PUnbound -> pat
        Term.PVar -> pat
      where
        canonicalizeConstructorPattern ref cid =
          case lookupCanonTerm (Referent.Con ref cid) of
            Referent.Con cref ccid -> (cref, ccid)
            Referent.Ref cref -> (cref, maxBound @ConstructorId)

    resolveReferent :: Referent.ReferentH -> Referent
    resolveReferent = Referent._Ref %~ resolveTermReference

    resolveTermReference :: TermRReference -> TermReference
    resolveTermReference =
      Reference.h_ %~ \case
        Nothing -> selfHash
        Just h -> h

    lookupTermLink :: Term.TermLink -> Referent
    lookupTermLink = lookupCanonTerm . resolveReferent

    lookupCanonTermRReference :: forall a. TermRReference -> m (Term.ResolvedF Symbol a)
    lookupCanonTermRReference r =
      lookupCanonReferent (Referent.Ref (resolveTermReference r))

    lookupCanonReferent :: forall a. Referent -> m (Term.ResolvedF Symbol a)
    lookupCanonReferent r = case lookupCanonTerm r of
      Referent.Ref x -> pure (Term.Ref x)
      -- if the term reference now maps to a constructor we need
      -- to lookup if it is a data constructor or an ability
      -- handler
      Referent.Con typeRef constructorId ->
        (database ^. #loadConstructorType) typeRef <&> \case
          ConstructorType.Data -> Term.Constructor (lookupCanonType typeRef) constructorId
          ConstructorType.Effect -> Term.Request (lookupCanonType typeRef) constructorId

newtype EC = EC {unEC :: Int}
  deriving (Show)
  deriving (Eq, Ord, Num, Enum) via Int

boingoBeats ::
  forall ref.
  Ord ref =>
  (ref -> Set ref) ->
  Relation ref ref ->
  Relation ref ref ->
  ()
boingoBeats refToDependencies allUpdates userUpdates =
  let coreECs :: Bimap EC (Set ref)
      coreECs =
        allUpdates
          & computeEquivalenceClasses
          & UFMap.toClasses
          & map (\(_, refs, _) -> refs)
          & zip [0 ..]
          & Bimap.fromList

      -- \| Compute and look up a ref in the reverse mapping (Set ref -> EC)
      lookupCoreEC :: ref -> Maybe EC
      lookupCoreEC =
        let m =
              coreECs
                & Bimap.toList
                & foldl' (\acc (i, refs) -> foldl' (\acc2 ref -> Map.insert ref i acc2) acc refs) Map.empty
         in \ref -> Map.lookup ref m

      allUpdatesLhs = Relation.dom allUpdates
      allUpdatesRhs = Relation.ran allUpdates

      userUpdatesLhs = Relation.dom userUpdates
      userUpdatesRhs = Relation.ran userUpdates

      -- Arya question for tomorrow: what about self-loops? (#foo3 calls #foo)
      -- Arya thinks we just ignore self-edges
      -- Mitchell thinks: hmm they don't seem to harm anything

      -- \| Relation.member a b if a depends on b.
      coreDependencyGraph :: Relation EC EC
      coreDependencyGraph = Relation.fromMultimap mm
        where
          mm =
            coreECs & Bimap.toMap & Map.map \class_ ->
              let dependencies = foldMap refToDependencies . Set.intersection class_
                  -- The dependencies of *all* of the old stuff affected by the merge
                  lcaDeps = dependencies allUpdatesLhs
                  -- The dependencies of *the user-touched subset* of the old stuff
                  userUpdateLhsDeps = dependencies userUpdatesLhs
                  -- The dependencies of *the user-touched subset* of the new stuff
                  userUpdateRhsDeps = dependencies userUpdatesRhs
               in userUpdateRhsDeps `Set.union` (lcaDeps `Set.difference` userUpdateLhsDeps)
                    & Set.mapMaybe lookupCoreEC

      -- allDependencyGraph :: Relation EC EC
      -- allDependencyGraph = coreDependencyGraph <> extraDependents

      dependenciesOfDependentsOfCore :: Relation ref ref
      dependenciesOfDependentsOfCore =
        Relation.fromMultimap $
          getTransitiveDependents
            refToDependencies
            (wundefined "scope / modified namespace")
            (wundefined "query / core class references")

      -- -- The dependencies of dependents
      -- -- TODO: Need to prove that there's no overlap between core and extra
      -- extraECs :: Bimap EC (Set ref)
      -- extraECs =
      --   let startIndex = EC (Bimap.size coreECs)
      --    in Bimap.fromList ([startIndex ..] `zip` (map Set.singleton . toList . Relation.dom) dependenciesOfDependentsOfCore)

      -- extraDependents :: Relation EC EC
      -- extraDependents =
      --   let startIndex = Bimap.size equivalenceClasses

      isConflicted :: EC -> Maybe Bool
      isConflicted = fmap unBit . (isConflictedBits UVector.!?) . unEC

      isConflictedBits :: UVector.Vector Bit
      isConflictedBits = UVector.generate (Bimap.size coreECs) \idx ->
        let ecMembers = coreECs Bimap.! (EC idx)
            memberUserChanges = Set.intersection ecMembers userUpdatesRhs
         in Bit $ Set.size memberUserChanges > 1

      conflictedNodes :: Set EC
      conflictedNodes = Set.filter (fromMaybe err . isConflicted) (Relation.dom coreDependencyGraph)
        where
          err = error "impossible: every node in the core graph should be in the map"

      -- Q: How do we move the conflicted nodes and their dependents to a separate Map?
      unconflictedMap, conflictedMap :: Map EC (Set EC)
      unconflictedMap = wundefined
      conflictedMap = wundefined

      outputMappings :: Map EC ref
      outputMappings = wundefined
   in -- 1. We have the mapping for the core nodes (DONE: coreClassDependencies).
      -- 2. Next we do these in any order: (DONE)
      --     * classify the core nodes into conflicted or not (DONE: isConflicted)
      --     * add (from the LCA + both branches) the transitive dependents of all the core nodes. (DONE: getTransitiveDependents)
      -- 3. Next we move <the conflicted nodes + all of their dependents>
      --     to a separate Map. The keys will be fully disjoint between the two maps.
      --     (suggestion: with a `seen :: Set v`)
      -- 4. Then we run Staryafish on the `Graph.flattenSCC <$> Graph.stronglyConnComp graph`
      --     of the "unconflicted" Map, and write the results to a new namespace.
      --     Note: Could split this into:
      --      4a. run Staryafish on the core group, preserving some state
      --      4b. run simpler propagation algorithm, on the rest, initialized with the state from 4a
      --     Note 2: Could do a single pass but branch to simpler logic when the EC size = 1
      -- 5. Then we pretty-print the `Graph.flattenSCC <$> Graph.stronglyConnComp graph`
      --     of the "conflicted" map and write the results to a scratch file.

      -- - Somewhere fit in something about not deleting dependencies of newly added definitions.
      -- This can probably be in Step 4.
      -- - Does everything break if I rename anything?
      --
      wundefined

-- \| Returns the set of all transitive dependents of the given set of references.
-- Uses dynamic programming to follow every transitive dependency from `scope` to `query`.
getTransitiveDependents ::
  forall ref.
  Ord ref =>
  -- | load dependents for a ref
  (ref -> Set ref) ->
  -- | "scope" e.g. the LCA namespace - fully removed references + newly added definitions
  -- Anything in scope that is a dependent of the query set will be returned, and also intermediate dependents.
  Set ref ->
  -- | "query" e.g. core class references
  Set ref ->
  Map ref (Set ref)
-- ^ the encountered transitive dependents, and for each, the set of their direct dependencies,
-- since we have it here anyway and need it later.
getTransitiveDependents refToDependencies scope query = search Map.empty query (Set.toList scope)
  where
    search :: Map ref (Set ref) -> Set ref -> [ref] -> Map ref (Set ref)
    search dependents _ [] = dependents
    search dependents seen (ref : unseen) =
      if Set.member ref seen
        then search dependents seen unseen
        else
          let refDependencies = refToDependencies ref
              (dependentDeps, uncategorizedDeps) = Set.partition isDependent refDependencies
              unseenDeps = Set.filter (not . isSeen) uncategorizedDeps
           in if null unseenDeps
                then -- we're ready to make a decision about ref

                  let seen' = Set.insert ref seen
                   in if null dependentDeps -- ref is not dependent on any of the query set
                        then search dependents seen' unseen
                        else search (Map.insert ref refDependencies dependents) seen' unseen
                else search dependents seen (toList unseenDeps ++ ref : unseen)
      where
        -- \| split the dependencies into three groups: known dependents, known independents, and unseen
        -- It would be nice to short circuit if (any (flip Set.member dependents) dependencies)
        -- and simply declare ref a dependent, but we can't do that because we might have unnamed dependencies.
        -- that we won't detect unless we keep going.
        -- If we can eventually know that all dependencies are named, then we can change this to short circuit.
        isDependent = flip Map.member dependents
        isSeen = flip Set.member seen

data Intervention v a = IResolveConflict (Defn v a) [Defn v a] | IKindCheck | ITypeCheck
  deriving (Eq, Ord, Show)

-- some cases where we want user intervention:
-- a generated decl fails to kind check hahahaha
-- a generated term fails to typecheck

-- a type was updated and deleted a constructor, but a new use of that constructor was added, so we didn't delete the constructor (or we renamed it foo.deleted_<hashprefix>)
-- could we have some convention that shows certain names as deprecated?

-- some things we want to warn about?:
-- a definition was deleted, but a new dependent was added, so we didn't delete the definition (or we renamed it foo.deleted_<hashprefix>)

-- staryafish :: (Monad m, Var v)
--   => (TypeReference -> m (Decl v)) -- ^ Load a type by reference
--   -> (TermReference -> m (Term v)) -- ^ Load a term by reference
--   -> [[(EC, Set TypeReference, TypeReference)]] -- ^ EC SCCs in topological order, dependencies before dependents
--   -> m (Map EC (TypeReference, ManualIntervention))

--

data Result v a = RSimple DefnRef | RSuccess DefnRefId (Defn v a) | RNeedsIntervention (Intervention v a) | RSkippedDependency EC
  deriving (Eq, Ord, Show)

data WorkItem = WiType EC | WiTypes (NESet EC) | WiTerm EC | WiTerms (NESet EC)
  deriving (Eq, Ord, Show)

data Defn v a = DefnTerm (V1.Term v a) (V1.Type v a) | DefnDecl (V1.Decl v a) | DefnCtor
  deriving (Eq, Ord, Show)

data DefnRef = DrTypeRef V1.TypeReference | DrTermish V1.Referent
  deriving (Eq, Ord, Show)

data DefnRefId = DriTypeRefId V1.TypeReferenceId | DriTermRefId V1.Referent.Id
  deriving (Eq, Ord, Show)

-- | The work queue comes in as [Set EC].
-- Each (Set EC) represents a possible cycle/component in the output namespace.
-- A single EC does not correspond to a cycle/component, it may contain references from many different cycle/components.
-- The cycle/components are not important here, except that the (Set EC) represents potentially one or more components in the output namespace.
processWorkItems ::
  forall m v a.
  (Monad m, Var v, Show a) =>
  (V1.TermReferenceId -> m (Term v)) ->
  (V1.TypeReferenceId -> m (Decl v)) ->
  (EC -> DefnRef) ->
  BiMultimap EC DefnRef ->
  BiMultimap EC DefnRef ->
  Relation EC EC ->
  ((DefnRef -> Maybe (Result v a)) -> DefnRef -> (DefnRef, (Defn v a))) ->
  Map EC (Result v a) ->
  [WorkItem] ->
  m (Map EC (Result v a))
processWorkItems
  (loadTerm :: V1.TermReferenceId -> m (Term v))
  (loadDecl :: V1.TypeReferenceId -> m (Decl v))
  (latestDefnRef :: EC -> DefnRef)
  -- \^ to be able to rewrite them
  (ecMembers :: BiMultimap EC DefnRef)
  -- \^ any of these must be replaced by the synthesized result for the EC
  (ecLatestMembers :: BiMultimap EC DefnRef)
  (ecDependencies :: Relation EC EC)
  -- \^ used for finding dependents to be moved to scratch
  (rewriteComponent :: ((DefnRef -> Maybe (Result v a)) -> DefnRef -> (DefnRef, Defn v a)))
  (replacements :: Map EC (Result v a))
  (queue :: [WorkItem]) =
    let recurse = processWorkItems loadTerm loadDecl latestDefnRef ecMembers ecLatestMembers ecDependencies rewriteComponent
     in case queue of
          [] -> pure replacements
          wi : queue' ->
            case wi of
              WiType ec -> do
                result <- handleOneType ec
                recurse (Map.insert ec result replacements) queue'
              WiTypes ecs -> wundefined
              WiTerm ec -> wundefined
              WiTerms ecs -> wundefined
    where
      handleOneType :: EC -> m (Result v a)
      handleOneType ec = case latestDefnRef ec of
        DrTypeRef r@V1.Reference.DerivedId {} -> do
          decl <- fmap wundefined . loadDecl . wundefined $ r
          let rewrittenDecl = rewriteDeclDependencies decl
          let hashed = wundefined . Hashing.V2.hashClosedDecl @v . wundefined $ rewrittenDecl -- is this ok?
          pure $ RSuccess (DriTypeRefId hashed) (DefnDecl rewrittenDecl)
        dr@(DrTypeRef V1.Reference.Builtin {}) -> pure $ RSimple dr
        DrTermish _ -> error "expected a type, got a term"

      handleManyTypes :: NESet EC -> m (Result v a)
      handleManyTypes ecs = do
        let latestRefs :: Map EC V1.TypeReferenceId
            latestRefs =
              Map.fromList
                [ (ec, r)
                  | ec <- toList ecs,
                    let r = case latestDefnRef ec of
                          DrTypeRef (V1.Reference.DerivedId r) -> r
                          _ -> error "expected a derived id"
                ]
        latestDefns <- --  :: Map V1Reference.Id (V1Decl.Decl Symbol ())
          Map.fromList <$> traverse (\(ec, r) -> (r,) <$> loadDecl r) (Map.toList latestRefs)
        -- V1Decl.unhashComponent
        wundefined
      -- Staryafish algorithm:
      -- 1. Load the latest version of each definition
      --   (those on the end of a solid arrow, or those with no outbound solid arrows):
      --   #a.0 (untouched), #f.0 (bob change), #d.0 (alice change)
      -- 2. Equivalency groups:
      --     "v0" = {#a.0, #d.1, #e.0}
      --     "v1"  = {#b.0, #d.2, #f.0}
      --     "v2" = {#c.0, #d.0}
      -- 3. Make substitutions in terms we loaded:
      --     "v0" = #a.0 such that #b.0 replaced by "v1"
      --     "v1"  = #f.0 such that #c.0 replaced by "v2"
      --     "v2" = #d.0 such that #d.1 replaced by "v0"
      -- 4. Hash those

      -- latestDefnsDrTypeRef (V1Reference.DerivedId r) =
      -- latestDefnRef ec >>= \case {}
      -- DrTypeRef
      -- decls <- Map.fromList <$> traverse (\ec -> \DefnType (ec,) <$> loadLatestDefn ec) (toList ecs)

      rewriteDeclDependencies :: decl -> decl
      rewriteDeclDependencies decl =
        let declDependencies :: Set V1.TypeReference
            declDependencies = V1.Decl.declTypeDependencies @v (wundefined decl)
            typeReplacements :: [(V1.TypeReference, V1.TypeReference)]
            typeReplacements = mapMaybe (\dep -> (dep,) <$> typeReplacementRef dep) (toList declDependencies)
            rewrittenDecl = foldl' (\decl (old, new) -> wundefined "replace old with new in decl" old new decl) decl typeReplacements
         in rewrittenDecl

      typeReplacementRef :: V1.TypeReference -> Maybe V1.TypeReference
      typeReplacementRef dep =
        -- for a dependency, look up its EC, and look up the replacement for that EC if any.
        case BiMultimap.lookupR (DrTypeRef dep) ecMembers of
          Just ec -> case Map.lookup ec replacements of
            Just (RSuccess (DriTypeRefId r) _) -> Just (V1.Reference.DerivedId r)
            Just r -> error $ "tried to look up a dependency that has already exhibited some problem: " ++ show ec ++ ": " ++ show r
            Nothing -> error $ "tried to look up a dependency that hasn't been processed yet: " ++ show ec
          Nothing -> Nothing

-- Staryafish algorithm:
-- 1. Load the latest version of each definition
--   (those on the end of a solid arrow, or those with no outbound solid arrows):
--   #a.0 (untouched), #f.0 (bob change), #d.0 (alice change)
-- 2. Equivalency groups:
--     "v0" = {#a.0, #d.1, #e.0}
--     "v1"  = {#b.0, #d.2, #f.0}
--     "v2" = {#c.0, #d.0}
-- 3. Make substitutions in terms we loaded:
--     "v0" = #a.0 such that #b.0 replaced by "v1"
--     "v1"  = #f.0 such that #c.0 replaced by "v2"
--     "v2" = #d.0 such that #d.1 replaced by "v0"
-- 4. Hash those

-- | Split the work queue in two (dependent on target, not dependent on target)
moveDependents :: Relation EC EC -> EC -> [Set EC] -> ([Set EC], [Set EC])
moveDependents
  (ecDependencies :: Relation EC EC)
  (target :: EC)
  (queue :: [Set EC]) =
    let getDependents :: EC -> Set EC
        getDependents ec = Relation.lookupRan ec ecDependencies
        transitiveDependents = transitiveClosure1' getDependents target
     in partition (any (\e -> Set.member e transitiveDependents)) queue

moveDependents' :: Relation EC EC -> EC -> [WorkItem] -> ([WorkItem], [WorkItem])
moveDependents'
  (ecDependencies :: Relation EC EC)
  (target :: EC)
  (queue :: [WorkItem]) =
    let getDependents :: EC -> Set EC
        getDependents ec = Relation.lookupRan ec ecDependencies
        transitiveDependents = transitiveClosure1' getDependents target
     in partition (any (`Set.member` transitiveDependents) . workItemToECs) queue

workItemToECs :: WorkItem -> NESet EC
workItemToECs = \case
  WiType ec -> NESet.singleton ec
  WiTypes ecs -> ecs
  WiTerm ec -> NESet.singleton ec
  WiTerms ecs -> ecs

data DeclError = DeDependentKind (V1.TypeReference, V1.TypeReference)
  deriving (Eq, Ord, Show)

data TermError
  = TeDependentKind (V1.TypeReference, V1.TypeReference)
  | TeFailedTypecheck (V1.TypeReference, V1.TypeReference)
  deriving (Eq, Ord, Show)

-- | Staryafish on type decls.
-- `component` must have 2+ elements.
rewriteDeclComponent ::
  forall m v a.
  (Monad m, Var v, Monoid a, Show a) =>
  (V1.TypeReferenceId -> m (V1.Decl v a)) ->
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  Map EC (V1.Decl v a) ->
  m (Map EC (Result v a))
rewriteDeclComponent loadDecl ecForRef finished component = do
  when (Map.size component < 2) $
    error "rewriteDeclComponent: this function is only for components with mutual recursion"
  -- we're going to iterate, doing one replacement step for each dependency of each member of the component
  let declDependencies :: V1.Decl v a -> Set V1.TypeReference
      declDependencies decl = V1.Decl.declTypeDependencies decl
      -- goes from a set of dependencies to a list of replacement expressions
      rewrittenComponent = traverse rewriteDecl component
        where
          ctorType = V1.Decl.constructors_ . Lens.each . Lens._3
          rewriteDecl decl = foldl' stepDecl decl <$> (setupTypeReplacements (declDependencies decl))
          setupTypeReplacements dependencies =
            concat <$> traverse (buildTypeReplacement loadDecl ecForRef finished component) (toList dependencies)
          stepDecl decl (oldType, newType) =
            V1.Decl.modifyAsDataDecl (Lens.over ctorType (Maybe.rewrite (ABT.rewriteExpression oldType newType))) decl
  -- todo: do we kind-check first? or hash first?
  V2.Convert.hashDecls <$> (Map.mapKeys (V1.Var.mergeEcVar . unEC) <$> rewrittenComponent) >>= \case
    Left errors -> error $ "rewriteDeclComponent: hashDecls failed: " ++ show errors
    Right hashed -> pure $ foldl' addReplacement finished hashed
      where
        addReplacement :: Map EC (Result v a) -> (v, V1.Reference.Id, V1.Decl.Decl v a) -> Map EC (Result v a)
        addReplacement replacements (v, declId, decl) =
          let ec = case V1.Var.typeOf v of
                V1.Var.EquivalenceClass ec -> EC ec
                t -> error $ "unexpected Var type " ++ show t
           in Map.insert ec (RSuccess (DriTypeRefId declId) (DefnDecl decl)) replacements

-- | implementation detail of rewriteDeclComponent and rewriteTermComponent
-- returns a list of replacements that should be attempted, if any, in order.
buildTypeReplacement ::
  (Monad m, Var v, Monoid a, Show a) =>
  (V1.TypeReferenceId -> m (V1.Decl v a)) ->
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  Map EC (V1.Decl v a) ->
  V1.TypeReference ->
  m [(V1.Type v a, V1.Type v a)]
buildTypeReplacement loadDecl ecForRef finished component rOld = do
  mayOldDecl <- case rOld of
    V1.Reference.Builtin {} -> pure Nothing
    V1.Reference.DerivedId rOld -> Just <$> loadDecl rOld
  -- construct the fully-saturated replacement
  let saturatedReplacement ty decl = mayOldDecl <&> \oldDecl -> (applySaturated (ref rOld) oldDecl, applySaturated ty decl)
  case ecForRef (DrTypeRef rOld) of
    Just ec ->
      -- Is it part of the current component?
      case (Map.lookup ec component, Map.lookup ec finished) of
        -- Yes, it's part of the current component.
        (Just latestDecl, Nothing) ->
          pure . catMaybes $
            [ saturatedReplacement (var ec) latestDecl,
              Just (ref rOld, var ec)
            ]
        -- Is it part of an earlier component?
        (Nothing, Just (RSimple (DrTypeRef rNew))) -> pure [(ref rOld, ref rNew)]
        (Nothing, Just (RSuccess (DriTypeRefId rNew) (DefnDecl newDecl))) ->
          pure . catMaybes $
            [ saturatedReplacement (refId rNew) newDecl,
              Just (ref rOld, refId rNew)
            ]
        (Just {}, Just {}) -> error $ "rewriteDeclComponent: " ++ show ec ++ " showed up as both finished and active"
        (Nothing, Just e) -> error $ "rewriteDeclComponent: we shouldn't see this for a dependency: " ++ show ec ++ " " ++ show e
        (Nothing, Nothing) -> error $ "rewriteDeclComponent: we should see something for " ++ show ec
    -- r is not part of any EC, so we leave it alone.
    Nothing -> pure []
  where
    ref r = V1.Type.ref mempty r
    refId r = V1.Type.refId mempty r
    var ec = V1.Type.var mempty (V1.Var.mergeEcVar (unEC ec))
    -- given ty, decl; return the applied type (ty a b c...) where `a b c...` are the type vars bound by decl
    applySaturated ty decl = V1.Type.apps' ty (V1.Type.var mempty <$> V1.Decl.bound (V1.Decl.asDataDecl decl))

-- This converts `Reference`s it finds that are in the input `Map`
-- back to free variables
rewriteTermComponent ::
  forall m v a.
  (Monad m, Var v, Monoid a, Eq a, Show a) =>
  (V1.TypeReferenceId -> m (V1.Decl v a)) ->
  (V1.Reference.Id -> m (V1.Term v a)) ->
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  Map EC (V1.Term v a) ->
  m (Map EC (Result v a))
rewriteTermComponent loadDecl loadTerm ecForRef finished component = do
  when (Map.size component < 2) $
    error "Merge.rewriteTermComponent: this function is only for components with mutual recursion"
  let termDependencies :: V1.Term v a -> Set LabeledDependency
      termDependencies term = V1.Term.labeledDependencies term
      rewrittenComponent = traverse rewriteTerm component
        where
          rewriteTerm term = foldl' stepTerm term <$> setupReplacements (termDependencies term)
          stepTerm :: V1.Term.Term v a -> Either (V1.Type.Type v a, V1.Type.Type v a) (V1.Term.Term v a, V1.Term.Term v a) -> V1.Term.Term v a
          stepTerm term = \case
            Left (oldType, newType) -> term & Maybe.rewrite (V1.Term.rewriteSignatures oldType newType)
            Right (oldTerm, newTerm) ->
              term
                & Maybe.rewrite (V1.Term.rewriteCasesLHS oldTerm newTerm)
                & Maybe.rewrite (V1.ABT.rewriteExpression oldTerm newTerm)

      setupReplacements :: Set LabeledDependency -> m [Either (V1.Type v a, V1.Type v a) (V1.Term v a, V1.Term v a)]
      setupReplacements dependencies = concat <$> traverse buildReplacement (toList dependencies)
        where
          buildReplacement :: LabeledDependency -> m [Either (V1.Type v a, V1.Type v a) (V1.Term v a, V1.Term v a)]
          buildReplacement = \case
            LD.TypeReference rOld -> fmap Left <$> buildTypeReplacement loadDecl ecForRef finished mempty rOld
            LD.TermReferent rOld -> do
              mayOldTerm <- case rOld of
                V1.Referent.Con {} -> pure Nothing
                V1.Referent.Ref (V1.Reference.Builtin {}) -> pure Nothing
                V1.Referent.Ref (V1.Reference.DerivedId rOld) -> Just <$> loadTerm rOld
              -- construct the fully-saturated replacement
              let saturatedReplacement fn decl = Nothing -- todo -- mayOldTerm <&> \oldTerm -> Right (applySaturated (ref rOld) (params oldTerm), applySaturated fn (params newTerm))
              -- -- given ty, decl; return the applied type (ty a b c...) where `a b c...` are the type vars bound by decl
              -- where applySaturated fn params = V1.Term.apps' fn (V1.Term.var mempty <$> params)
              case ecForRef (DrTermish rOld) of
                Nothing -> pure []
                Just ec ->
                  case (Map.lookup ec component, Map.lookup ec finished) of
                    -- Is it part of the current component.
                    (Just latestTerm, Nothing) ->
                      pure . fmap Right . catMaybes $
                        [ saturatedReplacement (var ec) latestTerm,
                          Just (ref rOld, var ec)
                        ]
                    -- Is it part of an earlier component?
                    (Nothing, Just (RSimple (DrTermish rNew))) -> pure [Right (ref rOld, ref rNew)]
                    (Nothing, Just (RSuccess (DriTermRefId rNew) (DefnTerm newTerm _newTermType))) ->
                      pure . fmap Right . catMaybes $
                        [ saturatedReplacement (refId rNew) newTerm,
                          Just (ref rOld, refId rNew)
                        ]
                    (Just {}, Just {}) -> error $ "rewriteTermComponent: " ++ show ec ++ " showed up as both finished and active"
                    (Nothing, Just e) -> error $ "rewriteTermComponent: we shouldn't see this for a dependency: " ++ show ec ++ " " ++ show e
                    (Nothing, Nothing) -> error $ "rewriteTermComponent: we should see something for " ++ show ec
          ref :: V1.Referent -> V1.Term v a
          ref r = V1.Term.fromReferent mempty r
          refId :: V1.Referent.Id -> V1.Term v a
          refId r = V1.Term.fromReferentId mempty r
          var :: EC -> V1.Term v a
          var ec = V1.Term.var mempty (V1.Var.mergeEcVar (unEC ec))

  wundefined "todo: typecheck" rewrittenComponent >>= \case
    Left failures ->
      wundefined "todo: insert failure results into `finished"
    Right typecheckedResult -> do
      let hashed = V2.Convert.hashTermComponents typecheckedResult
      let addReplacement :: Map EC (Result v a) -> (v, (V1.Reference.Id, V1.Term v a, V1.Type v a, extra)) -> Map EC (Result v a)
          addReplacement m (v, (r, tm, tp, _extra)) =
            let ec = case V1.Var.typeOf v of
                  V1.Var.EquivalenceClass ec -> EC ec
                  t -> error $ "unexpected Var type " ++ show t
             in Map.insert ec (RSuccess (DriTermRefId $ V1.Referent.RefId r) (DefnTerm tm tp)) m
      pure $ foldl' addReplacement finished (Map.toList hashed)

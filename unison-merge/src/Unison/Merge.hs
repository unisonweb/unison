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

import Control.Lens (review, (%~), (^.), (^?))
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Bit (Bit (Bit, unBit))
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as ConstructorType
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import Unison.PatternMatchCoverage.UFMap (UFMap)
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Var (Var)

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
  let equivalenceClasses :: Bimap EC (Set ref)
      equivalenceClasses =
        allUpdates
          & computeEquivalenceClasses
          & UFMap.toClasses
          & map (\(_, refs, _) -> refs)
          & zip [0 ..]
          & Bimap.fromList

      -- \| Compute and look up a ref in the reverse mapping (Set ref -> EC)
      whichEquivalenceClass :: ref -> Maybe EC
      whichEquivalenceClass =
        let m =
              equivalenceClasses
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
            equivalenceClasses & Bimap.toMap & Map.map \class_ ->
              let dependencies = foldMap refToDependencies . Set.intersection class_
                  -- The dependencies of *all* of the old stuff affected by the merge
                  lcaDeps = dependencies allUpdatesLhs
                  -- The dependencies of *the user-touched subset* of the old stuff
                  userUpdateLhsDeps = dependencies userUpdatesLhs
                  -- The dependencies of *the user-touched subset* of the new stuff
                  userUpdateRhsDeps = dependencies userUpdatesRhs
               in userUpdateRhsDeps `Set.union` (lcaDeps `Set.difference` userUpdateLhsDeps)
                    & Set.mapMaybe whichEquivalenceClass

      allDependencyGraph :: Relation EC EC
      allDependencyGraph = coreDependencyGraph <> extraDependents
      -- coreDependencyRelation = Relation.fromMultimap coreDependencyGraph

      dependenciesOfDependentsOfCore :: Relation ref ref
      dependenciesOfDependentsOfCore =
        Relation.fromMultimap $
          getTransitiveDependents
            refToDependencies
            (wundefined "scope / modified namespace")
            (wundefined "query / core class references")

      extraECs :: Bimap EC (Set ref)
      extraECs =
        let startIndex = EC (Bimap.size equivalenceClasses)
         in Bimap.fromList ([startIndex ..] `zip` (map Set.singleton . toList . Relation.dom) dependenciesOfDependentsOfCore)

      extraDependents :: Relation EC EC
      extraDependents =
        let startIndex = Bimap.size equivalenceClasses
         in wundefined

      isConflicted :: EC -> Maybe Bool
      isConflicted = fmap unBit . (isConflictedBits UVector.!?) . unEC

      isConflictedBits :: UVector.Vector Bit
      isConflictedBits = UVector.generate (Bimap.size equivalenceClasses) \idx ->
        let ecMembers = equivalenceClasses Bimap.! (EC idx)
            memberUserChanges = Set.intersection ecMembers userUpdatesRhs
         in Bit $ Set.size memberUserChanges > 1

      conflictedNodes :: Set EC
      conflictedNodes = Set.filter (fromMaybe err . isConflicted) (Relation.dom coreDependencyGraph)
        where
          err = error "impossible: every node in the core graph should be in the map"
   in -- Q: How do we move the conflicted nodes and their dependents to a separate Map?
      --

      -- 1. We have the mapping for the core nodes (DONE: coreClassDependencies).
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
  (ref -> Set ref) ->
  Set ref ->
  -- \^ "scope" e.g. the LCA namespace - fully removed references + newly added definitions
  -- Anything in scope that is a dependent of the query set will be returned, and also intermediate dependents.
  Set ref ->
  -- \^ "query" e.g. core class references
  Map ref (Set ref)
-- \^ the encountered transitive dependents, and their direct dependencies
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

staryafish ::
  (Monad m, Var v) =>
  -- Get a term by reference
  (TermReference -> m (Term v)) ->
  -- The type substitutions to make
  (TypeReference -> TypeReference) ->
  -- The constructor substitutions to make
  ((TypeReference, ConstructorId, ConstructorType) -> (TypeReference, ConstructorId, ConstructorType)) ->
  -- The term substitutions to make, which we will add to (if the substitution succeeds)
  Map TermReference TermReference ->
  [TermReference] ->
  m ()
staryafish getTerm _ _ _ termRefs = do
  terms <- traverse getTerm termRefs

  -- Perform type substitutions in-place

  -- Perform term substitutions in-place

  -- Perform data constructor substitutions in-place

  -- Replace each term reference that's in an equivalency group with a new fresh variable

  -- Hash

  pure ()

{-# LANGUAGE RecordWildCards #-}

module Unison.Merge
  ( Database (..),
    isUserTypeUpdate,
    isUserTermUpdate,
    DefnRef (..),

    -- * Random misc things, temporarily exported
    Canonicalizer (..),
    makeCanonicalizer,
    Node (..),
    pattern NodeTms,
    pattern NodeTys,
    Changes (..),
    makeCoreEcs,
    makeCoreEcDependencies,

    -- * EC things (temporarily exported)
    EC (..),

    -- * Dag things
    Dag,
    reifyDag,
    dagTransitiveDependents,
  )
where

import Control.Lens (Lens', (%~), (^.))
import Control.Lens qualified as Lens
import Control.Monad.Validate (ValidateT, runValidateT)
import Control.Monad.Validate qualified as Validate
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Bit (Bit (Bit, unBit))
import Data.Foldable (foldlM)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Lazy qualified as LazyMap
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Vector.Unboxed qualified as UVector
import Safe (elemIndexJust)
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Reference (RReference, Reference, Reference' (..), TermRReference, TermReference, TermReferenceId, TypeRReference, TypeReference, TypeReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle (HashHandle)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Term (ClosedTerm, Term)
import U.Codebase.Term qualified as Term
import U.Codebase.Type as Type
import U.Core.ABT qualified as ABT
import Unison.ABT qualified as ABT
import Unison.ABT qualified as V1.ABT
import Unison.ConstructorReference qualified as V1
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as ConstructorType
import Unison.Core.ConstructorId (ConstructorId)
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration qualified as V1.Decl
import Unison.FileParsers qualified as FP
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as Hashing.Convert
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.PatternMatchCoverage.UFMap (UFMap)
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.Reference qualified as V1
import Unison.Reference qualified as V1.Reference
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Term qualified as V1
import Unison.Term qualified as V1.Term
import Unison.Type qualified as V1
import Unison.Type qualified as V1.Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile.Type qualified as UF
import Unison.Util.Map qualified as Map
import Unison.Util.Maybe qualified as Maybe
import Unison.Util.Monoid (foldMapM)
import Unison.Util.NEMap qualified as NEMap
import Unison.Util.NESet qualified as NESet
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
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
    -- FIXME use ConstructorReference when there's only one Reference
    loadConstructorTypeSignature :: TypeReferenceId -> ConstructorId -> m (TypeD Symbol),
    loadTerm :: TermReferenceId -> m (Term Symbol),
    loadType :: TypeReferenceId -> m (Decl Symbol)
  }
  deriving stock (Generic)

makeCanonicalize :: forall a. Ord a => Relation a a -> a -> a
makeCanonicalize updates =
  let canonicalizeMap = UFMap.freeze equivalenceClasses
      canonicalize r = Map.findWithDefault r r canonicalizeMap
      equivalenceClasses = computeEquivalenceClasses updates
   in canonicalize

ufmapToECs :: Ord a => UFMap a a -> Bimap EC (Set a)
ufmapToECs =
  Bimap.fromList . zipWith (\ec (_, refs, _) -> (EC ec, refs)) [0 ..] . UFMap.toClasses

-- | Compute equivalence classes from updates.
computeEquivalenceClasses :: forall x. Ord x => Relation x x -> UFMap x x
computeEquivalenceClasses updates =
  let nodes :: Set x
      nodes = Relation.dom updates `Set.union` Relation.ran updates

      edges :: [(x, x)]
      edges = Relation.toList updates

      nodesOnly :: UFMap x x
      nodesOnly = foldl' (\b a -> UFMap.insert a a b) UFMap.empty nodes

      addEdge :: UFMap x x -> (x, x) -> UFMap x x
      addEdge m0 (a, b) = fromMaybe m0 $ runIdentity $ UFMap.union a b m0 \canonK nonCanonV m -> do
        let m' =
              UFMap.alter
                canonK
                (error "impossible")
                (\_ equivClassSize canonV -> UFMap.Canonical equivClassSize (min canonV nonCanonV))
                m
        Identity (Just m')
   in foldl' addEdge nodesOnly edges

-- Post-condition: sets have 2+ elements
groupUpdatesIntoEquivalenceClasses :: Ord a => Relation a a -> [Set a]
groupUpdatesIntoEquivalenceClasses =
  map (\(_, refs, _) -> refs) . UFMap.toClasses . computeEquivalenceClasses

data Canonicalizer m = Canonicalizer
  { termsAreEqual :: TermWithSelfHash -> TermWithSelfHash -> m Bool,
    typesAreEqual :: TypeWithSelfHash -> TypeWithSelfHash -> Bool
  }
  deriving stock (Generic)

data TermWithSelfHash
  = TermWithSelfHash !Hash !(Term Symbol)

data TypeWithSelfHash
  = TypeWithSelfHash !Hash !(TypeD Symbol)

makeCanonicalizer ::
  forall m.
  Monad m =>
  HashHandle ->
  Database m ->
  Relation TypeReference TypeReference ->
  Relation Referent Referent ->
  Canonicalizer m
makeCanonicalizer hashHandle database typeUpdates termUpdates =
  Canonicalizer {termsAreEqual, typesAreEqual}
  where
    canonicalizeTypeRef :: TypeReference -> TypeReference
    canonicalizeTypeRef =
      makeCanonicalize typeUpdates

    canonicalizeTermRef :: Referent -> Referent
    canonicalizeTermRef =
      makeCanonicalize termUpdates

    termsAreEqual :: TermWithSelfHash -> TermWithSelfHash -> m Bool
    termsAreEqual term0 term1 = do
      cterm0 <- canonicalizeTerm database canonicalizeTypeRef canonicalizeTermRef term0
      cterm1 <- canonicalizeTerm database canonicalizeTypeRef canonicalizeTermRef term1
      pure (HashHandle.hashClosedTerm hashHandle cterm0 == HashHandle.hashClosedTerm hashHandle cterm1)

    typesAreEqual :: TypeWithSelfHash -> TypeWithSelfHash -> Bool
    typesAreEqual type0 type1 =
      hashType type0 == hashType type1

    hashType :: TypeWithSelfHash -> Reference
    hashType (TypeWithSelfHash selfHash ty) =
      HashHandle.toReference hashHandle (canonicalizeType canonicalizeTypeRef selfHash ty)

isUserTermUpdate ::
  forall m.
  Monad m =>
  Database m ->
  Canonicalizer m ->
  (Referent, Referent) ->
  m Bool
isUserTermUpdate database canonicalizer = \case
  (Referent.Ref {}, Referent.Con {}) -> pure True
  (Referent.Con {}, Referent.Ref {}) -> pure True
  (Referent.Con (Reference.ReferenceDerived typeRef0) cid0, Referent.Con (Reference.ReferenceDerived typeRef1) cid1) -> do
    type0 <- (database ^. #loadConstructorTypeSignature) typeRef0 cid0
    type1 <- (database ^. #loadConstructorTypeSignature) typeRef1 cid1
    pure $
      (canonicalizer ^. #typesAreEqual)
        (TypeWithSelfHash (typeRef0 ^. Reference.idH) type0)
        (TypeWithSelfHash (typeRef1 ^. Reference.idH) type1)
  (Referent.Ref (Reference.ReferenceDerived ref0), Referent.Ref (Reference.ReferenceDerived ref1)) -> do
    term0 <- (database ^. #loadTerm) ref0
    term1 <- (database ^. #loadTerm) ref1
    termsAreEqual <-
      (canonicalizer ^. #termsAreEqual)
        (TermWithSelfHash (ref0 ^. Reference.idH) term0)
        (TermWithSelfHash (ref1 ^. Reference.idH) term1)
    pure (not termsAreEqual)
  -- Builtin-to-derived, derived-to-builtin, and builtin-to-builtin are all clearly user updates
  (Referent.Con {}, Referent.Con {}) -> pure True
  (Referent.Ref {}, Referent.Ref {}) -> pure True

isUserTypeUpdate ::
  forall m.
  (Monad m) =>
  Database m ->
  Canonicalizer m ->
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
  (TypeReference, TypeReference) ->
  m Bool
isUserTypeUpdate database canonicalizer getConstructorMapping = \case
  (ReferenceBuiltin _, ReferenceBuiltin _) -> pure True
  (ReferenceBuiltin _, ReferenceDerived _) -> pure True
  (ReferenceDerived _, ReferenceBuiltin _) -> pure True
  (ReferenceDerived oldRef, ReferenceDerived newRef) -> do
    oldDecl <- (database ^. #loadType) oldRef
    newDecl <- (database ^. #loadType) newRef
    pure
      case Decl.declType oldDecl == Decl.declType newDecl of
        False -> True
        True ->
          case getConstructorMapping oldRef oldDecl newRef newDecl of
            Nothing -> True
            Just mapping ->
              any
                ( \(oldCon, newCon) ->
                    not $
                      (canonicalizer ^. #typesAreEqual)
                        (TypeWithSelfHash oldHash oldCon)
                        (TypeWithSelfHash newHash newCon)
                )
                (zip (Decl.constructorTypes oldDecl) (mapping (Decl.constructorTypes newDecl)))
              where
                oldHash = oldRef ^. Reference.idH
                newHash = newRef ^. Reference.idH

canonicalizeType :: (TypeReference -> TypeReference) -> Hash -> TypeD Symbol -> TypeT Symbol
canonicalizeType canonicalizeTypeRef selfHash =
  Type.rmap (canonicalizeTypeRef . Reference.closeRReference selfHash)

canonicalizeTerm ::
  forall m.
  Monad m =>
  Database m ->
  (TypeReference -> TypeReference) ->
  (Referent -> Referent) ->
  TermWithSelfHash ->
  m (ClosedTerm Symbol)
canonicalizeTerm database canonicalizeTypeRef canonicalizeTermRef (TermWithSelfHash selfHash theTerm) =
  ABT.transformM canonicalizeTermF theTerm
  where
    canonicalizeTermF :: Term.F Symbol x -> m (Term.ClosedF Symbol x)
    canonicalizeTermF = \case
      Term.Ann a typ -> pure $ Term.Ann a (Type.rmap canonicalizeTypeRef typ)
      Term.Constructor r cid -> lookupCanonReferent (Referent.Con r cid)
      Term.Match s cs -> pure $ Term.Match s (canonicalizeCase <$> cs)
      Term.Ref r -> lookupCanonReferent (Referent.Ref (Reference.closeRReference selfHash r))
      Term.Request r cid -> lookupCanonReferent (Referent.Con r cid)
      Term.TermLink r -> pure $ Term.TermLink (lookupTermLink r)
      Term.TypeLink r -> pure $ Term.TypeLink (canonicalizeTypeRef r)
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
          case canonicalizeTermRef (Referent.Con ref cid) of
            Referent.Con cref ccid -> (cref, ccid)
            Referent.Ref cref -> (cref, maxBound @ConstructorId)

    resolveReferent :: Referent.ReferentH -> Referent
    resolveReferent =
      Referent._Ref %~ Reference.closeRReference selfHash

    lookupTermLink :: Term.TermLink -> Referent
    lookupTermLink =
      canonicalizeTermRef . resolveReferent

    lookupCanonReferent :: forall a. Referent -> m (Term.ClosedF Symbol a)
    lookupCanonReferent r =
      case canonicalizeTermRef r of
        Referent.Ref x -> pure (Term.Ref x)
        Referent.Con typeRef constructorId -> makeConstructorTerm typeRef constructorId

    makeConstructorTerm :: TypeReference -> ConstructorId -> m (Term.ClosedF Symbol a)
    makeConstructorTerm typeRef constructorId = do
      mkTerm <-
        (database ^. #loadConstructorType) typeRef <&> \case
          ConstructorType.Data -> Term.Constructor
          ConstructorType.Effect -> Term.Request
      pure (mkTerm typeRef constructorId)

newtype EC = EC {unEC :: Int}
  deriving (Show)
  deriving (Enum, Eq, Num, Ord) via Int

data Node tm ty
  = Node'Term tm
  | Node'Terms (NESet tm) -- 2+ terms
  | Node'Type ty
  | Node'Types (NESet ty) -- 2+ types
  deriving stock (Eq, Ord, Show)

pattern NodeTms :: Set tm -> Node tm ty
pattern NodeTms tms <- (asNodeTms -> Just tms)

pattern NodeTys :: Set ty -> Node tm ty
pattern NodeTys tys <- (asNodeTys -> Just tys)

{-# COMPLETE NodeTys, NodeTms #-}

asNodeTms :: Node tm ty -> Maybe (Set tm)
asNodeTms = \case
  Node'Term tm -> Just (Set.singleton tm)
  Node'Terms tms -> Just (NESet.toSet tms)
  Node'Type _ -> Nothing
  Node'Types _ -> Nothing

asNodeTys :: Node tm ty -> Maybe (Set ty)
asNodeTys = \case
  Node'Type ty -> Just (Set.singleton ty)
  Node'Types tys -> Just (NESet.toSet tys)
  Node'Term _ -> Nothing
  Node'Terms _ -> Nothing

-- Create graph nodes from type conflicted adds.
typeConflictedAddsToNodes :: Ord ty => Relation ty ty -> [Node tm ty]
typeConflictedAddsToNodes =
  conflictedAddsToNodes Node'Types

-- Create graph nodes from term conflicted adds.
termConflictedAddsToNodes :: Ord tm => Relation tm tm -> [Node tm ty]
termConflictedAddsToNodes =
  conflictedAddsToNodes Node'Terms

conflictedAddsToNodes :: Ord ref => (NESet ref -> node) -> Relation ref ref -> [node]
conflictedAddsToNodes mkNode =
  Relation.toList >>> map (nonEmptySetFromPair >>> mkNode)
  where
    nonEmptySetFromPair (x, y) =
      NESet.insert y (NESet.singleton x)

-- Create graph nodes from type updates.
typeUpdatesToNodes :: Ord ty => Relation ty ty -> [Node tm ty]
typeUpdatesToNodes =
  groupUpdatesIntoEquivalenceClasses
    >>> map (NESet.unsafeFromSet >>> Node'Types) -- safe; each EC will have 2+ elements

-- Create graph nodes from term updates.
termUpdatesToNodes :: Ord tm => Relation tm tm -> [Node tm ty]
termUpdatesToNodes =
  groupUpdatesIntoEquivalenceClasses
    >>> map (NESet.unsafeFromSet >>> Node'Terms) -- safe; each EC will have 2+ elements

data Changes ty tm = Changes
  { termConflictedAdds :: Relation tm tm,
    typeConflictedAdds :: Relation ty ty,
    termUpdates :: Relation tm tm,
    typeUpdates :: Relation ty ty,
    termUserUpdates :: Relation tm tm,
    typeUserUpdates :: Relation ty ty
  }
  deriving stock (Generic)

makeCoreEcs :: forall tm ty. (Ord tm, Ord ty) => Changes ty tm -> Bimap EC (Node tm ty)
makeCoreEcs changes =
  -- Make graph nodes out of all the type/term add conflicts and updates, combine them together, and assign each a
  -- unique EC number
  let typeNodes :: [Node x ty]
      typeNodes =
        fold
          [ typeConflictedAddsToNodes (changes ^. #typeConflictedAdds),
            typeUpdatesToNodes (changes ^. #typeUpdates)
          ]
      termNodes :: [Node tm x]
      termNodes =
        fold
          [ termConflictedAddsToNodes (changes ^. #termConflictedAdds),
            termUpdatesToNodes (changes ^. #termUpdates)
          ]
   in Bimap.fromList (zip [0 ..] (typeNodes ++ termNodes))

-- makeCoreEcDependencies tycons tydeps tmdeps changes core
--
--   tycons: a function that resolves a type to a list of term constructors.
--     for example, given the type reference
--       #somehash -- Maybe
--     it should return the term references for the constructors
--       #somehash#0 -- Nothing
--       #somehash#1 -- Just
--
--   tydeps: a function that returns the direct dependencies of a type
--
--   tmdeps: a function that returns the direct dependencies of a term. a term that uses a data constructor (in either
--     pattern or constructor position) is said to depend on that particular constructor term, rather than on its type.
--
--   changes: the add conflicts and updates
--
--   core: the core equivalence classes computed from the updates
makeCoreEcDependencies ::
  forall m tm ty.
  (Monad m, Ord tm, Ord ty) =>
  (ty -> m [tm]) ->
  (ty -> m (Set ty)) ->
  (tm -> m (Set (Either ty tm))) ->
  Changes ty tm ->
  Bimap EC (Node tm ty) ->
  m (Relation EC EC)
makeCoreEcDependencies getTypeConstructorTerms getTypeDependencies getTermDependencies changes coreEcs = do
  Relation.fromMultimap <$> traverse getNodeDependencyEcs (Bimap.toMap coreEcs)
  where
    -- Make a couple lookup functions from term/type back to EC
    (lookupTypeEc, lookupTermEc) =
      makeEcLookupFunctions coreEcs

    -- Get the set of ECs that a type depends on
    getTypeDependencyEcs :: ty -> m (Set EC)
    getTypeDependencyEcs ty = do
      constructorTerms <- getTypeConstructorTerms ty
      deps <- getTypeDependencies ty
      pure (Set.fromList (mapMaybe lookupTermEc constructorTerms) <> Set.mapMaybe lookupTypeEc deps)

    -- Get the set of ECs that a term depends on
    getTermDependencyEcs :: tm -> m (Set EC)
    getTermDependencyEcs =
      getTermDependencies >>> fmap (Set.mapMaybe (either lookupTypeEc lookupTermEc))

    getNodeDependencyEcs :: Node tm ty -> m (Set EC)
    getNodeDependencyEcs = \case
      NodeTms tms -> go getTermDependencyEcs termConflictedAdds termUpdatesLhs termUserUpdatesLhs termUserUpdatesRhs tms
      NodeTys tys -> go getTypeDependencyEcs typeConflictedAdds typeUpdatesLhs typeUserUpdatesLhs typeUserUpdatesRhs tys
      where
        go ::
          forall ref x y.
          Ord ref =>
          (ref -> m (Set EC)) ->
          Map ref x ->
          Map ref x ->
          Map ref y ->
          Map ref y ->
          Set ref ->
          m (Set EC)
        go getDependencyEcs conflictedAdds updatesLhs userUpdatesLhs userUpdatesRhs tys = do
          let dependenciesIn :: Map ref z -> m (Set EC)
              dependenciesIn =
                (`Set.intersectKeys` tys) >>> foldMapM getDependencyEcs
          lcaDeps <- dependenciesIn updatesLhs
          conflictedAddsDeps <- dependenciesIn conflictedAdds
          userUpdatesLhsDeps <- dependenciesIn userUpdatesLhs
          userUpdatesRhsDeps <- dependenciesIn userUpdatesRhs
          pure $
            Set.unions
              [ conflictedAddsDeps,
                userUpdatesRhsDeps,
                lcaDeps `Set.difference` userUpdatesLhsDeps
              ]

    -- Only the keys matter in all of these sets, we just don't compute actual sets here, since we only need to
    -- intersect the keys with another set, and it's more efficient to use Set.intersectKeys
    termConflictedAdds =
      LazyMap.union
        (Relation.domain (changes ^. #termConflictedAdds))
        (Relation.range (changes ^. #termConflictedAdds))
    typeConflictedAdds =
      LazyMap.union
        (Relation.domain (changes ^. #typeConflictedAdds))
        (Relation.range (changes ^. #typeConflictedAdds))
    termUpdatesLhs = Relation.domain (changes ^. #termUpdates)
    typeUpdatesLhs = Relation.domain (changes ^. #typeUpdates)
    termUserUpdatesLhs = Relation.domain (changes ^. #termUserUpdates)
    typeUserUpdatesLhs = Relation.domain (changes ^. #typeUserUpdates)
    termUserUpdatesRhs = Relation.range (changes ^. #termUserUpdates)
    typeUserUpdatesRhs = Relation.range (changes ^. #typeUserUpdates)

makeEcLookupFunctions :: forall tm ty. (Ord tm, Ord ty) => Bimap EC (Node tm ty) -> (ty -> Maybe EC, tm -> Maybe EC)
makeEcLookupFunctions coreEcs =
  let typeToEcMap :: Map ty EC
      termToEcMap :: Map tm EC
      (typeToEcMap, termToEcMap) =
        coreEcs
          & Bimap.toList
          & foldl' insertEc (Map.empty, Map.empty)
   in ((`Map.lookup` typeToEcMap), (`Map.lookup` termToEcMap))
  where
    insertEc :: (Map ty EC, Map tm EC) -> (EC, Node tm ty) -> (Map ty EC, Map tm EC)
    insertEc (!accTy, !accTm) (ec, node) =
      case node of
        NodeTms tms -> let !accTm1 = insertSet accTm tms in (accTy, accTm1)
        NodeTys tys -> let !accTy1 = insertSet accTy tys in (accTy1, accTm)
      where
        insertSet :: Ord k => Map k EC -> Set k -> Map k EC
        insertSet = foldl' insertOne

        insertOne :: Ord k => Map k EC -> k -> Map k EC
        insertOne acc x = Map.insert x ec acc

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

      -- Compute and look up a ref in the reverse mapping (Set ref -> EC)
      lookupCoreEC :: ref -> Maybe EC
      lookupCoreEC =
        let insertEC :: Map ref EC -> (EC, Set ref) -> Map ref EC
            insertEC m (i, refs) =
              foldl' (\acc ref -> Map.insert ref i acc) m refs

            refToEcMap =
              coreECs
                & Bimap.toList
                & foldl' insertEC Map.empty
         in \ref -> Map.lookup ref refToEcMap

      allUpdatesLhs = Relation.dom allUpdates
      allUpdatesRhs = Relation.ran allUpdates

      userUpdatesLhs = Relation.dom userUpdates
      userUpdatesRhs = Relation.ran userUpdates

      -- Relation.member a b if a depends on b.
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

type Dag a =
  Map a (Set a)

-- If    1
--      / \
--     2   3
--      \ / \
--       4   5
--
-- then `reifyDag loadDependencies [1]` will return
--
--   1 => {2, 3}
--   2 => {4}
--   3 => {4, 5}
--   4 => {}
--   5 => {}
reifyDag ::
  forall a m t.
  (Foldable t, Monad m, Ord a) =>
  -- | A function that looks up adjacent vertices.
  (a -> m (Set a)) ->
  -- | "Seed" vertices that populate the initial dag, and are used to look up adjacent vertices (recursively) until the
  -- entire dag is formed.
  t a ->
  m (Dag a)
reifyDag loadAdjacent =
  go Map.empty
  where
    go :: forall t. Foldable t => Dag a -> t a -> m (Dag a)
    go =
      foldlM \dag vertex ->
        case Map.member vertex dag of
          True -> pure dag
          False -> do
            adjacent <- loadAdjacent vertex
            dag1 <- go dag adjacent
            pure $! Map.insert vertex adjacent dag1

-- If    1
--      / \
--     2   3
--      \ / \
--       4   5
--
-- then this will return a function that maps a set of elements to its set of transitive dependents, for example
--
--   dagTransitiveDependents m {1}    = {1}
--   dagTransitiveDependents m {2}    = {1, 2}
--   dagTransitiveDependents m {4}    = {1, 2, 3, 4}
--   dagTransitiveDependents m {2, 5} = {1, 2, 3, 5}
--
-- It accepts an entire set rather than a single element for efficiency in the case that the caller is interested in
-- unioning the transitive dependents of multiple elements together. That is, if you want to get the union of
-- transitive dependents of elements {x, y, z}, then it is more efficient to call
--
--   dagTransitiveDependents m {x, y, z}
--
-- than it is to call
--
--   dagTransitiveDependents m {x} `union` dagTransitiveDependents m {y} `union` dagTransitiveDependents m {z}
--
-- though both expressions will compute the same set.
dagTransitiveDependents :: forall a. Ord a => Dag a -> Set a -> Set a
dagTransitiveDependents dag vertices =
  let isTransitiveDependent :: Map a Bool
      isTransitiveDependent =
        LazyMap.mapWithKey
          (\dependent dependencies -> Set.member dependent vertices || any (isTransitiveDependent Map.!) dependencies)
          dag
   in Map.keysSet isTransitiveDependent

-- possibly more efficient of reifyDag + dagTransitiveDependents

-- | Returns the set of all transitive dependents of the given set of references.
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
  Dag ref
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
        -- split the dependencies into three groups: known dependents, known independents, and unseen
        -- It would be nice to short circuit if (any (flip Set.member dependents) dependencies)
        -- and simply declare ref a dependent, but we can't do that because we might have unnamed dependencies.
        -- that we won't detect unless we keep going.
        -- If we can eventually know that all dependencies are named, then we can change this to short circuit.
        isDependent = flip Map.member dependents
        isSeen = flip Set.member seen

data Intervention v a
  = IResolveConflict (NonEmpty DefnRef) (NonEmpty DefnRef) -- old(s) new(s)
  | IKindCheck
  | ITypeCheck
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

data Result v a
  = RSimple DefnRef
  | RSuccess DefnRefId (Defn v a)
  | RNeedsIntervention (Intervention v a)
  | RSkippedDependencies (NESet EC)
  deriving (Eq, Ord, Show)

goodResult :: Result v a -> Bool
goodResult = \case
  RSimple {} -> True
  RSuccess {} -> True
  _ -> False

data WorkItem
  = WiType EC (Set EC)
  | WiTypes (NESet EC) (Set EC)
  | WiTerm EC
  | WiTerms (NESet EC)
  deriving (Eq, Ord, Show)

data Defn v a
  = DefnTerm (V1.Term v a) (V1.Type v a)
  | -- a constructor mapping from latest to final decl
    DefnDecl (V1.Decl v a)
  deriving (Eq, Ord, Show)

type DefnRef = LabeledDependency

data DefnRefId = DriTypeRefId V1.TypeReferenceId | DriTermRefId V1.TermReferenceId
  deriving (Eq, Ord, Show)

data DefnTag = DtLca | DtLatest
  deriving (Eq, Ord, Show)

data EcHandle = EcHandle
  { -- any of these should be replaced by the synthesized result
    memberToEc :: DefnRef -> Maybe EC,
    -- i.e. user update(s) RHS
    ecUserUpdatedMembers :: EC -> Maybe (NonEmpty DefnRef),
    -- i.e. LCA version(s)
    -- note: we only look at original members if there are no user-updated members.
    --       if there are no user-updated members, then these original members are should be
    --       equal up to alpha equivalence, and it shouldn't matter which one we choose for anything
    ecOriginalMembers :: EC -> NonEmpty DefnRef,
    -- use this for figuring out transitive dependents
    ecDependenciesRelation :: Relation EC EC
  }

data LatestMember = LmLatest DefnRef | LmConflict (NonEmpty DefnRef) (NonEmpty DefnRef) deriving (Show)

latestMember :: EcHandle -> EC -> LatestMember
latestMember EcHandle {ecUserUpdatedMembers, ecOriginalMembers} ec =
  case ecUserUpdatedMembers ec of
    Just (updated :| []) -> LmLatest updated
    Just updated -> LmConflict (ecOriginalMembers ec) updated
    -- if there are no user-updated members but more than one original member,
    -- that means that multiple original definitions were auto-propagated to the same new result
    -- which means that any of the origianl definitions is an equally good starting point,
    -- as they should be identical up to EC-canonical alpha-equivalence
    Nothing -> LmLatest $ NonEmpty.head (ecOriginalMembers ec)

ecDependencies, ecDependents :: EcHandle -> EC -> Set EC
ecDependencies eh ec = Relation.lookupDom ec (ecDependenciesRelation eh)
ecDependents eh ec = Relation.lookupRan ec (ecDependenciesRelation eh)

-- | The work queue comes in as [NESet EC].
-- Each (NESet EC) represents a possible cycle/component in the output namespace.
-- A single EC does not correspond to a cycle/component, it may contain references from many different cycle/components.
-- The cycle/components are not important here, except that the (NESet EC) represents potentially one or more components in the output namespace.
processWorkItems ::
  forall m v a.
  (Monad m, Var v, Monoid a, Show a, Ord a) =>
  (V1.TermReferenceId -> m (V1.Term v a)) ->
  (V1.TypeReferenceId -> m (V1.Decl v a)) ->
  EcHandle ->
  [WorkItem] ->
  m (Map EC (Result v a))
processWorkItems loadTerm loadDecl ech = go Map.empty TL.empty
  where
    go finished typeLookup = \case
      [] -> pure finished
      wi : queue' ->
        -- This writes a result for every workitem we encounter, including ones that we skipped due to a problem with their dependencies.
        -- This is partly because when we're done, we want to separate the successful WIs from the failed ones, but it's not "monotonic".
        -- Basically this lets us leverage the original serialization of the dependency graph when trying to figure out what to do with the results.
        let runAndRecurse :: WorkItem -> [WorkItem] -> (WorkItem -> m (NEMap EC (Result v a))) -> m (Map EC (Result v a))
            runAndRecurse wi queue' run = do
              let wiECs = workItemToECs wi
              -- check for failed dependencies
              let wiDependencies = foldMap (ecDependencies ech) wiECs
                  isNotGood ec = maybe False (not . goodResult) $ Map.lookup ec finished
                  badDependencies = Set.filter isNotGood wiDependencies
              newResults <- case NESet.nonEmptySet badDependencies of
                Nothing ->
                  -- check for conflicted terms
                  runValidateT (traverse_ checkForConflicts wiECs) >>= \case
                    Left conflicts -> pure $ recordProblems wiECs conflicts
                    Right () -> run wi
                Just badResults -> pure $ NEMap.fromSet (const $ RSkippedDependencies badResults) wiECs
              let finished' = NEMap.withNonEmpty newResults (newResults <>) finished
                  newTypeLookup = resultsToTypeLookup newResults
              go (NEMap.toMap finished') (newTypeLookup <> typeLookup) queue'
         in runAndRecurse wi queue' \case
              WiType ec _ctorEcs -> handleOneType ec
              WiTypes ecs _ctorEcs -> handleManyTypes ecs
              WiTerm ec -> handleOneTerm typeLookup ec
              WiTerms ecs -> handleManyTerms typeLookup ecs
      where
        handleOneType :: EC -> m (NEMap EC (Result v a))
        handleOneType ec = case latestMember ech ec of
          LmLatest (LD.DerivedType r) -> rewriteSingleDecl (memberToEc ech) finished ec <$> (r,) <$> loadDecl r
          LmLatest dr@(LD.BuiltinType {}) -> pure $ NEMap.singleton ec $ RSimple dr
          LmLatest (LD.TermReferent {}) -> error "handleOneType: latest member shouldn't be a term"
          LmConflict {} -> error "handleOneType: this conflict should have been caught in the previous case block"

        checkForConflicts :: EC -> ValidateT (NEMap EC (Intervention v a)) m ()
        checkForConflicts ec = case latestMember ech ec of
          LmLatest {} -> pure ()
          LmConflict original updated -> Validate.dispute $ NEMap.singleton ec $ IResolveConflict original updated

        handleOneTerm typeLookup ec = case latestMember ech ec of
          LmLatest dr@(LD.BuiltinTerm {}) -> pure . NEMap.singleton ec $ RSimple dr
          LmLatest (LD.DerivedTerm r) -> handleManyTerms typeLookup (NESet.singleton ec) -- todo: single version?
          LmLatest (LD.ConReference {}) -> error "handleOneTerm: constructors should be grouped with their types"
          LmLatest (LD.TypeReference {}) -> error "handleOneTerm: latest member shouldn't be a type"
          LmConflict original updated -> pure . NEMap.singleton ec $ RNeedsIntervention (IResolveConflict original updated)

        handleManyTerms :: TypeLookup v a -> NESet EC -> m (NEMap EC (Result v a))
        handleManyTerms typeLookup ecs = rewriteTermComponent typeLookup (memberToEc ech) finished <$> latestTerms
          where
            latestTerms = NEMap.fromSetM loadLatestMember ecs
            loadLatestMember :: EC -> m (V1.Term v a)
            loadLatestMember ec = case latestMember ech ec of
              LmLatest (LD.DerivedType r) -> loadTerm r
              e -> error $ "handleManyTypes: we should only see loadable decls here, not " ++ show e

        handleManyTypes :: NESet EC -> m (NEMap EC (Result v a))
        handleManyTypes typeEcs = rewriteDeclComponent (memberToEc ech) finished <$> latestDecls
          where
            latestDecls = NEMap.fromSetA loadLatestDecl typeEcs
            loadLatestDecl :: EC -> m (V1.TypeReferenceId, V1.Decl v a)
            loadLatestDecl ec = case latestMember ech ec of
              LmLatest (LD.DerivedType r) -> fmap (r,) $ loadDecl r
              e -> error $ "handleManyTypes: we should only see loadable decls here, not " ++ show e

        recordProblems :: NESet EC -> NEMap EC (Intervention v a) -> NEMap EC (Result v a)
        recordProblems ecs problems = NEMap.fromSet makeResult ecs
          where
            makeResult ec = case NEMap.lookup ec problems of
              Just i -> RNeedsIntervention i
              Nothing -> RSkippedDependencies problemSet
            problemSet = NEMap.keysSet problems

-- latestDefns <- --  :: Map V1Reference.Id (V1Decl.Decl Symbol ())
--   Map.fromList <$> traverse (\(ec, r) -> (r,) <$> loadDecl r) (Map.toList latestRefs)
-- -- V1Decl.unhashComponent
-- wundefined

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
-- LD.TypeReference
-- decls <- Map.fromList <$> traverse (\ec -> \DefnType (ec,) <$> loadLatestDefn ec) (toList ecs)

-- rewriteDeclDependencies :: decl -> decl
-- rewriteDeclDependencies decl =
--   let declDependencies :: Set V1.TypeReference
--       declDependencies = V1.Decl.declTypeDependencies @v (wundefined decl)
--       typeReplacements :: [(V1.TypeReference, V1.TypeReference)]
--       typeReplacements = mapMaybe (\dep -> (dep,) <$> typeReplacementRef dep) (toList declDependencies)
--       rewrittenDecl = foldl' (\decl (old, new) -> wundefined "replace old with new in decl" old new decl) decl typeReplacements
--    in rewrittenDecl

-- typeReplacementRef :: V1.TypeReference -> Maybe V1.TypeReference
-- typeReplacementRef dep =
--   -- for a dependency, look up its EC, and look up the replacement for that EC if any.
--   case memberToEc ech (LD.TypeReference dep)  of
--     Just ec -> case Map.lookup ec replacements of
--       Just (RSuccess (DriTypeRefId r) _) -> Just (V1.Reference.DerivedId r)
--       Just r -> error $ "tried to look up a dependency that has already exhibited some problem: " ++ show ec ++ ": " ++ show r
--       Nothing -> error $ "tried to look up a dependency that hasn't been processed yet: " ++ show ec
--     Nothing -> Nothing

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

-- -- | Split the work queue in two (dependent on target, not dependent on target)
-- moveDependents :: Relation EC EC -> EC -> [Set EC] -> ([Set EC], [Set EC])
-- moveDependents
--   (ecDependencies :: Relation EC EC)
--   (target :: EC)
--   (queue :: [Set EC]) =
--     let getDependents :: EC -> Set EC
--         getDependents ec = Relation.lookupRan ec ecDependencies
--         transitiveDependents = transitiveClosure1' getDependents target
--      in partition (any (\e -> Set.member e transitiveDependents)) queue

-- moveDependents' :: Relation EC EC -> EC -> [WorkItem] -> ([WorkItem], [WorkItem])
-- moveDependents'
--   (ecDependencies :: Relation EC EC)
--   (target :: EC)
--   (queue :: [WorkItem]) =
--     let getDependents :: EC -> Set EC
--         getDependents ec = Relation.lookupRan ec ecDependencies
--         transitiveDependents = transitiveClosure1' getDependents target
--      in partition (any (`Set.member` transitiveDependents) . workItemToECs) queue

workItemToECs :: WorkItem -> NESet EC
workItemToECs = \case
  WiType ec ctors -> NESet.insertSet ec ctors
  WiTypes ecs ctors -> flip NESet.unionSet ecs ctors
  WiTerm ec -> NESet.singleton ec
  WiTerms ecs -> ecs

data DeclError = DeDependentKind (V1.TypeReference, V1.TypeReference)
  deriving (Eq, Ord, Show)

data TermError
  = TeDependentKind (V1.TypeReference, V1.TypeReference)
  | TeFailedTypecheck (V1.TypeReference, V1.TypeReference)
  deriving (Eq, Ord, Show)

rewriteSingleDecl ::
  (V1.ABT.Var v, Show v, Monoid a, Show a) =>
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  EC ->
  (V1.TypeReferenceId, V1.Decl v a) ->
  NEMap EC (Result v a)
rewriteSingleDecl ecForRef finished ec (r, decl) = NEMap.insertMap ec declResult ctorResultMap
  where
    ctorType = V1.Decl.constructorType decl
    declResult = RSuccess (DriTypeRefId r') (DefnDecl decl')
    ctorResultMap :: Map EC (Result v a)
    ctorResultMap = Map.fromList $ mapMaybe makeCtorResult $ Map.toList ctorMapping
    r' = Hashing.Convert.hashClosedDecl decl'
    -- apply all rewrites to the decl
    decl' = foldl' (flip performOneDeclRewrite) decl (mapMaybe getReplacement . toList $ V1.Decl.declTypeDependencies decl)
    -- create a structural find/replace pair for one dependency to its "finished" result.
    -- `ec` is just included for debug output.
    getReplacement rOld = do
      ec <- ecForRef (LD.TypeReference rOld)
      simpleFinishedTypeReplacement finished ec rOld
    -- pair old and new ctors according to their names in a transformed decl pair
    ctorMapping = buildConstructorMapping (r, decl) (r', decl')
    -- convert one ctor mapping to a ctor result
    makeCtorResult :: (V1.ConstructorReferenceId, V1.ConstructorReferenceId) -> Maybe (EC, Result v a)
    makeCtorResult (oldCtor, newCtor) = case ecForRef $ crIdToDefnRef oldCtor of
      Nothing -> trace ("Warning: No EC for " ++ show oldCtor ++ ". It may not have been named?") $ Nothing
      Just ctorEc -> Just (ctorEc, RSimple $ crIdToDefnRef newCtor)
      where
        crIdToDefnRef = LD.referentId . flip V1.Referent.ConId ctorType

performOneDeclRewrite ::
  (V1.ABT.Var v, Show v) =>
  (V1.Type v a, V1.Type v a) ->
  V1.Decl.Decl v a ->
  V1.Decl.Decl v a
performOneDeclRewrite (oldType, newType) =
  V1.Decl.modifyAsDataDecl (Lens.over ctorTypes (Maybe.rewrite (ABT.rewriteExpression oldType newType)))
  where
    ctorTypes = V1.Decl.constructors_ . Lens.each . Lens._3

-- | Staryafish on type decls.
-- `component` must have 2+ elements.
rewriteDeclComponent ::
  forall v a.
  (Var v, Monoid a, Show a) =>
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  NEMap EC (V1.TypeReferenceId, V1.Decl v a) ->
  NEMap EC (Result v a)
rewriteDeclComponent ecForRef finished component =
  if NEMap.size component < 2
    then error "rewriteDeclComponent: this function is only for components with mutual recursion"
    else updatedResults
  where
    updatedResults =
      -- Hashing.Convert.hashDecls isn't currently aw are of NEMaps, but the input is NonEmpty and the output must be too.
      NEMap.unsafeFromMap $ foldl' (flip addReplacement) mempty hashedComponent
      where
        -- for each decl, update the decl and its constructor mapping
        addReplacement :: (v, V1.TypeReferenceId, V1.Decl v a) -> Map EC (Result v a) -> Map EC (Result v a)
        addReplacement (v, r', decl') =
          Map.insert ec (RSuccess (DriTypeRefId r') (DefnDecl decl')) . Map.union ctorResults
          where
            ec = ecFromVar v
            (r, decl) = (NEMap.!) component ec
            ctorResults = Map.fromList $ mapMaybe makeCtorResult $ Map.toList ctorMapping
            ctorMapping = buildConstructorMapping (r, decl) (r', decl')
            -- convert one ctor mapping to a ctor result
            makeCtorResult :: (V1.ConstructorReferenceId, V1.ConstructorReferenceId) -> Maybe (EC, Result v a)
            makeCtorResult (oldCtor, newCtor) = case ecForRef $ ctorRefIdToDefnRef oldCtor of
              Nothing -> trace ("Warning: No EC for " ++ show oldCtor ++ ". It may not have been named?") $ Nothing -- todo: is this fine?
              Just ctorEc -> Just (ctorEc, RSimple $ ctorRefIdToDefnRef newCtor)
            ctorType = V1.Decl.constructorType decl
            ctorRefIdToDefnRef = LD.referentId . flip V1.Referent.ConId ctorType
    hashedComponent = case Hashing.Convert.hashDecls (Map.mapKeys ecToVar (NEMap.toMap rewrittenComponent)) of
      Left errors -> error $ "rewriteDeclComponent: hashDecls failed: " ++ show errors
      Right hashed -> hashed
      where
        rewrittenComponent = fmap performRewrites (snd <$> component)
        performRewrites decl = foldl' (flip performOneDeclRewrite) decl (setupRewrites (V1.Decl.declTypeDependencies decl))
        setupRewrites dependencies =
          mapMaybe (buildSimpleTypeReplacementExpression ecForRef finished (NEMap.keysSet component)) (toList dependencies)

-- | build a constructor mapping between two decls, assuming that they share the same constructor names
-- e.g. when one has been synthesized from the other via auto-propagation.
buildConstructorMapping :: Eq v => (V1.TypeReferenceId, V1.Decl v a) -> (V1.TypeReferenceId, V1.Decl v a) -> Map V1.ConstructorReferenceId V1.ConstructorReferenceId
buildConstructorMapping (r1, d1) (r2, d2) =
  Map.fromList
    [ (V1.ConstructorReference r1 c1, V1.ConstructorReference r2 (fromIntegral c2))
      | (name, c1) <- ctors1 `zip` [0 ..],
        let c2 = elemIndexJust name ctors2
    ]
  where
    ctors1 = V1.Decl.constructorVars . V1.Decl.asDataDecl $ d1
    ctors2 = V1.Decl.constructorVars . V1.Decl.asDataDecl $ d2

-- | Construct some appropriate before/after pairs to feed into the structural find/replace functions
-- to update these type decls.
-- Used by rewriteDeclComponent and rewriteTermComponent.
--
-- `rOld` is some dependency reference that we want to replace throughout some dependent definition not mentioned here.
-- The output is 0+ before/after type expressions that we're meant to replace in the dependent definition.
-- e.g. 0 replacements if the dependency has not been updated in this merge
--      1 or more for dependencies that we're going to replace in 1 or more ways
buildSaturatedTypeReplacement ::
  forall m v a.
  (Monad m, Var v, Monoid a, Show a) =>
  (V1.TypeReferenceId -> m (V1.Decl v a)) ->
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  Map EC (V1.Decl v a) ->
  V1.TypeReference ->
  m (Maybe (V1.Type v a, V1.Type v a))
buildSaturatedTypeReplacement loadDecl ecForRef finished component rOld = do
  mayOldDecl <- case rOld of
    V1.Reference.Builtin {} -> pure Nothing
    V1.Reference.DerivedId rOld -> Just <$> loadDecl rOld
  -- construct the fully-saturated replacement
  let saturatedReplacement :: V1.Type v a -> V1.Decl v a -> Maybe (V1.Type v a, V1.Type v a)
      saturatedReplacement ty decl = mayOldDecl <&> \oldDecl -> (applySaturated (refType rOld) oldDecl, applySaturated ty decl)
  -- look up the EC to determine what we need to replace the old ref with and how.
  case ecForRef (LD.TypeReference rOld) of
    Just ec ->
      -- Is it part of the current component?
      case (Map.lookup ec component, Map.lookup ec finished) of
        -- Yes, it's part of the current component.
        (Just latestDecl, Nothing) -> pure $ saturatedReplacement (varType ec) latestDecl
        -- Is it part of an earlier component?
        (Nothing, Just (RSimple {})) -> pure Nothing -- idk how to saturate this builtin
        (Nothing, Just (RSuccess (DriTypeRefId rNew) (DefnDecl newDecl))) ->
          pure $ saturatedReplacement (refIdType rNew) newDecl
        (Just {}, Just {}) -> error $ "buildSaturatedTypeReplacement: " ++ show ec ++ " showed up as both finished and active"
        (Nothing, Just e) -> error $ "buildSaturatedTypeReplacement: we shouldn't see this for a dependency: " ++ show ec ++ " " ++ show e
        (Nothing, Nothing) -> error $ "rewriteDeclbuildSaturatedTypeReplacementComponent: we should see something for " ++ show ec
    -- r is not part of any EC, so we leave it alone.
    Nothing -> pure Nothing
  where
    -- given ty, decl; return the applied type (ty a b c...) where `a b c...` are the type vars bound by decl
    applySaturated ty decl = V1.Type.apps' ty (V1.Type.var mempty <$> V1.Decl.bound (V1.Decl.asDataDecl decl))

resultsToTypeLookup :: NEMap EC (Result v a) -> TypeLookup v a
resultsToTypeLookup finished = TL.TypeLookup typeOfTerms dataDecls effectDecls
  where
    typeOfTerms = Map.fromList [(V1.Reference.DerivedId r, tp) | RSuccess (DriTermRefId r) (DefnTerm _tm tp) <- toList finished]
    dataDecls = Map.fromList [(V1.Reference.DerivedId r, dd) | RSuccess (DriTypeRefId r) (DefnDecl (Right dd)) <- toList finished]
    effectDecls = Map.fromList [(V1.Reference.DerivedId r, ed) | RSuccess (DriTypeRefId r) (DefnDecl (Left ed)) <- toList finished]

-- rewrite dependencies in a term component, including hashing, and save the results
-- todo: do we care about the type from the database? or do we actually want to unwrap-or-infer it?
-- return the results. The results don't include `finished`, you have to combine them yourself.
--
-- `typeLookup` should include everything in `finished`.
-- The returned result will not contain anything in `finished`; it can be combined after returning.
rewriteTermComponent ::
  forall v a.
  (Var v, Monoid a, Ord a, Show a) =>
  TypeLookup v a ->
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  NEMap EC (V1.Term v a) ->
  NEMap EC (Result v a)
rewriteTermComponent typeLookup ecForRef finished component =
  if NEMap.size component < 2
    then error "Merge.rewriteTermComponent: this function is only for components with mutual recursion"
    else -- TODO: validateUnisonFile here?
    case Result.result $ FP.synthesizeFile typecheckingEnv uf of
      Just tuf -> formatResults (typecheckedComponent tuf)
      Nothing -> errorResults
  where
    uf :: UnisonFile v a = UF.UnisonFileId dds eds tms ws
      where
        dds = mempty
        eds = mempty
        ws = mempty
        tms :: [(v, a, V1.Term v a)]
        tms = fmap (\(ec, tm) -> (ecToVar ec, mempty, tm)) . toList $ NEMap.toList rewrittenComponent
    typecheckingEnv = Typechecker.Env ambientAbilities typeLookup termsByShortName
      where
        ambientAbilities = mempty
        termsByShortName = mempty
    errorResults = NEMap.map (const $ RNeedsIntervention ITypeCheck) component
    formatResults = NEMap.map makeResult . NEMap.mapKeysMonotonic ecFromVar
      where
        makeResult (termId, term, typ) = RSuccess (DriTermRefId termId) (DefnTerm term typ)
    typecheckedComponent :: TypecheckedUnisonFile v a -> NEMap v (V1.TermReferenceId, V1.Term v a, V1.Type v a)
    typecheckedComponent tuf =
      NEMap.unsafeFromMap $ Map.map (\(_a, r, _k, tm, tp) -> (r, tm, tp)) (UF.hashTermsId tuf)
    rewrittenComponent = fmap performRewrites component
    performRewrites tm = foldl' (flip performOneRewrite) tm (setupTermRewrites (V1.Term.labeledDependencies tm))
    performOneRewrite = \case
      Left (oldType, newType) ->
        Maybe.rewrite (V1.Term.rewriteSignatures oldType newType)
      Right (oldTerm, newTerm) ->
        Maybe.rewrite (V1.ABT.rewriteExpression oldTerm newTerm)
          . Maybe.rewrite (V1.Term.rewriteCasesLHS oldTerm newTerm)
    setupTermRewrites =
      mapMaybe (buildSimpleTermReplacementExpression ecForRef finished (NEMap.keysSet component)) . toList

buildSimpleTypeReplacementExpression ::
  (Var v, Monoid a, Show a) =>
  (DefnRef -> Maybe EC) ->
  (Map EC (Result v a)) ->
  NESet EC ->
  V1.TypeReference ->
  Maybe (V1.Type v a, V1.Type v a)
buildSimpleTypeReplacementExpression ecForRef finished component rOld =
  case ecForRef (LD.TypeReference rOld) of
    Just ec ->
      asum
        [ simpleComponentTypeReplacement component ec rOld,
          simpleFinishedTypeReplacement finished ec rOld
        ]
    Nothing -> Nothing

simpleComponentTypeReplacement ::
  (Var v, Monoid a) =>
  NESet EC ->
  EC ->
  V1.Reference.Reference ->
  Maybe (V1.Type.Type v a, V1.Type.Type v a)
simpleComponentTypeReplacement component ec rOld =
  case NESet.member ec component of
    True -> Just (refType rOld, varType ec)
    False -> Nothing

simpleFinishedTypeReplacement ::
  (Ord v, Monoid a, Show v, Show a) =>
  Map EC (Result v a) ->
  EC ->
  V1.Reference.Reference ->
  Maybe (V1.Type.Type v a, V1.Type.Type v a)
simpleFinishedTypeReplacement finished ec rOld =
  case Map.lookup ec finished of
    Just (RSimple (LD.TypeReference rNew)) -> Just (refType rOld, refType rNew)
    Just (RSuccess (DriTypeRefId rNew) _newDefn) -> Just (refType rOld, refIdType rNew)
    Just err -> error $ "simpleFinishedTypeReplacement: " ++ show rOld ++ " " ++ show ec ++ " showed up as " ++ show err
    Nothing -> Nothing

buildSimpleTermReplacementExpression ::
  (Var v, Monoid a, Show a) =>
  (DefnRef -> Maybe EC) ->
  Map EC (Result v a) ->
  NESet EC ->
  LabeledDependency ->
  Maybe (Either (V1.Type v a, V1.Type v a) (V1.Term v a, V1.Term v a))
buildSimpleTermReplacementExpression ecForRef finished currentTermComponent = \case
  dr@(LD.TypeReference rOld) ->
    ecForRef dr
      <&> Left . \ec ->
        case Map.lookup ec finished of
          Just (RSimple (LD.TypeReference rNew)) -> (refType rOld, refType rNew)
          Just (RSuccess (DriTypeRefId rNew) _newDecl) -> (refType rOld, refIdType rNew)
          Just res -> error $ "buildSimpleTypeReplacementExpression: looked up dependency " ++ show dr ++ ", which had finished with an error: " ++ show res
          Nothing -> error $ "buildSimpleTermReplacementExpressions: looked up dependency " ++ show dr ++ ", which should be finished, but isn't."
  dr@(LD.TermReferent rOld) ->
    ecForRef dr
      <&> Right . \ec ->
        case (NESet.member ec currentTermComponent, Map.lookup ec finished) of
          (True, Nothing) -> (refTerm rOld, varTerm ec)
          (True, Just {}) -> error $ "buildSimpleTypeReplacementExpression: " ++ show ec ++ " showed up as both finished and active"
          (False, Just (RSimple (LD.TermReferent rNew))) -> (refTerm rOld, refTerm rNew)
          (False, Just (RSuccess (DriTermRefId rNew) _newDefn)) -> (refTerm rOld, refIdTerm rNew)
          (False, Just res) -> error $ "buildSimpleTypeReplacementExpression: looked up dependency " ++ show dr ++ ", which had finished with an error: " ++ show res
          (False, Nothing) -> error $ "buildSimpleTermReplacementExpressions: looked up dependency " ++ show dr ++ ", which should be either finished or active, but is neither."

ecToVar :: Var v => EC -> v
ecToVar = V1.Var.mergeEcVar . unEC

ecFromVar :: Var v => v -> EC
ecFromVar v = case V1.Var.typeOf v of
  V1.Var.EquivalenceClass ec -> EC ec
  t -> error $ "ecFromVar: unexpected Var type " ++ show t

refType :: (Ord v, Monoid a) => V1.Reference.Reference -> V1.Type.Type v a
refType = V1.Type.ref mempty

refIdType :: (Ord v, Monoid a) => V1.TypeReferenceId -> V1.Type.Type v a
refIdType = V1.Type.refId mempty

varType :: (Var v, Monoid a) => EC -> V1.Type.Type v a
varType = V1.Type.var mempty . ecToVar

refTerm :: (Ord v, Monoid a) => V1.Referent.Referent -> V1.Term.Term2 vt at ap v a
refTerm = V1.Term.fromReferent mempty

refIdTerm :: (Ord v, Monoid a) => V1.TermReferenceId -> V1.Term.Term2 vt at ap v a
refIdTerm = V1.Term.refId mempty

varTerm :: (Var v, Monoid a) => EC -> V1.Term.Term2 vt at ap v a
varTerm = V1.Term.var mempty . ecToVar

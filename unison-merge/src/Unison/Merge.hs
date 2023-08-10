module Unison.Merge () where

import Control.Lens (review, (%~), (^?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Reference (TermRReference, TermReference, TypeReference)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle (HashHandle)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Term (Term)
import U.Codebase.Term qualified as Term
import U.Codebase.Type as Type
import U.Core.ABT (ABT)
import U.Core.ABT qualified as ABT
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as ConstructorType
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation

data NamespaceDiff reference referent = NamespaceDiff
  { -- Mapping from old term to new term.
    termUpdates :: Map referent referent,
    -- Mapping from old type to new type.
    typeUpdates :: Map reference reference
  }

type ConstructorMapping = forall a. [a] -> [a]

computeTypeECs :: Map reference reference -> Map reference reference -> [Set reference]
computeTypeECs = undefined

computeEquivClassLookupFunc :: forall x. Ord x => Relation x x -> x -> x
computeEquivClassLookupFunc rel =
  let nodes :: Set x
      nodes = Map.keysSet (Relation.domain rel) `Set.union` Map.keysSet (Relation.range rel)

      edges :: [(x, x)]
      edges = Relation.toList rel

      ufmap :: UFMap.UFMap x x
      ufmap =
        let nodesOnly = foldl' (\b a -> UFMap.insert a a b) UFMap.empty nodes
            addEdge :: (x, x) -> UFMap.UFMap x x -> UFMap.UFMap x x
            addEdge (a, b) m0 = fromMaybe m0 $ runIdentity $ UFMap.union a b m0 \canonk nonCanonV m -> do
              let m' =
                    UFMap.alter
                      canonk
                      (error "impossible")
                      (\_ equivClassSize canonV -> UFMap.Canonical equivClassSize (min canonV nonCanonV))
                      m
              Identity (Just m')
         in foldl' (\b a -> addEdge a b) nodesOnly edges
      canonMap :: Map x x
      canonMap = UFMap.freeze ufmap
   in \k -> case Map.lookup k canonMap of
        Just x -> x
        Nothing -> k

computeTermUserUpdates ::
  forall m.
  Monad m =>
  HashHandle ->
  (TypeReference -> TypeReference) ->
  (TypeReference -> m ConstructorType) ->
  (Referent -> m (Term Symbol)) ->
  Relation Referent Referent ->
  m (Relation Referent Referent)
computeTermUserUpdates hashHandle lookupCanonType lookupConstructorType loadTerm allUpdates =
  Relation.fromList <$> filterM isUserUpdate (Relation.toList allUpdates)
  where
    lookupCanonTerm :: Referent -> Referent
    lookupCanonTerm = computeEquivClassLookupFunc allUpdates

    isUserUpdate :: (Referent, Referent) -> m Bool
    isUserUpdate (t0, t1) = do
      t0 <- canonicalizeTerm lookupCanonTerm lookupConstructorType lookupCanonType =<< loadTerm t0
      t1 <- canonicalizeTerm lookupCanonTerm lookupConstructorType lookupCanonType =<< loadTerm t1
      undefined

    isUserUpdateConstructor :: (TypeReference, ConstructorId) -> (TypeReference, ConstructorId) -> m Bool
    isUserUpdateConstructor = undefined

computeTypeUserUpdates ::
  forall m.
  (Monad m) =>
  HashHandle ->
  (TypeReference -> m (Decl Symbol)) ->
  (TypeReference -> TypeReference -> ConstructorMapping) ->
  Relation TypeReference TypeReference ->
  m (Relation TypeReference TypeReference)
computeTypeUserUpdates hashHandle loadDecl constructorMapping allUpdates =
  Relation.fromList <$> filterM isUserUpdate0 (Relation.toList allUpdates)
  where
    lookupCanon :: TypeReference -> TypeReference
    lookupCanon = computeEquivClassLookupFunc allUpdates

    isUserUpdate0 :: (TypeReference, TypeReference) -> m Bool
    isUserUpdate0 (oldRef, newRef) = do
      oldDecl <- loadDecl oldRef
      newDecl <- loadDecl newRef
      pure
        case Decl.declType oldDecl == Decl.declType newDecl of
          True -> isUserUpdateDecl oldRef oldDecl newRef newDecl
          False -> True

    isUserUpdateDecl ::
      TypeReference ->
      Decl Symbol ->
      TypeReference ->
      Decl Symbol ->
      Bool
    isUserUpdateDecl oldRef oldDecl newRef newDecl =
      or
        [ Decl.modifier oldDecl /= Decl.modifier newDecl,
          length (Decl.bound oldDecl) /= length (Decl.bound newDecl),
          length (Decl.constructorTypes oldDecl) /= length (Decl.constructorTypes newDecl),
          case (oldRef, newRef) of
            (Reference.ReferenceDerived (Reference.Id h0 _), Reference.ReferenceDerived (Reference.Id h1 _)) ->
              any
                (\(a, b) -> not (alphaEquivalentTypesModCandidateRefs h0 h1 a b))
                ( zip
                    (Decl.constructorTypes oldDecl)
                    (constructorMapping oldRef newRef (Decl.constructorTypes newDecl))
                )
            (Reference.ReferenceBuiltin txt0, Reference.ReferenceBuiltin txt1) -> txt0 /= txt1
            (Reference.ReferenceBuiltin _, Reference.ReferenceDerived _) -> True
            (Reference.ReferenceDerived _, Reference.ReferenceBuiltin _) -> True
        ]

    alphaEquivalentTypesModCandidateRefs :: Hash -> Hash -> TypeD Symbol -> TypeD Symbol -> Bool
    alphaEquivalentTypesModCandidateRefs hlhs hrhs lhs0 rhs0 =
      let lhs = Type.rmap (Reference._RReferenceReference %~ lookupCanon) lhs0
          rhs = Type.rmap (Reference._RReferenceReference %~ lookupCanon) rhs0
       in HashHandle.toReferenceDecl hashHandle hlhs lhs == HashHandle.toReferenceDecl hashHandle hrhs rhs

canonicalizeTerm ::
  forall m.
  Monad m =>
  (Referent -> Referent) ->
  (TypeReference -> m ConstructorType) ->
  (TypeReference -> TypeReference) ->
  Term Symbol ->
  m (Term Symbol)
canonicalizeTerm lookupCanonTerm lookupConstructorType lookupCanonType =
  ABT.transformM go
  where
    go :: forall a. Term.F Symbol a -> m (Term.F Symbol a)
    go = \case
      Term.Int i -> pure $ Term.Int i
      Term.Nat n -> pure $ Term.Nat n
      Term.Float d -> pure $ Term.Float d
      Term.Boolean b -> pure $ Term.Boolean b
      Term.Text t -> pure $ Term.Text t
      Term.Char c -> pure $ Term.Char c
      Term.Ref r -> lookupCanonTermRReference r
      Term.Constructor r cid -> lookupCanonReferent (Referent.Con r cid)
      Term.Request r cid -> lookupCanonReferent (Referent.Con r cid)
      Term.Handle e h -> pure $ Term.Handle e h
      Term.App f a -> pure $ Term.App f a
      Term.Ann a typ -> pure $ Term.Ann a (Type.rmap lookupCanonType typ)
      Term.List s -> pure $ Term.List s
      Term.If c t f -> pure $ Term.If c t f
      Term.And p q -> pure $ Term.And p q
      Term.Or p q -> pure $ Term.Or p q
      Term.Lam b -> pure $ Term.Lam b
      Term.LetRec bs b -> pure $ Term.LetRec bs b
      Term.Let a b -> pure $ Term.Let a b
      Term.Match s cs -> pure $ Term.Match s (goCase <$> cs)
      Term.TermLink r -> pure $ Term.TermLink (lookupTermLink r)
      Term.TypeLink r -> pure $ Term.TypeLink (lookupCanonType r)

    goCase :: forall t a. Term.MatchCase t TypeReference a -> Term.MatchCase t TypeReference a
    goCase (Term.MatchCase pat g body) = Term.MatchCase (Term.rmapPattern id lookupCanonType pat) g body

    lookupTermLink :: Term.TermLink -> Term.TermLink
    lookupTermLink = Referent._ReferentHReferent %~ lookupCanonTerm

    lookupCanonTermRReference :: forall a. TermRReference -> m (Term.F Symbol a)
    lookupCanonTermRReference r = case r ^? Reference._RReferenceReference of
      Nothing -> pure (Term.Ref r)
      Just r -> lookupCanonReferent (Referent.Ref r)

    lookupCanonReferent :: forall a. Referent -> m (Term.F Symbol a)
    lookupCanonReferent r = case lookupCanonTerm r of
      Referent.Ref x -> pure (Term.Ref (review Reference._RReferenceReference x))
      -- if the term reference now maps to a constructor we need
      -- to lookup if it is a data constructor or an ability
      -- handler
      Referent.Con typeRef constructorId ->
        lookupConstructorType typeRef >>= \case
          ConstructorType.Data -> pure (Term.Constructor (lookupCanonType typeRef) constructorId)
          ConstructorType.Effect -> pure (Term.Request (lookupCanonType typeRef) constructorId)

{-# LANGUAGE RecordWildCards #-}

module Unison.Merge2
  ( -- * Merge database
    MergeDatabase (..),
    referent2to1,
    makeMergeDatabase,

    -- * Precondition violation
    PreconditionViolation (..),

    -- * Library dependencies
    mergeLibdeps,

    -- * Namespace diff
    nameBasedNamespaceDiff,

    -- * Typechecking
    whatToTypecheck,
    computeUnisonFile,

    -- * Pretty printing
    MergeOutput (..),
    ScratchDefn (..),
    Conflict (..),
    ConflictOrGood (..),

    -- * Misc / organize these later
    UpdatesRefnt,
    DiffOp (..),
    DeepRefs,
    DeepRefsId',
    RefToName,
    TwoWay (..),
    TwoOrThreeWay (..),
  )
where

import Control.Lens (mapped, over, traverseOf, traversed, view, (^.), _3)
import Control.Monad.State.Strict (State, evalState, evalStateT)
import Control.Monad.State.Strict qualified as State
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Either.Combinators (fromLeft', fromRight')
import Data.Foldable (foldlM)
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Monoid (Endo (..))
import Data.Set qualified as Set
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType (RtTerm, RtType),
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorReference qualified as V1
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as CT
import Unison.Core.Project (ProjectBranchName)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.Diff (TwoOrThreeWay (..), TwoWay (..), nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Libdeps (mergeLibdeps)
import Unison.Merge.PreconditionViolation (PreconditionViolation (..))
import Unison.Name (Name)
import Unison.Parser.Ann qualified as V1 (Ann)
import Unison.Prelude
import Unison.Reference qualified as V1 (TermReference, TypeReference)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Sqlite (Transaction)
import Unison.Symbol qualified as V1 (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as V1 (Term)
import Unison.Term qualified as V1.Term
import Unison.Type qualified as Type
import Unison.Type qualified as V1 (Type)
import Unison.Type qualified as V1.Type
import Unison.UnisonFile.Env qualified as UFE
import Unison.UnisonFile.Names qualified as UFN
import Unison.UnisonFile.Type (UnisonFile)
import Unison.UnisonFile.Type qualified as UF
import Unison.Util.Maybe qualified as Maybe
import Unison.Util.Nametree (Defns (..))
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as V1 (WatchKind)

-- | DeepRefs is basically a one-way Names (many to one, rather than many to many)
-- It can represent the input or output namespace.
-- It includes constructors, which many other contexts here don't.
type DeepRefs =
  Defns (Map Name Referent) (Map Name TypeReference)

-- | DeepRefsId' is like DeepRefs', but maps to Ids instead of References.
-- It could be used, for example, to represent a set of Derived definitions to typecheck
type DeepRefsId' =
  Defns (Map Name TermReferenceId) (Map Name TypeReferenceId)

type RefToName =
  Defns (Map Referent Name) (Map TypeReference Name)

type UpdatesRefnt =
  Defns (Map Name Referent) (Map Name TypeReference)

-- for updated definitions, we want to know which branch to find the updated version in
-- for transitive dependents of updates (which are not updates themselves), we don't care which version we get
-- for conflicted definitions, we need to print both versions, but we don't typecheck anything

-- merge :: Applicative m => DeepRefs -> DeepRefs -> DeepRefs -> m DeepRefs
-- merge ppe lca a b =
--   pure wundefined

-- typecheckStuff = wundefined

-- writeStuffToScratchFile = wundefined
-- writeStuffToNamespace

-- we're either going to find the name in alice's deeprefs or bob's
-- (or LCA?)

-- figure

-- if no conflicts:
---- figure out what we want to typecheck

-- data BranchHandle m = BranchHandle
--   { bhTransitiveDependents :: LabeledDependency -> m (Set LabeledDependency),
--     bhUpdates :: Map Name Hash,
--     bhNameToTypeRef :: Name -> Maybe TypeReference,
--     bhNameToTermRef :: Name -> Maybe Referent,
--     bhTypeRefToName :: TypeReference -> Maybe Name,
--     bhTermRefToName :: Referent -> Maybe Name
--   }

-- data WhichBranch = Alice | Bob

-- whatToTypecheck :: (BranchHandle m, TransitiveDeps) -> (BranchHandle m, TransitiveDeps) -> m TransitiveDeps

{-
- [ ] Typecheck
    - Defns to typecheck:
        - For each updated name, typecheck the associated definition and a latest version of the transitive dependents of that name in each branch.
    - *Arya* Terms to typecheck: (all the updates and their transitive dependents):
        - [x] Get the updates + dependents set with Arya algorithm sqlite, parameterized by the namespace and the updates (we do this in sqlite)
        - [x] Filter deepStuff by the dependents set to get aliceDepTerms' :: Map Name TermReference (we do this in sqlite)
        - [x] Compute latest terms by Map.unionWith (f combinedUpdates) aliceDepTerms' bobDepTerms'
    - [ ] Later: Compute the set of Safe Deletes that aren't transitive dependencies of Added/Updated things.
        - [ ] Bonus: Warn for suppressed deletes.
    - [x] Build UnisonFile to typecheck
        - [x] Convert from `TermReference` or `TypeReference` to an actual `Term` / `Decl` to typecheck.
            - [ ] todo
            - [ ] Read the term/decl
            - [ ] Any `Map Reference Name` will do, we can get one by reversing `latestTerms`.
    - [ ] Construct a typechecking Env, and typecheck.
        - [ ] call Unison.Cli.computeTypecheckingEnvironment
    - If typechecking succeeds:
        - [ ] Some of the results do not exist on either branch. Write resulting definitions to codebase.
        - [ ] Construct an in-memory namespace starting from LCA deep{terms,types}, and applying the adds, updates, and safe deletes.
        - [ ] Cons in-memory namespace onto current.
    - If typechecking fails:
        - [ ] Create new branch for merge results.
            - [ ] Merge* the libs
            - [ ] Three options:
              - [ ] Nothing lol
              - [ ] Dependencies of updated things + added things, minus whatever's in the scratch file
              - [ ] Dependencies of updated things + successfully updated things (propagation-style)
        - [ ] Needs a better PPE than what we've talked about so far, but we can always come up with a good one earlier.
        - [ ] Populate the scratch file with whatever isn't in the new branch.
-}

-- todo: I think we might want to build the terms (or prepare to build the terms)
--       see what references they have left over
--       and build the typechecking env around those.
-- Basically just call `Unison.Cli.computeTypecheckingEnvironment`

-- everything that is an update (could be builtins) or transitive dependent of update (won't be builtins) needs to be switched to a Var
-- builtins can't go into the scratch file, so they have to go into the typechecking Env to be resolved during typechecking

---- Envs
-- There's a Typechecker.Env, which includes patterns I guess and TDNR lookups
-- and a UnisonFile.Env, which includes regular Names for substitution into terms maybe?

---- What dependencies do we need for typechecking?
-- I guess any updates which are builtins can
--    a) be substituted into the terms in advance, or
--    b) be replaced with a Var, and put into a UnisonFile.Env / name lookup environment
--

---- What needs to go into a scratch file for typechecking?
-- Answer is: anything that isn't going into the namespace that the scratch file will be typechecked against.
--      i.e.: Everything from the resulting namespace that isn't in the UnisonFile

-- | `ppes` for everything that needs to be typechecked, and all their dependencies
-- loadTerm / loadDecl, optionally with caching
-- the terms to typecheck (the dependents of updates)
-- the updates themselves
computeUnisonFile ::
  MergeDatabase ->
  RefToName ->
  DeepRefsId' ->
  UpdatesRefnt ->
  Transaction (UnisonFile V1.Symbol V1.Ann)
computeUnisonFile
  db@MergeDatabase {loadV1Decl, loadV1Term}
  ppes
  (Defns {terms = termsToTypecheck, types = declsToTypecheck})
  Defns {terms = combinedTermUpdates0, types = combinedTypeUpdates} = do
    combinedTermUpdates <- traverse (referent2to1 db) combinedTermUpdates0

    {-
    dependencies
    updates
    dependents

    right now: required: {dependents, required, dependencies optional
    later need {dependencies, updates, dependents}

    update type Foo = Foo #Bar

    -}

    updatedDecls <-
      let setupDecl = fmap (substForDecl ppes declNeedsUpdate combinedTypeUpdates) . loadV1Decl
            where
              declNeedsUpdate = flip Map.member declsToTypecheck
       in traverse setupDecl declsToTypecheck
    let -- todo: handle errors better:
        env :: UFE.Env V1.Symbol V1.Ann = (fromRight' . fromRight') envResult
          where
            -- This `Names` can be `mempty` because there shouldn't be any free vars in these declarations.
            -- There will be references that come from the codebase, or there will be vars that we just substed in
            -- right now for other decls listed here.
            envResult = UFN.environmentFor mempty dataDeclarations effectDeclarations
            effectDeclarations :: Map V1.Symbol (V1.Decl.EffectDeclaration V1.Symbol V1.Ann)
            dataDeclarations :: Map V1.Symbol (V1.Decl.DataDeclaration V1.Symbol V1.Ann)
            (fmap fromLeft' -> effectDeclarations, fmap fromRight' -> dataDeclarations) =
              updatedDecls
                & Map.mapKeys Name.toVar
                & Map.partition (\case Left {} -> True; Right {} -> False)
    updatedTerms <-
      let setupTerm = fmap (substForTerm ppes termNeedsUpdate updatedTypes combinedTermUpdates) . loadV1Term
            where
              ctorNeedsUpdate = flip Map.member termsToTypecheck -- todo: amend this to include constructor names for declsToTypecheck?
              termNeedsUpdate = flip Map.member termsToTypecheck -- todo: amend this to include constructor names for declsToTypecheck?
              updatedTypes :: Map Name TypeReference =
                Map.mapKeys Name.unsafeFromVar $
                  fmap (Reference.ReferenceDerived . fst) (UFE.datasId env)
                    <> fmap (Reference.ReferenceDerived . fst) (UFE.effectsId env)
       in traverse setupTerm termsToTypecheck
    let uf = UF.UnisonFileId (UFE.datasId env) (UFE.effectsId env) terms watches
          where
            terms :: [(V1.Symbol, V1.Ann {- ann for whole binding -}, V1.Term V1.Symbol V1.Ann)]
            terms = fmap (\(n, t) -> (Name.toVar n, mempty, t)) $ Map.toList updatedTerms
            watches :: Map V1.WatchKind [(V1.Symbol, V1.Ann {- ann for whole watch -}, V1.Term V1.Symbol V1.Ann)]
            watches = mempty
    pure uf
    where
      -- \| Perform substitions in a term for all the direct and indirect updates
      -- RefToName gives us our var names. It only needs to contain things that appear in typechecking.
      -- `dependents` and `updates` just give us the latest input version for term definitions.
      -- `dependents` includes things that need to be typechecked, and updates includes things that don't.
      -- What about patterns?
      substForTerm ::
        forall v a.
        (Var v, Eq a, Monoid a) =>
        RefToName ->
        (Name -> Bool) ->
        Map Name TypeReference ->
        Map Name V1.Referent ->
        (V1.Term v a) ->
        (V1.Term v a)
      substForTerm
        Defns {terms = ppeTerms, types = ppeTypes}
        termNeedsUpdate
        updatedTypes
        updatedTerms
        term =
          updatesTerm term
          where
            updatesTerm =
              flip (foldl' updateTermDeps) rtsTermRefs
                . flip (foldl' updatePatterns) allCtors
                . flip (foldl' updateSignatures) rtsTypeAnnRefs
                . flip (foldl' updateTypeLinks) rtsTypeLinks
                . flip (foldl' updateTermLinks) rtsTermLinks
              where
                allCtors = Set.map (,CT.Data) rtsDataCtors <> Set.map (,CT.Effect) rtsEffectCtors
                RefsToSubst {..} =
                  V1.Term.generalizedDependencies
                    V1.Term.GdHandler
                      { gdTermRef = \r -> mempty {rtsTermRefs = Set.singleton r},
                        gdTypeRef = \r -> mempty {rtsTypeAnnRefs = Set.singleton r},
                        gdLiteralType = const mempty,
                        gdDataCtor = \r i -> mempty {rtsDataCtors = Set.singleton (V1.ConstructorReference r i)},
                        gdEffectCtor = \r i -> mempty {rtsEffectCtors = Set.singleton (V1.ConstructorReference r i)},
                        gdTermLink = const mempty,
                        gdTypeLink = \r -> mempty {rtsTypeLinks = Set.singleton r},
                        gdLiteralPattern = const mempty,
                        gdDataPattern = \r i -> mempty {rtsDataPatterns = Set.singleton (V1.ConstructorReference r i)},
                        gdEffectPattern = \r i -> mempty {rtsEffectPatterns = Set.singleton (V1.ConstructorReference r i)}
                      }
                    term

                updatePatterns :: V1.Term v a -> (ConstructorReference, ConstructorType) -> V1.Term v a
                updatePatterns term (cr@(ConstructorReference typeRef conId), ct) =
                  Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteCasesLHS old new term) term
                  where
                    old = V1.Term.fromReferent mempty (V1.Referent.Con cr ct)
                    new :: Maybe (V1.Term v a)
                    new = do
                      name <- Map.lookup (Referent.Con typeRef conId) ppeTerms
                      case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                        (True, _) -> Nothing -- a pattern was deleted and replaced with a term dependent of another update. we can't do anything great here, and a warning would be nice
                        (False, Just V1.Referent.Ref {}) -> Nothing -- a pattern was deleted and replaced with a new term. a warning about this would be nice
                        (False, Just r@V1.Referent.Con {}) -> Just $ V1.Term.fromReferent mempty r
                        (False, Nothing) -> Nothing

                updateTypeLinks :: V1.Term v a -> TypeReference -> V1.Term v a
                updateTypeLinks term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    old = V1.Term.typeLink mempty ref
                    new = do
                      name <- Map.lookup ref ppeTypes
                      case Map.lookup name updatedTypes of
                        Just u -> Just $ V1.Term.typeLink mempty u
                        Nothing -> Nothing

                updateSignatures :: V1.Term v a -> TypeReference -> V1.Term v a
                updateSignatures term ref = Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteSignatures old new term) term
                  where
                    old = Type.ref mempty ref
                    new = do
                      name <- Map.lookup ref ppeTypes
                      case Map.lookup name updatedTypes of
                        Just u -> Just $ Type.ref mempty u
                        Nothing -> Nothing

                updateTermDeps :: V1.Term v a -> Reference -> V1.Term v a
                updateTermDeps term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    old = V1.Term.ref mempty ref
                    new = do
                      name <- Map.lookup (Referent.Ref ref) ppeTerms
                      case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                        (True, _) -> Just $ V1.Term.var mempty (Name.toVar name)
                        (_, Just u) -> Just $ V1.Term.fromReferent mempty u
                        (False, Nothing) -> Nothing

                updateTermLinks :: V1.Term v a -> V1.Referent -> V1.Term v a
                updateTermLinks term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    name :: Maybe Name = Map.lookup ref (error "substForTerm:updateTermLinks: unimplemented" ppeTerms)
                    old = V1.Term.termLink mempty ref
                    new =
                      name >>= \name -> case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                        (True, _) -> error $ "substForTerm: We can't set up a var for the termLink " ++ show name
                        (_, Just u) -> Just $ V1.Term.termLink mempty u
                        (False, Nothing) -> Nothing

      -- a. LCA:   foo#foo calls bar#bar, bar#bar calls baz#baz
      -- b. Alice: updates bar#bar2, autopropagates to foo#foo2
      -- c. Bob:   updates foo#foo3
      -- d. Alice updates: {bar#bar2}
      -- e. Bob updates: {foo#foo3}
      -- f. Bob dependents of Alice updates' names: {#foo3} -> {foo -> #foo3}
      -- g. Alice dependents of Bob updates' names: {}      -> {}
      -- h. What to typecheck: f union g = {#foo3}   or {foo -> #foo3}
      -- i. Combined updates {bar#bar2, foo#foo3}
      -- load #foo3, and encounter ref #bar

      -- \| Perform substutitions on decl constructor types for all the direct and indirect updates
      -- `ppe` we use to look up names for dependencies that will go into the new decl for checking. dependencies of decls can only be decls
      -- `declNeedsUpdate name` iff `name` is a dependent of one of the updated definitions.
      --  -- ^it may also be an updated definition itself
      -- `updates` are the latest versions of updated  definitions. We use the latest version from here if it's not a dependent of any other updates.
      -- Precondition: decl's constructor names are properly located from the namespace (WhateverDecl.WhateverTerm, because we will want those names in the output constructor)
      substForDecl :: forall v a. (Var v, Monoid a) => RefToName -> (Name -> Bool) -> Map Name TypeReference -> V1.Decl v a -> V1.Decl v a
      substForDecl Defns {types = ppe} declNeedsUpdate updatedTypes decl =
        V1.Decl.modifyAsDataDecl updateDecl decl
        where
          updateDecl decl = decl {V1.Decl.constructors' = map (over _3 updateCtorDependencies) $ V1.Decl.constructors' decl}
          updateCtorDependencies ctor = foldl' updateType ctor $ V1.Type.dependencies ctor
          updateType :: V1.Type v a -> Reference -> V1.Type v a
          updateType typ ref = Maybe.rewrite (\typ -> new >>= \new -> ABT.rewriteExpression old new typ) typ
            where
              old :: V1.Type v a
              old = Type.ref mempty ref
              -- A "dependent" is gonna be part of the typechecking, so it gets replaced with a var.
              -- An update that isn't also a dependent just gets replaced with the latest ref.
              -- A ref that corresponds to neither doesn't need to be replaced.
              new :: Maybe (V1.Type v a)
              new = do
                name <- Map.lookup ref ppe
                case (declNeedsUpdate name, Map.lookup name updatedTypes) of
                  -- for sure, not definition whose id triggers the `var` case
                  -- will end up in the scratch file, but some same-named definition will.
                  (True, _) -> Just $ Type.var mempty (Name.toVar name)
                  (False, Just u) -> Just $ Type.ref mempty u
                  (False, Nothing) -> Nothing

-- | `ppes` for everything that needs to be typechecked, and all their dependencies
-- loadTerm / loadDecl, optionally with caching
-- the terms to typecheck (the dependents of updates)
-- the updates themselves
computeUnisonFile2 ::
  MergeDatabase ->
  Defns (Map Referent Referent) (Map TypeReference TypeReference) ->
  Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  RefToName ->
  DeepRefsId' ->
  UpdatesRefnt ->
  Transaction (UnisonFile V1.Symbol V1.Ann)
computeUnisonFile2
  db@MergeDatabase {loadV1Decl, loadV1Term}
  unconflictedUpdates0
  dependents
  ppes
  (Defns {terms = termsToTypecheck, types = declsToTypecheck})
  Defns {terms = combinedTermUpdates0, types = combinedTypeUpdates} = do
    unconflictedUpdates <- do
      let f (x, y) = (,) <$> referent2to1 db x <*> referent2to1 db y
      traverseOf #terms (fmap Map.fromList . traverse f . Map.toList) unconflictedUpdates0
    combinedTermUpdates <- traverse (referent2to1 db) combinedTermUpdates0

    {-
    dependencies
    updates
    dependents

    right now: required: {dependents, required, dependencies optional
    later need {dependencies, updates, dependents}

    update type Foo = Foo #Bar

    -}

    decls :: Map V1.Symbol (V1.Decl V1.Symbol V1.Ann) <-
      namedDependents
        & view #types
        & Bimap.toMap
        & traverse (fmap (substituteDecl (unconflictedUpdates ^. #types) (namedDependents ^. #types)) . loadV1Decl)

    terms :: Map V1.Symbol (V1.Term V1.Symbol V1.Ann) <-
      namedDependents
        & view #terms
        & Bimap.toMap
        & traverse (fmap (substituteTerm unconflictedUpdates namedDependents undefined undefined undefined undefined) . loadV1Term)

    let -- todo: handle errors better:
        env :: UFE.Env V1.Symbol V1.Ann = (fromRight' . fromRight') envResult
          where
            -- This `Names` can be `mempty` because there shouldn't be any free vars in these declarations.
            -- There will be references that come from the codebase, or there will be vars that we just substed in
            -- right now for other decls listed here.
            envResult = UFN.environmentFor mempty dataDeclarations effectDeclarations
            effectDeclarations :: Map V1.Symbol (V1.Decl.EffectDeclaration V1.Symbol V1.Ann)
            dataDeclarations :: Map V1.Symbol (V1.Decl.DataDeclaration V1.Symbol V1.Ann)
            (fmap fromLeft' -> effectDeclarations, fmap fromRight' -> dataDeclarations) =
              Map.partition (\case Left {} -> True; Right {} -> False) decls
    updatedTerms <-
      let setupTerm = fmap (substForTerm ppes termNeedsUpdate updatedTypes combinedTermUpdates) . loadV1Term
            where
              ctorNeedsUpdate = flip Map.member termsToTypecheck -- todo: amend this to include constructor names for declsToTypecheck?
              termNeedsUpdate = flip Map.member termsToTypecheck -- todo: amend this to include constructor names for declsToTypecheck?
              updatedTypes :: Map Name TypeReference =
                Map.mapKeys Name.unsafeFromVar $
                  fmap (Reference.ReferenceDerived . fst) (UFE.datasId env)
                    <> fmap (Reference.ReferenceDerived . fst) (UFE.effectsId env)
       in traverse setupTerm termsToTypecheck
    let uf = UF.UnisonFileId (UFE.datasId env) (UFE.effectsId env) terms watches
          where
            terms :: [(V1.Symbol, V1.Ann {- ann for whole binding -}, V1.Term V1.Symbol V1.Ann)]
            terms = fmap (\(n, t) -> (Name.toVar n, mempty, t)) $ Map.toList updatedTerms
            watches :: Map V1.WatchKind [(V1.Symbol, V1.Ann {- ann for whole watch -}, V1.Term V1.Symbol V1.Ann)]
            watches = mempty
    pure uf
    where
      -- \| Perform substitions in a term for all the direct and indirect updates
      -- RefToName gives us our var names. It only needs to contain things that appear in typechecking.
      -- `dependents` and `updates` just give us the latest input version for term definitions.
      -- `dependents` includes things that need to be typechecked, and updates includes things that don't.
      -- What about patterns?
      substForTerm ::
        forall v a.
        (Var v, Eq a, Monoid a) =>
        RefToName ->
        (Name -> Bool) ->
        Map Name TypeReference ->
        Map Name V1.Referent ->
        (V1.Term v a) ->
        (V1.Term v a)
      substForTerm
        Defns {terms = ppeTerms, types = ppeTypes}
        termNeedsUpdate
        updatedTypes
        updatedTerms
        term =
          updatesTerm term
          where
            updatesTerm =
              flip (foldl' updateTermDeps) rtsTermRefs
                . flip (foldl' updatePatterns) allCtors
                . flip (foldl' updateSignatures) rtsTypeAnnRefs
                . flip (foldl' updateTypeLinks) rtsTypeLinks
                . flip (foldl' updateTermLinks) rtsTermLinks
              where
                allCtors = Set.map (,CT.Data) rtsDataCtors <> Set.map (,CT.Effect) rtsEffectCtors
                RefsToSubst {..} =
                  V1.Term.generalizedDependencies
                    V1.Term.GdHandler
                      { gdTermRef = \r -> mempty {rtsTermRefs = Set.singleton r},
                        gdTypeRef = \r -> mempty {rtsTypeAnnRefs = Set.singleton r},
                        gdLiteralType = const mempty,
                        gdDataCtor = \r i -> mempty {rtsDataCtors = Set.singleton (V1.ConstructorReference r i)},
                        gdEffectCtor = \r i -> mempty {rtsEffectCtors = Set.singleton (V1.ConstructorReference r i)},
                        gdTermLink = const mempty,
                        gdTypeLink = \r -> mempty {rtsTypeLinks = Set.singleton r},
                        gdLiteralPattern = const mempty,
                        gdDataPattern = \r i -> mempty {rtsDataPatterns = Set.singleton (V1.ConstructorReference r i)},
                        gdEffectPattern = \r i -> mempty {rtsEffectPatterns = Set.singleton (V1.ConstructorReference r i)}
                      }
                    term

                updatePatterns :: V1.Term v a -> (ConstructorReference, ConstructorType) -> V1.Term v a
                updatePatterns term (cr@(ConstructorReference typeRef conId), ct) =
                  Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteCasesLHS old new term) term
                  where
                    old = V1.Term.fromReferent mempty (V1.Referent.Con cr ct)
                    new :: Maybe (V1.Term v a)
                    new = do
                      name <- Map.lookup (Referent.Con typeRef conId) ppeTerms
                      case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                        (True, _) -> Nothing -- a pattern was deleted and replaced with a term dependent of another update. we can't do anything great here, and a warning would be nice
                        (False, Just V1.Referent.Ref {}) -> Nothing -- a pattern was deleted and replaced with a new term. a warning about this would be nice
                        (False, Just r@V1.Referent.Con {}) -> Just $ V1.Term.fromReferent mempty r
                        (False, Nothing) -> Nothing

                updateTypeLinks :: V1.Term v a -> TypeReference -> V1.Term v a
                updateTypeLinks term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    old = V1.Term.typeLink mempty ref
                    new = do
                      name <- Map.lookup ref ppeTypes
                      case Map.lookup name updatedTypes of
                        Just u -> Just $ V1.Term.typeLink mempty u
                        Nothing -> Nothing

                updateSignatures :: V1.Term v a -> TypeReference -> V1.Term v a
                updateSignatures term ref = Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteSignatures old new term) term
                  where
                    old = Type.ref mempty ref
                    new = do
                      name <- Map.lookup ref ppeTypes
                      case Map.lookup name updatedTypes of
                        Just u -> Just $ Type.ref mempty u
                        Nothing -> Nothing

                updateTermDeps :: V1.Term v a -> Reference -> V1.Term v a
                updateTermDeps term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    old = V1.Term.ref mempty ref
                    new = do
                      name <- Map.lookup (Referent.Ref ref) ppeTerms
                      case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                        (True, _) -> Just $ V1.Term.var mempty (Name.toVar name)
                        (_, Just u) -> Just $ V1.Term.fromReferent mempty u
                        (False, Nothing) -> Nothing

                updateTermLinks :: V1.Term v a -> V1.Referent -> V1.Term v a
                updateTermLinks term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    name :: Maybe Name = Map.lookup ref (error "substForTerm:updateTermLinks: unimplemented" ppeTerms)
                    old = V1.Term.termLink mempty ref
                    new =
                      name >>= \name -> case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                        (True, _) -> error $ "substForTerm: We can't set up a var for the termLink " ++ show name
                        (_, Just u) -> Just $ V1.Term.termLink mempty u
                        (False, Nothing) -> Nothing

      -- give unique name to each type and term in dependents
      namedDependents :: forall v. Var v => Defns (Bimap v TermReferenceId) (Bimap v TypeReferenceId)
      namedDependents =
        (`evalState` Set.empty) do
          terms <- generateNamesFor (dependents ^. #terms)
          types <- generateNamesFor (dependents ^. #types)
          pure Defns {terms, types}
        where
          generateNamesFor :: Ord ref => Set ref -> State (Set v) (Bimap v ref)
          generateNamesFor =
            foldlM
              ( \acc ref -> do
                  var <- fresh
                  pure $! Bimap.insert var ref acc
              )
              Bimap.empty

          fresh :: State (Set v) v
          fresh = do
            used <- State.get
            let var = Var.freshIn used (Var.typed Var.Propagate)
            State.put $! Set.insert var used
            pure var

substituteDecl ::
  forall a v.
  (Monoid a, Var v) =>
  Map TypeReference TypeReference ->
  Bimap v TypeReferenceId ->
  V1.Decl v a ->
  V1.Decl v a
substituteDecl unconflictedUpdates namedDependents =
  over (V1.Decl.dataDecl_ . V1.Decl.constructors_ . mapped . _3) substituteConstructor
  where
    substituteConstructor :: V1.Type v a -> V1.Type v a
    substituteConstructor ctor =
      foldr
        ( \old typ ->
            fromMaybe typ do
              new <- replaceTypeReference unconflictedUpdates namedDependents old
              ABT.rewriteExpression (Type.ref mempty old) new typ
        )
        ctor
        (V1.Type.dependencies ctor)

replaceTypeReference ::
  forall a v.
  (Monoid a, Ord v) =>
  Map TypeReference TypeReference ->
  Bimap v TypeReferenceId ->
  TypeReference ->
  Maybe (V1.Type v a)
replaceTypeReference unconflictedUpdates namedDependents old = do
  asum
    [ Type.var mempty <$> dependentTypeName old,
      Type.ref mempty <$> Map.lookup old unconflictedUpdates
    ]
  where
    dependentTypeName :: TypeReference -> Maybe v
    dependentTypeName = \case
      ReferenceBuiltin _ -> Nothing
      ReferenceDerived rid -> Bimap.lookupR rid namedDependents

-- TODO update links after type checking
substituteTerm ::
  forall v a.
  (Var v, Eq a, Monoid a) =>
  Defns (Map V1.Referent V1.Referent) (Map TypeReference TypeReference) ->
  Defns (Bimap v TermReferenceId) (Bimap v TypeReferenceId) ->
  RefToName ->
  (Name -> Bool) ->
  Map Name TypeReference ->
  Map Name V1.Referent ->
  V1.Term v a ->
  V1.Term v a
substituteTerm unconflictedUpdates namedDependents Defns {terms = ppeTerms, types = ppeTypes} termNeedsUpdate updatedTypes updatedTerms term =
  let RefsToSubst {rtsDataCtors, rtsDataPatterns, rtsEffectCtors, rtsEffectPatterns, rtsTermRefs, rtsTypeAnnRefs} =
        V1.Term.generalizedDependencies
          V1.Term.GdHandler
            { gdDataCtor = \r i -> mempty {rtsDataCtors = Set.singleton (V1.ConstructorReference r i)},
              gdDataPattern = \r i -> mempty {rtsDataPatterns = Set.singleton (V1.ConstructorReference r i)},
              gdEffectCtor = \r i -> mempty {rtsEffectCtors = Set.singleton (V1.ConstructorReference r i)},
              gdEffectPattern = \r i -> mempty {rtsEffectPatterns = Set.singleton (V1.ConstructorReference r i)},
              gdLiteralPattern = const mempty,
              gdLiteralType = const mempty,
              gdTermLink = const mempty,
              gdTermRef = \r -> mempty {rtsTermRefs = Set.singleton r},
              gdTypeLink = const mempty,
              gdTypeRef = \r -> mempty {rtsTypeAnnRefs = Set.singleton r}
            }
          term
   in appEndo
        ( fold
            [ foldMap (Endo . substituteDataConstructorReferenceInApplication) rtsDataCtors,
              foldMap (Endo . substituteTermReference) rtsTermRefs,
              foldMap (Endo . updatePatterns) (Set.map (,CT.Data) rtsDataCtors <> Set.map (,CT.Effect) rtsEffectCtors),
              foldMap (Endo . substituteTypeReferenceInSignature) rtsTypeAnnRefs
            ]
        )
        term
  where
    substituteDataConstructorReferenceInApplication :: ConstructorReference -> V1.Term v a -> V1.Term v a
    substituteDataConstructorReferenceInApplication old term =
      fromMaybe term do
        new <-
          asum
            [ V1.Term.var mempty <$> dependentTermName old,
              V1.Term.fromReferent mempty <$> Map.lookup (V1.Referent.Ref old) (unconflictedUpdates ^. #terms)
            ]
        ABT.rewriteExpression (V1.Term.ref mempty old) new term
      where
        dependentTermName :: TermReference -> Maybe v
        dependentTermName = \case
          ReferenceBuiltin _ -> Nothing
          ReferenceDerived rid -> Bimap.lookupR rid (namedDependents ^. #terms)

    updatePatterns :: (ConstructorReference, ConstructorType) -> V1.Term v a -> V1.Term v a
    updatePatterns (cr@(ConstructorReference typeRef conId), ct) term =
      -- Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteCasesLHS old new term) term
      -- old = V1.Term.fromReferent mempty (V1.Referent.Con cr ct)
      -- new :: Maybe (V1.Term v a)
      -- new = do
      --   name <- Map.lookup (Referent.Con typeRef conId) ppeTerms
      --   case (termNeedsUpdate name, Map.lookup name updatedTerms) of
      --     (True, _) -> Nothing -- a pattern was deleted and replaced with a term dependent of another update. we can't do anything great here, and a warning would be nice
      --     (False, Just V1.Referent.Ref {}) -> Nothing -- a pattern was deleted and replaced with a new term. a warning about this would be nice
      --     (False, Just r@V1.Referent.Con {}) -> Just $ V1.Term.fromReferent mempty r
      --     (False, Nothing) -> Nothing
      fromMaybe term do
        undefined

    substituteTypeReferenceInSignature :: TypeReference -> V1.Term v a -> V1.Term v a
    substituteTypeReferenceInSignature old term =
      fromMaybe term do
        new <- replaceTypeReference (unconflictedUpdates ^. #types) (namedDependents ^. #types) old
        V1.Term.rewriteSignatures (Type.ref mempty old) new term

    substituteTermReference :: TermReference -> V1.Term v a -> V1.Term v a
    substituteTermReference old term =
      fromMaybe term do
        new <-
          asum
            [ V1.Term.var mempty <$> dependentTermName old,
              V1.Term.fromReferent mempty <$> Map.lookup (V1.Referent.Ref old) (unconflictedUpdates ^. #terms)
            ]
        ABT.rewriteExpression (V1.Term.ref mempty old) new term
      where
        dependentTermName :: TermReference -> Maybe v
        dependentTermName = \case
          ReferenceBuiltin _ -> Nothing
          ReferenceDerived rid -> Bimap.lookupR rid (namedDependents ^. #terms)

data RefsToSubst = RefsToSubst
  { rtsTypeAnnRefs :: Set V1.TypeReference,
    rtsTermRefs :: Set V1.TermReference,
    rtsDataCtors :: Set V1.ConstructorReference,
    rtsEffectCtors :: Set V1.ConstructorReference,
    rtsDataPatterns :: Set V1.ConstructorReference,
    rtsEffectPatterns :: Set V1.ConstructorReference,
    rtsTypeLinks :: Set V1.TypeReference,
    rtsTermLinks :: Set V1.Referent
  }
  deriving (Eq, Ord)

instance Semigroup RefsToSubst where
  RefsToSubst a1 b1 c1 d1 e1 f1 g1 h1 <> RefsToSubst a2 b2 c2 d2 e2 f2 g2 h2 =
    RefsToSubst (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2) (h1 <> h2)

instance Monoid RefsToSubst where
  mempty = RefsToSubst mempty mempty mempty mempty mempty mempty mempty mempty

-- typecheck :: Codebase IO v a -> UnisonFile v a -> Transaction (Either (Seq (Result.Note v a)) (TypecheckedUnisonFile v a))
-- typecheck codebase uf = do
--   Unison.Syntax.FileParser.checkForDuplicateTermsAndConstructors' err uf
--   env <-
--     FileParsers.computeTypecheckingEnvironment
--       FileParsers.ShouldUseTndr'No
--       mempty -- ambient abilities
--       (Codebase.typeLookupForDependencies codebase)
--       uf
--   Except.runExceptT . Result.toEither $ synthesizeFile env uf
--   where
--     err _ = error ""

-- Q: Does this return all of the updates? A: suspect no currently

-- Question: What should these input types be?
-- drAlice and drBob could be `DeepRefs` because that's what the diff gives us,
-- or they could be `DeepRefsId` because they're also the set of possibilities for typechecking, and we wouldn't be needing to typecheck any builtins
-- it CAN'T be `DeepRefsId` because they're also the set of dependencies for looking up dependents, and if someone replaced a term with a ctor,
-- we need to find that ctor by name and look up its dependents.

-- Hypothesis: This implementation would be much simpler if we had a richer dependency lookup.
-- As it stands, we can't look up the dependents of a constructor or pattern; we instead look up the dependents of the decl they came from (which is weird?)
-- Similarly, when we ask "which <thing we can look up can lookup dependents for> is named <x>", and we want to look up constructors, that's weird.
-- - term replaced with ctor: look up dependents of term in the opposing branch (ok)
-- - ctor replaced with term: look up dependents of ctor in the opposing branch (we'll look up dependents of the decl instead, getting excess results, but it's ok)
-- This seems okay. So disregard.

-- Question: What happens if I update a ctor?
whatToTypecheck :: (DeepRefs, UpdatesRefnt) -> (DeepRefs, UpdatesRefnt) -> Transaction DeepRefsId'
whatToTypecheck (drAlice, aliceUpdates) (drBob, bobUpdates) = do
  -- 1. for each update, determine the corresponding old dependent
  let -- \| Find the `Reference.Id`s that comprise a namespace. Constructors show up as decl Ids.
      makeScope dr = Set.fromList $ doTerms (dr ^. #terms) <> doTypes (dr ^. #types)
        where
          doTerms :: Map Name Referent -> [Reference.Id]
          doTerms = mapMaybe Referent.toReferenceId . Map.elems
          doTypes :: Map Name TypeReference -> [Reference.Id]
          doTypes = mapMaybe Reference.toId . Map.elems

  let -- \| Returns things from `scope` that have the same names as things in `updates` updates
      -- the return type should be "things that can be dependencies in the db".
      -- This implementation returns a decl reference in place of a constructor reference.
      getByCorrespondingName :: DeepRefs -> UpdatesRefnt -> Set Reference
      getByCorrespondingName scope updates = Set.fromList $ doTerms (updates ^. #terms) <> doTypes (updates ^. #types)
        where
          -- \| doTerms will return a TypeReference from a Constructor
          doTerms :: Map Name Referent -> [Reference]
          doTerms = mapMaybe f . Map.keys
          f :: Name -> Maybe Reference
          f name = Referent.toReference <$> Map.lookup name (scope ^. #terms)
          doTypes :: Map Name TypeReference -> [Reference]
          doTypes = mapMaybe g . Map.keys
          g :: Name -> Maybe Reference
          g name = Map.lookup name (scope ^. #types)

  -- these dependents are just term/decl ids
  aliceDependents <- Ops.dependentsWithinScope (makeScope drAlice) (getByCorrespondingName drAlice bobUpdates)
  bobDependents <- Ops.dependentsWithinScope (makeScope drBob) (getByCorrespondingName drBob aliceUpdates)

  -- we want the latest versions of things to be typechecked.
  -- we're assuming aliceDependents / bobDependents includes everything that we want to typecheck (currently this means terms and decls)
  -- but where do constructors enter into this? and if we leave constructors out (which we do, below), where do we get constructor names for printing the decls?
  let prefer primary name secondary _ignored = case Map.lookup name primary of Just c -> c; Nothing -> secondary
  let latestDefn :: forall r. Ord r => Map Name r -> (Map Name r, Set r) -> (Map Name r, Set r) -> Map Name r
      latestDefn updates (aliceNames, aliceDefs) (bobNames, bobDefs) =
        Map.unionWithKey (prefer updates) aliceNamedDefs bobNamedDefs
        where
          -- we'll collect names for just the dependents, and then make sure we have the latest of each
          aliceNamedDefs :: Map Name r = filterDeepRefs aliceNames aliceDefs
          bobNamedDefs :: Map Name r = filterDeepRefs bobNames bobDefs
          filterDeepRefs :: Map Name r -> Set r -> Map Name r
          filterDeepRefs dr deps = Map.filter (flip Set.member deps) dr
      filterDependents rt m = Set.fromList [r | (r, t) <- Map.toList m, t == rt]

  let -- combinedUpdates — stuff can be updated to a constructor
      -- however, we're looking for the latest version of the thing to typecheck, and we can't typecheck a constructor
      --  can we typecheck a constructor? we just have to put its decl in instead? / too?
      combinedUpdates =
        Defns
          { terms = (aliceUpdates ^. #terms) <> (bobUpdates ^. #terms),
            types = (aliceUpdates ^. #types) <> (bobUpdates ^. #types)
          }

  -- todo: there's something confusing here about constructor names.
  --       the names should come from the diffs and the updates
  let -- terms to typecheck. they'll be Ids because that's all we have in the database as dependents.
      latestTermDependents :: Map Name TermReferenceId
      latestTermDependents = latestDefn updates' (setup drAlice aliceDependents) (setup drBob bobDependents)
        where
          setup :: DeepRefs -> Map Reference.Id ReferenceType -> (Map Name TermReferenceId, Set TermReferenceId)
          setup dr dependents = (dropCtorsAndBuiltins (dr ^. #terms), filterDependents RtTerm dependents)
          dropCtorsAndBuiltins = Map.mapMaybe Referent.toReferenceId
          updates' = dropCtorsAndBuiltins $ combinedUpdates ^. #terms

  let -- decls to typecheck
      latestTypeDependents :: Map Name TypeReferenceId
      latestTypeDependents = latestDefn updates' (setup drAlice aliceDependents) (setup drBob bobDependents)
        where
          setup :: DeepRefs -> Map Reference.Id ReferenceType -> (Map Name TypeReferenceId, Set TypeReferenceId)
          setup dr dependents = (dropBuiltins (dr ^. #types), filterDependents RtType dependents)
          dropBuiltins = Map.mapMaybe \case Reference.ReferenceDerived r -> Just r; _ -> Nothing
          updates' = dropBuiltins $ combinedUpdates ^. #types
  -- reminder: we're basically only typechecking the dependents; the updates themselves have already been typechecked.
  pure $ Defns latestTermDependents latestTypeDependents

data MergeOutput v a = MergeProblem
  { definitions ::
      Defns
        (Map Name (ConflictOrGood (V1.Term v a)))
        (Map Name (ConflictOrGood (TypeReference, V1.Decl v a)))
  }
  deriving stock (Generic)

instance Ord v => Functor (MergeOutput v) where
  fmap f (MergeProblem Defns {terms, types}) =
    MergeProblem
      ( Defns
          { terms = fmap (fmap (V1.Term.amap f)) terms,
            types = fmap (fmap (second $ V1.Decl.amap f)) types
          }
      )

data ScratchDefn v a = SdTerm (V1.Term v a) | SdDecl TypeReference (V1.Decl v a) -- could also be a builtin alias

data ConflictOrGood a
  = Conflict (Conflict ProjectBranchName a)
  | Good a
  deriving stock (Functor)

data Conflict branch a
  = ConflictUnknown !branch !branch !a !a
  deriving stock (Functor)

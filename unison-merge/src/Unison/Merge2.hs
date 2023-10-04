{-# LANGUAGE RecordWildCards #-}

module Unison.Merge2
  ( -- * Library dependencies
    mergeLibdeps,

    -- * Namespace diff
    nameBasedNamespaceDiff,

    -- * Typechecking
    computeUnisonFile,

    -- * Misc / organize these later
    DiffOp (..),
    Defns (..),
    DefnsA,
    DefnsB,
    NamespaceTree,
    TwoWay (..),
    TwoOrThreeWay (..),
  )
where

import Control.Lens (over, _3)
import Data.Either.Combinators (fromLeft', fromRight')
import Data.Foldable qualified as Foldable
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Tuple qualified as Tuple
import U.Codebase.Reference
  ( Reference,
    ReferenceType (RtTerm, RtType),
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.ABT qualified as ABT
import Unison.ConstructorReference qualified as V1
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Hash (Hash)
import Unison.Merge.Diff (TwoOrThreeWay (..), TwoWay (..), nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Libdeps (mergeLibdeps)
import Unison.Merge.NamespaceTypes (Defns (..), DefnsA, DefnsB, NamespaceTree)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Reference qualified as C
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as V1
import Unison.Term qualified as V1.Term
import Unison.Type qualified as Type
import Unison.Type qualified as V1
import Unison.Type qualified as V1.Type
import Unison.UnisonFile.Env qualified as UFE
import Unison.UnisonFile.Names qualified as UFN
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile.Type qualified as UF
import Unison.Util.Maybe qualified as Maybe
import Unison.Var (Var)
import Unison.WatchKind qualified as V1

newtype SynHash = SynHash Hash deriving (Eq, Ord, Show) via SynHash

-- | DeepRefs is basically a one-way Names (many to one, rather than many to many)
-- It can represent the input or output namespace.
-- It includes constructors, which many other contexts here don't.
data DeepRefs = DeepRefs
  { drTerms :: Map Name V1.Referent,
    drTypes :: Map Name TypeReference
  }

data DeepRefsId = DeepRefsId
  { drTermsId :: Map Name V1.Referent.Id,
    drTypesId :: Map Name TypeReferenceId
  }

-- | DeepRefs' maps names to just Terms and Types but not individual constructors
data DeepRefs' = DeepRefs'
  { drTerms' :: Map Name TermReference,
    drTypes' :: Map Name TypeReference
  }

-- | DeepRefsId' is like DeepRefs', but maps to Ids instead of References.
-- It could be used, for example, to represent a set of Derived definitions to typecheck
data DeepRefsId' = DeepRefsId'
  { drTermsId' :: Map Name TermReferenceId,
    drTypesId' :: Map Name TypeReferenceId
  }

data RefToName = RefToName
  { rtnTerms :: Map V1.Referent Name,
    rtnTypes :: Map TypeReference Name
  }

data RefIdToName = RefIdToName
  { rtnTermsId :: Map TermReferenceId Name,
    rtnTypesId :: Map TypeReferenceId Name
  }

-- | SynHashes are computed and used to detect add/update conflicts
data SynHashes = SynHashes
  { shTerms :: Map Name SynHash,
    shTypes :: Map Name SynHash
  }

data DiffConflicts = DiffConflicts
  { dcTerms :: Set Name,
    dcTypes :: Set Name
  }

-- | Updates include whatever a branch updated
-- Or it can represent the union of branches, if there are no conflicts.
-- Updates can't rely on only Ids, because you can update something to a builtin!
data Updates = Updates
  { updatedTerms :: Map Name TermReference,
    updatedTypes :: Map Name TypeReference
  }

data UpdatesId = UpdatesId
  { updatedTermsId :: Map Name TermReferenceId,
    updatedTypesId :: Map Name TypeReferenceId
  }

-- for updated definitions, we want to know which branch to find the updated version in
-- for transitive dependents of updates (which are not updates themselves), we don't care which version we get
-- for conflicted definitions, we need to print both versions, but we don't typecheck anything

-- | Includes the transitive deps within some namespace / DeepRefs
data TransitiveDeps = TransitiveDeps
  { tdTerms :: Map Name TermReferenceId,
    tdTypes :: Map Name TypeReferenceId
  }

deepRefsToPPE :: DeepRefsId' -> RefIdToName
deepRefsToPPE
  DeepRefsId' {drTermsId', drTypesId'} =
    RefIdToName
      (swapMap drTermsId')
      (swapMap drTypesId')
    where
      swapMap = Map.fromList . map Tuple.swap . Map.toList

type LoadTerm m = TermReferenceId -> m ()

type LoadDecl m = TypeReferenceId -> m ()

computeSyntacticHashes :: Applicative m => LoadTerm m -> LoadDecl m -> DeepRefs -> PrettyPrintEnv -> m SynHashes
computeSyntacticHashes loadTerm loadDecl deepRefs ppe = pure wundefined

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
  forall v a.
  (Var v, Monoid a, Eq a) =>
  RefToName ->
  (TermReferenceId -> Transaction (V1.Term v a)) ->
  (TypeReferenceId -> Transaction (V1.Decl v a)) ->
  WhatToTypecheck ->
  Updates ->
  Transaction (UnisonFile v a)
computeUnisonFile
  ppes
  loadTerm
  loadDecl
  (unWhatToTypecheck -> DeepRefsId' {drTermsId' = termsToTypecheck, drTypesId' = declsToTypecheck})
  Updates {updatedTerms = combinedTermUpdates, updatedTypes = combinedTypeUpdates} = do
    updatedDecls <-
      let setupDecl = fmap (substForDecl ppes declNeedsUpdate combinedTypeUpdates) . loadDecl
            where
              declNeedsUpdate = flip Map.member declsToTypecheck
       in traverse setupDecl declsToTypecheck
    let -- todo: handle errors better:
        env :: UFE.Env v a = (fromRight' . fromRight') envResult
          where
            envResult = UFN.environmentFor mempty dataDeclarations effectDeclarations
            effectDeclarations :: Map v (V1.Decl.EffectDeclaration v a)
            dataDeclarations :: Map v (V1.Decl.DataDeclaration v a)
            (fmap fromLeft' -> effectDeclarations, fmap fromRight' -> dataDeclarations) =
              updatedDecls
                & Map.mapKeys Name.toVar
                & Map.partition (\case Left {} -> True; Right {} -> False)
    updatedTerms <-
      let setupTerm = fmap (substForTerm ppes termNeedsUpdate updatedTypes updatedConstructors combinedTermUpdates) . loadTerm
            where
              termNeedsUpdate = flip Map.member termsToTypecheck
              updatedTypes :: Map Name TypeReference =
                Map.mapKeysMonotonic Name.unsafeFromVar $
                  fmap (Reference.ReferenceDerived . fst) (UFE.datasId env)
                    <> fmap (Reference.ReferenceDerived . fst) (UFE.effectsId env)
              declsId :: Map v (Reference.Id, V1.Decl v a)
              declsId = fmap (second Left) (UFE.effectsId env) <> fmap (second Right) (UFE.datasId env)
              updatedConstructors :: Map Name V1.Referent.Referent =
                Map.fromList
                  [ (Name.joinDot declName ctorName, V1.Referent.Con (V1.ConstructorReference r ctorId) ct)
                    | (vDecl, (id, decl)) <- Map.toList declsId,
                      let declName = Name.unsafeFromVar vDecl,
                      let ct = V1.Decl.constructorType decl,
                      let r = Reference.ReferenceDerived id,
                      (ctorId, vCtor) <- zip [0 ..] (V1.Decl.constructorVars (V1.Decl.asDataDecl decl)),
                      let ctorName = Name.unsafeFromVar vCtor
                  ]
                  <> Map.fromList []
       in traverse setupTerm termsToTypecheck
    let uf = UF.UnisonFileId (UFE.datasId env) (UFE.effectsId env) terms watches
          where
            terms :: [(v, a {- ann for whole binding -}, V1.Term v a)]
            terms = fmap (\(n, t) -> (Name.toVar n, mempty :: a, t)) $ Map.toList updatedTerms
            watches :: Map V1.WatchKind [(v, a {- ann for whole watch -}, V1.Term v a)]
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
        Map Name TermReference ->
        (V1.Term v a) ->
        (V1.Term v a)
      substForTerm
        RefToName {rtnTerms = ppeTerms, rtnTypes = ppeTypes}
        termNeedsUpdate
        updatedTypes
        updatedConstructors
        updatedTerms
        term =
          updatesTerm term
          where
            updatesTerm =
              flip (foldl' updateTermDeps) rtsTermRefs
                . flip (foldl' updatePatterns) allCtors
                . flip (foldl' updateConstructors) allCtors
                . flip (foldl' updateSignatures) rtsTypeAnnRefs
                . flip (foldl' updateTypeLinks) rtsTypeLinks
                . flip (foldl' updateTermLinks) rtsTermLinks
              where
                allCtors = Set.map (,CT.Data) rtsDataCtors <> Set.map (,CT.Effect) rtsEffectCtors
                RefsToSubst {..} =
                  mconcat . Foldable.toList $
                    V1.Term.generalizedDependencies
                      V1.Term.GdHandler
                        { gdTermRef = \r -> mempty {rtsTermRefs = Set.singleton r},
                          gdTypeRef = \r -> mempty {rtsTypeAnnRefs = Set.singleton r},
                          gdLiteralType = const mempty,
                          gdDataCtor = \r i -> mempty {rtsDataCtors = Set.singleton (V1.ConstructorReference r i)},
                          gdDataCtorType = const mempty,
                          gdEffectCtor = \r i -> mempty {rtsEffectCtors = Set.singleton (V1.ConstructorReference r i)},
                          gdEffectCtorType = const mempty,
                          gdTermLink = const mempty,
                          gdTypeLink = \r -> mempty {rtsTypeLinks = Set.singleton r}
                        }
                      term

                updateConstructors :: V1.Term v a -> (V1.ConstructorReference, ConstructorType) -> V1.Term v a
                updateConstructors term (cr, ct) = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    ctor = V1.Referent.Con cr ct
                    old = V1.Term.fromReferent mempty ctor
                    name :: Name = fromJust $ Map.lookup ctor ppeTerms
                    var :: v = Name.toVar name
                    new :: Maybe (V1.Term v a)
                    new = case (termNeedsUpdate name, Map.lookup name updatedTerms, Map.lookup name updatedConstructors) of
                      (True, _, _) -> Just $ V1.Term.var mempty var
                      (False, Just u, _) -> Just $ V1.Term.ref mempty u
                      (False, Nothing, Just r) -> Just $ V1.Term.fromReferent mempty r
                      (False, Nothing, Nothing) -> Nothing

                updatePatterns :: V1.Term v a -> (V1.ConstructorReference, ConstructorType) -> V1.Term v a
                updatePatterns term (cr, ct) = Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteCasesLHS old new term) term
                  where
                    ctor = V1.Referent.Con cr ct
                    old = V1.Term.fromReferent mempty ctor
                    name :: Name = fromJust $ Map.lookup ctor ppeTerms
                    new :: Maybe (V1.Term v a)
                    new = case (termNeedsUpdate name, Map.lookup name updatedTerms, Map.lookup name updatedConstructors) of
                      (True, _, _) -> Nothing -- a pattern was deleted and replaced with a term dependent of another update. we can't do anything great here, and a warning would be nice
                      (False, Just {}, _) -> Nothing -- a pattern was deleted and replaced with a new term. a warning about this would be nice
                      (False, Nothing, Just r) -> Just $ V1.Term.fromReferent mempty r
                      (False, Nothing, Nothing) -> Nothing

                updateTypeLinks :: V1.Term v a -> TypeReference -> V1.Term v a
                updateTypeLinks term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    old = V1.Term.typeLink mempty ref
                    name :: Name = fromJust $ Map.lookup ref ppeTypes
                    new = case Map.lookup name updatedTypes of
                      Just u -> Just $ V1.Term.typeLink mempty u
                      Nothing -> Nothing

                updateSignatures :: V1.Term v a -> TypeReference -> V1.Term v a
                updateSignatures term ref = Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteSignatures old new term) term
                  where
                    name :: Name = fromJust $ Map.lookup ref ppeTypes
                    old = Type.ref mempty ref
                    new = case Map.lookup name updatedTypes of
                      Just u -> Just $ Type.ref mempty u
                      Nothing -> Nothing

                updateTermDeps :: V1.Term v a -> Reference -> V1.Term v a
                updateTermDeps term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    name :: Name = fromJust $ Map.lookup (V1.Referent.Ref ref) ppeTerms
                    var = Name.toVar name
                    old = V1.Term.ref mempty ref
                    new = case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                      (True, _) -> Just $ V1.Term.var mempty var
                      (_, Just u) -> Just $ V1.Term.ref mempty u
                      (False, Nothing) -> Nothing

                updateTermLinks :: V1.Term v a -> V1.Referent -> V1.Term v a
                updateTermLinks term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
                  where
                    name :: Name = fromJust $ Map.lookup ref (error "substForTerm:updateTermLinks: unimplemented" ppeTerms)
                    old = V1.Term.termLink mempty ref
                    new = case (termNeedsUpdate name, Map.lookup name updatedTerms) of
                      (True, _) -> error $ "substForTerm: We can't set up a var for the termLink " ++ show name
                      (_, Just u) -> Just $ V1.Term.typeLink mempty u
                      (False, Nothing) -> Nothing

      -- \| Perform substutitions on decl constructor types for all the direct and indirect updates
      -- `ppe` we use to look up names for dependencies that will go into the new decl for checking. dependencies of decls can only be decls
      -- `declNeedsUpdate name` iff `name` is a dependent of one of the updated definitions.
      -- `updates` are the latest versions of updated  definitions. We use the latest version from here if it's not a dependent of any other updates.
      -- Precondition: decl's constructor names are properly located from the namespace (WhateverDecl.WhateverTerm, because we will want those names in the output constructor)
      substForDecl :: forall v a. (Var v, Monoid a) => RefToName -> (Name -> Bool) -> Map Name TypeReference -> (V1.Decl v a) -> (V1.Decl v a)
      substForDecl RefToName {rtnTypes = ppe} declNeedsUpdate updatedTypes decl =
        V1.Decl.modifyAsDataDecl updateDecl decl
        where
          updateDecl decl = decl {V1.Decl.constructors' = map (over _3 updateCtorDependencies) $ V1.Decl.constructors' decl}
          updateCtorDependencies ctor = foldl' updateType ctor $ V1.Type.dependencies ctor
          updateType :: V1.Type v a -> Reference -> V1.Type v a
          updateType typ ref = Maybe.rewrite (\typ -> new >>= \new -> ABT.rewriteExpression old new typ) typ
            where
              name :: Name
              name = fromJust $ Map.lookup ref ppe
              var :: v
              var = Name.toVar name
              old :: V1.Type v a
              old = Type.ref mempty ref
              -- A "dependent" is gonna be part of the typechecking, so it gets replaced with a var.
              -- An update that isn't also a dependent just gets replaced with the latest ref.
              -- A ref that corresponds to neither doesn't need to be replaced.
              new :: Maybe (V1.Type v a)
              new = case (declNeedsUpdate name, Map.lookup name updatedTypes) of
                (True, _) -> Just $ Type.var mempty var
                (False, Just u) -> Just $ Type.ref mempty u
                (False, Nothing) -> Nothing

data RefsToSubst = RefsToSubst
  { rtsTypeAnnRefs :: Set V1.TypeReference,
    rtsTermRefs :: Set V1.TermReference,
    rtsDataCtors :: Set V1.ConstructorReference,
    rtsEffectCtors :: Set V1.ConstructorReference,
    rtsTypeLinks :: Set V1.TypeReference,
    rtsTermLinks :: Set V1.Referent
  }
  deriving (Eq, Ord)

instance Semigroup RefsToSubst where
  RefsToSubst a1 b1 c1 d1 e1 f1 <> RefsToSubst a2 b2 c2 d2 e2 f2 =
    RefsToSubst (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)

instance Monoid RefsToSubst where
  mempty = RefsToSubst mempty mempty mempty mempty mempty mempty

typecheck :: UnisonFile v a -> Transaction (Either (Seq (Result.Note v a)) (TypecheckedUnisonFile v a))
typecheck = wundefined

-- typecheck uf = do
--   Unison.Syntax.FileParser.checkForDuplicateTermsAndConstructors' escape uf
--   env <-
--     Cli.Typecheck.computeTypecheckingEnvironment
--       Cli.Typecheck.ShouldUseTndr'No
--       mempty -- ambient abilities
--       makeTypeLookup
--       uf
--   Except.runExceptT . Result.toEither $ Cli.Typecheck.synthesizeFile env uf

-- Q: Does this return all of the updates? A: suspect no currently
newtype WhatToTypecheck = WhatToTypecheck {unWhatToTypecheck :: DeepRefsId'}

-- How does this function fit in now?

-- | We look at the transitive dependents of the <things with a <name that received an update>>.
-- We choose a latest version of each of those transitive dependents, and return the sets.
whatToTypecheck :: DeepRefsId' -> DeepRefsId' -> Updates -> Transaction WhatToTypecheck
whatToTypecheck drAlice drBob combinedUpdates = do
  -- we don't need to typecheck all adds, only the ones that are dependents of updates
  let deepRefsToReferenceIds dr = Set.fromList $ Map.elems (drTermsId' dr) <> Map.elems (drTypesId' dr)
  let updatesToReferences u = Set.fromList $ Map.elems (updatedTerms u) <> Map.elems (updatedTypes u)
  aliceDependents <- Ops.dependentsWithinScope (deepRefsToReferenceIds drAlice) (updatesToReferences combinedUpdates)
  bobDependents <- Ops.dependentsWithinScope (deepRefsToReferenceIds drAlice) (updatesToReferences combinedUpdates)
  let chooseLatest updates name a _b = case Map.lookup name updates of Just c -> c; Nothing -> a
  let latestDependents :: forall r. Ord r => Map Name r -> (Map Name r, Set r) -> (Map Name r, Set r) -> Map Name r
      latestDependents updates (drAlice', aliceDependents') (drBob', bobDependents') =
        Map.unionWithKey (chooseLatest updates) aliceNamedDefs bobNamedDefs
        where
          aliceNamedDefs :: Map Name r = filterDeepRefs drAlice' aliceDependents'
          bobNamedDefs :: Map Name r = filterDeepRefs drBob' bobDependents'
          filterDeepRefs :: Map Name r -> Set r -> Map Name r
          filterDeepRefs dr deps = Map.filter (flip Set.member deps) dr
      filterDependents rt m = Set.fromList [r | (r, t) <- Map.toList m, t == rt]
  let latestTermDependents :: Map Name TermReferenceId
      latestTermDependents = latestDependents updates' (setup drAlice aliceDependents) (setup drBob bobDependents)
        where
          setup dr dependents = (drTermsId' dr, filterDependents RtTerm dependents)
          updates' = Map.fromList [(n, r) | (n, C.ReferenceDerived r) <- Map.toList $ updatedTerms combinedUpdates]
  let latestTypeDependents :: Map Name TypeReferenceId
      latestTypeDependents = latestDependents updates' (setup drAlice aliceDependents) (setup drBob bobDependents)
        where
          setup dr dependents = (drTypesId' dr, filterDependents RtType dependents)
          updates' = Map.fromList [(n, r) | (n, C.ReferenceDerived r) <- Map.toList $ updatedTypes combinedUpdates]
  pure . WhatToTypecheck $ DeepRefsId' latestTermDependents latestTypeDependents

-- not sure if this is actually going to be used now
data PartitionedUpdates = PartitionedUpdates {puBuiltins :: Updates, puDerived :: UpdatesId}

partitionUpdates :: Updates -> PartitionedUpdates
partitionUpdates updates = PartitionedUpdates puBuiltins puDerived
  where
    puBuiltins :: Updates = Updates builtinTerms builtinTypes
    puDerived :: UpdatesId = UpdatesId derivedTerms derivedTypes
    (builtinTerms, fmap Reference.unsafeId -> derivedTerms) = Map.partition Reference.isBuiltin (updatedTerms updates)
    (builtinTypes, fmap Reference.unsafeId -> derivedTypes) = Map.partition Reference.isBuiltin (updatedTypes updates)

-- for each updated name
-- typecheck the associated defintiion
-- and the latest versions of the transitive dependents of that name in each branch
-- Q: What does it mean to check the latest version of the transitive dependen

--
---- load what we want to typecheck in a suitable form for typechecking
------ synthesizeFile :: Env v a -> UnisonFile v a -> ResultT (Seq (Note v a)) m (UF.TypecheckedUnisonFile v a)
---- conditionally construct a new namespace and/or scratch file
------ namespace
-------- create name mapping for result
------ scratch file
-------- create a PPE suitable for scratch-filing if different from the one we used for typechecking
--
---- thingsWeNeedToTweakAndTypecheck :: (DeepRefs, Diff) -> (DeepRefs, Diff) -> DeepRefs
---- thingsWeNeedToTweakAndTypecheck (aRefs, aDiff) (bRefs, bDiff) = wundefined

-- if yes conflicts:
----

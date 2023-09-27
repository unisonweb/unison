{-# LANGUAGE RecordWildCards #-}

module Unison.Merge2
  ( -- * Library dependencies
    mergeLibdeps,

    -- * Namespace diff
    nameBasedNamespaceDiff,

    -- * Misc / organize these later
    DiffOp (..),
    NamespaceDefns (..),
  )
where

import Control.Lens (Lens', over, (%~), (^.), _3)
import Control.Lens qualified as Lens
import Control.Monad.Validate (ValidateT, runValidateT)
import Control.Monad.Validate qualified as Validate
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Bit (Bit (Bit, unBit))
import Data.Either.Combinators (fromLeft', fromRight')
import Data.Foldable (foldlM)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Lazy qualified as LazyMap
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Tuple qualified as Tuple
import Data.Vector.Unboxed qualified as UVector
import Safe (elemIndexJust)
import U.Codebase.Branch.Type qualified as V2
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Reference
  ( RReference,
    Reference,
    Reference' (..),
    ReferenceType (RtTerm, RtType),
    TermRReference,
    TermReference,
    TermReferenceId,
    TypeRReference,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle (HashHandle)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Term (ClosedTerm, Term)
import U.Codebase.Term qualified as Term
import U.Codebase.Type as Type
import U.Core.ABT qualified as ABT
import Unison.ABT qualified as ABT
import Unison.ABT qualified as V1.ABT
import Unison.ConstructorReference qualified as V1
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as CT
import Unison.ConstructorType qualified as ConstructorType
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Core.Project (ProjectBranchName)
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration qualified as V1.Decl
import Unison.FileParsers qualified as FP
import Unison.FileParsers qualified as FileParsers
import Unison.FileParsers qualified as Unison.Cli
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as Hashing.Convert
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Merge.Diff (NamespaceDefns (..), nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Libdeps (mergeLibdeps)
import Unison.Name (Name)
import Unison.PatternMatchCoverage.UFMap (UFMap)
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Reference qualified as C
import Unison.Reference qualified as V1
import Unison.Reference qualified as V1.Reference
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as V1
import Unison.Term qualified as V1.Term
import Unison.Type qualified as Type
import Unison.Type qualified as V1
import Unison.Type qualified as V1.Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile.Env qualified as UFE
import Unison.UnisonFile.Names qualified as UFN
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
import Unison.WatchKind qualified as V1

newtype SynHash = SynHash Hash deriving (Eq, Ord, Show) via SynHash

-- | DeepRefs is basically a one-way Names (many to one, rather than many to many)
-- It can represent the input or output namespace.
-- It includes constructors, which many other contexts here don't.
data DeepRefs = DeepRefs
  { drTerms :: Map Name Referent,
    drTypes :: Map Name TypeReference
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
  { rtnTerms :: Map TermReference Name,
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
    - [in progress] Build UnisonFile to typecheck
        - [ ] Convert from `TermReference` or `TypeReference` to an actual `Term` / `Decl` to typecheck.
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

computeUnisonFile ::
  forall v a.
  (Var v, Monoid a) =>
  (TermReferenceId -> Transaction (V1.Term v a)) ->
  (TypeReferenceId -> Transaction (V1.Decl v a)) ->
  (Name -> v) ->
  DeepRefsId' ->
  Updates ->
  Transaction (UnisonFile v a)
computeUnisonFile loadTerm loadDecl nameToV toTypecheck combinedUpdates = do
  loadedTerms <- wundefined -- traverse (fmap (substForTerm toTypecheck combinedUpdates) . loadTerm) (drTermsId' toTypecheck)
  loadedTypes <- traverse (fmap (substForDecl wundefined toTypecheck combinedUpdates) . loadDecl) (drTypesId' toTypecheck)
  let effectDeclarations :: Map v (V1.Decl.EffectDeclaration v a)
      dataDeclarations :: Map v (V1.Decl.DataDeclaration v a)
      (fmap fromLeft' -> effectDeclarations, fmap fromRight' -> dataDeclarations) =
        loadedTypes
          & Map.mapKeys nameToV
          & Map.partition (\case Left {} -> True; Right {} -> False)
      terms :: [(v, a {- ann for whole binding -}, V1.Term v a)]
      terms = fmap (\(n, t) -> (nameToV n, mempty :: a, t)) $ Map.toList loadedTerms
      envResult = UFN.environmentFor mempty dataDeclarations effectDeclarations
      -- todo: handle errors better:
      env :: UFE.Env v a = (fromRight' . fromRight') envResult
      dataDeclarationsId :: Map v (TypeReferenceId, V1.Decl.DataDeclaration v a)
      dataDeclarationsId = UFE.datasId env
      effectDeclarationsId :: Map v (TypeReferenceId, V1.Decl.EffectDeclaration v a)
      effectDeclarationsId = UFE.effectsId env
      watches :: Map V1.WatchKind [(v, a {- ann for whole watch -}, V1.Term v a)]
      watches = mempty
  pure $ UF.UnisonFileId dataDeclarationsId effectDeclarationsId terms watches

substForTerm :: forall v a. (Var v, Eq a, Monoid a) => RefToName -> DeepRefsId' -> Updates -> (V1.Term v a) -> (V1.Term v a)
substForTerm RefToName {rtnTypes = ppe} DeepRefsId' {drTypesId' = dependents} Updates {updatedTypes = updates} term =
  updatesTerm term
  where
    updatesTerm term = foldl' updateTermDeps (foldl' updatePatterns (foldl' updateSignatures term typeDeps) $ Map.toList ctorDeps) termDeps
      where
        (termDeps, typeDeps, ctorDeps) =
          foldl'
            ( \(tms, tys, cts) -> \case
                LD.TypeReference r -> (tms, Set.insert r tys, cts)
                LD.TermReference r -> (Set.insert r tms, tys, cts)
                LD.ConReference r ct -> (tms, tys, Map.insert r ct cts)
            )
            mempty
            $ labeledDependencies' term
        labeledDependencies' :: V1.Term v a -> Set LabeledDependency
        labeledDependencies' =
          -- termRef typeRef literalType dataConstructor dataType effectConstructor effectType
          Set.mapMaybe id
            . V1.Term.generalizedDependencies
              (Just . LD.termRef)
              (Just . LD.typeRef)
              (const Nothing)
              (\r i -> Just $ LD.dataConstructor (V1.ConstructorReference r i))
              (const Nothing)
              (\r i -> Just $ LD.effectConstructor (V1.ConstructorReference r i))
              (const Nothing)
        updateTermDeps :: V1.Term v a -> Reference -> V1.Term v a
        updateTermDeps term ref = Maybe.rewrite (\term -> new >>= \new -> ABT.rewriteExpression old new term) term
          where
            name :: Name = fromJust $ Map.lookup ref ppe
            var = Name.toVar name
            old = V1.Term.ref mempty ref
            new = case (Map.lookup name dependents, Map.lookup name updates) of
              (Just {}, _) -> Just $ V1.Term.var mempty var
              (_, Just u) -> Just $ V1.Term.ref mempty u
              (Nothing, Nothing) -> Nothing
        updatePatterns :: V1.Term v a -> (V1.ConstructorReference, ConstructorType) -> V1.Term v a
        updatePatterns term (cr, ct) = wundefined
        updateSignatures :: V1.Term v a -> Reference -> V1.Term v a
        updateSignatures term ref = Maybe.rewrite (\term -> new >>= \new -> V1.Term.rewriteSignatures old new term) term
          where
            name :: Name = fromJust $ Map.lookup ref ppe
            var = Name.toVar name
            old = Type.ref mempty ref
            new = case (Map.lookup name dependents, Map.lookup name updates) of
              (Just {}, _) -> Just $ Type.var mempty var
              (_, Just u) -> Just $ Type.ref mempty u
              (Nothing, Nothing) -> Nothing

substForDecl :: forall v a. (Var v, Monoid a) => RefToName -> DeepRefsId' -> Updates -> (V1.Decl v a) -> (V1.Decl v a)
substForDecl RefToName {rtnTypes = ppe} DeepRefsId' {drTypesId' = dependents} Updates {updatedTypes = updates} decl =
  V1.Decl.modifyAsDataDecl f decl
  where
    f decl = decl {V1.Decl.constructors' = map (over _3 updatesCtor) $ V1.Decl.constructors' decl}
    updatesCtor ctor = foldl' updateType ctor $ V1.Type.dependencies ctor
    updateType :: V1.Type v a -> Reference -> V1.Type v a
    updateType typ ref = Maybe.rewrite (\typ -> new >>= \new -> ABT.rewriteExpression old new typ) typ
      where
        name :: Name = fromJust $ Map.lookup ref ppe
        var = Name.toVar name
        old = Type.ref mempty ref
        new = case (Map.lookup name dependents, Map.lookup name updates) of
          (Just {}, _) -> Just $ Type.var mempty var
          (_, Just u) -> Just $ Type.ref mempty u
          (Nothing, Nothing) -> Nothing

-- typecheck :: UnisonFile v a -> Transaction (Either (Seq (Note v a)) (TypecheckedUnisonFile v a))
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

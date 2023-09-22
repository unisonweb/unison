{-# LANGUAGE RecordWildCards #-}

module Unison.Merge2
  (
  )
where

import Control.Lens (Lens', (%~), (^.))
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
import Unison.Term qualified as V1
import Unison.Term qualified as V1.Term
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

data DeepRefs = DeepRefs
  { dsTerms :: Map Name Referent,
    dsTypes :: Map Name TypeReference
  }

data DeepRefs' = DeepRefs'
  { dsTerms' :: Map Name TermReferenceId,
    dsTypes' :: Map Name TypeReferenceId
  }

data SynHashes = SynHashes
  { shTerms :: Map Name SynHash,
    shTypes :: Map Name SynHash
  }

data DiffOp
  = DoAdded SynHash
  | DoUpdated SynHash
  | DoDeleted

data Diff = Diff
  { diffTerms :: Map Name DiffOp,
    diffTypes :: Map Name DiffOp
  }

-- loading for typechecking only requires Ids, so if we need Builtins later,
-- consider changing this
data Updates = Updates
  { updatedTerms :: Map Name TermReferenceId,
    updatedTypes :: Map Name TypeReferenceId
  }

-- for updated definitions, we want to know which branch to find the updated version in
-- for transitive dependents of updates (which are not updates themselves), we don't care which version we get
-- for conflicted definitions, we need to print both versions, but we don't typecheck anything

data DiffConflicts = DiffConflicts
  { dcTerms :: Set Name,
    dcTypes :: Set Name
  }

-- | Includes the updates and their transitive deps within some namespace / DeepRefs
data TransitiveDeps = TransitiveDeps
  { tdTerms :: Map Name TermReferenceId,
    tdTypes :: Map Name TypeReferenceId
  }

-- | Added or updated references for these unconflicted names
data CombinedUpdates = CombinedNewTerms
  { cntTerms :: Map Name TermReference,
    cntTypes :: Map Name TypeReference
  }

data CombinedUpdatesId = CombinedNewTermsId
  { cntTermsId :: Map Name TermReferenceId,
    cntTypesId :: Map Name TypeReferenceId
  }

deepRefsToPPE :: DeepRefs -> PrettyPrintEnv = wundefined

type LoadTerm m = TermReferenceId -> m ()

type LoadDecl m = TypeReferenceId -> m ()

computeSyntacticHashes :: Applicative m => LoadTerm m -> LoadDecl m -> DeepRefs -> PrettyPrintEnv -> m SynHashes
computeSyntacticHashes loadTerm loadDecl deepRefs ppe = pure wundefined

computeDiff :: SynHashes -> SynHashes -> Diff
computeDiff old new = wundefined

computeConflicts :: Diff -> Diff -> DiffConflicts
computeConflicts a b = wundefined

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

computeUnisonFile ::
  forall v a.
  (Var v, Monoid a) =>
  (TermReferenceId -> Transaction (V1.Term v a)) ->
  (TypeReferenceId -> Transaction (V1.Decl v a)) ->
  (Name -> v) ->
  DeepRefs' ->
  Transaction (UnisonFile v a)
computeUnisonFile loadTerm loadDecl nameToV dr = do
  loadedTerms <- traverse loadTerm (dsTerms' dr)
  loadedTypes <- traverse loadDecl (dsTypes' dr)
  let effectDeclarations :: Map v (V1.Decl.EffectDeclaration v a)
      dataDeclarations :: Map v (V1.Decl.DataDeclaration v a)
      (fmap fromLeft' -> effectDeclarations, fmap fromRight' -> dataDeclarations) =
        loadedTypes
          & Map.mapKeys nameToV
          & Map.partition (\case Left {} -> True; Right {} -> False)
      terms :: [(v, a {- ann for whole binding -}, V1.Term v a)]
      terms = fmap (\(n, t) -> (nameToV n, mempty :: a, t)) $ Map.toList loadedTerms
      envResult = UFN.environmentFor mempty dataDeclarations effectDeclarations
      env :: UFE.Env v a = wundefined envResult
      dataDeclarationsId :: Map v (TypeReferenceId, V1.Decl.DataDeclaration v a)
      dataDeclarationsId = UFE.datasId env
      effectDeclarationsId :: Map v (TypeReferenceId, V1.Decl.EffectDeclaration v a)
      effectDeclarationsId = UFE.effectsId env
      watches :: Map V1.WatchKind [(v, a {- ann for whole watch -}, V1.Term v a)]
      watches = mempty
  pure $ UF.UnisonFileId dataDeclarationsId effectDeclarationsId terms watches

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
whatToTypecheck :: DeepRefs' -> DeepRefs' -> CombinedUpdates -> Transaction DeepRefs'
whatToTypecheck drAlice drBob updates = do
  -- we don't need to typecheck all adds, only the ones that are dependents of updates
  let deepRefsToReferenceIds dr = Set.fromList $ Map.elems (dsTerms' dr) <> Map.elems (dsTypes' dr)
  let updatesToReferences u = Set.fromList $ Map.elems (cntTerms u) <> Map.elems (cntTypes u)
  aliceDependents <- Ops.dependentsWithinScope (deepRefsToReferenceIds drAlice) (updatesToReferences updates)
  bobDependents <- Ops.dependentsWithinScope (deepRefsToReferenceIds drAlice) (updatesToReferences updates)
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
          setup dr dependents = (dsTerms' dr, filterDependents RtTerm dependents)
          updates' = Map.fromList [(n, r) | (n, C.ReferenceDerived r) <- Map.toList $ cntTerms updates]
  let latestTypeDependents :: Map Name TypeReferenceId
      latestTypeDependents = latestDependents updates' (setup drAlice aliceDependents) (setup drBob bobDependents)
        where
          setup dr dependents = (dsTypes' dr, filterDependents RtType dependents)
          updates' = Map.fromList [(n, r) | (n, C.ReferenceDerived r) <- Map.toList $ cntTerms updates]
  pure $ DeepRefs' latestTermDependents latestTypeDependents

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

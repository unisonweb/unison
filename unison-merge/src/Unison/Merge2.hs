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
import U.Codebase.Branch.Type qualified as V2
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
import Unison.Core.Project (ProjectBranchName)
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration qualified as V1.Decl
import Unison.FileParsers qualified as FP
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as Hashing.Convert
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.PatternMatchCoverage.UFMap (UFMap)
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Reference qualified as V1
import Unison.Reference qualified as V1.Reference
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
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

-- need the deep* for each branch, for finding dependents
-- need the updates, to find dependents of
-- need some access to db
whatToTypecheck :: Applicative m => DeepRefs -> DeepRefs -> CombinedUpdatesId -> m DeepRefs'
whatToTypecheck drAlice drBob updates = do
  let termsToTypecheck :: Map Name TermReferenceId = wundefined
      typesToTypecheck :: Map Name TypeReferenceId = wundefined

  pure $ DeepRefs' termsToTypecheck typesToTypecheck

--

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

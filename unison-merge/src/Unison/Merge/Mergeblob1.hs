module Unison.Merge.Mergeblob1
  ( Mergeblob1 (..),
    hydratedDefnsLabeledDependencies,
    makeMergeblob1,
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration.Dependencies qualified as Decl
import Unison.DeclCoherencyCheck (IncoherentDeclReason, lenientCheckDeclCoherency)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.LabeledDependency qualified as LD
import Unison.Merge.CombineDiffs (CombinedDiffOp, combineDiffs)
import Unison.Merge.Diff (diffSynhashedDefns, diffSynhashedDefns', humanizeDiffs, synhashDefns)
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.HumanDiffOp (HumanDiffOp)
import Unison.Merge.Libdeps (applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames, mergeLibdepsDiffs)
import Unison.Merge.Mergeblob0 (Mergeblob0 (..))
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.Rename (Rename, SimpleRenames, makeRenames, makeRenames', makeSimpleRenames)
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (GThreeWay, ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Unconflicts (Unconflicts)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3)
import Unison.Util.Nametree (Nametree)

data Mergeblob1 libdep = Mergeblob1
  { conflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference),
    declNameLookups :: GThreeWay PartialDeclNameLookup DeclNameLookup,
    defns :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference,
    diffsFromLCA :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    humanDiffsFromLCA :: TwoWay (DefnsF2 (Map Name) HumanDiffOp Referent TypeReference),
    hydratedDefns ::
      ThreeWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        ),
    lcaLibdeps :: Map NameSegment libdep,
    libdeps :: Map NameSegment libdep,
    libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep)),
    renames :: TwoWay (DefnsF [] Rename Rename),
    simpleRenames :: TwoWay (Defns SimpleRenames SimpleRenames),
    unconflicts :: DefnsF Unconflicts Referent TypeReference
  }

-- | Get a names object for all the hydrated definitions AND their direct dependencies
hydratedDefnsLabeledDependencies ::
  DefnsF
    (Map Name)
    (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
    (TypeReferenceId, Decl Symbol Ann) ->
  Set LD.LabeledDependency
hydratedDefnsLabeledDependencies defns =
  let termDeps :: Set LD.LabeledDependency
      termDeps =
        foldOf
          ( folded
              . beside
                (to Reference.DerivedId . to LD.TermReference . to Set.singleton)
                (beside (to Term.labeledDependencies) (to Type.labeledDependencies))
          )
          defns.terms

      typeDeps :: Set LD.LabeledDependency
      typeDeps =
        defns.types
          & foldMap \(typeRefId, typeDecl) ->
            Decl.labeledDeclDependenciesIncludingSelfAndFieldAccessors (Reference.DerivedId typeRefId) typeDecl
   in Set.union termDeps typeDeps

makeMergeblob1 ::
  forall libdep.
  (Eq libdep) =>
  ThreeWay Names {- Names for _at least_ every reference in 'hydratedDefnDependencies' -} ->
  Mergeblob0 libdep ->
  ThreeWay (Map NameSegment libdep) ->
  ThreeWay
    ( DefnsF
        (Map Name)
        (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        (TypeReferenceId, Decl Symbol Ann)
    ) ->
  Either (EitherWay IncoherentDeclReason) (Mergeblob1 libdep)
makeMergeblob1 names3 blob libdeps3 hydratedDefns = do
  let renames =
        makeRenames' . Updated.map (bimap BiMultimap.fromRange BiMultimap.fromRange) <$> wundefined -- blob.synhashedDefns

  let simpleRenames =
        makeSimpleRenames <$> renames

  -- Diff LCA->Alice and LCA->Bob
  let (diffsFromLCA, propagatedUpdates) =
        diffSynhashedDefns' wundefined -- blob.synhashedDefns

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff =
        combineDiffs diffsFromLCA

  let humanDiffsFromLCA =
        humanizeDiffs names3 diffsFromLCA propagatedUpdates

  -- Partition the combined diff into the conflicted things and the unconflicted things
  let (conflicts, unconflicts) =
        partitionCombinedDiffs (ThreeWay.forgetLca blob.defns) (ThreeWay.gforgetLca blob.declNameLookups) diff

  -- Diff and merge libdeps
  let libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep))
      libdepsDiffs =
        diffLibdeps libdeps3

  let libdeps :: Map NameSegment libdep
      libdeps =
        applyLibdepsDiff getTwoFreshLibdepNames libdeps3 (mergeLibdepsDiffs libdepsDiffs)

  pure
    Mergeblob1
      { conflicts,
        declNameLookups = blob.declNameLookups,
        defns = blob.defns,
        diff,
        diffsFromLCA,
        humanDiffsFromLCA,
        hydratedDefns,
        lcaLibdeps = libdeps3.lca,
        libdeps,
        libdepsDiffs,
        renames,
        simpleRenames,
        unconflicts
      }

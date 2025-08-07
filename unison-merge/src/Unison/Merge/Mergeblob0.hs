module Unison.Merge.Mergeblob0
  ( Mergeblob0 (..),
    makeMergeblob0,
    MergeblobDebugLog0 (..),
  )
where

import Control.Lens.Fold (folded)
import Data.Map.Strict qualified as Map
import Data.Set.Lens (setOf)
import Unison.Codebase.Branch (UnconflictedBranchView (..))
import Unison.DataDeclaration (Decl)
import Unison.DeclCoherencyCheck (IncoherentDeclReason)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge.CombineDiffs (CombinedDiffOp, combineDiffs)
import Unison.Merge.Diff (diffSynhashedDefns', humanizeDiffs, synhashDefns0, synhashLcaDefns)
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.HumanDiffOp (HumanDiffOp)
import Unison.Merge.Libdeps (applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames, mergeLibdepsDiffs)
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.Rename (makeRenames', makeSimpleRenames)
import Unison.Merge.Synhashed (Synhashed)
import Unison.Merge.ThreeWay (GThreeWay (..), ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Unconflicts (Unconflicts)
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, zipDefnsWith)

data Mergeblob0 libdep = Mergeblob0
  { conflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference),
    declNameLookups :: GThreeWay PartialDeclNameLookup DeclNameLookup,
    defns :: ThreeWay UnconflictedBranchView,
    defnsIds :: ThreeWay (DefnsF Set TermReferenceId TypeReferenceId),
    diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference,
    diffsFromLCA :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    humanDiffsFromLCA :: TwoWay (DefnsF2 (Map Name) HumanDiffOp Referent TypeReference),
    -- Hydrated narrowed definitions. These are not necessarily all of the definitions needed for actually rendering
    -- a file, e.g. it doesn't contain dependents. It's included here because we did some work to hydrate these, and if
    -- we need to hydrate more *later*, we ought to look in this map first (to save duplicate work).
    hydratedNarrowedDefns ::
      Defns
        (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
        (Map TypeReferenceId (Decl Symbol Ann)),
    libdeps :: Updated (Map NameSegment libdep),
    synhashedNarrowedDefns :: TwoWay (Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference)),
    unconflicts :: DefnsF Unconflicts Referent TypeReference
  }

data MergeblobDebugLog0 m = MergeblobDebugLog0
  { debugLogDefns :: ThreeWay (DefnsF (Map Name) Referent TypeReference) -> m (),
    debugLogNarrowedDefns :: TwoWay (Updated (DefnsF (Map Name) Referent TypeReference)) -> m (),
    debugLogSynhashedNarrowedDefns ::
      TwoWay
        ( GUpdated
            (DefnsF2 (Map Name) Synhashed Referent TypeReference)
            (DefnsF2 (Map Name) Synhashed Referent TypeReference)
        ) ->
      m (),
    debugLogDiffsFromLCA :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) -> m (),
    debugLogDiff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> m ()
  }

makeMergeblob0 ::
  forall libdep m.
  (Eq libdep, Monad m) =>
  MergeblobDebugLog0 m ->
  ( DefnsF Set TermReferenceId TypeReferenceId ->
    m (Defns (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)))
  ) ->
  ThreeWay Names ->
  ThreeWay UnconflictedBranchView ->
  ThreeWay (Map NameSegment libdep) ->
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  m (Either (EitherWay IncoherentDeclReason) (Mergeblob0 libdep))
makeMergeblob0 log hydrate allNames defns libdeps declNameLookups = do
  let defnsByName = bimap BiMultimap.range BiMultimap.range . (.defns) <$> defns

  log.debugLogDefns defnsByName

  let toIds :: DefnsF (Map Name) Referent TypeReference -> DefnsF Set TermReferenceId TypeReferenceId
      toIds =
        bimap
          (setOf (folded @(Map Name) . Referent.termReference_ . Reference._DerivedId))
          (setOf (folded @(Map Name) . Reference._DerivedId))

  let defnsIds :: ThreeWay (DefnsF Set TermReferenceId TypeReferenceId)
      defnsIds =
        toIds <$> defnsByName

  -- Narrow definitions to those that could have different syntactic hashes
  let narrowedDefns0 =
        -- narrowDefns declNameLookups defnsByName
        TwoWay
          { alice = Updated {old = defnsByName.lca, new = defnsByName.alice},
            bob = Updated {old = defnsByName.lca, new = defnsByName.bob}
          }

  log.debugLogNarrowedDefns narrowedDefns0

  let narrowedDefns =
        TwoWay.updatedToThreeWay narrowedDefns0

  -- Hydrate only the narrowed definitions
  hydratedNarrowedDefns <-
    hydrate (fold (toIds <$> narrowedDefns))

  -- Compute the syntactic hashes of the narrowed+hydrated definitions
  let synhashedNarrowedDefns :: TwoWay (Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference))
      synhashedNarrowedDefns =
        actualHonk fst allNames declNameLookups narrowedDefns0 hydratedNarrowedDefns

  log.debugLogSynhashedNarrowedDefns synhashedNarrowedDefns

  -- Identify all renames
  let renames =
        makeRenames' . Updated.map (bimap BiMultimap.fromRange BiMultimap.fromRange) <$> synhashedNarrowedDefns

  -- Filter all renames down to just "simple" renames
  let simpleRenames =
        makeSimpleRenames <$> renames

  -- Diff LCA->Alice and LCA->Bob
  let (diffsFromLCA, propagatedUpdates) =
        diffSynhashedDefns' synhashedNarrowedDefns

  log.debugLogDiffsFromLCA diffsFromLCA

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference
      diff =
        combineDiffs diffsFromLCA

  log.debugLogDiff diff

  -- "Humanize" diffs... this is a bit of tech debt, to remove once we better-represent (& apply) renames
  let humanDiffsFromLCA =
        humanizeDiffs allNames diffsFromLCA propagatedUpdates

  -- Partition the combined diff into the conflicted things and the unconflicted things
  let (conflicts, unconflicts) =
        partitionCombinedDiffs ((.defns) <$> ThreeWay.forgetLca defns) (ThreeWay.gforgetLca declNameLookups) diff

  -- Diff and merge libdeps
  let mergedLibdeps :: Map NameSegment libdep
      mergedLibdeps =
        applyLibdepsDiff
          getTwoFreshLibdepNames
          libdeps
          (mergeLibdepsDiffs (diffLibdeps libdeps))

  pure $
    Right
      Mergeblob0
        { conflicts,
          declNameLookups,
          defns,
          defnsIds,
          diff,
          diffsFromLCA,
          libdeps = Updated {old = libdeps.lca, new = mergedLibdeps},
          humanDiffsFromLCA,
          hydratedNarrowedDefns,
          synhashedNarrowedDefns,
          unconflicts
        }

actualHonk ::
  (term -> Term Symbol Ann) ->
  ThreeWay Names ->
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  TwoWay (Updated (DefnsF (Map Name) Referent TypeReference)) ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId (Decl Symbol Ann)) ->
  TwoWay (GUpdated (DefnsF2 (Map Name) Synhashed Referent TypeReference) (DefnsF2 (Map Name) Synhashed Referent TypeReference))
actualHonk toTerm allNames declNameLookups defns hydratedDefns =
  Updated
    <$> ( zipDefnsWith
            Map.intersection
            Map.intersection
            ( synhashLcaDefns
                toTerm
                ppe
                declNameLookups.lca
                (TwoWay.updatedToThreeWay defns).lca
                hydratedDefns
            )
            <$> oldDefns
        )
    <*> ( synhashDefns0 toTerm ppe hydratedDefns
            <$> ThreeWay.gforgetLca declNameLookups
            <*> newDefns
        )
  where
    oldDefns = (.old) <$> defns
    newDefns = (.new) <$> defns

    ppeds :: ThreeWay PrettyPrintEnvDecl
    ppeds =
      allNames <&> \names -> PPED.makePPED (PPE.namer names) (PPE.suffixifyByHash names)

    ppe :: PrettyPrintEnv
    ppe =
      ppeds.alice.unsuffixifiedPPE
        `PPE.addFallback` ppeds.bob.unsuffixifiedPPE
        `PPE.addFallback` ppeds.lca.unsuffixifiedPPE

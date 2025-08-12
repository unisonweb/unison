module Unison.Merge.Diffblob
  ( Diffblob (..),
    makeDiffblob,
    DiffblobLog (..),
  )
where

import Control.Lens.Fold (folded)
import Data.Map.Strict qualified as Map
import Data.Set.Lens (setOf)
import Unison.DataDeclaration (Decl)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge.CombineDiffs (CombinedDiffOp, combineDiffs)
import Unison.Merge.Diff (diffSynhashedDefns, humanizeDiffs)
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.HumanDiffOp (HumanDiffOp)
import Unison.Merge.Libdeps (applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames, mergeLibdepsDiffs)
import Unison.Merge.Narrow (narrowDefns)
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.Rename (SimpleRenames, makeRenames, makeSimpleRenames)
import Unison.Merge.Synhash (synhashDefns, synhashLcaDefns)
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
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, zipDefnsWith)

data Diffblob libdep = Diffblob
  { conflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference),
    declNameLookups :: GThreeWay PartialDeclNameLookup DeclNameLookup,
    defns :: ThreeWay UnconflictedLocalDefnsView,
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
    simpleRenames :: TwoWay (Defns SimpleRenames SimpleRenames),
    unconflicts :: DefnsF Unconflicts Referent TypeReference
  }

data DiffblobLog m = DiffblobLog
  { logDefns :: ThreeWay (DefnsF (Map Name) Referent TypeReference) -> m (),
    logNarrowedDefns :: TwoWay (Updated (DefnsF (Map Name) Referent TypeReference)) -> m (),
    logSynhashedNarrowedDefns ::
      TwoWay
        ( GUpdated
            (DefnsF2 (Map Name) Synhashed Referent TypeReference)
            (DefnsF2 (Map Name) Synhashed Referent TypeReference)
        ) ->
      m (),
    logDiffsFromLCA :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) -> m (),
    logDiff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> m ()
  }

makeDiffblob ::
  forall libdep m.
  (Eq libdep, Monad m) =>
  DiffblobLog m ->
  ( DefnsF Set TermReferenceId TypeReferenceId ->
    m
      ( Defns
          (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
          (Map TypeReferenceId (Decl Symbol Ann))
      )
  ) ->
  ThreeWay Names ->
  ThreeWay UnconflictedLocalDefnsView ->
  ThreeWay (Map NameSegment libdep) ->
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  m (Diffblob libdep)
makeDiffblob logger hydrate allNames defns libdeps declNameLookups = do
  let defnsByName = bimap BiMultimap.range BiMultimap.range . (.defns) <$> defns

  logger.logDefns defnsByName

  let defnsIds :: ThreeWay (DefnsF Set TermReferenceId TypeReferenceId)
      defnsIds =
        toIds <$> defnsByName

  -- Narrow definitions to those that could have different syntactic hashes
  let narrowedDefns =
        narrowDefns declNameLookups defnsByName

  logger.logNarrowedDefns narrowedDefns

  -- Hydrate only the narrowed definitions
  hydratedNarrowedDefns <-
    hydrate (foldMap (Updated.foldMap toIds) narrowedDefns)

  -- Compute the syntactic hashes of the narrowed+hydrated definitions
  let synhashedNarrowedDefns :: TwoWay (Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference))
      synhashedNarrowedDefns =
        makeSynhashedNarrowedDefns fst allNames declNameLookups narrowedDefns hydratedNarrowedDefns

  logger.logSynhashedNarrowedDefns synhashedNarrowedDefns

  -- Identify all renames
  let renames =
        makeRenames . Updated.map (bimap BiMultimap.fromRange BiMultimap.fromRange) <$> synhashedNarrowedDefns

  -- Filter all renames down to just "simple" renames
  let simpleRenames =
        makeSimpleRenames <$> renames

  -- Diff LCA->Alice and LCA->Bob
  let (diffsFromLCA, propagatedUpdates) =
        diffSynhashedDefns synhashedNarrowedDefns

  logger.logDiffsFromLCA diffsFromLCA

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference
      diff =
        combineDiffs diffsFromLCA

  logger.logDiff diff

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

  pure
    Diffblob
      { conflicts,
        declNameLookups,
        defns,
        defnsIds,
        diff,
        diffsFromLCA,
        libdeps = Updated {old = libdeps.lca, new = mergedLibdeps},
        humanDiffsFromLCA,
        hydratedNarrowedDefns,
        simpleRenames,
        unconflicts
      }

toIds :: DefnsF (Map Name) Referent TypeReference -> DefnsF Set TermReferenceId TypeReferenceId
toIds =
  bimap
    (setOf (folded @(Map Name) . Referent.termReference_ . Reference._DerivedId))
    (setOf (folded @(Map Name) . Reference._DerivedId))

makeSynhashedNarrowedDefns ::
  (term -> Term Symbol Ann) ->
  ThreeWay Names ->
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  TwoWay (Updated (DefnsF (Map Name) Referent TypeReference)) ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId (Decl Symbol Ann)) ->
  TwoWay (GUpdated (DefnsF2 (Map Name) Synhashed Referent TypeReference) (DefnsF2 (Map Name) Synhashed Referent TypeReference))
makeSynhashedNarrowedDefns toTerm allNames declNameLookups defns hydratedDefns =
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
    <*> ( synhashDefns toTerm ppe hydratedDefns
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

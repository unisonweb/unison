module Unison.Merge.Diffblob
  ( Diffblob (..),
    makeDiffblob,
    makeFastForwardDiffblob,
    DiffblobLog (..),
    emptyDiffblobLog,
  )
where

import Control.Lens.Fold (folded)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration.Dependencies qualified as Decl
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LabeledDependency
import Unison.Merge.CombineDiffs (CombinedDiffOp, combineDiffs)
import Unison.Merge.Diff (diffSynhashedDefns, diffSynhashedDefns1)
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.Libdeps (applyLibdepsDiff, diffLibdeps, diffLibdeps1, getTwoFreshLibdepNames, mergeLibdepsDiffs)
import Unison.Merge.Narrow (narrowDefns, narrowDefnsTotal)
import Unison.Merge.PartitionCombinedDiffs (assumeUnconflicts, partitionCombinedDiffs)
import Unison.Merge.Rename (Rename, SimpleRenames (..), makeRenames, makeSimpleRenames)
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
import Unison.NamesUtils qualified as NamesUtils
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.PartialDeclNameLookup qualified as PartialDeclNameLookup
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
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
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
    -- Hydrated narrowed definitions. These are not necessarily all of the definitions needed for actually rendering
    -- a file, e.g. it doesn't contain dependents. It's included here because we did some work to hydrate these, and if
    -- we need to hydrate more *later*, we ought to look in this map first (to save duplicate work).
    hydratedNarrowedDefns ::
      Defns
        (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
        (Map TypeReferenceId (Decl Symbol Ann)),
    libdeps :: Updated (Map NameSegment libdep),
    libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep)),
    propagatedUpdates :: TwoWay (DefnsF (Map Name) (Updated Referent) (Updated TypeReference)),
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

emptyDiffblobLog :: (Applicative m) => DiffblobLog m
emptyDiffblobLog =
  let f _ = pure () in DiffblobLog f f f f f

makeDiffblob ::
  forall libdep m.
  (Eq libdep, Monad m) =>
  DiffblobLog m ->
  ( ThreeWay (DefnsF Set TermReferenceId TypeReferenceId) ->
    m
      ( Defns
          (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
          (Map TypeReferenceId (Decl Symbol Ann))
      )
  ) ->
  (ThreeWay (Set LabeledDependency) -> m (ThreeWay Names)) ->
  ThreeWay UnconflictedLocalDefnsView ->
  ThreeWay (Map NameSegment libdep) ->
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  m (Diffblob libdep)
makeDiffblob logger hydrate loadNames defns libdeps declNameLookups = do
  let defnsByName = NamesUtils.byName . (.defns) <$> defns

  logger.logDefns defnsByName

  let defnsIds :: ThreeWay (DefnsF Set TermReferenceId TypeReferenceId)
      defnsIds =
        toIds <$> defnsByName

  -- Narrow definitions to those that could have different syntactic hashes
  let narrowedDefns =
        narrowDefns declNameLookups defnsByName

  let narrowedDefns3 =
        TwoWay.updatedToThreeWay narrowedDefns

  let narrowedDefnsIds3 =
        toIds <$> narrowedDefns3

  logger.logNarrowedDefns narrowedDefns

  -- Hydrate only the narrowed definitions
  hydratedNarrowedDefns <-
    hydrate narrowedDefnsIds3

  -- Load the names of all dependencies hydrated definitions
  dependencyNames <-
    let hydratedNarrowedDefnsList = bimap Map.toList Map.toList hydratedNarrowedDefns
        f refs = List.filter (\(ref, _) -> Set.member ref refs)
     in loadNames $
          (\defns -> toLabeledDependencies (zipDefnsWith f f defns hydratedNarrowedDefnsList))
            <$> narrowedDefnsIds3

  -- Compute the syntactic hashes of the narrowed+hydrated definitions
  let synhashedNarrowedDefns :: TwoWay (Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference))
      synhashedNarrowedDefns =
        makeSynhashedNarrowedDefns
          fst
          dependencyNames
          declNameLookups
          narrowedDefns
          hydratedNarrowedDefns

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

  -- Partition the combined diff into the conflicted things and the unconflicted things
  let (conflicts, unconflicts) =
        partitionCombinedDiffs ((.defns) <$> ThreeWay.forgetLca defns) (ThreeWay.gforgetLca declNameLookups) diff

  -- Diff and merge libdeps
  let libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep))
      libdepsDiffs =
        diffLibdeps libdeps

  let mergedLibdeps :: Map NameSegment libdep
      mergedLibdeps =
        applyLibdepsDiff
          getTwoFreshLibdepNames
          libdeps
          (mergeLibdepsDiffs libdepsDiffs)

  pure
    Diffblob
      { conflicts,
        declNameLookups,
        defns,
        defnsIds,
        diff,
        diffsFromLCA,
        libdeps = Updated {old = libdeps.lca, new = mergedLibdeps},
        libdepsDiffs,
        hydratedNarrowedDefns,
        propagatedUpdates,
        simpleRenames,
        unconflicts
      }

-- | Like 'makeDiffblob', but for a fast forward, and when the LCA is known not to have any type declarations with
-- missing constructor names.
makeFastForwardDiffblob ::
  forall libdep m.
  (Eq libdep, Monad m) =>
  ( Updated (DefnsF Set TermReferenceId TypeReferenceId) ->
    m
      ( Defns
          (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
          (Map TypeReferenceId (Decl Symbol Ann))
      )
  ) ->
  (Updated (Set LabeledDependency) -> m (Updated Names)) ->
  Updated UnconflictedLocalDefnsView ->
  Updated (Map NameSegment libdep) ->
  Updated DeclNameLookup ->
  m (Diffblob libdep)
makeFastForwardDiffblob hydrate loadNames defns libdeps declNameLookups = do
  let defnsByName = Updated.map (NamesUtils.byName . (.defns)) defns

  let defnsIds :: Updated (DefnsF Set TermReferenceId TypeReferenceId)
      defnsIds =
        Updated.map toIds defnsByName

  -- Narrow definitions to those that could have different syntactic hashes
  let narrowedDefns :: Updated (DefnsF (Map Name) Referent TypeReference)
      narrowedDefns =
        narrowDefnsTotal declNameLookups defnsByName

  let narrowedDefnsIds :: Updated (DefnsF Set TermReferenceId TypeReferenceId)
      narrowedDefnsIds =
        Updated.map toIds narrowedDefns

  -- Hydrate only the narrowed definitions
  hydratedNarrowedDefns <-
    hydrate narrowedDefnsIds

  -- Load the names of all dependencies hydrated definitions
  dependencyNames <-
    let hydratedNarrowedDefnsList = bimap Map.toList Map.toList hydratedNarrowedDefns
        f refs = List.filter (\(ref, _) -> Set.member ref refs)
     in loadNames $
          Updated.map
            (\defns -> toLabeledDependencies (zipDefnsWith f f defns hydratedNarrowedDefnsList))
            narrowedDefnsIds

  -- Compute the syntactic hashes of the narrowed+hydrated definitions
  let synhashedNarrowedDefns :: Updated (DefnsF2 (Map Name) Synhashed Referent TypeReference)
      synhashedNarrowedDefns =
        makeSynhashedNarrowedDefnsForFastForward
          fst
          dependencyNames
          declNameLookups
          narrowedDefns
          hydratedNarrowedDefns

  -- logger.logSynhashedNarrowedDefns synhashedNarrowedDefns

  -- Identify all renames
  let renames :: DefnsF [] Rename Rename
      renames =
        makeRenames (Updated.map (bimap BiMultimap.fromRange BiMultimap.fromRange) synhashedNarrowedDefns)

  -- Filter all renames down to just "simple" renames
  let simpleRenames :: Defns SimpleRenames SimpleRenames
      simpleRenames =
        makeSimpleRenames renames

  -- Diff Alice->Bob
  let (diffFromLCA, propagatedUpdates) =
        diffSynhashedDefns1 synhashedNarrowedDefns

  -- logger.logDiffsFromLCA diffsFromLCA

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference
      diff =
        combineDiffs
          TwoWay
            { alice = Defns Map.empty Map.empty,
              bob = diffFromLCA
            }

  -- logger.logDiff diff

  -- View the combined diff as unconflicted things
  let unconflicts =
        assumeUnconflicts diff

  -- Diff and merge libdeps
  let libdepsDiff :: Map NameSegment (DiffOp libdep)
      libdepsDiff =
        diffLibdeps1 libdeps

  let libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep))
      libdepsDiffs =
        TwoWay
          { alice = Map.empty,
            bob = libdepsDiff
          }

  let mergedLibdeps :: Map NameSegment libdep
      mergedLibdeps =
        applyLibdepsDiff
          getTwoFreshLibdepNames
          ThreeWay
            { lca = libdeps.old,
              alice = libdeps.old,
              bob = libdeps.new
            }
          (mergeLibdepsDiffs libdepsDiffs)

  pure
    Diffblob
      { conflicts = TwoWay.bothWays (Defns Map.empty Map.empty),
        declNameLookups =
          GThreeWay
            { lca = PartialDeclNameLookup.fromDeclNameLookup declNameLookups.old,
              alice = declNameLookups.old,
              bob = declNameLookups.new
            },
        defns = updatedToThreeWay defns,
        defnsIds = updatedToThreeWay defnsIds,
        diff,
        diffsFromLCA =
          TwoWay
            { alice = Defns Map.empty Map.empty,
              bob = diffFromLCA
            },
        libdeps = Updated {old = libdeps.old, new = mergedLibdeps},
        libdepsDiffs,
        hydratedNarrowedDefns,
        propagatedUpdates =
          TwoWay
            { alice = Defns Map.empty Map.empty,
              bob = propagatedUpdates
            },
        simpleRenames =
          TwoWay
            { alice =
                Defns
                  (SimpleRenames Map.empty Map.empty)
                  (SimpleRenames Map.empty Map.empty),
              bob = simpleRenames
            },
        unconflicts
      }
  where
    -- View update as Alice+Bob (where LCA = Alice)
    updatedToThreeWay :: Updated a -> ThreeWay a
    updatedToThreeWay Updated {old, new} =
      ThreeWay {lca = old, alice = old, bob = new}

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
                (fold oldDefns) -- left-biased map union is fine, the maps have equal values at equal keys
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

makeSynhashedNarrowedDefnsForFastForward ::
  (term -> Term Symbol Ann) ->
  Updated Names ->
  Updated DeclNameLookup ->
  Updated (DefnsF (Map Name) Referent TypeReference) ->
  Defns (Map TermReferenceId term) (Map TypeReferenceId (Decl Symbol Ann)) ->
  GUpdated (DefnsF2 (Map Name) Synhashed Referent TypeReference) (DefnsF2 (Map Name) Synhashed Referent TypeReference)
makeSynhashedNarrowedDefnsForFastForward toTerm allNames declNameLookups defns hydratedDefns =
  Updated.zipWith (synhashDefns toTerm ppe hydratedDefns) declNameLookups defns
  where
    ppeds :: Updated PrettyPrintEnvDecl
    ppeds =
      Updated.map (\names -> PPED.makePPED (PPE.namer names) (PPE.suffixifyByHash names)) allNames

    ppe :: PrettyPrintEnv
    ppe =
      ppeds.old.unsuffixifiedPPE
        `PPE.addFallback` ppeds.new.unsuffixifiedPPE

toLabeledDependencies ::
  (Foldable f) =>
  DefnsF f (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann) ->
  Set LabeledDependency
toLabeledDependencies defns =
  Set.union
    ( defns.terms & foldMap \(ref, (term, typ)) ->
        Set.insert
          (LabeledDependency.derivedTerm ref)
          (Term.labeledDependencies term <> Type.labeledDependencies typ)
    )
    ( defns.types & foldMap \(ref, decl) ->
        Decl.labeledDeclDependenciesIncludingSelfAndFieldAccessors (Reference.DerivedId ref) decl
    )

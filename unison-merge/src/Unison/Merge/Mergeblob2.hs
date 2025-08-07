module Unison.Merge.Mergeblob2
  ( Mergeblob2 (..),
    Mergeblob2Error (..),
    makeMergeblob2,
  )
where

import Control.Monad.Trans.Except qualified as Except
import Data.Bifoldable (bifold, bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Unison.Codebase.Branch.Type (UnconflictedBranchView (..))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Merge.FindConflictedAlias (findConflictedAlias)
import Unison.Merge.Mergeblob0 (Mergeblob0 (..))
import Unison.Merge.PartitionCombinedDiffs (narrowConflictsToNonBuiltins)
import Unison.Merge.Render (renderUnisonFiles)
import Unison.Merge.ThreeWay (GThreeWay, ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Merge.Updated (Updated)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.Reference (Reference, Reference' (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defn (Defn)
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith, zipDefnsWith3, zipDefnsWith4)
import Unison.Util.Map qualified as Map
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Set qualified as Set

data Mergeblob2 libdep = Mergeblob2
  { conflicts :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
    coreDependencies :: TwoWay (DefnsF Set TermReference TypeReference),
    declNameLookups :: GThreeWay PartialDeclNameLookup DeclNameLookup,
    defns :: ThreeWay UnconflictedBranchView,
    dependents :: TwoWay (DefnsF Set TermReferenceId TypeReferenceId),
    hasConflicts :: Bool,
    hydratedNarrowedDefns ::
      Defns
        (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
        (Map TypeReferenceId (Decl Symbol Ann)),
    libdeps :: Updated (Map NameSegment libdep),
    libdepsNames :: Updated Names,
    stageOne :: DefnsF (Map Name) Referent TypeReference,
    unconflicts :: DefnsF Unconflicts Referent TypeReference,
    uniqueTypeGuids :: TwoWay (Map Name Text),
    -- `unparsedFile` (no mergetool) xor `unparsedSoloFiles` (yes mergetool) are ultimately given to the user
    unparsedFile :: Pretty ColorText,
    unparsedSoloFiles :: ThreeWay (Pretty ColorText)
  }

data Mergeblob2Error
  = Mergeblob2Error'ConflictedAlias (EitherWay (Defn (Name, Name) (Name, Name)))
  | Mergeblob2Error'ConflictedBuiltin (Defn Name Name)

makeMergeblob2 ::
  (Monad m) =>
  ( DefnsF Set TermReferenceId TypeReferenceId ->
    m (Defns (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)))
  ) ->
  (Set Reference.Id -> Set Reference -> m (DefnsF Set TermReferenceId TypeReferenceId)) ->
  (Map NameSegment libdep -> m Names) ->
  Mergeblob0 libdep ->
  TwoWay Text ->
  m (Either Mergeblob2Error (Mergeblob2 libdep))
makeMergeblob2 hydrate loadDependents loadLibdepsNames blob authors = Except.runExceptT do
  -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
  whenJust (findConflictedAlias blob.defns.lca.defns blob.diffsFromLCA.alice) \conflict ->
    Except.throwE (Mergeblob2Error'ConflictedAlias (Alice conflict))
  whenJust (findConflictedAlias blob.defns.lca.defns blob.diffsFromLCA.bob) \conflict ->
    Except.throwE (Mergeblob2Error'ConflictedAlias (Bob conflict))

  conflicts <-
    Except.except (narrowConflictsToNonBuiltins blob.conflicts)
      & Except.withExceptT Mergeblob2Error'ConflictedBuiltin

  let conflictsNames :: TwoWay (DefnsF Set Name Name)
      conflictsNames =
        bimap Map.keysSet Map.keysSet <$> conflicts

  let coreDependencies :: TwoWay (DefnsF Set TermReference TypeReference)
      coreDependencies =
        identifyCoreDependencies
          ((.defns) <$> ThreeWay.forgetLca blob.defns)
          (bimap (Set.fromList . Map.elems) (Set.fromList . Map.elems) <$> conflicts)
          blob.unconflicts

  dependentsIds <- do
    lift do
      for ((,) <$> ThreeWay.forgetLca blob.defnsIds <*> coreDependencies) \(defns, deps) ->
        loadDependents (bifold defns) (bifold deps)

  hydratedDefnsById <- do
    let unhydratedConflictsAndDependentsIds :: DefnsF Set TermReferenceId TypeReferenceId
        unhydratedConflictsAndDependentsIds =
          zipDefnsWith
            Set.differenceMap
            Set.differenceMap
            (foldMap (bimap Map.elemsSet Map.elemsSet) conflicts <> fold dependentsIds)
            blob.hydratedNarrowedDefns

    hydratedConflictsAndDependents <- lift (hydrate unhydratedConflictsAndDependentsIds)

    -- Left-biased map union is ok here since the maps are disjoint
    pure (blob.hydratedNarrowedDefns <> hydratedConflictsAndDependents)

  let hydratedDefnsByName ::
        ThreeWay
          ( DefnsF
              (Map Name)
              (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
              (TypeReferenceId, Decl Symbol Ann)
          )
      hydratedDefnsByName =
        nameHydratedRefs hydratedDefnsById . bimap BiMultimap.range BiMultimap.range . (.defns) <$> blob.defns

  let dependentsNames :: TwoWay (DefnsF Set Name Name)
      dependentsNames =
        let -- Compute the set of dependents names
            allDependentsNames :: TwoWay (DefnsF Set Name Name)
            allDependentsNames =
              zipDefnsWith
                (\defns deps -> Map.foldMapWithKey (f deps) (BiMultimap.domain defns))
                (\defns deps -> Map.foldMapWithKey (g deps) (BiMultimap.domain defns))
                <$> ((.defns) <$> ThreeWay.forgetLca blob.defns)
                <*> dependentsIds
              where
                f :: Set TermReferenceId -> Referent -> NESet Name -> Set Name
                f deps defn0 names
                  | Just defn <- Referent.toTermReferenceId defn0,
                    Set.member defn deps =
                      Set.NonEmpty.toSet names
                  | otherwise = Set.empty
                g :: Set TypeReferenceId -> TypeReference -> NESet Name -> Set Name
                g deps defn0 names
                  | ReferenceDerived defn <- defn0,
                    Set.member defn deps =
                      Set.NonEmpty.toSet names
                  | otherwise = Set.empty
         in -- Filter it down by identifying the unconflicted dependents we need to pull into the Unison file (either
            -- first for typechecking, if there aren't conflicts, or else for manual conflict resolution without a
            -- typechecking step, if there are)
            mergeDependents conflictsNames blob.unconflicts allDependentsNames

  libdepsNames <-
    lift (Updated.traverse loadLibdepsNames blob.libdeps)

  let (unparsedFile, unparsedSoloFiles) =
        renderUnisonFiles
          authors
          blob.declNameLookups
          (bimap BiMultimap.range BiMultimap.range . (.defns) <$> blob.defns)
          hydratedDefnsByName
          libdepsNames
          conflictsNames
          dependentsNames

  pure $
    Mergeblob2
      { conflicts,
        coreDependencies,
        declNameLookups = blob.declNameLookups,
        defns = blob.defns,
        dependents = dependentsIds,
        -- Eh, they'd either both be null, or neither, but just check both maps anyway
        hasConflicts = not (defnsAreEmpty conflicts.alice) || not (defnsAreEmpty conflicts.bob),
        hydratedNarrowedDefns = blob.hydratedNarrowedDefns,
        libdeps = blob.libdeps,
        libdepsNames,
        stageOne =
          makeStageOne
            (ThreeWay.gforgetLca blob.declNameLookups)
            conflictsNames
            blob.unconflicts
            dependentsNames
            (bimap BiMultimap.range BiMultimap.range blob.defns.lca.defns),
        unconflicts = blob.unconflicts,
        uniqueTypeGuids =
          Map.mapMaybe (DataDeclaration.uniqueTypeGuid . snd) . (.types)
            <$> ThreeWay.forgetLca hydratedDefnsByName,
        unparsedFile,
        unparsedSoloFiles
      }

identifyCoreDependencies ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  DefnsF Unconflicts Referent TypeReference ->
  TwoWay (DefnsF Set TermReference TypeReference)
identifyCoreDependencies defns conflicts unconflicts = do
  let soloUpdatedNames = Unconflicts.soloUpdatedNames unconflicts
  fold
    [ -- One source of dependencies: One's own updates (including those that the other party also happened to make).
      -- This is required even though it may seem as though one's already propagated that update. Consider if Alice
      -- updates X and adds a new transitive dependent Z (where Z calls Y calls X). We want X as an Alice core
      -- dependency, not just a B one, so that any update to Y can ultimately propagate again to Z.
      --
      -- Second source of dependencies: Alice's versions of Bob's unconflicted deletes and updates, and vice-versa.
      -- (This is name-based: if Bob updates the *name* "foo", then we go find the thing that Alice calls "foo" (if
      -- anything), no matter what its hash is.)
      let f :: (Ord ref) => Set Name -> Set Name -> Set Name -> BiMultimap ref Name -> BiMultimap ref Name
          f myUpdates bothUpdates theirDeletesAndUpdates =
            BiMultimap.restrictRan (Set.unions [myUpdates, bothUpdates, theirDeletesAndUpdates])
       in defnsReferences
            <$> ( zipDefnsWith4 f f
                    <$> soloUpdatedNames
                    <*> TwoWay.bothWays (Unconflicts.bothUpdatedNames unconflicts)
                    <*> TwoWay.swap (Unconflicts.soloDeletedNames unconflicts <> soloUpdatedNames)
                    <*> defns
                ),
      -- Third source of dependencies: Alice's own conflicted things, and ditto for Bob.
      --
      -- An example: suppose Alice has foo#alice and Bob has foo#bob, so foo is conflicted. Furthermore, suppose
      -- Alice has bar#bar that depends on foo#alice.
      --
      -- We want Alice's #alice to be considered a dependency, so that when we go off and find dependents of these
      -- dependencies to put in the scratch file for type checking and propagation, we find bar#bar.
      --
      -- Note that this is necessary even if bar#bar is unconflicted! We don't want bar#bar to be put directly
      -- into the namespace / parsing context for the conflicted merge, because it has an unnamed reference on
      -- foo#alice. It rather ought to be in the scratchfile alongside the conflicted foo#alice and foo#bob, so
      -- that when that conflict is resolved, it will propagate to bar.
      bimap (Set.map Reference.DerivedId) (Set.map Reference.DerivedId) <$> conflicts
    ]

defnsReferences ::
  Defns (BiMultimap Referent name) (BiMultimap TypeReference name) ->
  DefnsF Set TermReference TypeReference
defnsReferences defns =
  List.foldl' f Defns {terms = Set.empty, types = BiMultimap.dom defns.types} (Set.toList (BiMultimap.dom defns.terms))
  where
    f :: DefnsF Set TermReference TypeReference -> Referent -> DefnsF Set TermReference TypeReference
    f acc = \case
      Referent.Con (ConstructorReference ref _) _ ->
        let !types = Set.insert ref acc.types
         in Defns {terms = acc.terms, types}
      Referent.Ref ref ->
        let !terms = Set.insert ref acc.terms
         in Defns {terms, types = acc.types}

mergeDependents ::
  forall term typ.
  TwoWay (DefnsF Set Name Name) ->
  DefnsF Unconflicts typ term ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay (DefnsF Set Name Name)
mergeDependents conflicts unconflicts dependents =
  let merge = zipDefnsWith4 mergeDependentsV mergeDependentsV
      split = bitraverse splitV splitV
   in split $
        merge
          (TwoWay.sequenceDefns conflicts)
          (TwoWay.sequenceDefns (Unconflicts.soloDeletedNames unconflicts))
          (TwoWay.sequenceDefns (Unconflicts.soloUpdatedNames unconflicts))
          (TwoWay.sequenceDefns (bimap (Map.fromSet (const ())) (Map.fromSet (const ())) <$> dependents))
  where
    splitV :: Map Name (EitherWay ()) -> TwoWay (Set Name)
    splitV =
      Map.foldlWithKey'
        ( \acc name -> \case
            EitherWay.Alice () -> let !alice = Set.insert name acc.alice in TwoWay {alice, bob = acc.bob}
            EitherWay.Bob () -> let !bob = Set.insert name acc.bob in TwoWay {alice = acc.alice, bob}
        )
        (TwoWay Set.empty Set.empty)

-- Merge Alice and Bob dependents together.
--
-- For an Alice dependent,
--
--   1. If it's Alice-conflicted, drop it (since we only want to return *unconflicted* dependents).
--   2. Otherwise, if Bob deleted it, drop it.
--   3. Otherwise, if Bob updated it, use Bob's version.
--   4. Otherwise, either Alice updated it (so use her version) or neither party updated it (so it's synhash-equal, and
--      we can therefore arbitrarily use Alice's).
mergeDependentsV ::
  forall name.
  (Ord name) =>
  TwoWay (Set name) ->
  TwoWay (Set name) ->
  TwoWay (Set name) ->
  TwoWay (Map name ()) ->
  Map name (EitherWay ())
mergeDependentsV conflicts deletes updates =
  TwoWay.twoWay $
    Map.merge
      (Map.mapMaybeMissing onlyAlice)
      (Map.mapMaybeMissing onlyBob)
      (Map.zipWithMaybeMatched aliceAndBob)
  where
    onlyAlice :: name -> () -> Maybe (EitherWay ())
    onlyAlice name ()
      | Set.member name conflicts.alice = Nothing
      | Set.member name deletes.bob = Nothing
      | Set.member name updates.bob = Just (EitherWay.Bob ())
      | otherwise = Just (EitherWay.Alice ())

    onlyBob :: name -> () -> Maybe (EitherWay ())
    onlyBob name ()
      | Set.member name conflicts.bob = Nothing
      | Set.member name deletes.alice = Nothing
      | Set.member name updates.alice = Just (EitherWay.Alice ())
      | otherwise = Just (EitherWay.Bob ())

    aliceAndBob :: name -> () -> () -> Maybe (EitherWay ())
    aliceAndBob name () ()
      | Set.member name conflicts.alice = Nothing
      | Set.member name conflicts.bob = Nothing
      | Set.member name updates.bob = Just (EitherWay.Bob ())
      | otherwise = Just (EitherWay.Alice ())

nameHydratedRefs ::
  Defns (Map TermReferenceId term) (Map TypeReferenceId typ) ->
  DefnsF (Map name) Referent TypeReference ->
  DefnsF (Map name) (TermReferenceId, term) (TypeReferenceId, typ)
nameHydratedRefs =
  zipDefnsWith (f Referent.toTermReferenceId) (f Reference.toId)
  where
    f :: (defn -> Maybe Reference.Id) -> Map Reference.Id term -> Map name defn -> Map name (Reference.Id, term)
    f toId refToDefn nameToRef =
      Map.mapMaybe (toId >=> \ref -> (ref,) <$> Map.lookup ref refToDefn) nameToRef

makeStageOne ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF Set Name Name) ->
  DefnsF Unconflicts term typ ->
  TwoWay (DefnsF Set Name Name) ->
  DefnsF (Map Name) term typ ->
  DefnsF (Map Name) term typ
makeStageOne declNameLookups conflicts unconflicts dependents =
  zipDefnsWith3 makeStageOneV makeStageOneV unconflicts (f conflicts <> f dependents)
  where
    f :: TwoWay (DefnsF Set Name Name) -> DefnsF Set Name Name
    f defns =
      fold (refIdsToNames <$> declNameLookups <*> defns)

makeStageOneV :: Unconflicts v -> Set Name -> Map Name v -> Map Name v
makeStageOneV unconflicts namesToDelete =
  (`Map.withoutKeys` namesToDelete) . Unconflicts.apply unconflicts

-- Given just named term/type reference ids, fill out all names that occupy the term and type namespaces. This is simply
-- the given names plus all of the types' constructors.
--
-- For example, if the input is
--
--   declNameLookup = {
--     "Maybe" => ["Maybe.Nothing", "Maybe.Just"]
--   }
--   defns = {
--     terms = { "foo" => #foo }
--     types = { "Maybe" => #Maybe }
--   }
--
-- then the output is
--
--   defns = {
--     terms = { "foo", "Maybe.Nothing", "Maybe.Just" }
--     types = { "Maybe" }
--   }
refIdsToNames :: DeclNameLookup -> DefnsF Set Name Name -> DefnsF Set Name Name
refIdsToNames declNameLookup =
  bifoldMap goTerms goTypes
  where
    goTerms :: Set Name -> DefnsF Set Name Name
    goTerms terms =
      Defns {terms, types = Set.empty}

    goTypes :: Set Name -> DefnsF Set Name Name
    goTypes types =
      Defns
        { terms = foldMap (Set.fromList . DeclNameLookup.expectConstructorNames declNameLookup) types,
          types
        }

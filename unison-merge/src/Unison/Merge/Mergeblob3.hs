module Unison.Merge.Mergeblob3
  ( Mergeblob3 (..),
    makeMergeblob3,
  )
where

import Control.Lens (mapped)
import Data.Align (align)
import Data.Bifoldable (bifoldMap)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.These (These (..))
import Data.Zip (unzip)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup, expectConstructorNames)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.Merge.Mergeblob2 (Mergeblob2 (..))
import Unison.Merge.PrettyPrintEnv (makePrettyPrintEnv)
import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay)
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Unconflicts (Unconflicts)
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Name (Name)
import Unison.Names (Names (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Reference (Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith, zipDefnsWith3, zipDefnsWith4)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Prelude hiding (unzip)

data Mergeblob3 = Mergeblob3
  { libdeps :: Names,
    stageOne :: DefnsF (Map Name) Referent TypeReference,
    stageTwo :: DefnsF (Map Name) Referent TypeReference,
    uniqueTypeGuids :: Map Name Text,
    unparsedFile :: Pretty ColorText
  }

makeMergeblob3 ::
  Mergeblob2 libdep ->
  TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  Names ->
  TwoWay Text ->
  Mergeblob3
makeMergeblob3 blob dependents0 libdeps authors =
  let conflictsNames :: TwoWay (DefnsF Set Name Name)
      conflictsNames =
        bimap Map.keysSet Map.keysSet <$> blob.conflicts

      -- Identify the unconflicted dependents we need to pull into the Unison file (either first for typechecking, if
      -- there aren't conflicts, or else for manual conflict resolution without a typechecking step, if there are)
      dependents :: TwoWay (DefnsF Set Name Name)
      dependents =
        filterDependents
          conflictsNames
          blob.soloUpdatesAndDeletes
          ( let f :: Set TermReferenceId -> Referent -> NESet Name -> Set Name
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
             in zipDefnsWith
                  (\defns deps -> Map.foldMapWithKey (f deps) (BiMultimap.domain defns))
                  (\defns deps -> Map.foldMapWithKey (g deps) (BiMultimap.domain defns))
                  <$> ThreeWay.forgetLca blob.defns
                  <*> dependents0
          )

      (renderedConflicts, renderedDependents) =
        renderConflictsAndDependents
          blob.declNameLookups
          blob.hydratedDefns
          conflictsNames
          dependents
          (defnsToNames <$> ThreeWay.forgetLca blob.defns)
          libdeps
   in Mergeblob3
        { libdeps,
          stageOne =
            makeStageOne
              blob.declNameLookups
              conflictsNames
              blob.unconflicts
              dependents
              (bimap BiMultimap.range BiMultimap.range blob.defns.lca),
          uniqueTypeGuids = makeUniqueTypeGuids blob.hydratedDefns,
          stageTwo =
            makeStageTwo
              blob.declNameLookups
              conflictsNames
              blob.unconflicts
              dependents
              (bimap BiMultimap.range BiMultimap.range <$> blob.defns),
          unparsedFile = makePrettyUnisonFile authors renderedConflicts renderedDependents
        }

filterDependents ::
  (Ord name) =>
  TwoWay (DefnsF Set name name) ->
  TwoWay (DefnsF Set name name) ->
  TwoWay (DefnsF Set name name) ->
  TwoWay (DefnsF Set name name)
filterDependents conflicts soloUpdatesAndDeletes dependents0 =
  -- There is some subset of Alice's dependents (and ditto for Bob of course) that we don't ultimately want/need to put
  -- into the scratch file: those for which any of the following are true:
  --
  --   1. It is Alice-conflicted (since we only want to return *unconflicted* things).
  --   2. It was deleted by Bob.
  --   3. It was updated by Bob and not updated by Alice.
  let dependents1 =
        zipDefnsWith Set.difference Set.difference
          <$> dependents0
          <*> (conflicts <> TwoWay.swap soloUpdatesAndDeletes)

      -- Of the remaining dependents, it's still possible that the maps are not disjoint. But whenever the same name key
      -- exists in Alice's and Bob's dependents, the value will either be equal (by Unison hash)...
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {"foo" => #alice} } }
      --
      -- ...or synhash-equal (i.e. the term or type received different auto-propagated updates)...
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {"foo" => #bob}   } }
      --
      -- So, we can arbitrarily keep Alice's, because they will render the same.
      --
      --   { alice = { terms = {"foo" => #alice} } }
      --   { bob   = { terms = {}                } }
      dependents2 =
        dependents1 & over #bob \bob ->
          zipDefnsWith Set.difference Set.difference bob dependents1.alice
   in dependents2

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

makeStageTwo ::
  forall term typ.
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF Set Name Name) ->
  DefnsF Unconflicts term typ ->
  TwoWay (DefnsF Set Name Name) ->
  ThreeWay (DefnsF (Map Name) term typ) ->
  DefnsF (Map Name) term typ
makeStageTwo declNameLookups conflicts unconflicts dependents defns =
  zipDefnsWith4 makeStageTwoV makeStageTwoV defns.lca aliceBiasedDependents unconflicts aliceConflicts
  where
    aliceConflicts :: DefnsF (Map Name) term typ
    aliceConflicts =
      zipDefnsWith
        (\defns conflicts -> Map.restrictKeys defns (conflicts <> aliceConstructorsOfTypeConflicts))
        Map.restrictKeys
        defns.alice
        conflicts.alice

    aliceConstructorsOfTypeConflicts :: Set Name
    aliceConstructorsOfTypeConflicts =
      foldMap
        (Set.fromList . DeclNameLookup.expectConstructorNames declNameLookups.alice)
        conflicts.alice.types

    aliceBiasedDependents :: DefnsF (Map Name) term typ
    aliceBiasedDependents =
      TwoWay.twoWay
        (zipDefnsWith (Map.unionWith const) (Map.unionWith const))
        (zipDefnsWith Map.restrictKeys Map.restrictKeys <$> ThreeWay.forgetLca defns <*> dependents)

makeStageTwoV :: Map Name v -> Map Name v -> Unconflicts v -> Map Name v -> Map Name v
makeStageTwoV lca dependents unconflicts conflicts =
  Map.unionWith const conflicts (Unconflicts.apply unconflicts (Map.unionWith const dependents lca))

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
        { terms = foldMap (Set.fromList . expectConstructorNames declNameLookup) types,
          types
        }

renderConflictsAndDependents ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)) ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay Names ->
  Names ->
  ( TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)),
    TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
  )
renderConflictsAndDependents declNameLookups hydratedDefns conflicts dependents names libdepsNames =
  unzip $
    ( \declNameLookup (conflicts, dependents) ->
        let render = renderDefnsForUnisonFile declNameLookup ppe . over (#terms . mapped) snd
         in (render conflicts, render dependents)
    )
      <$> declNameLookups
      <*> hydratedConflictsAndDependents
  where
    hydratedConflictsAndDependents ::
      TwoWay
        ( DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann),
          DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)
        )
    hydratedConflictsAndDependents =
      ( \as bs cs ->
          ( zipDefnsWith Map.restrictKeys Map.restrictKeys as bs,
            zipDefnsWith Map.restrictKeys Map.restrictKeys as cs
          )
      )
        <$> hydratedDefns
        <*> conflicts
        <*> dependents

    ppe :: PrettyPrintEnvDecl
    ppe =
      makePrettyPrintEnv names libdepsNames

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames defns =
  Names
    { terms = Relation.fromMap (BiMultimap.range defns.terms),
      types = Relation.fromMap (BiMultimap.range defns.types)
    }

makePrettyUnisonFile ::
  TwoWay Text ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettyUnisonFile authors conflicts dependents =
  fold
    [ conflicts
        -- Merge the two maps together into one, remembering who authored what
        & TwoWay.twoWay (zipDefnsWith align align)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each conflict, types then terms (even though a type can conflict with a term, in which case they
        -- would not be adjacent in the file), with an author comment above each conflicted thing
        & ( let f =
                  foldMap \case
                    This x -> alice x
                    That y -> bob y
                    These x y -> alice x <> bob y
                  where
                    alice = prettyBinding (Just (Pretty.text authors.alice))
                    bob = prettyBinding (Just (Pretty.text authors.bob))
             in bifoldMap f f
          ),
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      let thereAre defns = TwoWay.or (not . defnsAreEmpty <$> defns)
       in if thereAre conflicts && thereAre dependents
            then
              fold
                [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
                  "-- conflicted definitions above.\n\n"
                ]
            else mempty,
      dependents
        -- Merge dependents together into one map (they are disjoint)
        & TwoWay.twoWay (zipDefnsWith Map.union Map.union)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each dependent, types then terms, without bothering to comment attribution
        & (let f = foldMap (prettyBinding Nothing) in bifoldMap f f)
    ]
  where
    prettyBinding maybeComment binding =
      fold
        [ case maybeComment of
            Nothing -> mempty
            Just comment -> "-- " <> comment <> "\n",
          binding,
          "\n",
          "\n"
        ]

    inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
    inAlphabeticalOrder =
      bimap f f
      where
        f = map snd . List.sortOn (Name.toText . fst) . Map.toList

-- Given Alice's and Bob's hydrated defns, make a mapping from unique type name to unique type GUID, preferring Alice's
-- GUID if they both have one.
makeUniqueTypeGuids ::
  TwoWay
    ( DefnsF
        (Map Name)
        (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        (TypeReferenceId, Decl Symbol Ann)
    ) ->
  Map Name Text
makeUniqueTypeGuids hydratedDefns =
  let -- Start off with just Alice's GUIDs
      aliceGuids :: Map Name Text
      aliceGuids =
        Map.mapMaybe (declGuid . snd) hydratedDefns.alice.types

      -- Define a helper that adds a Bob GUID only if it's not already in the map (so, preferring Alice)
      addBobGuid :: Map Name Text -> (Name, (TypeReferenceId, Decl Symbol Ann)) -> Map Name Text
      addBobGuid acc (name, (_, bobDecl)) =
        Map.alter
          ( \case
              Nothing -> bobGuid
              Just aliceGuid -> Just aliceGuid
          )
          name
          acc
        where
          bobGuid :: Maybe Text
          bobGuid =
            declGuid bobDecl

      -- Tumble in all of Bob's GUIDs with that helper
      allTheGuids :: Map Name Text
      allTheGuids =
        List.foldl' addBobGuid aliceGuids (Map.toList hydratedDefns.bob.types)
   in allTheGuids
  where
    declGuid :: Decl v a -> Maybe Text
    declGuid decl =
      case (DataDeclaration.asDataDecl decl).modifier of
        DataDeclaration.Structural -> Nothing
        DataDeclaration.Unique guid -> Just guid

{-# LANGUAGE OverloadedRecordDot #-}

-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( combineDiffs,
  )
where

import Control.Lens (over, view, (%~))
import Data.Bitraversable (bitraverse)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Merge.AliceIorBob (AliceIorBob (..), whoL)
import Unison.Merge.DeclNameLookup (DeclNameLookup, expectConstructorNames, expectDeclName)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (Reference, TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Referent' qualified as Referent'
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF)

-- Alice exclusive-or Bob?
data AliceXorBob
  = Alice
  | Bob

-- | The combined result of two diffs on the same thing.
data DiffOp2 v
  = Added2 !AliceIorBob !v
  | Updated2 !AliceIorBob !v -- new value
  | Deleted2 !AliceIorBob !v -- old value
  | -- | An add-add or an update-update conflict. We don't consider update-delete a conflict; the delete gets ignored.
    Conflict !(TwoWay v)

data Flicts v = Flicts
  { conflicts :: !(Map Name (TwoWay v)),
    unconflicts :: !(Unconflicts v)
  }
  deriving stock (Generic)

-- | Combine LCA->Alice diff and LCA->Bob diff, then partition into conflicted and unconflicted things.
combineDiffs ::
  TwoWay DeclNameLookup ->
  TwoWay (BiMultimap Referent Name) ->
  TwoWay (DefnsF (Map Name) (DiffOp (Synhashed Referent)) (DiffOp (Synhashed TypeReference))) ->
  Either
    Name
    ( TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
      DefnsF Unconflicts Referent TypeReference
    )
combineDiffs declNameLookups terms diffs = do
  let termConflicts0 :: Map Name (TwoWay Referent)
      termUnconflicts0 :: Unconflicts Referent
      typeConflicts0 :: Map Name (TwoWay TypeReference)
      typeUnconflicts0 :: Unconflicts TypeReference
      Flicts termConflicts0 termUnconflicts0 = partition2 (justTheTerms diffs)
      Flicts typeConflicts0 typeUnconflicts0 = partition2 (justTheTypes diffs)

  let initialConflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference)
      initialConflicts =
        twiddleTermConflicts declNameLookups termConflicts0 <> justTypes (sequenceA typeConflicts0)

  let moreTermConflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference)
      moreTermConflicts =
        justTerms (discoverTermConflicts declNameLookups terms (justTheTypes initialConflicts))

  conflicts <- assertThereAreNoBuiltins (initialConflicts <> moreTermConflicts)

  Right
    ( conflicts,
      Defns
        { terms = dropConflictedUnconflictedTerms declNameLookups conflicts termUnconflicts0,
          types = dropConflictedUnconflictedTypes (justTheTypes conflicts) typeUnconflicts0
        }
    )

partition2 :: TwoWay (Map Name (DiffOp (Synhashed v))) -> Flicts v
partition2 diffs =
  partition (alignWith (combine Alice Bob) diffs.alice diffs.bob)

partition :: Map Name (DiffOp2 v) -> Flicts v
partition =
  Map.foldlWithKey'
    (\s k v -> insert k v s)
    Flicts
      { unconflicts =
          let empty = TwoWayI Map.empty Map.empty Map.empty
           in Unconflicts empty empty empty,
        conflicts = Map.empty
      }
  where
    insert :: Name -> DiffOp2 v -> Flicts v -> Flicts v
    insert k = \case
      Added2 who v -> #unconflicts . #adds . whoL who %~ Map.insert k v
      Deleted2 who v -> #unconflicts . #deletes . whoL who %~ Map.insert k v
      Updated2 who v -> #unconflicts . #updates . whoL who %~ Map.insert k v
      Conflict v -> #conflicts %~ Map.insert k v

combine :: AliceXorBob -> AliceXorBob -> These (DiffOp (Synhashed v)) (DiffOp (Synhashed v)) -> DiffOp2 v
combine this that = \case
  This x -> one this x
  That x -> one that x
  These (Added x) (Added y)
    | x /= y -> Conflict (twoWay this x.value y.value)
    | otherwise -> Added2 AliceAndBob x.value
  These (Updated _ x) (Updated _ y)
    | x /= y -> Conflict (twoWay this x.value y.value)
    | otherwise -> Updated2 AliceAndBob x.value
  -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
  These (Updated _ x) (Deleted _) -> Updated2 (xor2ior this) x.value
  These (Deleted x) (Deleted _) -> Deleted2 AliceAndBob x.value
  -- Handle delete+update the same as update+delete
  These x@(Deleted _) y -> combine that this (These y x)
  -- These don't make sense - e.g. someone can't update something that wasn't there
  These (Updated _ _) (Added _) -> error "impossible"
  These (Added _) (Deleted _) -> error "impossible"
  These (Added _) (Updated _ _) -> error "impossible"

one :: AliceXorBob -> DiffOp (Synhashed v) -> DiffOp2 v
one who = \case
  Added x -> Added2 (xor2ior who) x.value
  Deleted x -> Deleted2 (xor2ior who) x.value
  Updated _ x -> Updated2 (xor2ior who) x.value

-- Make a two way, given who is on the left.
twoWay :: AliceXorBob -> v -> v -> TwoWay v
twoWay Alice alice bob = TwoWay {alice, bob}
twoWay Bob bob alice = TwoWay {alice, bob}

xor2ior :: AliceXorBob -> AliceIorBob
xor2ior = \case
  Alice -> OnlyAlice
  Bob -> OnlyBob

------------------------------------------------------------------------------------------------------------------------
-- Conflict twiddling

-- returned maps don't necessarily have the same keys, e.g. if incoming conflict is
--
--   {
--     terms = {
--       "Maybe.Just" => {
--         alice = #Alice#0,
--         bob = #bob
--       }
--     }
--   }
--
-- (where Alice has a constructor and Bob has a term), then the outgoing maps will be
--
--   {
--     alice = {
--       types = {
--         "Maybe" => #Alice
--       }
--     },
--     bob = {
--       terms = {
--         "Maybe.Just" => #bob
--       }
--     }
--   }

twiddleTermConflicts ::
  TwoWay DeclNameLookup ->
  Map Name (TwoWay Referent) ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference)
twiddleTermConflicts declNameLookups =
  Map.foldlWithKey' f (let empty = Defns Map.empty Map.empty in TwoWay empty empty)
  where
    f ::
      TwoWay (DefnsF (Map Name) TermReference TypeReference) ->
      Name ->
      TwoWay Referent ->
      TwoWay (DefnsF (Map Name) TermReference TypeReference)
    f acc name referents =
      twiddleTermConflict name <$> declNameLookups <*> referents <*> acc

twiddleTermConflict ::
  Name ->
  DeclNameLookup ->
  Referent ->
  DefnsF (Map Name) TermReference TypeReference ->
  DefnsF (Map Name) TermReference TypeReference
twiddleTermConflict name declNameLookup = \case
  Referent'.Con' (ConstructorReference ref _) _ -> over #types (Map.insert (expectDeclName declNameLookup name) ref)
  Referent'.Ref' ref -> over #terms (Map.insert name ref)

discoverTermConflicts ::
  TwoWay DeclNameLookup ->
  TwoWay (BiMultimap Referent Name) ->
  TwoWay (Map Name TypeReference) ->
  TwoWay (Map Name TermReference)
discoverTermConflicts declNameLookups terms typeConflicts =
  TwoWay.swap (f <$> declNameLookups <*> typeConflicts <*> TwoWay.swap terms)
  where
    f ::
      DeclNameLookup ->
      Map Name TypeReference ->
      BiMultimap Referent Name ->
      Map Name TermReference
    f myDeclNameLookup myTypeConflicts theirTerms =
      Map.foldlWithKey' (g myDeclNameLookup theirTerms) Map.empty myTypeConflicts

    g ::
      DeclNameLookup ->
      BiMultimap Referent Name ->
      Map Name TermReference ->
      Name ->
      TypeReference ->
      Map Name TermReference
    g myDeclNameLookup theirTerms acc myDeclName _ =
      List.foldl' (h theirTerms) acc (expectConstructorNames myDeclNameLookup myDeclName)

    h :: BiMultimap Referent Name -> Map Name TermReference -> Name -> Map Name TermReference
    h theirTerms acc myConName =
      fromMaybe acc do
        theirReferent <- BiMultimap.lookupRan myConName theirTerms
        theirTerm <- Referent.toTermReference theirReferent
        Just (Map.insert myConName theirTerm acc)

assertThereAreNoBuiltins ::
  TwoWay (DefnsF (Map Name) TermReference TypeReference) ->
  Either Name (TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId))
assertThereAreNoBuiltins =
  traverse (bitraverse (Map.traverseWithKey assertTermIsntBuiltin) (Map.traverseWithKey assertTypeIsntBuiltin))
  where
    assertTermIsntBuiltin :: Name -> TermReference -> Either Name TermReferenceId
    assertTermIsntBuiltin name ref =
      case Reference.toId ref of
        Nothing -> Left name
        Just refId -> Right refId

    -- Same body as above, but could be different some day (e.g. return value tells you what namespace)
    assertTypeIsntBuiltin :: Name -> TypeReference -> Either Name TypeReferenceId
    assertTypeIsntBuiltin name ref =
      case Reference.toId ref of
        Nothing -> Left name
        Just refId -> Right refId

------------------------------------------------------------------------------------------------------------------------
-- Unconflict twiddling

dropConflictedUnconflictedTerms ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  Unconflicts Referent ->
  Unconflicts Referent
dropConflictedUnconflictedTerms declNameLookups conflicts =
  over #adds f . over #updates f
  where
    f :: TwoWayI (Map Name Referent) -> TwoWayI (Map Name Referent)
    f =
      over #alice (Map.filterWithKey aliceIsConflicted)
        . over #bob (Map.filterWithKey bobIsConflicted)
        . over #both (Map.filterWithKey \name ref -> aliceIsConflicted name ref || bobIsConflicted name ref)
      where
        isConflicted :: DeclNameLookup -> DefnsF (Map Name) TermReferenceId TypeReferenceId -> Name -> Referent -> Bool
        isConflicted declNameLookup conflicts name = \case
          Referent'.Con' _ _ -> Map.notMember (expectDeclName declNameLookup name) conflicts.types
          Referent'.Ref' _ -> Map.notMember name conflicts.terms

        aliceIsConflicted = isConflicted declNameLookups.alice conflicts.alice
        bobIsConflicted = isConflicted declNameLookups.bob conflicts.bob

dropConflictedUnconflictedTypes ::
  TwoWay (Map Name types) ->
  Unconflicts TypeReference ->
  Unconflicts TypeReference
dropConflictedUnconflictedTypes conflicts =
  over #adds f . over #updates f
  where
    f :: TwoWayI (Map Name Reference) -> TwoWayI (Map Name Reference)
    f =
      over #alice dropAliceConflicted
        . over #bob dropBobConflicted
        . over #both (dropAliceConflicted . dropBobConflicted)
      where
        dropAliceConflicted = (`Map.difference` conflicts.alice)
        dropBobConflicted = (`Map.difference` conflicts.bob)

------------------------------------------------------------------------------------------------------------------------
-- Misc. helpers

justTerms :: TwoWay (Map name terms) -> TwoWay (DefnsF (Map name) terms types)
justTerms =
  fmap (\terms -> Defns terms Map.empty)

justTheTerms :: TwoWay (DefnsF (Map name) terms types) -> TwoWay (Map name terms)
justTheTerms =
  fmap (view #terms)

justTypes :: TwoWay (Map name types) -> TwoWay (DefnsF (Map name) terms types)
justTypes =
  fmap (\types -> Defns Map.empty types)

justTheTypes :: TwoWay (DefnsF (Map name) terms types) -> TwoWay (Map name types)
justTheTypes =
  fmap (view #types)

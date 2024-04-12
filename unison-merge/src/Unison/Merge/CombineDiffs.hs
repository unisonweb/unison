{-# LANGUAGE OverloadedRecordDot #-}

-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( combineDiffs,
  )
where

import Control.Lens (Lens', over, view, (%~), (.~), _1, _2)
import Data.Bifoldable (binull)
import Data.Bitraversable (bitraverse)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Merge.AliceIorBob (AliceIorBob (..))
import Unison.Merge.AliceXorBob (AliceXorBob (..))
import Unison.Merge.AliceXorBob qualified as AliceXorBob
import Unison.Merge.DeclNameLookup (DeclNameLookup, expectConstructorNames, expectDeclName)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoDiffOps (TwoDiffOps (..), combineTwoDiffOps)
import Unison.Merge.TwoWay (TwoWay (..), twoWay)
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.TwoWayI qualified as TwoWayI
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
import Unison.Util.Map qualified as Map

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
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (DefnsF (Map Name) (DiffOp (Synhashed Referent)) (DiffOp (Synhashed TypeReference))) ->
  Either
    Name
    ( TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
      DefnsF Unconflicts Referent TypeReference
    )
combineDiffs declNameLookups defns diffs = do
  let honk =
        oingoBoingo
          declNameLookups
          defns
          ( bimap
              (twoWay (alignWith combine'terms))
              (twoWay (Map.alignWithKey (combine'types declNameLookups)))
              (TwoWay.sequenceDefns diffs)
          )

  let (termConflicts0, termUnconflicts0) = partition2 (TwoWay.justTheTerms diffs)
      (typeConflicts0, typeUnconflicts0) = partition2 (TwoWay.justTheTypes diffs)

  let initialConflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference)
      initialConflicts =
        twiddleTermConflicts declNameLookups termConflicts0 <> justTypes (sequenceA typeConflicts0)

  let moreTermConflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference)
      moreTermConflicts =
        justTerms (discoverTermConflicts declNameLookups (view #terms <$> defns) (TwoWay.justTheTypes initialConflicts))

  conflicts <- assertThereAreNoBuiltins (initialConflicts <> moreTermConflicts)

  let termUnconflicts = dropConflictedUnconflictedTerms declNameLookups conflicts termUnconflicts0
  let typeUnconflicts = dropConflictedUnconflictedTypes (TwoWay.justTheTypes conflicts) typeUnconflicts0

  Right (conflicts, Defns termUnconflicts typeUnconflicts)

oingoBoingo ::
  TwoWay DeclNameLookup ->
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Defns (Map Name (DiffOp2 Referent)) (Map Name (DiffOp2 TypeReference)) ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference)
oingoBoingo declNameLookups defns diff =
  let initialQueues :: TwoWay (DefnsF [] Name Name)
      initialQueues =
        bitraverse honkTerms honkTypes diff

      -- added term: we keep the add
      -- added constructor:
      --   - if its type is me-conflicted then we drop the add
      --   - else if its type is them-conflicted then we promote our type to me-conflicted
      --     - example:
      --         LCA:   type Foo = Foo | Bar
      --         Alice: type Foo = Foo | Bar | Baz
      --         Bob:   type Foo = Foo | Oink
      --
      --       Alice diff:
      --         updated type Foo
      --         updated con Foo.Bar
      --         updated con Foo.Baz
      --
      --       Bob diff:
      --         deleted con Foo.Bar
      --         added con Foo.Oink
      --
      --       in this example, "Foo.Oink" is added in Bob, "Foo" is not conflicted in Bob, but
      --
      --
      -- after finding an initial batch of type conflicts,
      --
      -- 1. some constructor adds/updates will be for types that are conflicted. we can drop these!
      -- 2. others will be for types that are updates, but aren't conflicted. we keep these!
      -- 3. others will be for types that haven't been updated, but we only keep these if the other person's type isn't
      --    conflicted or updated; if it is, then our changes to the type's constructor essentially promote the type to
      --    conflicted! uh oh! this can cascade... so what order do we do these checks in?

      honkTerms :: Map Name (DiffOp2 term) -> TwoWay [Name]
      honkTerms =
        Map.foldlWithKey' f (TwoWay [] [])
        where
          f :: TwoWay [Name] -> Name -> DiffOp2 term -> TwoWay [Name]
          f acc name = \case
            Conflict _ -> (name :) <$> acc
            -- FIXME are these right? just dealing with conflicts for now, then will circle back and think
            Added2 _ _ -> acc
            Deleted2 _ _ -> acc
            Updated2 _ _ -> acc

      -- FIXME same as honkTerms, forever tho?
      honkTypes :: Map Name (DiffOp2 typ) -> TwoWay [Name]
      honkTypes =
        Map.foldlWithKey' f (TwoWay [] [])
        where
          f :: TwoWay [Name] -> Name -> DiffOp2 typ -> TwoWay [Name]
          f acc name = \case
            Conflict _ -> (name :) <$> acc
            -- FIXME are these right? just dealing with conflicts for now, then will circle back and think
            Added2 _ _ -> acc
            Deleted2 _ _ -> acc
            Updated2 _ _ -> acc
   in boingo2
        declNameLookups
        defns
        S
          { me = Alice,
            conflicts = mempty,
            stacks = initialQueues
          }

data S = S
  { me :: !AliceXorBob,
    conflicts :: !(TwoWay (DefnsF (Map Name) TermReference TypeReference)),
    stacks :: !(TwoWay (DefnsF [] Name Name))
  }
  deriving stock (Generic)

boingo2 ::
  TwoWay DeclNameLookup ->
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  S ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference)
boingo2 declNameLookups defns =
  loop
  where
    loop :: S -> TwoWay (DefnsF (Map Name) TermReference TypeReference)
    loop s =
      case (view myTermStack_ s, view myTypeStack_ s, binull (view theirStacks_ s)) of
        (name : names, _, _) -> loop (poppedTerm name (s & myTermStack_ .~ names))
        ([], name : names, _) -> loop (poppedType name (s & myTypeStack_ .~ names))
        ([], [], False) -> loop (s & #me %~ AliceXorBob.swap)
        ([], [], True) -> s.conflicts
      where
        poppedTerm name =
          case BiMultimap.lookupRan name (view (me_ . #terms) defns) of
            Nothing -> id
            Just (Referent.Ref ref) -> over myTermConflicts_ (Map.insert name ref)
            Just (Referent.Con _ _) -> over myTypeStack_ (expectDeclName myDeclNameLookup name :)

        poppedType name =
          case BiMultimap.lookupRan name (view (me_ . #types) defns) of
            Nothing -> id
            Just ref ->
              (foldr (.) id)
                [ over myTypeConflicts_ (Map.insert name ref),
                  over theirTermStack_ (expectConstructorNames myDeclNameLookup name ++)
                ]

        me_ :: Lens' (TwoWay a) a
        me_ = TwoWay.who_ s.me

        myConflicts_ :: Lens' S (DefnsF (Map Name) TermReference TypeReference)
        myConflicts_ = #conflicts . me_

        myTermConflicts_ :: Lens' S (Map Name TermReference)
        myTermConflicts_ = myConflicts_ . #terms

        myTypeConflicts_ :: Lens' S (Map Name TermReference)
        myTypeConflicts_ = myConflicts_ . #types

        myStacks_ :: Lens' S (DefnsF [] Name Name)
        myStacks_ = #stacks . me_

        myTermStack_ :: Lens' S [Name]
        myTermStack_ = myStacks_ . #terms

        myTypeStack_ :: Lens' S [Name]
        myTypeStack_ = myStacks_ . #types

        myDeclNameLookup :: DeclNameLookup
        myDeclNameLookup = view me_ declNameLookups

        them_ :: Lens' (TwoWay a) a
        them_ = TwoWay.who_ (AliceXorBob.swap s.me)

        theirStacks_ :: Lens' S (DefnsF [] Name Name)
        theirStacks_ = #stacks . them_

        theirTermStack_ :: Lens' S [Name]
        theirTermStack_ = theirStacks_ . #terms

partition2 :: TwoWay (Map Name (DiffOp (Synhashed v))) -> (Map Name (TwoWay v), Unconflicts v)
partition2 =
  partition . twoWay (alignWith (combine Alice Bob))

partition :: Map Name (DiffOp2 v) -> (Map Name (TwoWay v), Unconflicts v)
partition =
  Map.foldlWithKey'
    (\s k v -> insert k v s)
    ( Map.empty,
      let empty = TwoWayI Map.empty Map.empty Map.empty
       in Unconflicts empty empty empty
    )
  where
    insert :: Name -> DiffOp2 v -> (Map Name (TwoWay v), Unconflicts v) -> (Map Name (TwoWay v), Unconflicts v)
    insert k = \case
      Conflict v -> _1 %~ Map.insert k v
      Added2 who v -> _2 . #adds . TwoWayI.who_ who %~ Map.insert k v
      Deleted2 who v -> _2 . #deletes . TwoWayI.who_ who %~ Map.insert k v
      Updated2 who v -> _2 . #updates . TwoWayI.who_ who %~ Map.insert k v

combine :: AliceXorBob -> AliceXorBob -> These (DiffOp (Synhashed v)) (DiffOp (Synhashed v)) -> DiffOp2 v
combine this that = \case
  This x -> one this x
  That x -> one that x
  These (DiffOp'Add x) (DiffOp'Add y)
    | x /= y -> Conflict (two this x.value y.value)
    | otherwise -> Added2 AliceAndBob x.value
  These (DiffOp'Update _ x) (DiffOp'Update _ y)
    | x /= y -> Conflict (two this x.value y.value)
    | otherwise -> Updated2 AliceAndBob x.value
  -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
  These (DiffOp'Update _ x) (DiffOp'Delete _) -> Updated2 (xor2ior this) x.value
  These (DiffOp'Delete x) (DiffOp'Delete _) -> Deleted2 AliceAndBob x.value
  -- Handle delete+update the same as update+delete
  These x@(DiffOp'Delete _) y -> combine that this (These y x)
  -- These don't make sense - e.g. someone can't update something that wasn't there
  These (DiffOp'Update _ _) (DiffOp'Add _) -> error "impossible"
  These (DiffOp'Add _) (DiffOp'Delete _) -> error "impossible"
  These (DiffOp'Add _) (DiffOp'Update _ _) -> error "impossible"
  where
    one :: AliceXorBob -> DiffOp (Synhashed v) -> DiffOp2 v
    one who = \case
      DiffOp'Add x -> Added2 (xor2ior who) x.value
      DiffOp'Delete x -> Deleted2 (xor2ior who) x.value
      DiffOp'Update _ x -> Updated2 (xor2ior who) x.value

    -- Make a two way, given who is on the left.
    two :: AliceXorBob -> v -> v -> TwoWay v
    two Alice alice bob = TwoWay {alice, bob}
    two Bob bob alice = TwoWay {alice, bob}

combine'terms :: These (DiffOp (Synhashed term)) (DiffOp (Synhashed term)) -> DiffOp2 term
combine'terms =
  combineTwoDiffOps >>> \case
    TwoDiffOps'Add who x -> Added2 (xor2ior who) x.value
    TwoDiffOps'Delete who x -> Deleted2 (xor2ior who) x.value
    TwoDiffOps'Update who _old new -> Updated2 (xor2ior who) new.value
    TwoDiffOps'AddAdd TwoWay {alice, bob}
      | alice /= bob -> Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> Added2 AliceAndBob alice.value
    TwoDiffOps'DeleteDelete x -> Deleted2 AliceAndBob x.value
    -- These two are not a conflicts, perhaps only temporarily, because it's easier to implement. We just ignore these
    -- deletes and keep the updates.
    TwoDiffOps'DeleteUpdate _old new -> Updated2 OnlyBob new.value
    TwoDiffOps'UpdateDelete _old new -> Updated2 OnlyAlice new.value
    TwoDiffOps'UpdateUpdate _old TwoWay {alice, bob}
      | alice /= bob -> Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> Updated2 AliceAndBob alice.value

combine'types ::
  TwoWay DeclNameLookup ->
  Name ->
  These (DiffOp (Synhashed typ)) (DiffOp (Synhashed typ)) ->
  DiffOp2 typ
combine'types declNameLookups name =
  combineTwoDiffOps >>> \case
    TwoDiffOps'Add who x -> Added2 (xor2ior who) x.value
    TwoDiffOps'Delete who x -> Deleted2 (xor2ior who) x.value
    -- Treat one person updating a type and the other just moving constructors around as a conflict
    TwoDiffOps'Update who old new
      | differentNamesForConstructors ->
          TwoWay {alice = new.value, bob = old.value}
            & (case who of Alice -> id; Bob -> TwoWay.swap)
            & Conflict
      | otherwise -> Updated2 (xor2ior who) new.value
    TwoDiffOps'AddAdd TwoWay {alice, bob}
      | conflicting alice bob -> Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> Added2 AliceAndBob alice.value
    TwoDiffOps'DeleteDelete x -> Deleted2 AliceAndBob x.value
    TwoDiffOps'DeleteUpdate _old new -> Updated2 OnlyBob new.value
    TwoDiffOps'UpdateDelete _old new -> Updated2 OnlyAlice new.value
    TwoDiffOps'UpdateUpdate _old TwoWay {alice, bob}
      | conflicting alice bob -> Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> Updated2 AliceAndBob alice.value
  where
    -- We consider type decls in conflict if they are different (obviously) *or* if they don't have the exact same names
    -- for all of the constructors.
    --
    -- This is, in a sense, a conservative definition of "conflicted" - surely Alice ought to be able to rename one
    -- constructor, and Bob another.
    --
    -- However, it simplifies two cases, and possibly more, to just throw this kind of thing back to the user to
    -- resolve:
    --
    --   1. Alice and Bob each rename the same constructor to two different things, resulting in a decl that violates
    --      the condition that each constructor has one name.
    --
    --   2. Alice updates a type decl while Bob merely renames one of its constructors.
    conflicting :: Synhashed typ -> Synhashed typ -> Bool
    conflicting alice bob =
      alice /= bob || differentNamesForConstructors

    differentNamesForConstructors :: Bool
    differentNamesForConstructors =
      expectConstructorNames declNameLookups.alice name /= expectConstructorNames declNameLookups.bob name

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

justTypes :: TwoWay (Map name types) -> TwoWay (DefnsF (Map name) terms types)
justTypes =
  fmap (\types -> Defns Map.empty types)

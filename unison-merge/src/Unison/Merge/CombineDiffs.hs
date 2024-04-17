{-# LANGUAGE OverloadedRecordDot #-}

-- | Combine two diffs together.
module Unison.Merge.CombineDiffs
  ( combineDiffs,
  )
where

import Control.Lens (Lens', over, view, (%~), (.~))
import Data.Bitraversable (bitraverse)
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.These (These (..))
import Unison.Merge.AliceIorBob (AliceIorBob (..))
import Unison.Merge.AliceXorBob (AliceXorBob (..))
import Unison.Merge.AliceXorBob qualified as AliceXorBob
import Unison.Merge.DeclNameLookup (DeclNameLookup (..), expectConstructorNames, expectDeclName)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.TwoDiffOps (TwoDiffOps (..), combineTwoDiffOps)
import Unison.Merge.TwoWay (TwoWay (..), twoWay)
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.TwoWayI qualified as TwoWayI
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (Reference' (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, DefnsF4, defnsAreEmpty)
import Unison.Util.Map qualified as Map

-- | The combined result of two diffs on the same thing.
data CombinedDiffOps ref
  = CombinedDiffOps'Add !AliceIorBob !ref
  | CombinedDiffOps'Delete !AliceIorBob !ref -- old value
  | CombinedDiffOps'Update !AliceIorBob !ref !ref -- old value, new value
  | -- An add-add or an update-update conflict. We don't consider update-delete a conflict; the delete gets ignored.
    CombinedDiffOps'Conflict !(TwoWay ref)

-- | Combine LCA->Alice diff and LCA->Bob diff, then partition into conflicted and unconflicted things.
combineDiffs ::
  TwoWay DeclNameLookup ->
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  Either
    Name
    ( TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
      DefnsF Unconflicts Referent TypeReference
    )
combineDiffs declNameLookups defns diffs = do
  let diffs1 :: DefnsF4 TwoWay (Map Name) DiffOp Synhashed Referent TypeReference
      diffs1 =
        TwoWay.sequenceDefns diffs

  let diffs2 :: DefnsF2 (Map Name) CombinedDiffOps Referent TypeReference
      diffs2 =
        let f = twoWay (alignWith combine)
         in bimap f f diffs1

  let conflicts0 = identifyConflicts declNameLookups defns diffs2

  let unconflicts = identifyUnconflicts declNameLookups conflicts0 diffs2

  conflicts <- assertThereAreNoBuiltins conflicts0

  Right (conflicts, unconflicts)

identifyConflicts ::
  HasCallStack =>
  TwoWay DeclNameLookup ->
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  DefnsF2 (Map Name) CombinedDiffOps Referent TypeReference ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference)
identifyConflicts declNameLookups defns =
  loop . makeInitialIdentifyConflictsState
  where
    loop :: S -> TwoWay (DefnsF (Map Name) TermReference TypeReference)
    loop s =
      case (view myTermStack_ s, view myTypeStack_ s, defnsAreEmpty (view theirStacks_ s)) of
        (name : names, _, _) -> loop (poppedTerm name (s & myTermStack_ .~ names))
        ([], name : names, _) -> loop (poppedType name (s & myTypeStack_ .~ names))
        ([], [], False) -> loop (s & #me %~ AliceXorBob.swap)
        ([], [], True) -> s.conflicts
      where
        poppedTerm :: Name -> S -> S
        poppedTerm name =
          case BiMultimap.lookupRan name (view (me_ . #terms) defns) of
            Nothing -> id
            Just (Referent.Ref ref) -> over myTermConflicts_ (Map.insert name ref)
            Just (Referent.Con _ _) -> over myTypeStack_ (expectDeclName myDeclNameLookup name :)

        poppedType :: Name -> S -> S
        poppedType name s =
          fromMaybe s do
            ref <- BiMultimap.lookupRan name (view (me_ . #types) defns)
            conflicts <- Map.upsertF (maybe (Just ref) (const Nothing)) name (view myTypeConflicts_ s)
            Just $
              s
                & myTypeConflicts_ .~ conflicts
                & case ref of
                  ReferenceBuiltin _ -> id -- builtin types don't have constructors
                  ReferenceDerived _ -> theirTermStack_ %~ (expectConstructorNames myDeclNameLookup name ++)

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

identifyUnconflicts ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference) ->
  DefnsF2 (Map Name) CombinedDiffOps Referent TypeReference ->
  DefnsF Unconflicts Referent TypeReference
identifyUnconflicts declNameLookups conflicts =
  bimap (identifyTermUnconflicts declNameLookups conflicts) (identifyTypeUnconflicts (view #types <$> conflicts))

identifyTermUnconflicts ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference) ->
  Map Name (CombinedDiffOps Referent) ->
  Unconflicts Referent
identifyTermUnconflicts declNameLookups conflicts =
  Map.foldlWithKey' (\acc name op -> f name op acc) Unconflicts.empty
  where
    f :: Name -> CombinedDiffOps Referent -> Unconflicts Referent -> Unconflicts Referent
    f name = \case
      CombinedDiffOps'Add who ref ->
        case ref of
          Referent.Ref _ -> keepIt1
          Referent.Con _ _ -> if constructor who then ignoreIt else keepIt1
        where
          keepIt1 = keepIt #adds who name ref
      CombinedDiffOps'Update who _old new ->
        case new of
          Referent.Ref _ ->
            case who of
              OnlyAlice -> if termIsConflicted.alice then ignoreIt else keepIt1
              OnlyBob -> if termIsConflicted.bob then ignoreIt else keepIt1
              AliceAndBob -> keepIt1
          Referent.Con _ _ -> if constructor who then ignoreIt else keepIt1
        where
          keepIt1 = keepIt #updates who name new
      CombinedDiffOps'Delete who ref -> keepIt #deletes who name ref
      CombinedDiffOps'Conflict _ -> ignoreIt
      where
        -- Ignore added/updated constructors whose types are conflicted
        constructor :: AliceIorBob -> Bool
        constructor = \case
          OnlyAlice -> constructorHasConflictedType.alice
          OnlyBob -> constructorHasConflictedType.bob
          AliceAndBob -> TwoWay.or constructorHasConflictedType

        constructorHasConflictedType :: TwoWay Bool
        constructorHasConflictedType =
          (\conflicts1 declNameLookup -> Map.member (expectDeclName declNameLookup name) conflicts1.types)
            <$> conflicts
            <*> declNameLookups

        termIsConflicted :: TwoWay Bool
        termIsConflicted =
          Map.member name . view #terms <$> conflicts

identifyTypeUnconflicts ::
  TwoWay (Map Name TypeReference) ->
  Map Name (CombinedDiffOps TypeReference) ->
  Unconflicts TypeReference
identifyTypeUnconflicts conflicts =
  Map.foldlWithKey' (\acc name ref -> f name ref acc) Unconflicts.empty
  where
    f :: Name -> CombinedDiffOps TypeReference -> Unconflicts TypeReference -> Unconflicts TypeReference
    f name = \case
      CombinedDiffOps'Add who ref -> addOrUpdate #adds who ref
      CombinedDiffOps'Update who _old new -> addOrUpdate #updates who new
      CombinedDiffOps'Delete who ref -> keepIt #deletes who name ref
      CombinedDiffOps'Conflict _ -> ignoreIt
      where
        addOrUpdate :: Lens' (Unconflicts v) (TwoWayI (Map Name v)) -> AliceIorBob -> v -> Unconflicts v -> Unconflicts v
        addOrUpdate l who ref =
          case who of
            OnlyAlice -> if typeIsConflicted.alice then ignoreIt else keepIt1
            OnlyBob -> if typeIsConflicted.bob then ignoreIt else keepIt1
            AliceAndBob -> if TwoWay.or typeIsConflicted then ignoreIt else keepIt1
          where
            keepIt1 = keepIt l who name ref

        typeIsConflicted :: TwoWay Bool
        typeIsConflicted =
          Map.member name <$> conflicts

keepIt ::
  Lens' (Unconflicts v) (TwoWayI (Map Name v)) ->
  AliceIorBob ->
  Name ->
  v ->
  Unconflicts v ->
  Unconflicts v
keepIt what who name ref =
  over (what . TwoWayI.who_ who) (Map.insert name ref)

ignoreIt :: Unconflicts v -> Unconflicts v
ignoreIt =
  id

makeInitialIdentifyConflictsState :: DefnsF2 (Map Name) CombinedDiffOps Referent TypeReference -> S
makeInitialIdentifyConflictsState diff =
  S
    { me = Alice,
      conflicts = mempty,
      stacks =
        let f = TwoWay.bothWays . justTheConflictedNames
         in bitraverse f f diff
    }

-- Given a combined diff, return the names that are conflicted.
justTheConflictedNames :: Map Name (CombinedDiffOps a) -> [Name]
justTheConflictedNames =
  Map.foldlWithKey' f []
  where
    f :: [Name] -> Name -> CombinedDiffOps term -> [Name]
    f names name = \case
      CombinedDiffOps'Conflict _ -> name : names
      CombinedDiffOps'Add _ _ -> names
      CombinedDiffOps'Delete _ _ -> names
      CombinedDiffOps'Update _ _ _ -> names

data S = S
  { me :: !AliceXorBob,
    conflicts :: !(TwoWay (DefnsF (Map Name) TermReference TypeReference)),
    stacks :: !(TwoWay (DefnsF [] Name Name))
  }
  deriving stock (Generic)

combine :: These (DiffOp (Synhashed ref)) (DiffOp (Synhashed ref)) -> CombinedDiffOps ref
combine =
  combineTwoDiffOps >>> \case
    TwoDiffOps'Add who x -> CombinedDiffOps'Add (xor2ior who) x.value
    TwoDiffOps'Delete who x -> CombinedDiffOps'Delete (xor2ior who) x.value
    TwoDiffOps'Update who old new -> CombinedDiffOps'Update (xor2ior who) old.value new.value
    TwoDiffOps'AddAdd TwoWay {alice, bob}
      | alice /= bob -> CombinedDiffOps'Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> CombinedDiffOps'Add AliceAndBob alice.value
    TwoDiffOps'DeleteDelete x -> CombinedDiffOps'Delete AliceAndBob x.value
    -- These two are not a conflicts, perhaps only temporarily, because it's easier to implement. We just ignore these
    -- deletes and keep the updates.
    TwoDiffOps'DeleteUpdate old new -> CombinedDiffOps'Update OnlyBob old.value new.value
    TwoDiffOps'UpdateDelete old new -> CombinedDiffOps'Update OnlyAlice old.value new.value
    TwoDiffOps'UpdateUpdate old TwoWay {alice, bob}
      | alice /= bob -> CombinedDiffOps'Conflict TwoWay {alice = alice.value, bob = bob.value}
      | otherwise -> CombinedDiffOps'Update AliceAndBob old.value alice.value

xor2ior :: AliceXorBob -> AliceIorBob
xor2ior = \case
  Alice -> OnlyAlice
  Bob -> OnlyBob

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

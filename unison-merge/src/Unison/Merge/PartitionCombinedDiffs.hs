module Unison.Merge.PartitionCombinedDiffs
  ( partitionCombinedDiffs,
  )
where

import Control.Lens (Lens')
import Data.Bitraversable (bitraverse)
import Data.Map.Strict qualified as Map
import Unison.Merge.CombineDiffs (CombinedDiffOp (..))
import Unison.Merge.DeclNameLookup (DeclNameLookup (..), expectConstructorNames, expectDeclName)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.TwoWayI qualified as TwoWayI
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Merge.Updated (Updated (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (Reference' (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, defnsAreEmpty)
import Unison.Util.Map qualified as Map

-- | Combine LCA->Alice diff and LCA->Bob diff, then partition into conflicted and unconflicted things.
partitionCombinedDiffs ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay DeclNameLookup ->
  DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference ->
  Either
    Name
    ( TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
      DefnsF Unconflicts Referent TypeReference
    )
partitionCombinedDiffs defns declNameLookups diffs = do
  let conflicts0 = identifyConflicts declNameLookups defns diffs
  let unconflicts = identifyUnconflicts declNameLookups conflicts0 diffs
  conflicts <- assertThereAreNoBuiltins conflicts0
  Right (conflicts, unconflicts)

data S = S
  { me :: !(EitherWay ()),
    conflicts :: !(TwoWay (DefnsF (Map Name) TermReference TypeReference)),
    stacks :: !(TwoWay (DefnsF [] Name Name))
  }
  deriving stock (Generic)

makeInitialIdentifyConflictsState :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> S
makeInitialIdentifyConflictsState diff =
  S
    { me = Alice (), -- arbitrary initial person
      conflicts = mempty,
      stacks =
        let f = TwoWay.bothWays . justTheConflictedNames
         in bitraverse f f diff
    }

identifyConflicts ::
  (HasCallStack) =>
  TwoWay DeclNameLookup ->
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference)
identifyConflicts declNameLookups defns =
  loop . makeInitialIdentifyConflictsState
  where
    loop :: S -> TwoWay (DefnsF (Map Name) TermReference TypeReference)
    loop s =
      case (view myTermStack_ s, view myTypeStack_ s, defnsAreEmpty (view theirStacks_ s)) of
        (name : names, _, _) -> loop (poppedTerm name (s & myTermStack_ .~ names))
        ([], name : names, _) -> loop (poppedType name (s & myTypeStack_ .~ names))
        ([], [], False) -> loop (s & #me %~ EitherWay.swap)
        ([], [], True) -> s.conflicts
      where
        poppedTerm :: Name -> S -> S
        poppedTerm name =
          case BiMultimap.lookupRan name (view myTerms_ defns) of
            Nothing -> id
            Just (Referent.Ref ref) -> over myTermConflicts_ (Map.insert name ref)
            Just (Referent.Con _ _) -> over myTypeStack_ (expectDeclName myDeclNameLookup name :)

        poppedType :: Name -> S -> S
        poppedType name s =
          fromMaybe s do
            ref <- BiMultimap.lookupRan name (view myTypes_ defns)
            -- Bail early here (by returning Nothing in the first argument to upsertF) if we've already recorded this
            -- type as conflicted, because in that case we've already added its constructor names to the other person's
            -- term stack, and we only want to do that once.
            typeConflicts <- Map.upsertF (maybe (Just ref) (const Nothing)) name (view myTypeConflicts_ s)
            Just $
              s
                & myTypeConflicts_ .~ typeConflicts
                & case ref of
                  ReferenceBuiltin _ -> id -- builtin types don't have constructors
                  ReferenceDerived _ -> theirTermStack_ %~ (expectConstructorNames myDeclNameLookup name ++)

        me_ :: Lens' (TwoWay a) a
        me_ = TwoWay.who_ s.me

        myTerms_ :: Lens' (TwoWay (Defns terms types)) terms
        myTerms_ = me_ . #terms

        myTypes_ :: Lens' (TwoWay (Defns terms types)) types
        myTypes_ = me_ . #types

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
        them_ = TwoWay.who_ (EitherWay.swap s.me)

        theirStacks_ :: Lens' S (DefnsF [] Name Name)
        theirStacks_ = #stacks . them_

        theirTermStack_ :: Lens' S [Name]
        theirTermStack_ = theirStacks_ . #terms

identifyUnconflicts ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference) ->
  DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference ->
  DefnsF Unconflicts Referent TypeReference
identifyUnconflicts declNameLookups conflicts =
  bimap (identifyTermUnconflicts declNameLookups conflicts) (identifyTypeUnconflicts (view #types <$> conflicts))

identifyTermUnconflicts ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) TermReference TypeReference) ->
  Map Name (CombinedDiffOp Referent) ->
  Unconflicts Referent
identifyTermUnconflicts declNameLookups conflicts =
  Map.foldlWithKey' (\acc name op -> f name op acc) Unconflicts.empty
  where
    f :: Name -> CombinedDiffOp Referent -> Unconflicts Referent -> Unconflicts Referent
    f name = \case
      CombinedDiffOp'Add who ->
        case EitherWayI.value who of
          Referent.Ref _ -> keepIt1
          Referent.Con _ _ -> if constructor who then ignoreIt else keepIt1
        where
          keepIt1 = keepIt #adds who name
      CombinedDiffOp'Update who ->
        case (EitherWayI.value who).new of
          Referent.Ref _ ->
            case who of
              OnlyAlice _ -> if termIsConflicted.alice then ignoreIt else keepIt1
              OnlyBob _ -> if termIsConflicted.bob then ignoreIt else keepIt1
              AliceAndBob _ -> keepIt1
          Referent.Con _ _ -> if constructor who then ignoreIt else keepIt1
        where
          keepIt1 = keepIt #updates (view #new <$> who) name
      CombinedDiffOp'Delete who -> keepIt #deletes who name
      CombinedDiffOp'Conflict _ -> ignoreIt
      where
        -- Ignore added/updated constructors whose types are conflicted
        constructor :: EitherWayI a -> Bool
        constructor = \case
          OnlyAlice _ -> constructorHasConflictedType.alice
          OnlyBob _ -> constructorHasConflictedType.bob
          AliceAndBob _ -> TwoWay.or constructorHasConflictedType

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
  Map Name (CombinedDiffOp TypeReference) ->
  Unconflicts TypeReference
identifyTypeUnconflicts conflicts =
  Map.foldlWithKey' (\acc name ref -> f name ref acc) Unconflicts.empty
  where
    f :: Name -> CombinedDiffOp TypeReference -> Unconflicts TypeReference -> Unconflicts TypeReference
    f name = \case
      CombinedDiffOp'Add who -> addOrUpdate #adds who
      CombinedDiffOp'Update who -> addOrUpdate #updates (view #new <$> who)
      CombinedDiffOp'Delete who -> keepIt #deletes who name
      CombinedDiffOp'Conflict _ -> ignoreIt
      where
        addOrUpdate :: Lens' (Unconflicts v) (TwoWayI (Map Name v)) -> EitherWayI v -> Unconflicts v -> Unconflicts v
        addOrUpdate l who =
          case who of
            OnlyAlice _ -> if typeIsConflicted.alice then ignoreIt else keepIt1
            OnlyBob _ -> if typeIsConflicted.bob then ignoreIt else keepIt1
            AliceAndBob _ -> if TwoWay.or typeIsConflicted then ignoreIt else keepIt1
          where
            keepIt1 = keepIt l who name

        typeIsConflicted :: TwoWay Bool
        typeIsConflicted =
          Map.member name <$> conflicts

keepIt ::
  Lens' (Unconflicts v) (TwoWayI (Map Name v)) ->
  EitherWayI v ->
  Name ->
  Unconflicts v ->
  Unconflicts v
keepIt what who name =
  over (what . TwoWayI.who_ who) (Map.insert name (EitherWayI.value who))

ignoreIt :: Unconflicts v -> Unconflicts v
ignoreIt =
  id

-- Given a combined diff, return the names that are conflicted.
justTheConflictedNames :: Map Name (CombinedDiffOp a) -> [Name]
justTheConflictedNames =
  Map.foldlWithKey' f []
  where
    f :: [Name] -> Name -> CombinedDiffOp term -> [Name]
    f names name = \case
      CombinedDiffOp'Conflict _ -> name : names
      CombinedDiffOp'Add _ -> names
      CombinedDiffOp'Delete _ -> names
      CombinedDiffOp'Update _ -> names

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

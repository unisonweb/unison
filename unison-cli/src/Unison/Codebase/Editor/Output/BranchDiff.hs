{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import Control.Lens
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Codebase.BranchDiff (BranchDiff (BranchDiff))
import Unison.Codebase.BranchDiff qualified as BranchDiff
import Unison.Codebase.Patch qualified as P
import Unison.DataDeclaration (DeclOrBuiltin)
import Unison.HashQualified' (HashQualified)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Syntax.Name ()
import Unison.Type (Type)
import Unison.Util.Relation qualified as R

data BranchDiffOutput v a = BranchDiffOutput
  { updatedTypes :: [UpdateTypeDisplay v a],
    updatedTerms :: [UpdateTermDisplay v a],
    newTypeConflicts :: [UpdateTypeDisplay v a],
    newTermConflicts :: [UpdateTermDisplay v a],
    resolvedTypeConflicts :: [UpdateTypeDisplay v a],
    resolvedTermConflicts :: [UpdateTermDisplay v a],
    updatedPatches :: [PatchDisplay],
    addedTypes :: [AddedTypeDisplay v a],
    addedTerms :: [AddedTermDisplay v a],
    addedPatches :: [PatchDisplay],
    removedTypes :: [RemovedTypeDisplay v a],
    removedTerms :: [RemovedTermDisplay v a],
    removedPatches :: [PatchDisplay],
    renamedTypes :: [RenameTypeDisplay v a],
    renamedTerms :: [RenameTermDisplay v a]
  }
  deriving (Show)

isEmpty :: BranchDiffOutput v a -> Bool
isEmpty BranchDiffOutput {..} =
  null updatedTypes
    && null updatedTerms
    && null newTypeConflicts
    && null newTermConflicts
    && null resolvedTypeConflicts
    && null resolvedTermConflicts
    && null addedTypes
    && null addedTerms
    && null addedPatches
    && null removedTypes
    && null removedTerms
    && null removedPatches
    && null renamedTypes
    && null renamedTerms
    && null updatedPatches

-- Need to be able to turn a (Name,Reference) into a HashQualified relative to... what.
-- the new namespace?

data TermDisplay v a = TermDisplay
  { name :: HashQualified Name,
    ref :: Referent,
    type_ :: Maybe (Type v a)
  }
  deriving stock (Generic, Show)

instance Eq (TermDisplay v a) where
  t0 == t1 =
    (t0 ^. #ref == t1 ^. #ref) && (t0 ^. #name == t1 ^. #name)

-- | Compare term displays on name.
instance Ord (TermDisplay v a) where
  compare t0 t1 =
    Name.compareAlphabetical (t0 ^. #name) (t1 ^. #name) <> compare (t0 ^. #ref) (t1 ^. #ref)

data TypeDisplay v a = TypeDisplay
  { name :: HashQualified Name,
    ref :: Reference,
    decl :: Maybe (DeclOrBuiltin v a)
  }
  deriving stock (Generic, Show)

instance Eq (TypeDisplay v a) where
  t0 == t1 =
    (t0 ^. #ref == t1 ^. #ref) && (t0 ^. #name == t1 ^. #name)

-- | Compare type displays on name.
instance Ord (TypeDisplay v a) where
  compare t0 t1 =
    Name.compareAlphabetical (t0 ^. #name) (t1 ^. #name) <> compare (t0 ^. #ref) (t1 ^. #ref)

type AddedTermDisplay v a = ([HashQualified Name], Referent, Maybe (Type v a))

type AddedTypeDisplay v a = ([HashQualified Name], Reference, Maybe (DeclOrBuiltin v a))

type RemovedTermDisplay v a = ([HashQualified Name], Referent, Maybe (Type v a))

type RemovedTypeDisplay v a = ([HashQualified Name], Reference, Maybe (DeclOrBuiltin v a))

type SimpleTermDisplay v a = (HashQualified Name, Referent, Maybe (Type v a))

type SimpleTypeDisplay v a = (HashQualified Name, Reference, Maybe (DeclOrBuiltin v a))

data UpdateTermDisplay v a = UpdateTermDisplay
  { old :: Maybe [SimpleTermDisplay v a],
    new :: [TermDisplay v a]
  }
  deriving stock (Generic, Show)

instance Eq (UpdateTermDisplay v a) where
  u0 == u1 = (u0 ^? #new . _head) == (u1 ^? #new . _head)

instance Ord (UpdateTermDisplay v a) where
  compare u0 u1 =
    case (u0 ^. #new, u1 ^. #new) of
      (t0 : _, t1 : _) -> compare t0 t1
      (ts0, ts1) -> compare (null ts0) (null ts1)

data UpdateTypeDisplay v a = UpdateTypeDisplay
  { old :: Maybe [SimpleTypeDisplay v a],
    new :: [TypeDisplay v a]
  }
  deriving stock (Generic, Show)

instance Eq (UpdateTypeDisplay v a) where
  u0 == u1 = (u0 ^? #new . _head) == (u1 ^? #new . _head)

instance Ord (UpdateTypeDisplay v a) where
  compare u0 u1 =
    case (u0 ^. #new, u1 ^. #new) of
      (t0 : _, t1 : _) -> compare t0 t1
      (ts0, ts1) -> compare (null ts0) (null ts1)

type RenameTermDisplay v a = (Referent, Maybe (Type v a), Set (HashQualified Name), Set (HashQualified Name))

type RenameTypeDisplay v a = (Reference, Maybe (DeclOrBuiltin v a), Set (HashQualified Name), Set (HashQualified Name))

type PatchDisplay = (Name, P.PatchDiff)

toOutput ::
  forall m v a.
  (Monad m) =>
  (Referent -> m (Maybe (Type v a))) ->
  (Reference -> m (Maybe (DeclOrBuiltin v a))) ->
  Int ->
  Names ->
  Names ->
  BranchDiff.BranchDiff ->
  m (BranchDiffOutput v a)
toOutput
  typeOf
  declOrBuiltin
  hqLen
  names1
  names2
  (BranchDiff termsDiff typesDiff patchesDiff) = do
    let isSimpleUpdate, isNewConflict, isResolvedConflict :: (Eq r) => (Set r, Set r) -> Bool
        isSimpleUpdate (old, new) = Set.size old == 1 && Set.size new == 1
        isNewConflict (_old, new) = Set.size new > 1 -- should already be the case that old /= new
        isResolvedConflict (old, new) = Set.size old > 1 && Set.size new == 1

    ( updatedTypes :: [UpdateTypeDisplay v a],
      newTypeConflicts :: [UpdateTypeDisplay v a],
      resolvedTypeConflicts :: [UpdateTypeDisplay v a]
      ) <-
      let -- things where what the name pointed to changed
          nsUpdates :: Map Name (Set Reference, Set Reference) =
            BranchDiff.namespaceUpdates typesDiff
          loadOld :: Bool -> Name -> Reference -> m (SimpleTypeDisplay v a)
          loadOld forceHQ n r_old =
            (,,)
              <$> pure
                ( if forceHQ
                    then Names.hqTypeName' hqLen n r_old
                    else Names.hqTypeName hqLen names1 n r_old
                )
              <*> pure r_old
              <*> declOrBuiltin r_old
          loadNew :: Bool -> Name -> Reference -> m (TypeDisplay v a)
          loadNew forceHQ n r_new = do
            decl <- declOrBuiltin r_new
            pure
              TypeDisplay
                { name =
                    if forceHQ
                      then Names.hqTypeName' hqLen n r_new
                      else Names.hqTypeName hqLen names2 n r_new,
                  ref = r_new,
                  decl
                }
          loadEntry :: (Name, (Set Reference, Set Reference)) -> m (UpdateTypeDisplay v a)
          loadEntry (n, (Set.toList -> [rold], Set.toList -> [rnew]))
            | rold == rnew = do
                new <- for [rnew] (loadNew False n)
                pure
                  UpdateTypeDisplay
                    { old = Nothing,
                      new
                    }
          loadEntry (n, (rs_old, rs_new)) = do
            let forceHQ = Set.size rs_old > 1 || Set.size rs_new > 1
            old <- Just <$> for (toList rs_old) (loadOld forceHQ n)
            new <- for (toList rs_new) (loadNew forceHQ n)
            pure UpdateTypeDisplay {old, new}
       in liftA3
            (,,)
            (List.sort <$> for (Map.toList $ Map.filter isSimpleUpdate nsUpdates) loadEntry)
            (List.sort <$> for (Map.toList $ Map.filter isNewConflict nsUpdates) loadEntry)
            (List.sort <$> for (Map.toList $ Map.filter isResolvedConflict nsUpdates) loadEntry)

    ( updatedTerms :: [UpdateTermDisplay v a],
      newTermConflicts :: [UpdateTermDisplay v a],
      resolvedTermConflicts :: [UpdateTermDisplay v a]
      ) <-
      let -- things where what the name pointed to changed
          nsUpdates = BranchDiff.namespaceUpdates termsDiff
          loadOld forceHQ n r_old =
            (,,)
              <$> pure
                ( if forceHQ
                    then Names.hqTermName' hqLen n r_old
                    else Names.hqTermName hqLen names1 n r_old
                )
              <*> pure r_old
              <*> typeOf r_old
          loadNew :: Bool -> Name -> Referent -> m (TermDisplay v a)
          loadNew forceHQ n r_new = do
            type_ <- typeOf r_new
            pure
              TermDisplay
                { name =
                    if forceHQ
                      then Names.hqTermName' hqLen n r_new
                      else Names.hqTermName hqLen names2 n r_new,
                  ref = r_new,
                  type_
                }
          loadEntry :: (Name, (Set Referent, Set Referent)) -> m (UpdateTermDisplay v a)
          loadEntry (n, (rs_old, rs_new))
            -- if the references haven't changed, it's code for: only the metadata has changed
            -- and we can ignore the old references in the output.
            | rs_old == rs_new = do
                new <- for (toList rs_new) (loadNew False n)
                pure
                  UpdateTermDisplay
                    { old = Nothing,
                      new
                    }
            | otherwise = do
                let forceHQ = Set.size rs_old > 1 || Set.size rs_new > 1
                old <- Just <$> for (toList rs_old) (loadOld forceHQ n)
                new <- for (toList rs_new) (loadNew forceHQ n)
                pure UpdateTermDisplay {old, new}
       in liftA3
            (,,)
            -- this is sorting the Update section back into alphabetical Name order
            -- after calling loadEntry on the two halves.
            (List.sort <$> for (Map.toList $ Map.filter isSimpleUpdate nsUpdates) loadEntry)
            (List.sort <$> for (Map.toList $ Map.filter isNewConflict nsUpdates) loadEntry)
            (List.sort <$> for (Map.toList $ Map.filter isResolvedConflict nsUpdates) loadEntry)

    let updatedPatches :: [PatchDisplay] =
          [(name, diff) | (name, BranchDiff.Modify diff) <- Map.toList patchesDiff]

    addedTypes :: [AddedTypeDisplay v a] <- do
      let typeAdds :: [(Reference, Set Name)] =
            sortOn
              snd
              (Map.toList . R.toMultimap . BranchDiff.talladds $ typesDiff)
      for typeAdds \(r, ns) -> do
        let hqs = map (\n -> Names.hqTypeName hqLen names2 n r) (Set.toList ns)
        (hqs,r,) <$> declOrBuiltin r

    addedTerms :: [AddedTermDisplay v a] <- do
      let termAdds :: [(Referent, Set Name)] =
            sortOn
              snd
              (Map.toList . R.toMultimap . BranchDiff.talladds $ termsDiff)
      for termAdds \(r, ns) -> do
        let hqs = map (\n -> Names.hqTermName hqLen names2 n r) (Set.toList ns)
        (hqs,r,) <$> typeOf r

    let addedPatches :: [PatchDisplay] =
          [ (name, diff)
            | (name, BranchDiff.Create diff) <- Map.toList patchesDiff
          ]

    removedTypes :: [RemovedTypeDisplay v a] <-
      let typeRemoves :: [(Reference, [Name])] =
            sortOn snd $
              Map.toList . fmap toList . R.toMultimap . BranchDiff.tallremoves $
                typesDiff
       in for typeRemoves $ \(r, ns) ->
            (,,)
              <$> pure ((\n -> Names.hqTypeName hqLen names1 n r) <$> ns)
              <*> pure r
              <*> declOrBuiltin r

    removedTerms :: [RemovedTermDisplay v a] <-
      let termRemoves :: [(Referent, [Name])] =
            sortOn snd $
              Map.toList . fmap toList . R.toMultimap . BranchDiff.tallremoves $
                termsDiff
       in for termRemoves $ \(r, ns) ->
            (,,)
              <$> pure ((\n -> Names.hqTermName hqLen names1 n r) <$> ns)
              <*> pure r
              <*> typeOf r

    let removedPatches :: [PatchDisplay] =
          [ (name, diff)
            | (name, BranchDiff.Delete diff) <- Map.toList patchesDiff
          ]

    let renamedTerm :: Map Referent (Set Name, Set Name) -> m [RenameTermDisplay v a]
        renamedTerm renames =
          for (sortOn snd $ Map.toList renames) $ \(r, (ol'names, new'names)) ->
            (,,,)
              <$> pure r
              <*> typeOf r
              <*> pure (Set.map (\n -> Names.hqTermName hqLen names1 n r) ol'names)
              <*> pure (Set.map (\n -> Names.hqTermName hqLen names2 n r) new'names)

    let renamedType :: Map Reference (Set Name, Set Name) -> m [RenameTypeDisplay v a]
        renamedType renames =
          for (sortOn snd $ Map.toList renames) $ \(r, (ol'names, new'names)) ->
            (,,,)
              <$> pure r
              <*> declOrBuiltin r
              <*> pure (Set.map (\n -> Names.hqTypeName hqLen names1 n r) ol'names)
              <*> pure (Set.map (\n -> Names.hqTypeName hqLen names2 n r) new'names)

    renamedTypes :: [RenameTypeDisplay v a] <- renamedType (BranchDiff.trenames typesDiff)
    renamedTerms :: [RenameTermDisplay v a] <- renamedTerm (BranchDiff.trenames termsDiff)

    pure $
      BranchDiffOutput
        { updatedTypes,
          updatedTerms,
          newTypeConflicts,
          newTermConflicts,
          resolvedTypeConflicts,
          resolvedTermConflicts,
          updatedPatches,
          addedTypes,
          addedTerms,
          addedPatches,
          removedTypes,
          removedTerms,
          removedPatches,
          renamedTypes,
          renamedTerms
        }

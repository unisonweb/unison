{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.Output.BranchDiff where

import Control.Lens
import Data.Generics.Labels ()
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Codebase.BranchDiff (BranchDiff (BranchDiff), DiffSlice)
import qualified Unison.Codebase.BranchDiff as BranchDiff
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Patch as P
import Unison.DataDeclaration (DeclOrBuiltin)
import qualified Unison.HashQualified as HQ
import Unison.HashQualified2 (HashQualified)
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Runtime.IOSource (isPropagatedValue)
import Unison.Syntax.Name ()
import Unison.Type (Type)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation3 as R3
import Unison.Util.Set (symmetricDifference)

data MetadataDiff tm = MetadataDiff
  { addedMetadata :: [tm],
    removedMetadata :: [tm]
  }
  deriving (Ord, Eq, Functor, Foldable, Traversable, Show)

instance Semigroup (MetadataDiff tm) where
  a <> b =
    MetadataDiff
      (addedMetadata a <> addedMetadata b)
      (removedMetadata a <> removedMetadata b)

instance Monoid (MetadataDiff tm) where
  mempty = MetadataDiff mempty mempty

data BranchDiffOutput v a = BranchDiffOutput
  { updatedTypes :: [UpdateTypeDisplay v a],
    updatedTerms :: [UpdateTermDisplay v a],
    newTypeConflicts :: [UpdateTypeDisplay v a],
    newTermConflicts :: [UpdateTermDisplay v a],
    resolvedTypeConflicts :: [UpdateTypeDisplay v a],
    resolvedTermConflicts :: [UpdateTermDisplay v a],
    propagatedUpdates :: Int,
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
    && propagatedUpdates == 0

-- Need to be able to turn a (Name,Reference) into a HashQualified relative to... what.
-- the new namespace?

data TermDisplay v a = TermDisplay
  { name :: HashQualified Name,
    ref :: Referent,
    type_ :: Maybe (Type v a),
    metadata :: MetadataDiff (MetadataDisplay v a)
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
    decl :: Maybe (DeclOrBuiltin v a),
    metadata :: MetadataDiff (MetadataDisplay v a)
  }
  deriving stock (Generic, Show)

instance Eq (TypeDisplay v a) where
  t0 == t1 =
    (t0 ^. #ref == t1 ^. #ref) && (t0 ^. #name == t1 ^. #name)

-- | Compare type displays on name.
instance Ord (TypeDisplay v a) where
  compare t0 t1 =
    Name.compareAlphabetical (t0 ^. #name) (t1 ^. #name) <> compare (t0 ^. #ref) (t1 ^. #ref)

type AddedTermDisplay v a = ([(HashQualified Name, [MetadataDisplay v a])], Referent, Maybe (Type v a))

type AddedTypeDisplay v a = ([(HashQualified Name, [MetadataDisplay v a])], Reference, Maybe (DeclOrBuiltin v a))

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

type MetadataDisplay v a = (HQ.HashQualified Name, Referent, Maybe (Type v a))

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
  PPE.PrettyPrintEnv ->
  BranchDiff.BranchDiff ->
  m (BranchDiffOutput v a)
toOutput
  typeOf
  declOrBuiltin
  hqLen
  names1
  names2
  ppe
  (BranchDiff termsDiff typesDiff patchesDiff) = do
    let -- This calculates the new reference's metadata as:
        -- adds: now-attached metadata that was missing from
        --       any of the old references associated with the name
        -- removes: not-attached metadata that had been attached to any of
        --       the old references associated with the name
        getNewMetadataDiff :: (Ord r) => Bool -> DiffSlice r -> Name -> Set r -> r -> MetadataDiff Metadata.Value
        getNewMetadataDiff hidePropagatedMd s n rs_old r_new =
          let old_metadatas :: [Set Metadata.Value] =
                toList . R.toMultimap . R.restrictDom rs_old . R3.lookupD2 n $
                  BranchDiff.tremovedMetadata s
              old_intersection :: Set Metadata.Value =
                foldl' Set.intersection mempty old_metadatas
              old_union :: Set Metadata.Value =
                foldl' Set.union mempty old_metadatas
              new_metadata :: Set Metadata.Value =
                R.lookupDom n . R3.lookupD1 r_new $ BranchDiff.taddedMetadata s
              toDelete = if hidePropagatedMd then Set.singleton isPropagatedValue else mempty
           in MetadataDiff
                { addedMetadata = toList $ new_metadata `Set.difference` old_intersection `Set.difference` toDelete,
                  removedMetadata = toList $ old_union `Set.difference` new_metadata `Set.difference` toDelete
                }
        -- For the metadata on a definition to have changed, the name
        -- and the reference must have existed before and the reference
        -- must not have been removed and the name must not have been removed or added
        -- or updated 😅
        -- "getMetadataUpdates" = a defn has been updated via change of metadata
        getMetadataUpdates :: (Ord r) => DiffSlice r -> Map Name (Set r, Set r)
        getMetadataUpdates s =
          Map.fromList
            [ (n, (Set.singleton r, Set.singleton r)) -- the reference is unchanged
              | (r, n, v) <-
                  R3.toList $
                    BranchDiff.taddedMetadata s
                      <> BranchDiff.tremovedMetadata s,
                R.notMember r n (BranchDiff.talladds s),
                R.notMember r n (BranchDiff.tallremoves s),
                -- don't count it as a metadata update if it already's already a regular update
                let (oldRefs, newRefs) =
                      Map.findWithDefault mempty n (BranchDiff.tallnamespaceUpdates s)
                 in Set.notMember r oldRefs && Set.notMember r newRefs,
                --  trenames :: Map r (Set Name, Set Name), -- ref (old, new)
                case Map.lookup r (BranchDiff.trenames s) of
                  Nothing -> True
                  Just (olds, news) ->
                    Set.notMember n (symmetricDifference olds news),
                v /= isPropagatedValue
            ]

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
          -- things where the metadata changed (`uniqueBy` below removes these
          -- if they were already included in `nsUpdates)
          metadataUpdates = getMetadataUpdates typesDiff
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
          loadNew :: Bool -> Bool -> Name -> Set Reference -> Reference -> m (TypeDisplay v a)
          loadNew hidePropagatedMd forceHQ n rs_old r_new = do
            decl <- declOrBuiltin r_new
            metadata <- fillMetadata ppe (getNewMetadataDiff hidePropagatedMd typesDiff n rs_old r_new)
            pure
              TypeDisplay
                { name =
                    if forceHQ
                      then Names.hqTypeName' hqLen n r_new
                      else Names.hqTypeName hqLen names2 n r_new,
                  ref = r_new,
                  decl,
                  metadata
                }
          loadEntry :: Bool -> (Name, (Set Reference, Set Reference)) -> m (UpdateTypeDisplay v a)
          loadEntry hidePropagatedMd (n, (Set.toList -> [rold], Set.toList -> [rnew]))
            | rold == rnew = do
                new <- for [rnew] (loadNew hidePropagatedMd False n (Set.singleton rold))
                pure
                  UpdateTypeDisplay
                    { old = Nothing,
                      new
                    }
          loadEntry hidePropagatedMd (n, (rs_old, rs_new)) = do
            let forceHQ = Set.size rs_old > 1 || Set.size rs_new > 1
            old <- Just <$> for (toList rs_old) (loadOld forceHQ n)
            new <- for (toList rs_new) (loadNew hidePropagatedMd forceHQ n rs_old)
            pure UpdateTypeDisplay {old, new}
       in liftA3
            (,,)
            ( List.sort
                <$> liftA2
                  (<>)
                  (for (Map.toList $ Map.filter isSimpleUpdate nsUpdates) (loadEntry True))
                  (for (Map.toList metadataUpdates) (loadEntry False))
            )
            (List.sort <$> for (Map.toList $ Map.filter isNewConflict nsUpdates) (loadEntry True))
            (List.sort <$> for (Map.toList $ Map.filter isResolvedConflict nsUpdates) (loadEntry True))

    ( updatedTerms :: [UpdateTermDisplay v a],
      newTermConflicts :: [UpdateTermDisplay v a],
      resolvedTermConflicts :: [UpdateTermDisplay v a]
      ) <-
      let -- things where what the name pointed to changed
          nsUpdates = BranchDiff.namespaceUpdates termsDiff
          -- things where the metadata changed (`uniqueBy` below removes these
          -- if they were already included in `nsUpdates)
          metadataUpdates = getMetadataUpdates termsDiff
          loadOld forceHQ n r_old =
            (,,)
              <$> pure
                ( if forceHQ
                    then Names.hqTermName' hqLen n r_old
                    else Names.hqTermName hqLen names1 n r_old
                )
              <*> pure r_old
              <*> typeOf r_old
          loadNew :: Bool -> Bool -> Name -> Set Referent -> Referent -> m (TermDisplay v a)
          loadNew hidePropagatedMd forceHQ n rs_old r_new = do
            type_ <- typeOf r_new
            metadata <- fillMetadata ppe (getNewMetadataDiff hidePropagatedMd termsDiff n rs_old r_new)
            pure
              TermDisplay
                { name =
                    if forceHQ
                      then Names.hqTermName' hqLen n r_new
                      else Names.hqTermName hqLen names2 n r_new,
                  ref = r_new,
                  type_,
                  metadata
                }
          loadEntry :: Bool -> (Name, (Set Referent, Set Referent)) -> m (UpdateTermDisplay v a)
          loadEntry hidePropagatedMd (n, (rs_old, rs_new))
            -- if the references haven't changed, it's code for: only the metadata has changed
            -- and we can ignore the old references in the output.
            | rs_old == rs_new = do
                new <- for (toList rs_new) (loadNew hidePropagatedMd False n rs_old)
                pure
                  UpdateTermDisplay
                    { old = Nothing,
                      new
                    }
            | otherwise = do
                let forceHQ = Set.size rs_old > 1 || Set.size rs_new > 1
                old <- Just <$> for (toList rs_old) (loadOld forceHQ n)
                new <- for (toList rs_new) (loadNew hidePropagatedMd forceHQ n rs_old)
                pure UpdateTermDisplay {old, new}
       in liftA3
            (,,)
            -- this is sorting the Update section back into alphabetical Name order
            -- after calling loadEntry on the two halves.
            ( List.sort
                <$> liftA2
                  (<>)
                  (for (Map.toList $ Map.filter isSimpleUpdate nsUpdates) (loadEntry True))
                  (for (Map.toList metadataUpdates) (loadEntry False))
            )
            (List.sort <$> for (Map.toList $ Map.filter isNewConflict nsUpdates) (loadEntry True))
            (List.sort <$> for (Map.toList $ Map.filter isResolvedConflict nsUpdates) (loadEntry True))

    let propagatedUpdates :: Int =
          -- counting the number of named auto-propagated definitions
          (Set.size . Set.unions . toList . BranchDiff.propagatedUpdates) typesDiff
            + (Set.size . Set.unions . toList . BranchDiff.propagatedUpdates) termsDiff

    let updatedPatches :: [PatchDisplay] =
          [(name, diff) | (name, BranchDiff.Modify diff) <- Map.toList patchesDiff]

    addedTypes :: [AddedTypeDisplay v a] <- do
      let typeAdds :: [(Reference, [(Name, [Metadata.Value])])] =
            sortOn
              snd
              [ (r, nsmd)
                | (r, ns) <- Map.toList . R.toMultimap . BranchDiff.talladds $ typesDiff,
                  let nsmd =
                        [ (n, toList $ getAddedMetadata r n typesDiff)
                          | n <- toList ns
                        ]
              ]
      for typeAdds $ \(r, nsmd) -> do
        hqmds :: [(HashQualified Name, [MetadataDisplay v a])] <-
          for nsmd $ \(n, mdRefs) ->
            (,)
              <$> pure (Names.hqTypeName hqLen names2 n r)
              <*> fillMetadata ppe mdRefs
        (hqmds,r,) <$> declOrBuiltin r

    addedTerms :: [AddedTermDisplay v a] <- do
      let termAdds :: [(Referent, [(Name, [Metadata.Value])])] =
            sortOn
              snd
              [ (r, nsmd)
                | (r, ns) <- Map.toList . R.toMultimap . BranchDiff.talladds $ termsDiff,
                  let nsmd =
                        [ (n, toList $ getAddedMetadata r n termsDiff)
                          | n <- toList ns
                        ]
              ]
      for termAdds $ \(r, nsmd) -> do
        hqmds <- for nsmd $ \(n, mdRefs) ->
          (,)
            <$> pure (Names.hqTermName hqLen names2 n r)
            <*> fillMetadata ppe mdRefs
        (hqmds,r,) <$> typeOf r

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
          propagatedUpdates,
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
    where
      fillMetadata :: (Traversable t) => PPE.PrettyPrintEnv -> t Metadata.Value -> m (t (MetadataDisplay v a))
      fillMetadata ppe = traverse $ -- metadata values are all terms
        \(Referent.Ref -> mdRef) ->
          let name = PPE.termName ppe mdRef
           in (name,mdRef,) <$> typeOf mdRef
      getMetadata :: (Ord r) => r -> Name -> R3.Relation3 r Name Metadata.Value -> Set Metadata.Value
      getMetadata r n = R.lookupDom n . R3.lookupD1 r

      getAddedMetadata :: (Ord r) => r -> Name -> BranchDiff.DiffSlice r -> Set Metadata.Value
      getAddedMetadata r n slice = getMetadata r n $ BranchDiff.taddedMetadata slice

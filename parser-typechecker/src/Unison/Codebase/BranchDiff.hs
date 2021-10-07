module Unison.Codebase.BranchDiff where

import Unison.Prelude
import qualified Data.Set as Set
import qualified Data.Map as Map
import Unison.Codebase.Branch (Branch0(..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Patch (Patch, PatchDiff)
import Unison.Name (Name)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation3 as R3
import qualified Unison.Util.Relation4 as R4
import Unison.Util.Relation (Relation)
import Unison.Util.Relation3 (Relation3)
import Unison.Runtime.IOSource (isPropagatedValue)

data DiffType a = Create a | Delete a | Modify a deriving Show

-- todo: maybe simplify this file using Relation3?
data NamespaceSlice r = NamespaceSlice {
  names :: Relation r Name,
  metadata :: Relation3 r Name Metadata.Value
} deriving Show

data DiffSlice r = DiffSlice {
--  tpatchUpdates :: Relation r r, -- old new
  tallnamespaceUpdates :: Map Name (Set r, Set r),
  talladds :: Relation r Name,
  tallremoves :: Relation r Name,
  trenames :: Map r (Set Name, Set Name), -- ref (old, new)
  taddedMetadata :: Relation3 r Name Metadata.Value,
  tremovedMetadata :: Relation3 r Name Metadata.Value
} deriving Show

data BranchDiff = BranchDiff
  { termsDiff :: DiffSlice Referent
  , typesDiff :: DiffSlice Reference
  , patchesDiff :: Map Name (DiffType PatchDiff)
  } deriving Show

diff0 :: forall m. Monad m => Branch0 m -> Branch0 m -> m BranchDiff
diff0 old new = BranchDiff terms types <$> patchDiff old new where
  (terms, types) =
    computeSlices
      (deepr4ToSlice (Branch.deepTerms old) (Branch.deepTermMetadata old))
      (deepr4ToSlice (Branch.deepTerms new) (Branch.deepTermMetadata new))
      (deepr4ToSlice (Branch.deepTypes old) (Branch.deepTypeMetadata old))
      (deepr4ToSlice (Branch.deepTypes new) (Branch.deepTypeMetadata new))

patchDiff :: forall m. Monad m => Branch0 m -> Branch0 m -> m (Map Name (DiffType PatchDiff))
patchDiff old new = do
  let oldDeepEdits, newDeepEdits :: Map Name (Branch.EditHash, m Patch)
      oldDeepEdits = Branch.deepEdits' old
      newDeepEdits = Branch.deepEdits' new
  added <- do
    addedPatches :: Map Name Patch <-
      traverse snd $ Map.difference newDeepEdits oldDeepEdits
    pure $ fmap (\p -> Create (Patch.diff p mempty)) addedPatches
  removed <- do
    removedPatches :: Map Name Patch <-
      traverse snd $ Map.difference oldDeepEdits newDeepEdits
    pure $ fmap (\p -> Delete (Patch.diff mempty p)) removedPatches

  let f acc k = case (Map.lookup k oldDeepEdits, Map.lookup k newDeepEdits) of
        (Just (h1,p1), Just (h2,p2)) ->
          if h1 == h2 then pure acc
          else Map.singleton k . Modify <$> (Patch.diff <$> p2 <*> p1)
        _ -> error "we've done something very wrong"
  modified <- foldM f mempty (Set.intersection (Map.keysSet oldDeepEdits) (Map.keysSet newDeepEdits))
  pure $ added <> removed <> modified

deepr4ToSlice :: Ord r
              => R.Relation r Name
              -> Metadata.R4 r Name
              -> NamespaceSlice r
deepr4ToSlice deepNames deepMetadata =
  NamespaceSlice deepNames (R4.d124 deepMetadata)

computeSlices :: NamespaceSlice Referent
              -> NamespaceSlice Referent
              -> NamespaceSlice Reference
              -> NamespaceSlice Reference
              -> (DiffSlice Referent, DiffSlice Reference)
computeSlices oldTerms newTerms oldTypes newTypes = (termsOut, typesOut) where
  termsOut =
    let nc = allNames oldTerms newTerms
        nu = allNamespaceUpdates oldTerms newTerms in
    DiffSlice
      nu
      (allAdds nc nu)
      (allRemoves nc nu)
      (remainingNameChanges nc)
      (addedMetadata oldTerms newTerms)
      (removedMetadata oldTerms newTerms)
  typesOut =
    let nc = allNames oldTypes newTypes
        nu = allNamespaceUpdates oldTypes newTypes in
    DiffSlice
      nu
      (allAdds nc nu)
      (allRemoves nc nu)
      (remainingNameChanges nc)
      (addedMetadata oldTypes newTypes)
      (removedMetadata oldTypes newTypes)

  allNames :: Ord r => NamespaceSlice r -> NamespaceSlice r -> Map r (Set Name, Set Name)
  allNames old new = R.outerJoinDomMultimaps (names old) (names new)

  allAdds, allRemoves :: forall r. Ord r
                      => Map r (Set Name, Set Name)
                      -> Map Name (Set r, Set r)
                      -> Relation r Name
  allAdds    nc nu = R.fromMultimap . fmap snd . Map.filterWithKey f $ nc where
    f r (oldNames, newNames) = null oldNames && any (notInUpdates r) newNames
    -- if an add matches RHS of an update, we exclude it from "Adds"
    notInUpdates r name = case Map.lookup name nu of
      Nothing -> True
      Just (_, rs_new) -> Set.notMember r rs_new

  allRemoves nc nu = R.fromMultimap . fmap fst . Map.filterWithKey f $ nc where
    f r (oldNames, newNames) = null newNames && any (notInUpdates r) oldNames
    -- if a remove matches LHS of an update, we exclude it from "Removes"
    notInUpdates r name = case Map.lookup name nu of
      Nothing -> True
      Just (rs_old, _) -> Set.notMember r rs_old

  -- renames and stuff, name changes without a reference change
  remainingNameChanges :: forall r. Ord r
                       => Map r (Set Name, Set Name) -> Map r (Set Name, Set Name)
  remainingNameChanges =
    Map.filter (\(old, new) -> not (null old) && not (null new) && old /= new)

  allNamespaceUpdates :: Ord r => NamespaceSlice r -> NamespaceSlice r -> Map Name (Set r, Set r)
  allNamespaceUpdates old new =
    Map.filter f $ R.innerJoinRanMultimaps (names old) (names new)
    where f (old, new) = old /= new

  addedMetadata :: Ord r => NamespaceSlice r -> NamespaceSlice r -> Relation3 r Name Metadata.Value
  addedMetadata old new = metadata new `R3.difference` metadata old

  removedMetadata :: Ord r => NamespaceSlice r -> NamespaceSlice r -> Relation3 r Name Metadata.Value
  removedMetadata old new = metadata old `R3.difference` metadata new

-- the namespace updates that aren't propagated
namespaceUpdates :: Ord r => DiffSlice r -> Map Name (Set r, Set r)
namespaceUpdates s = Map.mapMaybeWithKey f (tallnamespaceUpdates s)
  where
  f name (olds, news) = let
    news' = Set.difference news (Map.findWithDefault mempty name propagated)
    in if null news' then Nothing else Just (olds, news')
  propagated = propagatedUpdates s

propagatedUpdates :: Ord r => DiffSlice r -> Map Name (Set r)
propagatedUpdates s = Map.fromList
  [ (name, news)
  | (name, (_olds0, news0)) <- Map.toList $ tallnamespaceUpdates s
  , let news = Set.filter propagated news0
        propagated rnew = R3.member rnew name isPropagatedValue (taddedMetadata s)
  , not (null news)
  ]

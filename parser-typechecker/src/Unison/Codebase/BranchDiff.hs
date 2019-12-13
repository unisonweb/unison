module Unison.Codebase.BranchDiff where

import Data.Map (Map)
import Data.Set (Set)
import Unison.Codebase.Branch (Branch0(..))
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Metadata (Star)
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Patch as P
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Name (Name)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as R
import Unison.Util.Relation4 (Relation4)

data DiffType a = Create a | Delete a | Modify a

-- todo: maybe simplify this file using Relation3?

data NamespaceSlice r = NamespaceSlice {
  names :: R.Relation r Name,
  metadata :: R.Relation r (Name, Metadata.Value)
}

data DiffSlice r = DiffSlice {
  tpatchUpdates :: R.Relation r r, -- old new
  tnamespaceUpdates :: R.Relation (r, r) Name,
  tadds :: R.Relation r Name,
  tremoves :: R.Relation r Name,
  tcopies :: Map r (Set Name, Set Name), -- ref (old, new)
  tmoves :: Map r (Set Name, Set Name), -- ref (old, new)
  taddedMetadata :: R.Relation r (Name, Metadata.Value),
  tremovedMetadata :: R.Relation r (Name, Metadata.Value)
}

data BranchDiff = BranchDiff
  { termsDiff :: DiffSlice Referent
  , typesDiff :: DiffSlice Reference
  -- patchesDiff :: Map Name (DiffType Patch.PatchDiff)
  }

diff0 :: forall m. Monad m => Branch0 m -> Branch0 m -> P.Patch -> BranchDiff
diff0 old new patch = BranchDiff terms types where
  (terms, types) =
    computeSlices
      (deepr4ToSlice (Branch.deepTerms old))
      (deepr4ToSlice (Branch.deepTerms new))
      (deepr4ToSlice (Branch.deepTypes old))
      (deepr4ToSlice (Branch.deepTypes new))
      patch

--unpackMetadata :: Branch0 m ->

deepr4ToSlice :: Relation4 r Name Metadata.Type Metadata.Value -> NamespaceSlice r
deepr4ToSlice = undefined -- NamespaceSlice (Star3.d1 s) (unpackMetadata s) where

computeSlices :: NamespaceSlice Referent
              -> NamespaceSlice Referent
              -> NamespaceSlice Reference
              -> NamespaceSlice Reference
              -> P.Patch
              -> (DiffSlice Referent, DiffSlice Reference)
computeSlices oldTerms newTerms oldTypes newTypes p = (termsOut, typesOut) where
  termPatchUpdates = patchToTermUpdates p
  typePatchUpdates = patchToTypeUpdates p
  termsOut = DiffSlice
    termPatchUpdates
    (namespaceUpdates oldTerms newTerms termPatchUpdates)
    (adds oldTerms newTerms termPatchUpdates)
    (removes oldTerms newTerms termPatchUpdates)
    (copies oldTerms newTerms)
    (moves oldTerms newTerms)
    (addedMetadata oldTerms newTerms)
    (removedMetadata oldTerms newTerms)
  typesOut = DiffSlice
    typePatchUpdates
    (namespaceUpdates oldTypes newTypes typePatchUpdates)
    (adds oldTypes newTypes typePatchUpdates)
    (removes oldTypes newTypes typePatchUpdates)
    (copies oldTypes newTypes)
    (moves oldTypes newTypes)
    (addedMetadata oldTypes newTypes)
    (removedMetadata oldTypes newTypes)

  copies :: Ord r => NamespaceSlice r -> NamespaceSlice r -> Map r (Set Name, Set Name)
  copies old new =
    -- pair the set of old names with the set of names that are only new
    R.toUnzippedMultimap $
      names old `R.joinDom` (names new `R.difference` names old)

  moves :: Ord r => NamespaceSlice r -> NamespaceSlice r -> Map r (Set Name, Set Name)
  moves old new =
    R.toUnzippedMultimap $
      (names old `R.difference` names new)
        `R.joinDom` (names new `R.difference` names old)

  adds :: Ord r => NamespaceSlice r -> NamespaceSlice r -> R.Relation r r -> R.Relation r Name
  adds old new edits =
    R.subtractDom (R.ran edits) (names new `R.difference` names old)

  removes :: Ord r => NamespaceSlice r -> NamespaceSlice r -> R.Relation r r -> R.Relation r Name
  removes old new edits =
    R.subtractDom (R.dom edits) (names old `R.difference` names new)

  patchToTermUpdates :: P.Patch -> R.Relation Referent Referent
  patchToTermUpdates p = R.fromList
    [ (Referent.Ref old, Referent.Ref new)
    | (old, TermEdit.Replace new _) <- R.toList $ P._termEdits p ]

  patchToTypeUpdates :: P.Patch -> R.Relation Reference Reference
  patchToTypeUpdates p = R.fromList
    [ (old, new)
    | (old, TypeEdit.Replace new) <- R.toList $ P._typeEdits p ]

  namespaceUpdates :: Ord r => NamespaceSlice r -> NamespaceSlice r -> R.Relation r r -> R.Relation (r, r) Name
  namespaceUpdates old new edits =
    R.filterDom f (names old `R.joinRan` names new)
    where f (old, new) = old /= new && R.notMember old new edits

  addedMetadata :: Ord r => NamespaceSlice r -> NamespaceSlice r -> R.Relation r (Name, Metadata.Value)
  addedMetadata old new =
    R.collectRan matchMdName
      (names new `R.joinDom` (metadata new `R.difference` metadata old))

  removedMetadata :: Ord r => NamespaceSlice r -> NamespaceSlice r -> R.Relation r (Name, Metadata.Value)
  removedMetadata old new =
    R.collectRan matchMdName
      (names old `R.joinDom` (metadata old `R.difference` metadata new))

  matchMdName (n1, p@(n2, _)) = if n1 == n2 then Just p else Nothing

--diff0 :: forall m. Monad m => Branch0 m -> Branch0 m -> m BranchDiff
--diff0 old new = do
--  let oldDeepEdits, newDeepEdits :: Map Name (EditHash, m Patch)
--      oldDeepEdits = Branch.deepEdits' old
--      newDeepEdits = Branch.deepEdits' new
--  diffEdits :: Map Name (DiffType Patch.PatchDiff) <- do
--    added <- do
--      addedPatches :: Map Name Patch <-
--        traverse snd $ Map.difference newDeepEdits oldDeepEdits
--      pure $ fmap (\p -> Create (Patch.diff p mempty)) addedPatches
--    removed <- do
--      removedPatches :: Map Name Patch <-
--        traverse snd $ Map.difference oldDeepEdits newDeepEdits
--      pure $ fmap (\p -> Delete (Patch.diff mempty p)) removedPatches
--
--    let f acc k = case (Map.lookup k oldDeepEdits, Map.lookup k newDeepEdits) of
--          (Just (h1,p1), Just (h2,p2)) ->
--            if h1 == h2 then pure acc
--            else Map.singleton k . Modify <$> (Patch.diff <$> p2 <*> p1)
--          _ -> error "we've done something very wrong"
--    modified <- foldM f mempty (Set.intersection (Map.keysSet oldDeepEdits) (Map.keysSet newDeepEdits))
--    pure $ added <> removed <> modified
--
--  pure $ BranchDiff
--    { addedTerms = Star3.difference (Branch.deepTerms new) (Branch.deepTerms old)
--    , removedTerms = Star3.difference (Branch.deepTerms old) (Branch.deepTerms new)
--    , addedTypes = Star3.difference (Branch.deepTypes new) (Branch.deepTypes old)
--    , removedTypes = Star3.difference (Branch.deepTypes old) (Branch.deepTypes new)
--    , changedPatches = diffEdits
--    }

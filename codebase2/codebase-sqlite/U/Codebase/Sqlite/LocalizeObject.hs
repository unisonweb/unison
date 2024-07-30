{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | This module facilitates the creation of "localized" versions of objects, suitable for storage.
--
-- Localization is a stateful process in which the real database identifiers contained within an object, e.g. 'DbBranch', are canonicalized
-- as local identifiers counting up from 0 in the order they are encountered in the object. The association between real and local
-- identifier is captured in a vector, where the @ith@ index maps local identifier @i@ to the real identifier it corresponds to.
--
-- For example, consider a branch object that refers to terms @#foo@ and @#bar@. In totally made-up syntax,
--
-- @
-- branch = {
--   terms = [#foo, #bar]
-- }
-- @
--
-- The localized version of this branch would be
--
-- @
-- branch = {
--   terms = [0, 1]
-- }
-- terms = [#foo, #bar]
-- @
--
-- where all terms, types, etc. within the @branch@ structure refer to offsets in the associated vectors.
module U.Codebase.Sqlite.LocalizeObject
  ( localizeBranch,
    localizeBranchG,
    localizePatch,
    localizePatchG,
  )
where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Trans.State.Strict qualified as State
import Data.Bitraversable (bitraverse)
import Data.Generics.Product (HasField (..))
import Data.Map.Strict qualified as Map
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.Branch.Format (BranchLocalIds)
import U.Codebase.Sqlite.Branch.Format qualified as Branch
import U.Codebase.Sqlite.Branch.Full (Branch' (..), DbBranch, LocalBranch)
import U.Codebase.Sqlite.Branch.Full qualified as Branch
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalHashId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )
import U.Codebase.Sqlite.Patch.Format (PatchLocalIds, PatchLocalIds')
import U.Codebase.Sqlite.Patch.Format qualified as Patch
import U.Codebase.Sqlite.Patch.Full (LocalPatch, Patch, Patch' (..))
import U.Codebase.Sqlite.Patch.TermEdit (LocalTermEdit, TermEdit')
import U.Codebase.Sqlite.Patch.TypeEdit (LocalTypeEdit, TypeEdit')
import U.Codebase.Sqlite.Reference (LocalReference, LocalReferenceH)
import U.Codebase.Sqlite.Referent (LocalReferent, LocalReferentH)
import Unison.Prelude
import Unison.Util.Map qualified as Map
import Unison.Util.Set qualified as Set

--------------------------------------------------------------------------------------------------------------------------------------------
-- High-level localization

-- | Localize a branch object.
localizeBranch :: DbBranch -> (BranchLocalIds, LocalBranch)
localizeBranch = localizeBranchG

-- | Generalized form of 'localizeBranch'.
localizeBranchG :: forall t d p c. (Ord t, Ord d, Ord p, Ord c) => Branch' t d p c -> (Branch.BranchLocalIds' t d p c, LocalBranch)
localizeBranchG (Branch terms types patches children) =
  (runIdentity . runLocalizeBranch) do
    Branch
      <$> Map.bitraverse localizeText (Map.bitraverse localizeReferent localizeBranchMetadata) terms
      <*> Map.bitraverse localizeText (Map.bitraverse localizeReference localizeBranchMetadata) types
      <*> Map.bitraverse localizeText localizePatchReference patches
      <*> Map.bitraverse localizeText localizeBranchReference children
  where
    localizeBranchMetadata ::
      Branch.MetadataSetFormat' t d ->
      State (LocalizeBranchState t d p c) (Branch.MetadataSetFormat' LocalTextId LocalDefnId)
    localizeBranchMetadata (Branch.Inline refs) =
      Branch.Inline <$> Set.traverse localizeReference refs

-- | Localize a patch object.
localizePatch :: Patch -> (PatchLocalIds, LocalPatch)
localizePatch = localizePatchG

localizePatchG :: forall t h d. (Ord t, Ord h, Ord d) => Patch' t h d -> (PatchLocalIds' t h d, LocalPatch)
localizePatchG (Patch termEdits typeEdits) =
  (runIdentity . runLocalizePatch) do
    Patch
      <$> Map.bitraverse localizeReferentH (Set.traverse localizeTermEdit) termEdits
      <*> Map.bitraverse localizeReferenceH (Set.traverse localizeTypeEdit) typeEdits
  where
    localizeTermEdit :: (TermEdit' t d) -> State (LocalizePatchState t h d) LocalTermEdit
    localizeTermEdit =
      bitraverse localizeText localizeDefn

    localizeTypeEdit :: TypeEdit' t d -> State (LocalizePatchState t h d) LocalTypeEdit
    localizeTypeEdit =
      bitraverse localizeText localizeDefn

--------------------------------------------------------------------------------------------------------------------------------------------
-- General-purpose localization

-- Contains references to branch objects.
class (Ord c) => ContainsBranches c s where
  branches_ :: Lens' s (Map c LocalBranchChildId)

-- Contains references to definition objects i.e. term/decl component objects.
class (Ord d) => ContainsDefns d s where
  defns_ :: Lens' s (Map d LocalDefnId)

-- Contains references to objects by their hash.
class (Ord h) => ContainsHashes h s where
  hashes_ :: Lens' s (Map h LocalHashId)

-- Contains references to patch objects.
class (Ord p) => ContainsPatches p s where
  patches_ :: Lens' s (Map p LocalPatchObjectId)

-- Contains text.
class (Ord t) => ContainsText t s where
  texts_ :: Lens' s (Map t LocalTextId)

-- The inner state of the localization of a branch object.
data LocalizeBranchState t d p c = LocalizeBranchState
  { texts :: Map t LocalTextId,
    defns :: Map d LocalDefnId,
    patches :: Map p LocalPatchObjectId,
    branches :: Map c LocalBranchChildId
  }
  deriving (Show, Generic)

instance (Ord t) => ContainsText t (LocalizeBranchState t d p c) where
  texts_ = field @"texts"

instance (Ord d) => ContainsDefns d (LocalizeBranchState t d p c) where
  defns_ = field @"defns"

instance (Ord p) => ContainsPatches p (LocalizeBranchState t d p c) where
  patches_ = field @"patches"

instance (Ord c) => ContainsBranches c (LocalizeBranchState t d p c) where
  branches_ = field @"branches"

-- | Run a computation that localizes a branch object, returning the local ids recorded within.
runLocalizeBranch :: forall m t d p c a. (Monad m, Ord t, Ord d, Ord p, Ord c) => StateT (LocalizeBranchState t d p c) m a -> m (Branch.BranchLocalIds' t d p c, a)
runLocalizeBranch action = do
  (result, (LocalizeBranchState localTexts localDefns localPatches localChildren)) <- State.runStateT action (LocalizeBranchState mempty mempty mempty mempty)
  let branchLocalIds :: Branch.BranchLocalIds' t d p c
      branchLocalIds =
        Branch.LocalIds
          { Branch.branchTextLookup = Map.valuesVector (Map.swap localTexts),
            Branch.branchDefnLookup = Map.valuesVector (Map.swap localDefns),
            Branch.branchPatchLookup = Map.valuesVector (Map.swap localPatches),
            Branch.branchChildLookup = Map.valuesVector (Map.swap localChildren)
          }
  pure (branchLocalIds, result)

-- The inner state of the localization of a patch object.
data LocalizePatchState t h d = LocalizePatchState
  { texts :: Map t LocalTextId,
    hashes :: Map h LocalHashId,
    defns :: Map d LocalDefnId
  }
  deriving (Show, Generic)

instance (Ord t) => ContainsText t (LocalizePatchState t h d) where
  texts_ = field @"texts"

instance (Ord h) => ContainsHashes h (LocalizePatchState t h d) where
  hashes_ = field @"hashes"

instance (Ord d) => ContainsDefns d (LocalizePatchState t h d) where
  defns_ = field @"defns"

-- Run a computation that localizes a patch object, returning the local ids recorded within.
runLocalizePatch :: forall t h d a m. (Monad m, Ord t, Ord h, Ord d) => StateT (LocalizePatchState t h d) m a -> m (PatchLocalIds' t h d, a)
runLocalizePatch action = do
  (result, (LocalizePatchState localTexts localHashes localDefns)) <- State.runStateT action (LocalizePatchState mempty mempty mempty)
  let patchLocalIds :: PatchLocalIds' t h d
      patchLocalIds =
        Patch.LocalIds
          { Patch.patchTextLookup = Map.valuesVector (Map.swap localTexts),
            Patch.patchHashLookup = Map.valuesVector (Map.swap localHashes),
            Patch.patchDefnLookup = Map.valuesVector (Map.swap localDefns)
          }
  pure (patchLocalIds, result)

-- Localize a branch object reference in any monad that encapsulates the stateful localization of an object that contains branch references.
localizeBranchReference :: (ContainsBranches c s, Monad m) => c -> StateT s m LocalBranchChildId
localizeBranchReference =
  zoom branches_ . localize

-- Localize a definition object reference in any monad that encapsulates the stateful localization of an object that contains definition
-- references.
localizeDefn :: (ContainsDefns d s, Monad m) => d -> StateT s m LocalDefnId
localizeDefn =
  zoom defns_ . localize

-- Localize a hash reference in any monad that encapsulates the stateful localization of an object that contains hash references.
localizeHash :: (ContainsHashes h s, Monad m) => h -> StateT s m LocalHashId
localizeHash =
  zoom hashes_ . localize

-- Localize a patch object reference in any monad that encapsulates the stateful localization of an object that contains patch references.
localizePatchReference :: (ContainsPatches p s, Monad m) => p -> StateT s m LocalPatchObjectId
localizePatchReference =
  zoom patches_ . localize

-- Localize a reference in any monad that encapsulates the stateful localization of an object that contains references.
localizeReference :: (ContainsDefns d s, ContainsText t s, Monad m) => Reference' t d -> StateT s m LocalReference
localizeReference =
  bitraverse localizeText localizeDefn

-- Localize a possibly-missing reference in any monad that encapsulates the stateful localization of an object that contains
-- possibly-missing references.
localizeReferenceH :: (ContainsHashes h s, ContainsText t s, Monad m) => Reference' t h -> StateT s m LocalReferenceH
localizeReferenceH =
  bitraverse localizeText localizeHash

-- Localize a referent in any monad that encapsulates the stateful localization of an object that contains referents.
localizeReferent :: forall d t s m. (ContainsDefns d s, ContainsText t s, Monad m) => (Referent' (Reference' t d) (Reference' t d)) -> StateT s m LocalReferent
localizeReferent =
  bitraverse localizeReference localizeReference

-- Localize a possibly-missing referent in any monad that encapsulates the stateful localization of an object that contains possibly-missing
-- referents.
localizeReferentH :: (ContainsHashes h s, ContainsText t s, Monad m, r ~ Reference' t h) => Referent' r r -> StateT s m LocalReferentH
localizeReferentH =
  bitraverse localizeReferenceH localizeReferenceH

-- Localize a text reference in any monad that encapsulates the stateful localization of an object that contains text.
localizeText :: (ContainsText t s, Monad m) => t -> StateT s m LocalTextId
localizeText =
  zoom texts_ . localize

-- Resolve a real id to its corresponding local id, either by looking it up in a map, or else using the next available local id, which is
-- recorded for next time.
localize :: (Coercible localId Word64, Monad m, Ord realId) => realId -> StateT (Map realId localId) m localId
localize realId = do
  mapping <- State.get
  case Map.lookup realId mapping of
    Nothing -> do
      let nextLocalId = coerce @Word64 (fromIntegral (Map.size mapping))
      State.put $! Map.insert realId nextLocalId mapping
      pure nextLocalId
    Just localId -> pure localId

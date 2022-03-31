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
    localizePatch,
  )
where

import Control.Lens
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bitraversable (bitraverse)
import Data.Generics.Product.Typed (HasType (typed))
import qualified Data.Map.Strict as Map
import U.Codebase.Sqlite.Branch.Format (BranchLocalIds)
import qualified U.Codebase.Sqlite.Branch.Format as Branch
import U.Codebase.Sqlite.Branch.Full (Branch' (..), DbBranch, LocalBranch)
import qualified U.Codebase.Sqlite.Branch.Full as Branch
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalHashId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )
import U.Codebase.Sqlite.Patch.Format (PatchLocalIds)
import qualified U.Codebase.Sqlite.Patch.Format as Patch
import U.Codebase.Sqlite.Patch.Full (LocalPatch, Patch, Patch' (..))
import U.Codebase.Sqlite.Patch.TermEdit (LocalTermEdit, TermEdit)
import U.Codebase.Sqlite.Patch.TypeEdit (LocalTypeEdit, TypeEdit)
import U.Codebase.Sqlite.Reference (LocalReference, LocalReferenceH, Reference, ReferenceH)
import U.Codebase.Sqlite.Referent (LocalReferent, LocalReferentH, Referent, ReferentH)
import Unison.Prelude
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

--------------------------------------------------------------------------------------------------------------------------------------------
-- High-level localization

-- | Localize a branch object.
localizeBranch :: DbBranch -> (BranchLocalIds, LocalBranch)
localizeBranch (Branch terms types patches children) =
  (runIdentity . runLocalizeBranch) do
    Branch
      <$> Map.bitraverse localizeText (Map.bitraverse localizeReferent localizeBranchMetadata) terms
      <*> Map.bitraverse localizeText (Map.bitraverse localizeReference localizeBranchMetadata) types
      <*> Map.bitraverse localizeText localizePatchReference patches
      <*> Map.bitraverse localizeText localizeBranchReference children
  where
    localizeBranchMetadata :: (ContainsDefns s, ContainsText s, Monad m) => Branch.DbMetadataSet -> StateT s m Branch.LocalMetadataSet
    localizeBranchMetadata (Branch.Inline refs) =
      Branch.Inline <$> Set.traverse localizeReference refs

-- | Localize a patch object.
localizePatch :: Patch -> (PatchLocalIds, LocalPatch)
localizePatch (Patch termEdits typeEdits) =
  (runIdentity . runLocalizePatch) do
    Patch
      <$> Map.bitraverse localizeReferentH (Set.traverse localizeTermEdit) termEdits
      <*> Map.bitraverse localizeReferenceH (Set.traverse localizeTypeEdit) typeEdits
  where
    localizeTermEdit :: (ContainsText s, ContainsDefns s, Monad m) => TermEdit -> StateT s m LocalTermEdit
    localizeTermEdit =
      bitraverse localizeText localizeDefn

    localizeTypeEdit :: (ContainsText s, ContainsDefns s, Monad m) => TypeEdit -> StateT s m LocalTypeEdit
    localizeTypeEdit =
      bitraverse localizeText localizeDefn

--------------------------------------------------------------------------------------------------------------------------------------------
-- General-purpose localization

-- Contains references to branch objects.
type ContainsBranches s =
  HasType (Map (BranchObjectId, CausalHashId) LocalBranchChildId) s

-- Contains references to definition objects i.e. term/decl component objects.
type ContainsDefns s =
  HasType (Map ObjectId LocalDefnId) s

-- Contains references to objects by their hash.
type ContainsHashes =
  HasType (Map HashId LocalHashId)

-- Contains references to patch objects.
type ContainsPatches =
  HasType (Map PatchObjectId LocalPatchObjectId)

-- Contains text.
type ContainsText =
  HasType (Map TextId LocalTextId)

-- The inner state of the localization of a branch object.
type LocalizeBranchState =
  ( Map TextId LocalTextId,
    Map ObjectId LocalDefnId,
    Map PatchObjectId LocalPatchObjectId,
    Map (BranchObjectId, CausalHashId) LocalBranchChildId
  )

-- Run a computation that localizes a branch object, returning the local ids recorded within.
runLocalizeBranch :: Monad m => StateT LocalizeBranchState m a -> m (BranchLocalIds, a)
runLocalizeBranch action = do
  (result, (localTexts, localDefns, localPatches, localChildren)) <- State.runStateT action (mempty @LocalizeBranchState)
  let branchLocalIds :: BranchLocalIds
      branchLocalIds =
        Branch.LocalIds
          { Branch.branchTextLookup = Map.valuesVector (Map.swap localTexts),
            Branch.branchDefnLookup = Map.valuesVector (Map.swap localDefns),
            Branch.branchPatchLookup = Map.valuesVector (Map.swap localPatches),
            Branch.branchChildLookup = Map.valuesVector (Map.swap localChildren)
          }
  pure (branchLocalIds, result)

-- The inner state of the localization of a patch object.
type LocalizePatchState =
  ( Map TextId LocalTextId,
    Map HashId LocalHashId,
    Map ObjectId LocalDefnId
  )

-- Run a computation that localizes a patch object, returning the local ids recorded within.
runLocalizePatch :: Monad m => StateT LocalizePatchState m a -> m (PatchLocalIds, a)
runLocalizePatch action = do
  (result, (localTexts, localHashes, localDefns)) <- State.runStateT action (mempty @LocalizePatchState)
  let patchLocalIds :: PatchLocalIds
      patchLocalIds =
        Patch.LocalIds
          { Patch.patchTextLookup = Map.valuesVector (Map.swap localTexts),
            Patch.patchHashLookup = Map.valuesVector (Map.swap localHashes),
            Patch.patchDefnLookup = Map.valuesVector (Map.swap localDefns)
          }
  pure (patchLocalIds, result)

-- Localize a branch object reference in any monad that encapsulates the stateful localization of an object that contains branch references.
localizeBranchReference :: (ContainsBranches s, Monad m) => (BranchObjectId, CausalHashId) -> StateT s m LocalBranchChildId
localizeBranchReference =
  zoom typed . localize

-- Localize a definition object reference in any monad that encapsulates the stateful localization of an object that contains definition
-- references.
localizeDefn :: (ContainsDefns s, Monad m) => ObjectId -> StateT s m LocalDefnId
localizeDefn =
  zoom typed . localize

-- Localize a hash reference in any monad that encapsulates the stateful localization of an object that contains hash references.
localizeHash :: (ContainsHashes s, Monad m) => HashId -> StateT s m LocalHashId
localizeHash =
  zoom typed . localize

-- Localize a patch object reference in any monad that encapsulates the stateful localization of an object that contains patch references.
localizePatchReference :: (ContainsPatches s, Monad m) => PatchObjectId -> StateT s m LocalPatchObjectId
localizePatchReference =
  zoom typed . localize

-- Localize a reference in any monad that encapsulates the stateful localization of an object that contains references.
localizeReference :: (ContainsDefns s, ContainsText s, Monad m) => Reference -> StateT s m LocalReference
localizeReference =
  bitraverse localizeText localizeDefn

-- Localize a possibly-missing reference in any monad that encapsulates the stateful localization of an object that contains
-- possibly-missing references.
localizeReferenceH :: (ContainsHashes s, ContainsText s, Monad m) => ReferenceH -> StateT s m LocalReferenceH
localizeReferenceH =
  bitraverse localizeText localizeHash

-- Localize a referent in any monad that encapsulates the stateful localization of an object that contains referents.
localizeReferent :: (ContainsDefns s, ContainsText s, Monad m) => Referent -> StateT s m LocalReferent
localizeReferent =
  bitraverse localizeReference localizeReference

-- Localize a possibly-missing referent in any monad that encapsulates the stateful localization of an object that contains possibly-missing
-- referents.
localizeReferentH :: (ContainsHashes s, ContainsText s, Monad m) => ReferentH -> StateT s m LocalReferentH
localizeReferentH =
  bitraverse localizeReferenceH localizeReferenceH

-- Localize a text reference in any monad that encapsulates the stateful localization of an object that contains text.
localizeText :: (ContainsText s, Monad m) => TextId -> StateT s m LocalTextId
localizeText =
  zoom typed . localize

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

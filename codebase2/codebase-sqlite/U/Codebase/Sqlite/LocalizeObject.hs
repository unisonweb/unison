-- | This module facilitates the creation of "localized" versions of objects, suitable for storage.
module U.Codebase.Sqlite.LocalizeObject
  ( -- * High-level localization
    localizeBranch,
    localizePatch,

    -- * General-purpose localization
    LocalizeBranchT,
    runLocalizeBranchT,

    -- ** Helpers
    localizeBranchReference,
    localizeBranchMetadata,
    localizeDefn,
    localizeHash,
    localizePatchReference,
    localizeReference,
    localizeReferenceH,
    localizeReferent,
    localizeReferentH,
    localizeTermEdit,
    localizeText,
    localizeTypeEdit,

    -- * @Contains@ constraints
    ContainsBranches,
    ContainsDefns,
    ContainsPatches,
    ContainsText,
  )
where

import Control.Lens
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bitraversable (bitraverse)
import Data.Coerce (Coercible, coerce)
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
import qualified U.Util.Map as Map
import qualified U.Util.Set as Set
import Unison.Prelude

--------------------------------------------------------------------------------------------------------------------------------------------
-- High-level localization

localizeBranch :: DbBranch -> (BranchLocalIds, LocalBranch)
localizeBranch (Branch terms types patches children) =
  (runIdentity . runLocalizeBranchT) do
    Branch
      <$> Map.bitraverse localizeText (Map.bitraverse localizeReferent localizeBranchMetadata) terms
      <*> Map.bitraverse localizeText (Map.bitraverse localizeReference localizeBranchMetadata) types
      <*> Map.bitraverse localizeText localizePatchReference patches
      <*> Map.bitraverse localizeText localizeBranchReference children

localizePatch :: Patch -> (PatchLocalIds, LocalPatch)
localizePatch (Patch termEdits typeEdits) =
  (runIdentity . runLocalizePatchT) do
    Patch
      <$> Map.bitraverse localizeReferentH (Set.traverse localizeTermEdit) termEdits
      <*> Map.bitraverse localizeReferenceH (Set.traverse localizeTypeEdit) typeEdits

--------------------------------------------------------------------------------------------------------------------------------------------
-- General-purpose localization

type ContainsBranches =
  HasType (Map (BranchObjectId, CausalHashId) LocalBranchChildId)

type ContainsDefns =
  HasType (Map ObjectId LocalDefnId)

type ContainsHashes =
  HasType (Map HashId LocalHashId)

type ContainsPatches =
  HasType (Map PatchObjectId LocalPatchObjectId)

type ContainsText =
  HasType (Map TextId LocalTextId)

type LocalizeBranchT =
  StateT LocalizeBranchState

type LocalizeBranchState =
  ( Map TextId LocalTextId,
    Map ObjectId LocalDefnId,
    Map PatchObjectId LocalPatchObjectId,
    Map (BranchObjectId, CausalHashId) LocalBranchChildId
  )

runLocalizeBranchT :: Monad m => LocalizeBranchT m a -> m (BranchLocalIds, a)
runLocalizeBranchT action = do
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

type LocalizePatchState =
  ( Map TextId LocalTextId,
    Map HashId LocalHashId,
    Map ObjectId LocalDefnId
  )

type LocalizePatchT =
  StateT LocalizePatchState

runLocalizePatchT :: Monad m => LocalizePatchT m a -> m (PatchLocalIds, a)
runLocalizePatchT action = do
  (result, (localTexts, localHashes, localDefns)) <- State.runStateT action (mempty @LocalizePatchState)
  let patchLocalIds :: PatchLocalIds
      patchLocalIds =
        Patch.LocalIds
          { Patch.patchTextLookup = Map.valuesVector (Map.swap localTexts),
            Patch.patchHashLookup = Map.valuesVector (Map.swap localHashes),
            Patch.patchDefnLookup = Map.valuesVector (Map.swap localDefns)
          }
  pure (patchLocalIds, result)

localizeBranchReference :: (ContainsBranches s, Monad m) => (BranchObjectId, CausalHashId) -> StateT s m LocalBranchChildId
localizeBranchReference =
  zoom typed . localize

localizeBranchMetadata :: (ContainsDefns s, ContainsText s, Monad m) => Branch.DbMetadataSet -> StateT s m Branch.LocalMetadataSet
localizeBranchMetadata (Branch.Inline refs) =
  Branch.Inline <$> Set.traverse localizeReference refs

localizeDefn :: (ContainsDefns s, Monad m) => ObjectId -> StateT s m LocalDefnId
localizeDefn =
  zoom typed . localize

localizeHash :: (ContainsHashes s, Monad m) => HashId -> StateT s m LocalHashId
localizeHash =
  zoom typed . localize

localizePatchReference :: (ContainsPatches s, Monad m) => PatchObjectId -> StateT s m LocalPatchObjectId
localizePatchReference =
  zoom typed . localize

localizeReference :: (ContainsDefns s, ContainsText s, Monad m) => Reference -> StateT s m LocalReference
localizeReference =
  bitraverse localizeText localizeDefn

localizeReferenceH :: (ContainsHashes s, ContainsText s, Monad m) => ReferenceH -> StateT s m LocalReferenceH
localizeReferenceH =
  bitraverse localizeText localizeHash

localizeReferent :: (ContainsDefns s, ContainsText s, Monad m) => Referent -> StateT s m LocalReferent
localizeReferent =
  bitraverse localizeReference localizeReference

localizeReferentH :: (ContainsHashes s, ContainsText s, Monad m) => ReferentH -> StateT s m LocalReferentH
localizeReferentH =
  bitraverse localizeReferenceH localizeReferenceH

localizeTermEdit :: (ContainsText s, ContainsDefns s, Monad m) => TermEdit -> StateT s m LocalTermEdit
localizeTermEdit =
  bitraverse localizeText localizeDefn

localizeText :: (ContainsText s, Monad m) => TextId -> StateT s m LocalTextId
localizeText =
  zoom typed . localize

localizeTypeEdit :: (ContainsText s, ContainsDefns s, Monad m) => TypeEdit -> StateT s m LocalTypeEdit
localizeTypeEdit =
  bitraverse localizeText localizeDefn

localize :: (Coercible localId Word64, Monad m, Ord realId) => realId -> StateT (Map realId localId) m localId
localize realId = do
  mapping <- State.get
  case Map.lookup realId mapping of
    Nothing -> do
      let nextLocalId = coerce @Word64 (fromIntegral (Map.size mapping))
      State.put $! Map.insert realId nextLocalId mapping
      pure nextLocalId
    Just localId -> pure localId

-- | This module facilitates the creation of "localized" versions of objects, suitable for storage.
module U.Codebase.Sqlite.LocalizeObject
  ( -- * High-level localization
    localizeBranch,

    -- * General-purpose localization
    LocalizeBranchT,
    runLocalizeBranchT,

    -- ** Helpers
    localizeBranchChild,
    localizeBranchMetadata,
    localizeDefn,
    localizePatch,
    localizeReference,
    localizeReferent,
    localizeText,

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
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )
import U.Codebase.Sqlite.Reference (LocalReference, Reference)
import U.Codebase.Sqlite.Referent (LocalReferent, Referent)
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
      <*> Map.bitraverse localizeText localizePatch patches
      <*> Map.bitraverse localizeText localizeBranchChild children

--------------------------------------------------------------------------------------------------------------------------------------------
-- General-purpose localization

type ContainsBranches =
  HasType (Map (BranchObjectId, CausalHashId) LocalBranchChildId)

type ContainsDefns =
  HasType (Map ObjectId LocalDefnId)

type ContainsPatches =
  HasType (Map PatchObjectId LocalPatchObjectId)

type ContainsText =
  HasType (Map TextId LocalTextId)

type LocalizeBranchT m a =
  StateT LocalizeBranchState m a

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

localizeBranchChild :: (ContainsBranches s, Monad m) => (BranchObjectId, CausalHashId) -> StateT s m LocalBranchChildId
localizeBranchChild =
  zoom typed . localize

localizeBranchMetadata :: (ContainsDefns s, ContainsText s, Monad m) => Branch.DbMetadataSet -> StateT s m Branch.LocalMetadataSet
localizeBranchMetadata (Branch.Inline refs) =
  Branch.Inline <$> Set.traverse localizeReference refs

localizeDefn :: (ContainsDefns s, Monad m) => ObjectId -> StateT s m LocalDefnId
localizeDefn =
  zoom typed . localize

localizePatch :: (ContainsPatches s, Monad m) => PatchObjectId -> StateT s m LocalPatchObjectId
localizePatch =
  zoom typed . localize

localizeReference :: (ContainsDefns s, ContainsText s, Monad m) => Reference -> StateT s m LocalReference
localizeReference =
  bitraverse localizeText localizeDefn

localizeReferent :: (ContainsDefns s, ContainsText s, Monad m) => Referent -> StateT s m LocalReferent
localizeReferent =
  bitraverse localizeReference localizeReference

localizeText :: (ContainsText s, Monad m) => TextId -> StateT s m LocalTextId
localizeText =
  zoom typed . localize

localize :: (Coercible localId Word64, Monad m, Ord realId) => realId -> StateT (Map realId localId) m localId
localize realId = do
  mapping <- State.get
  case Map.lookup realId mapping of
    Nothing -> do
      let nextLocalId = coerce @Word64 (fromIntegral (Map.size mapping))
      State.put $! Map.insert realId nextLocalId mapping
      pure nextLocalId
    Just localId -> pure localId

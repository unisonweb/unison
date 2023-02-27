module U.Codebase.Sqlite.Patch.Format
  ( PatchFormat (..),
    PatchLocalIds,
    PatchLocalIds' (..),
    SyncPatchFormat,
    SyncPatchFormat' (..),
    applyPatchDiffs,
    localPatchToPatch,
    localPatchDiffToPatchDiff,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import U.Codebase.Sqlite.DbId (HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId (LocalDefnId), LocalHashId (LocalHashId), LocalTextId (LocalTextId))
import U.Codebase.Sqlite.Patch.Diff (LocalPatchDiff, PatchDiff, PatchDiff' (..))
import qualified U.Codebase.Sqlite.Patch.Diff as Patch.Diff
import U.Codebase.Sqlite.Patch.Full (LocalPatch, Patch, Patch' (..))
import qualified U.Codebase.Sqlite.Patch.Full as Patch.Full
import Unison.Prelude

data PatchFormat
  = Full PatchLocalIds LocalPatch
  | Diff PatchObjectId PatchLocalIds LocalPatchDiff

type PatchLocalIds = PatchLocalIds' TextId HashId ObjectId

data PatchLocalIds' t h d = LocalIds
  { patchTextLookup :: Vector t,
    patchHashLookup :: Vector h,
    patchDefnLookup :: Vector d
  }

type SyncPatchFormat = SyncPatchFormat' PatchObjectId TextId HashId ObjectId

data SyncPatchFormat' parent text hash defn
  = SyncFull (PatchLocalIds' text hash defn) ByteString
  | -- | p is the identity of the thing that the diff is relative to
    SyncDiff parent (PatchLocalIds' text hash defn) ByteString

-- | Apply a list of patch diffs to a patch, left to right.
applyPatchDiffs :: Patch -> [PatchDiff] -> Patch
applyPatchDiffs =
  foldl' apply
  where
    apply :: Patch -> PatchDiff -> Patch
    apply (Patch termEdits typeEdits) (PatchDiff addedTermEdits addedTypeEdits removedTermEdits removedTypeEdits) =
      let !termEdits' = addRemove addedTermEdits removedTermEdits termEdits
          !typeEdits' = addRemove addedTypeEdits removedTypeEdits typeEdits
       in Patch
            { termEdits = termEdits',
              typeEdits = typeEdits'
            }

    addRemove :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b) -> Map a (Set b)
    addRemove add del src =
      Map.unionWith (<>) add (Map.differenceWith remove src del)

    remove :: (Ord b) => Set b -> Set b -> Maybe (Set b)
    remove src del =
      let diff = Set.difference src del
       in if Set.null diff then Nothing else Just diff

localPatchToPatch :: PatchLocalIds -> LocalPatch -> Patch
localPatchToPatch li =
  Patch.Full.trimap (lookupPatchLocalText li) (lookupPatchLocalHash li) (lookupPatchLocalDefn li)

localPatchDiffToPatchDiff :: PatchLocalIds -> LocalPatchDiff -> PatchDiff
localPatchDiffToPatchDiff li =
  Patch.Diff.trimap
    (lookupPatchLocalText li)
    (lookupPatchLocalHash li)
    (lookupPatchLocalDefn li)

lookupPatchLocalText :: PatchLocalIds -> LocalTextId -> TextId
lookupPatchLocalText li (LocalTextId w) = patchTextLookup li Vector.! fromIntegral w

lookupPatchLocalHash :: PatchLocalIds -> LocalHashId -> HashId
lookupPatchLocalHash li (LocalHashId w) = patchHashLookup li Vector.! fromIntegral w

lookupPatchLocalDefn :: PatchLocalIds -> LocalDefnId -> ObjectId
lookupPatchLocalDefn li (LocalDefnId w) = patchDefnLookup li Vector.! fromIntegral w

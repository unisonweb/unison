module U.Codebase.Sqlite.Patch.Format
  ( PatchFormat (..),
    PatchLocalIds (..),
    SyncPatchFormat (..),
    localPatchToPatch,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import U.Codebase.Sqlite.DbId (HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId (LocalDefnId), LocalHashId (LocalHashId), LocalTextId (LocalTextId))
import U.Codebase.Sqlite.Patch.Diff (LocalPatchDiff)
import U.Codebase.Sqlite.Patch.Full (LocalPatch, Patch)
import qualified U.Codebase.Sqlite.Patch.Full as Patch.Full
import Unison.Prelude

data PatchFormat
  = Full PatchLocalIds LocalPatch
  | Diff PatchObjectId PatchLocalIds LocalPatchDiff

data PatchLocalIds = LocalIds
  { patchTextLookup :: Vector TextId,
    patchHashLookup :: Vector HashId,
    patchDefnLookup :: Vector ObjectId
  }

data SyncPatchFormat
  = SyncFull PatchLocalIds ByteString
  | SyncDiff PatchObjectId PatchLocalIds ByteString

localPatchToPatch :: PatchLocalIds -> LocalPatch -> Patch
localPatchToPatch li =
  Patch.Full.trimap (lookupPatchLocalText li) (lookupPatchLocalHash li) (lookupPatchLocalDefn li)
  where
    lookupPatchLocalText :: PatchLocalIds -> LocalTextId -> TextId
    lookupPatchLocalText li (LocalTextId w) = patchTextLookup li Vector.! fromIntegral w

    lookupPatchLocalHash :: PatchLocalIds -> LocalHashId -> HashId
    lookupPatchLocalHash li (LocalHashId w) = patchHashLookup li Vector.! fromIntegral w

    lookupPatchLocalDefn :: PatchLocalIds -> LocalDefnId -> ObjectId
    lookupPatchLocalDefn li (LocalDefnId w) = patchDefnLookup li Vector.! fromIntegral w

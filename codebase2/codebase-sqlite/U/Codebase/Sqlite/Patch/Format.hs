{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Codebase.Sqlite.Patch.Format where

import Data.Vector (Vector)
import U.Codebase.Sqlite.DbId (HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.Patch.Diff (LocalPatchDiff)
import U.Codebase.Sqlite.Patch.Full (LocalPatch)
import Data.ByteString (ByteString)

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
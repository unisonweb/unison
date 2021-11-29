{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Codebase.Sqlite.Branch.Format where

import Data.Vector (Vector)
import U.Codebase.Sqlite.Branch.Diff (LocalDiff)
import U.Codebase.Sqlite.Branch.Full (LocalBranch)
import U.Codebase.Sqlite.DbId (CausalHashId, BranchObjectId, ObjectId, PatchObjectId, TextId)
import Data.ByteString (ByteString)

-- |you can use the exact same `BranchLocalIds` when converting between `Full` and `Diff`
data BranchFormat
  = Full BranchLocalIds LocalBranch
  | Diff BranchObjectId BranchLocalIds LocalDiff
  deriving Show

data BranchLocalIds = LocalIds
  { branchTextLookup :: Vector TextId,
    branchDefnLookup :: Vector ObjectId,
    branchPatchLookup :: Vector PatchObjectId,
    branchChildLookup :: Vector (BranchObjectId, CausalHashId)
  }
  deriving Show

data SyncBranchFormat
  = SyncFull BranchLocalIds ByteString
  | SyncDiff BranchObjectId BranchLocalIds ByteString

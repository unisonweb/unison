module U.Codebase.Sqlite.Branch.Format where

import Data.Vector (Vector)
import U.Codebase.Sqlite.Branch.Diff (LocalDiff)
import U.Codebase.Sqlite.Branch.Full (LocalBranch)
import U.Codebase.Sqlite.DbId (CausalHashId, BranchObjectId, ObjectId, PatchObjectId, TextId)

data BranchFormat
  = Full BranchLocalIds LocalBranch
  | Diff BranchObjectId BranchLocalIds LocalDiff

data BranchLocalIds = LocalIds
  { branchTextLookup :: Vector TextId,
    branchDefnLookup :: Vector ObjectId,
    branchPatchLookup :: Vector PatchObjectId,
    branchChildLookup :: Vector (BranchObjectId, CausalHashId)
  }

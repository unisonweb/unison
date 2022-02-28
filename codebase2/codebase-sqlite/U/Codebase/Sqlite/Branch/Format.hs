module U.Codebase.Sqlite.Branch.Format
  ( BranchFormat (..),
    BranchLocalIds (..),
    SyncBranchFormat (..),
    localToDbBranch,
    localToDbDiff,
    -- dbToLocalDiff,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import U.Codebase.Sqlite.Branch.Diff (Diff, LocalDiff)
import qualified U.Codebase.Sqlite.Branch.Diff as Branch.Diff
import U.Codebase.Sqlite.Branch.Full (DbBranch, LocalBranch)
import qualified U.Codebase.Sqlite.Branch.Full as Branch.Full
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )
import Unison.Prelude

-- | A 'BranchFormat' is a deserialized namespace object (@object.bytes@).
--
-- you can use the exact same `BranchLocalIds` when converting between `Full` and `Diff`
data BranchFormat
  = Full BranchLocalIds LocalBranch
  | Diff BranchObjectId BranchLocalIds LocalDiff
  deriving (Show)

-- | A 'BranchLocalIds' is a mapping between local ids (local to this object) encoded as offsets, and actual database ids.
--
-- For example, a @branchTextLookup@ vector of @[50, 74]@ means "local id 0 corresponds to database text id 50, and
-- local id 1 corresponds to database text id 74".
data BranchLocalIds = LocalIds
  { branchTextLookup :: Vector TextId,
    branchDefnLookup :: Vector ObjectId,
    branchPatchLookup :: Vector PatchObjectId,
    branchChildLookup :: Vector (BranchObjectId, CausalHashId)
  }
  deriving (Show)

data SyncBranchFormat
  = SyncFull BranchLocalIds ByteString
  | SyncDiff BranchObjectId BranchLocalIds ByteString

localToDbBranch :: BranchLocalIds -> LocalBranch -> DbBranch
localToDbBranch li =
  Branch.Full.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

localToDbDiff :: BranchLocalIds -> LocalDiff -> Diff
localToDbDiff li =
  Branch.Diff.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

lookupBranchLocalText :: BranchLocalIds -> LocalTextId -> TextId
lookupBranchLocalText li (LocalTextId w) = branchTextLookup li Vector.! fromIntegral w

lookupBranchLocalDefn :: BranchLocalIds -> LocalDefnId -> ObjectId
lookupBranchLocalDefn li (LocalDefnId w) = branchDefnLookup li Vector.! fromIntegral w

lookupBranchLocalPatch :: BranchLocalIds -> LocalPatchObjectId -> PatchObjectId
lookupBranchLocalPatch li (LocalPatchObjectId w) = branchPatchLookup li Vector.! fromIntegral w

lookupBranchLocalChild :: BranchLocalIds -> LocalBranchChildId -> (BranchObjectId, CausalHashId)
lookupBranchLocalChild li (LocalBranchChildId w) = branchChildLookup li Vector.! fromIntegral w

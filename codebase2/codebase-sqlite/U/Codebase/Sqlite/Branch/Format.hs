{-# OPTIONS_GHC -Wno-deprecations #-}

module U.Codebase.Sqlite.Branch.Format
  ( BranchFormat (..),
    BranchLocalIds,
    BranchLocalIds' (..),
    SyncBranchFormat,
    SyncBranchFormat' (..),
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
type BranchLocalIds = BranchLocalIds' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

-- temp_entity
--  branch #foo
--
-- temp_entity_missing_dependency
--   #foo depends on causal #bar
--   #foo depends on namespace #baz
--
-- 1. store causal #bar, go to flush dependencies like normal
-- 2. ... oh this case is different than the others - we don't want to delete that row

----
-- can't simply treat causal's value hash as a mandatory dependency because we can't be sure
-- that the causal doesn't already exist in the target codebase without the value.
-- it probably does exist together with the value (though we found cases in the past where it didn't
-- due to race conditions, but we fixed that and added transactions and it shouldn't happen again?),
-- but it's not enforced at the schema level.
-- to enforce it at the schema level, we'd have to do something like store namespace_object_id instead
-- of value_hash in causal, which would require a db migration for a thing we don't necessarily even want
-- long term.
-- so, we can't simply "require" the value hash as a dependency of the causals and expect things to work smoothly
-- without relying on prayer.
--
-- temp_entity
--  branch #foo
--  causal #bar

-- temp_entity_missing_dependency
--   #foo depends on causal #bar
--   #bar depends on namespace #baz
--

data BranchLocalIds' t d p c = LocalIds
  { branchTextLookup :: Vector t,
    branchDefnLookup :: Vector d,
    branchPatchLookup :: Vector p,
    branchChildLookup :: Vector c
  }
  deriving (Show)

data SyncBranchFormat' parent text defn patch child
  = SyncFull (BranchLocalIds' text defn patch child) ByteString
  | SyncDiff parent (BranchLocalIds' text defn patch child) ByteString

type SyncBranchFormat = SyncBranchFormat' BranchObjectId TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

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

module U.Codebase.Sqlite.Branch.Format
  ( BranchFormat (..),
    BranchLocalIds (..),
    SyncBranchFormat (..),
    localToDbBranch,
    dbToLocalBranch,
    localToDbDiff,
    -- dbToLocalDiff,
  )
where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import U.Codebase.Sqlite.Branch.Diff (LocalDiff, Diff)
import U.Codebase.Sqlite.Branch.Full (DbBranch, LocalBranch)
import qualified U.Codebase.Sqlite.Branch.Full as Branch.Full
import qualified U.Codebase.Sqlite.Branch.Diff as Branch.Diff
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )

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

dbToLocalBranch :: DbBranch -> (BranchLocalIds, LocalBranch)
dbToLocalBranch = undefined

lookupBranchLocalText :: BranchLocalIds -> LocalTextId -> TextId
lookupBranchLocalText li (LocalTextId w) = branchTextLookup li Vector.! fromIntegral w

lookupBranchLocalDefn :: BranchLocalIds -> LocalDefnId -> ObjectId
lookupBranchLocalDefn li (LocalDefnId w) = branchDefnLookup li Vector.! fromIntegral w

lookupBranchLocalPatch :: BranchLocalIds -> LocalPatchObjectId -> PatchObjectId
lookupBranchLocalPatch li (LocalPatchObjectId w) = branchPatchLookup li Vector.! fromIntegral w

lookupBranchLocalChild :: BranchLocalIds -> LocalBranchChildId -> (BranchObjectId, CausalHashId)
lookupBranchLocalChild li (LocalBranchChildId w) = branchChildLookup li Vector.! fromIntegral w

localToDbDiff :: BranchLocalIds -> LocalDiff -> Diff
localToDbDiff li = Branch.Diff.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)


{-
projects.arya.message = "hello, world"     -> <text constant> -> #abc
projects.arya.program = printLine message  -> printLine #abc  -> #def

projects.arya {
  terms = { "message" -> #abc
          , "program" -> #def
          }
}

text table =
  { 1 -> "hello, world"
  , 2 -> "message"
  , 3 -> "program"
  }

hash table =
  { 10 -> "abc"
  , 11 -> "def"
  }

object table =
  { ...
  }

projects.arya {
  terms = { TextId 2 -> Reference { builtin = null, object = ObjectId 20, position = 0 }
          , TextId 3 -> Reference { builtin = null, object = ObjectId 21, position = 0 }
          }
}

stored in original codebase:
projects.arya = BranchFormat.Full {
  localIds = {
    text = [2, 3]
    hash = [10, 11]
    object = [20, 21]
  }
  localBranch = {
    terms = { LocalTextId 0 -> Reference { builtin = null, object = LocalObjectId 0, position = 0 }
            , LocalTextId 1 -> Reference { builtin = null, object = LocalObjectId 1, position = 0 }
            }
    ...
  }
}

write to dest codebase:
text table =
  { ...
  , 901 -> "hello, world"
  , 902 -> "message"
  , 903 -> "program"
  }

hash table =
  { ...
  , 500 -> "abc"
  , 501 -> "def"
  }

projects.arya {

  -- updated copy of original localIds, with new mapping
  localIds = {
    text = [902, 903]
    hash = [500, 501]
    object = [300, 301]
  }

  -- copy unmodified from original
  localBranch = {
    terms = { LocalTextId 0 -> Reference { builtin = null, object = LocalObjectId 0, position = 0 }
            , LocalTextId 1 -> Reference { builtin = null, object = LocalObjectId 1, position = 0 }
            }
    ...
  }
}

-}

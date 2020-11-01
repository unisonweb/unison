{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Branch.Diff where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import U.Codebase.Sqlite.DbId (BranchObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.Reference (Reference)
import U.Codebase.Sqlite.Referent (Referent)

type NameSegment = TextId

type Metadata = Reference

data PatchOp = PatchRemove | PatchAddReplace PatchObjectId
data DefinitionOp = RemoveDef | AddDefWithMetadata (Set Metadata) | AlterDefMetadata (AddRemove Metadata)
data ChildOp = ChildRemove | ChildAddReplace BranchObjectId
type AddRemove a = Map a Bool

addsRemoves :: AddRemove a -> ([a], [a])
addsRemoves map =
  let (fmap fst -> adds, fmap fst -> removes) =
        List.partition snd (Map.toList map)
   in (adds, removes)

data Diff = Diff
  { terms :: Map NameSegment (Map Referent DefinitionOp),
    types :: Map NameSegment (Map Reference DefinitionOp),
    patches :: Map NameSegment PatchOp,
    children :: Map NameSegment ChildOp
  }

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Branch.Diff where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchObjectId, LocalCausalHashId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import qualified U.Util.Map as Map
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Set as Set

type LocalDiff = Diff' LocalTextId LocalDefnId LocalPatchObjectId (LocalBranchObjectId, LocalCausalHashId)
type Diff = Diff' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

data DefinitionOp' r = RemoveDef | AddDefWithMetadata (Set r) | AlterDefMetadata (AddRemove r)
data PatchOp' p = PatchRemove | PatchAddReplace p deriving Functor
data ChildOp' c = ChildRemove | ChildAddReplace c deriving Functor
type AddRemove a = Map a Bool

type LocalDefinitionOp = DefinitionOp' (Metadata LocalTextId LocalDefnId)
type LocalPatchOp = PatchOp' LocalPatchObjectId
type LocalChildOp = ChildOp' (LocalBranchObjectId, LocalCausalHashId)

type DefinitionOp = DefinitionOp' (Metadata TextId ObjectId)
type PatchOp = PatchOp' PatchObjectId
type ChildOp = ChildOp' (BranchObjectId, CausalHashId)

addsRemoves :: AddRemove a -> ([a], [a])
addsRemoves map = (adds, removes)
  where
    (fmap fst -> adds, fmap fst -> removes) = List.partition snd (Map.toList map)

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

data Diff' t h p c = Diff
  { terms :: Map t (Map (Referent'' t h) (DefinitionOp' (Metadata t h))),
    types :: Map t (Map (Reference' t h) (DefinitionOp' (Metadata t h))),
    patches :: Map t (PatchOp' p),
    children :: Map t (ChildOp' c)
  }

type Metadata t h = Reference' t h

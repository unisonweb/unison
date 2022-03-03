{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.Branch.Diff where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchChildId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import qualified Unison.Util.Map as Map

type LocalDiff = Diff' LocalTextId LocalDefnId LocalPatchObjectId LocalBranchChildId

type Diff = Diff' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

data DefinitionOp' r = RemoveDef | AddDefWithMetadata (Set r) | AlterDefMetadata (AddRemove r) deriving (Show)

data PatchOp' p = PatchRemove | PatchAddReplace p deriving (Functor, Show)

data ChildOp' c = ChildRemove | ChildAddReplace c deriving (Functor, Show)

type AddRemove a = Map a Bool

type LocalDefinitionOp = DefinitionOp' (Metadata LocalTextId LocalDefnId)

type LocalPatchOp = PatchOp' LocalPatchObjectId

type LocalChildOp = ChildOp' LocalBranchChildId

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
  deriving (Show)

type Metadata t h = Reference' t h

quadmap :: (Ord t', Ord h') => (t -> t') -> (h -> h') -> (p -> p') -> (c -> c') -> Diff' t h p c -> Diff' t' h' p' c'
quadmap ft fh fp fc (Diff terms types patches children) =
  Diff
    (Map.bimap ft (Map.bimap doReferent doDefnOp) terms)
    (Map.bimap ft (Map.bimap doReference doDefnOp) types)
    (Map.bimap ft doPatchOp patches)
    (Map.bimap ft doChildOp children)
  where
    doReferent = bimap doReference doReference
    doReference = bimap ft fh
    doDefnOp = \case
      RemoveDef -> RemoveDef
      AddDefWithMetadata rs -> AddDefWithMetadata (Set.map doReference rs)
      AlterDefMetadata ar -> AlterDefMetadata (Map.mapKeys doReference ar)
    doPatchOp = fmap fp
    doChildOp = fmap fc

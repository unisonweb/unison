{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Branch.Full where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchObjectId, LocalCausalHashId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import qualified U.Util.Map as Map
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Set as Set

type LocalBranch = Branch' LocalTextId LocalDefnId LocalPatchObjectId (LocalBranchObjectId, LocalCausalHashId)

type DbBranch = Branch' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

data Branch' t h p c = Branch
  { terms :: Map t (Map (Referent'' t h) (MetadataSetFormat' t h)),
    types :: Map t (Map (Reference' t h) (MetadataSetFormat' t h)),
    patches :: Map t p,
    children :: Map t c
  }

type LocalMetadataSet = MetadataSetFormat' LocalTextId LocalDefnId

type DbMetadataSet = MetadataSetFormat' TextId ObjectId

data MetadataSetFormat' t h = Inline (Set (Reference' t h))

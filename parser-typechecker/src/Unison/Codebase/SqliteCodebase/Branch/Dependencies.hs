{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.SqliteCodebase.Branch.Dependencies where

import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import U.Codebase.HashTags (CausalHash, PatchHash)
import Unison.Codebase.Branch.Type as Branch
import Unison.Hash (Hash)
import Unison.Prelude

type Branches m = [(CausalHash, m (Branch m))]

data Dependencies = Dependencies
  { patches :: Set PatchHash,
    terms :: Set Hash,
    decls :: Set Hash
  }
  deriving (Show)
  deriving (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Dependencies

data Dependencies' = Dependencies'
  { patches' :: [PatchHash],
    terms' :: [Hash],
    decls' :: [Hash]
  }
  deriving (Eq, Show)
  deriving (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Dependencies'

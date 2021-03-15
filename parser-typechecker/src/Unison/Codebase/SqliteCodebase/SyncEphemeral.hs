{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.SqliteCodebase.SyncEphemeral where

import Data.Set (Set)
import U.Codebase.HashTags (CausalHash)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import Unison.Hash (Hash)

data Dependencies = Dependencies
  { definitions :: Set Hash,
    branches :: Set Hash
  }

data Error
  = Sync22Error Sync22.Error
  | SrcMissingSchema [(Q.SchemaType, Q.SchemaName)]
  | DestMissingSchema [(Q.SchemaType, Q.SchemaName)]
  | DisappearingBranch CausalHash
  deriving (Show)


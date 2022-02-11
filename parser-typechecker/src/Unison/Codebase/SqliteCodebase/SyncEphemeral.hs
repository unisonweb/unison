{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.SqliteCodebase.SyncEphemeral where

import Data.Set (Set)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.DbId (SchemaVersion)
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import Unison.Hash (Hash)

data Dependencies = Dependencies
  { definitions :: Set Hash,
    branches :: Set Hash
  }

data Error
  = Sync22Error Sync22.Error
  | SrcWrongSchema SchemaVersion
  | DestWrongSchema SchemaVersion
  | DisappearingBranch CausalHash
  deriving (Show)


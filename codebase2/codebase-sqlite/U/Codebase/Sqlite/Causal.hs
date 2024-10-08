module U.Codebase.Sqlite.Causal
  ( GDbCausal (..),
    SyncCausalFormat,
    SyncCausalFormat' (..),
  )
where

import Data.Vector (Vector)
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId)
import Unison.Prelude

data GDbCausal causalHash valueHash = DbCausal
  { selfHash :: causalHash,
    valueHash :: valueHash,
    parents :: Set causalHash
  }

data SyncCausalFormat' causalHash valueHash = SyncCausalFormat
  { valueHash :: valueHash,
    parents :: Vector causalHash
  }

type SyncCausalFormat = SyncCausalFormat' CausalHashId BranchHashId

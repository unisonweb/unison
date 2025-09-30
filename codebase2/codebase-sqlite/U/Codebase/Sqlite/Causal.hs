module U.Codebase.Sqlite.Causal
  ( DbCausal,
    GDbCausal (..),
    SyncCausalFormat,
    SyncCausalFormat' (..),
    syncCausalFormatCausalHash_,
    syncCausalFormatValueHash_,
  )
where

import Control.Lens
import Data.Vector (Vector)
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId)
import Unison.Prelude

data GDbCausal causalHash valueHash = DbCausal
  { selfHash :: causalHash,
    valueHash :: valueHash,
    parents :: Set causalHash
  }

type DbCausal = GDbCausal CausalHashId BranchHashId

data SyncCausalFormat' causalHash valueHash = SyncCausalFormat
  { valueHash :: valueHash,
    parents :: Vector causalHash
  }
  deriving stock (Eq, Show)

syncCausalFormatCausalHash_ :: Traversal (SyncCausalFormat' causalHash valueHash) (SyncCausalFormat' causalHash' valueHash) causalHash causalHash'
syncCausalFormatCausalHash_ f (SyncCausalFormat v p) = SyncCausalFormat v <$> traverse f p

syncCausalFormatValueHash_ :: Lens (SyncCausalFormat' causalHash valueHash) (SyncCausalFormat' causalHash valueHash') valueHash valueHash'
syncCausalFormatValueHash_ f (SyncCausalFormat v p) = (\v' -> SyncCausalFormat v' p) <$> f v

type SyncCausalFormat = SyncCausalFormat' CausalHashId BranchHashId

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.ProjectReflog where

import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime)
import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId, ProjectId)
import Unison.Sqlite (FromRow (..), ToRow (..), field)

data Entry causal = Entry
  { project :: ProjectId,
    branch :: ProjectBranchId,
    time :: UTCTime,
    fromRootCausalHash :: causal,
    toRootCausalHash :: causal,
    reason :: Text
  }
  deriving stock (Show, Functor, Foldable, Traversable)

instance ToRow (Entry CausalHashId) where
  toRow (Entry proj branch time fromRootCausalHash toRootCausalHash reason) =
    toRow (proj, branch, time, fromRootCausalHash, toRootCausalHash, reason)

instance FromRow (Entry CausalHashId) where
  fromRow = do
    project <- field
    branch <- field
    time <- field
    fromRootCausalHash <- field
    toRootCausalHash <- field
    reason <- field
    pure $ Entry {..}

causals_ :: Traversal (Entry causal) (Entry causal') causal causal'
causals_ f (Entry {..}) = Entry project branch time <$> f fromRootCausalHash <*> f toRootCausalHash <*> pure reason

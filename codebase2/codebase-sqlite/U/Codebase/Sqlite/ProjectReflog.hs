{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Sqlite.ProjectReflog
  ( Entry (..),
    project_,
    branch_,
    projectAndBranch_,
  )
where

import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime)
import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId, ProjectId)
import Unison.Sqlite (FromRow (..), ToRow (..), field)

data Entry project branch causal = Entry
  { project :: project,
    branch :: branch,
    time :: UTCTime,
    fromRootCausalHash :: Maybe causal,
    toRootCausalHash :: causal,
    reason :: Text
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

project_ :: Lens (Entry project branch causal) (Entry project' branch causal) project project'
project_ = lens project (\e p -> e {project = p})

branch_ :: Lens (Entry project branch causal) (Entry project branch' causal) branch branch'
branch_ = lens branch (\e b -> e {branch = b})

-- | Both Project and Branch Ids are required to load a branch, so this is often more useful.
projectAndBranch_ :: Lens (Entry project branch causal) (Entry project' branch' causal) (project, branch) (project', branch')
projectAndBranch_ = lens (\Entry {..} -> (project, branch)) (\e (project, branch) -> e {project = project, branch = branch})

instance ToRow (Entry ProjectId ProjectBranchId CausalHashId) where
  toRow (Entry proj branch time fromRootCausalHash toRootCausalHash reason) =
    toRow (proj, branch, time, fromRootCausalHash, toRootCausalHash, reason)

instance FromRow (Entry ProjectId ProjectBranchId CausalHashId) where
  fromRow = do
    project <- field
    branch <- field
    time <- field
    fromRootCausalHash <- field
    toRootCausalHash <- field
    reason <- field
    pure $ Entry {..}

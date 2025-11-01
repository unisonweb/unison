{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateHistoryComments (hashHistoryCommentsMigration) where

import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import U.Codebase.Branch.Type qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId (..), ProjectId (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..), ProjectBranchRow (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache qualified as BranchCache
import Unison.Codebase.SqliteCodebase.Operations qualified as CodebaseOps
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.Debug qualified as Debug
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.Sqlite (queryListCol)
import Unison.Sqlite qualified as Sqlite
import Unison.Sqlite.Connection qualified as Connection
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Cache qualified as Cache
import UnliftIO qualified
import UnliftIO qualified as UnsafeIO

-- | This migration just deletes all the old name lookups, it doesn't recreate them.
-- On share we'll rebuild only the required name lookups from scratch.
hashHistoryCommentsMigration :: Sqlite.Transaction ()
hashHistoryCommentsMigration = do
  Queries.expectSchemaVersion 23
  hashAllHistoryComments
  Queries.setSchemaVersion 24

hashAllHistoryComments :: Sqlite.Transaction ()
hashAllHistoryComments = do
  historyComments <-
    Sqlite.queryListRow @(HistoryCommentId, CausalHash, Text, UTCTime)
      [Sqlite.sql|
    SELECT id, causal_hash.base32, author, created_at
      FROM history_comments
    |]
  for_ historyComments $ \(HistoryCommentId commentId, causalHash, author, createdAt) -> do
    let newCausalHash = hashHistoryComment causalHash author createdAt
    Sqlite.execute
      [Sqlite.sql|
      UPDATE history_comments
         SET causal_hash = ?
       WHERE id = ?
      |]
      (newCausalHash, commentId)

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateHistoryComments (hashHistoryCommentsMigration) where

import Data.Time (UTCTime)
import U.Codebase.HashTags
import U.Codebase.Sqlite.DbId (HistoryCommentId (..), HistoryCommentRevisionId (HistoryCommentRevisionId))
import U.Codebase.Sqlite.Orphans (AsSqlite (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Debug qualified as Debug
import Unison.Hash (Hash)
import Unison.Hashing.HistoryComments (hashHistoryComment, hashHistoryCommentRevision)
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.KeyThumbprint (KeyThumbprint)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

-- | This migration just deletes all the old name lookups, it doesn't recreate them.
-- On share we'll rebuild only the required name lookups from scratch.
hashHistoryCommentsMigration :: KeyThumbprint -> Sqlite.Transaction ()
hashHistoryCommentsMigration defaultKeyThumbprint = do
  Q.expectSchemaVersion 24
  hashAllHistoryComments defaultKeyThumbprint
  Q.setSchemaVersion 25

hashAllHistoryComments :: KeyThumbprint -> Sqlite.Transaction ()
hashAllHistoryComments defaultKeyThumbprint = do
  historyComments <-
    Sqlite.queryListRow @(HistoryCommentId, AsSqlite Hash, Text, UTCTime)
      [Sqlite.sql|
    SELECT comment.id, causal_hash.base32, comment.author, thumbprint.thumbprint, comment.created_at
      FROM history_comments comment
      JOIN hash causal_hash ON comment.causal_hash_id = causal_hash.id
    |]
  Debug.debugM Debug.Temp "Got comments" historyComments
  for_ historyComments $ \(HistoryCommentId commentId, causalHash, author, createdAt) -> do
    let historyComment =
          HistoryComment
            { author,
              createdAt,
              authorThumbprint = defaultKeyThumbprint,
              causal = coerce @_ @CausalHash causalHash,
              commentId = ()
            }
    let historyCommentHash = hashHistoryComment historyComment
    Debug.debugM Debug.Temp "Hashing history comment" (author, causalHash)
    historyCommentHashId <- Q.saveHistoryCommentHash historyCommentHash.commentId
    Sqlite.execute
      [Sqlite.sql|
      UPDATE history_comments
         SET comment_hash_id = :historyCommentHashId
       WHERE id = :commentId
      |]
  historyCommentRevisions <-
    Sqlite.queryListRow @(HistoryCommentRevisionId, Text, Text, UTCTime, AsSqlite Hash)
      [Sqlite.sql|
    SELECT hcr.id, hcr.subject, hcr.contents, hcr.created_at, comment_hash.base32
      FROM history_comment_revisions hcr
      JOIN history_comments comment ON hcr.comment_id = comment.id
      JOIN hash comment_hash ON comment.comment_hash_id = comment_hash.id
    |]
  Debug.debugM Debug.Temp "Got revisions" historyCommentRevisions
  for_ historyCommentRevisions $ \(HistoryCommentRevisionId revisionId, subject, content, createdAt, commentHash) -> do
    Debug.debugM Debug.Temp "Hashing history comment revision" (subject, content)
    let historyCommentRevision =
          HistoryCommentRevision
            { subject,
              content,
              createdAt,
              comment = coerce @_ @HistoryCommentHash commentHash,
              revisionId = ()
            }
    let historyCommentRevisionHash = hashHistoryCommentRevision historyCommentRevision
    commentRevisionHashId <- Q.saveHistoryCommentRevisionHash historyCommentRevisionHash.revisionId
    Sqlite.execute
      [Sqlite.sql|
      UPDATE history_comment_revisions
         SET revision_hash_id = :commentRevisionHashId
       WHERE id = :revisionId
      |]

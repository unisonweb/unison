{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateHistoryComments (hashHistoryCommentsMigration) where

import Data.Time (UTCTime)
import U.Codebase.HashTags
import U.Codebase.Sqlite.DbId (HistoryCommentId (..), HistoryCommentRevisionId (HistoryCommentRevisionId))
import U.Codebase.Sqlite.Orphans (AsSqlite (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Hash (Hash)
import Unison.Hashing.HistoryComments (hashHistoryComment, hashHistoryCommentRevision)
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.KeyThumbprint (KeyThumbprint (KeyThumbprint))
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

-- | This migration just deletes all the old name lookups, it doesn't recreate them.
-- On share we'll rebuild only the required name lookups from scratch.
hashHistoryCommentsMigration :: Sqlite.Transaction ()
hashHistoryCommentsMigration = do
  Q.expectSchemaVersion 23
  hashAllHistoryComments
  Q.setSchemaVersion 24

hashAllHistoryComments :: Sqlite.Transaction ()
hashAllHistoryComments = do
  historyComments <-
    Sqlite.queryListRow @(HistoryCommentId, AsSqlite Hash, Text, Text, UTCTime)
      [Sqlite.sql|
    SELECT id, causal_hash.base32, author, thumbprint.thumbprint, created_at
      FROM history_comments
      JOIN hash causal_hash ON history_comments.causal_hash_id = causal_hash.id
      JOIN key_thumbprint thumbprint ON history_comments.author_thumbprint_id = thumbprint.id
    |]
  for_ historyComments $ \(HistoryCommentId commentId, causalHash, author, authorThumbprint, createdAt) -> do
    let historyComment =
          HistoryComment
            { author,
              createdAt,
              authorThumbprint = KeyThumbprint authorThumbprint,
              causal = coerce @_ @CausalHash causalHash,
              commentId = ()
            }
    let historyCommentHash = hashHistoryComment historyComment
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
    SELECT id, subject, content, created_at, comment_hash.base32
      FROM history_comment_revisions
      JOIN hash comment_hash ON history_comment_revisions.comment_hash_id = comment_hash.id
    |]
  for_ historyCommentRevisions $ \(HistoryCommentRevisionId revisionId, subject, content, createdAt, commentHash) -> do
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

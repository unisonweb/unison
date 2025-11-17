{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateHistoryComments (hashHistoryCommentsMigration) where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import U.Codebase.HashTags
import U.Codebase.Sqlite.DbId (HistoryCommentId (..), HistoryCommentRevisionId (HistoryCommentRevisionId))
import U.Codebase.Sqlite.Orphans (AsSqlite (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.PersonalKey (PersonalPrivateKey)
import Unison.Auth.PersonalKey qualified as PersonalKey
import Unison.Debug qualified as Debug
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.HistoryComments.Hashing (hashHistoryComment, hashHistoryCommentRevision)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

-- Convert milliseconds since epoch to UTCTime _exactly_.
-- UTCTime has picosecond precision so this is lossless.
millisToUTCTime :: Int64 -> UTCTime
millisToUTCTime ms =
  toRational ms
    & (/ (1_000 :: Rational))
    & fromRational
    & posixSecondsToUTCTime

-- | This migration just deletes all the old name lookups, it doesn't recreate them.
-- On share we'll rebuild only the required name lookups from scratch.
hashHistoryCommentsMigration :: PersonalPrivateKey -> Sqlite.Transaction ()
hashHistoryCommentsMigration personalKey = do
  Q.expectSchemaVersion 24
  hashAllHistoryComments personalKey
  Q.setSchemaVersion 25

hashAllHistoryComments :: PersonalPrivateKey -> Sqlite.Transaction ()
hashAllHistoryComments personalKey = do
  let keyThumbprint = PersonalKey.personalKeyThumbprint personalKey
  keyThumbprintId <- Q.ensurePersonalKeyThumbprintId keyThumbprint
  historyComments <-
    Sqlite.queryListRow @(HistoryCommentId, AsSqlite Hash, Text, Int64)
      [Sqlite.sql|
    SELECT comment.id, causal_hash.base32, comment.author, CAST(comment.created_at  * 1000 AS INTEGER)
      FROM history_comments comment
      JOIN hash causal_hash ON comment.causal_hash_id = causal_hash.id
    |]
  Debug.debugM Debug.Temp "Got comments" historyComments
  for_ historyComments $ \(HistoryCommentId commentId, causalHash, author, createdAtMs) -> do
    let historyComment =
          HistoryComment
            { author,
              createdAt = millisToUTCTime createdAtMs,
              authorThumbprint = keyThumbprint,
              causal = coerce @_ @CausalHash causalHash,
              commentId = ()
            }
    let historyCommentHash = hashHistoryComment historyComment
    Debug.debugM Debug.Temp "Hashing history comment" (author, causalHash)
    historyCommentHashId <- Q.saveHistoryCommentHash historyCommentHash.commentId
    Sqlite.execute
      [Sqlite.sql|
      UPDATE history_comments
         SET comment_hash_id = :historyCommentHashId,
             author_thumbprint_id = :keyThumbprintId
       WHERE id = :commentId
      |]
  historyCommentRevisions <-
    Sqlite.queryListRow @(HistoryCommentRevisionId, Text, Text, Bool, Int64, AsSqlite Hash)
      [Sqlite.sql|
    SELECT hcr.id, hcr.subject, hcr.contents, hcr.hidden, CAST(hcr.created_at  * 1000 AS INTEGER), comment_hash.base32
      FROM history_comment_revisions hcr
      JOIN history_comments comment ON hcr.comment_id = comment.id
      JOIN hash comment_hash ON comment.comment_hash_id = comment_hash.id
    |]
  Debug.debugM Debug.Temp "Got revisions" historyCommentRevisions
  for_ historyCommentRevisions $ \(HistoryCommentRevisionId revisionId, subject, content, isHidden, createdAtMs, commentHash) -> do
    Debug.debugM Debug.Temp "Hashing history comment revision" (subject, content)
    let historyCommentRevision =
          HistoryCommentRevision
            { subject,
              content,
              isHidden,
              authorSignature = mempty,
              createdAt = millisToUTCTime createdAtMs,
              comment = coerce @_ @HistoryCommentHash commentHash,
              revisionId = ()
            }
    let historyCommentRevisionHash = hashHistoryCommentRevision historyCommentRevision
    let historyCommentRevisionHashBytes =
          historyCommentRevisionHash.revisionId
            & unHistoryCommentRevisionHash
            & Hash.toByteString
    PersonalKey.PersonalKeySignature authorSignature <-
      Sqlite.unsafeIO (PersonalKey.signWithPersonalKey personalKey historyCommentRevisionHashBytes) >>= \case
        Left err -> error $ "Migration failure: Failed to sign history comment revision: " ++ show err
        Right sig -> pure sig
    commentRevisionHashId <- Q.saveHistoryCommentRevisionHash historyCommentRevisionHash.revisionId
    Sqlite.execute
      [Sqlite.sql|
      UPDATE history_comment_revisions
         SET revision_hash_id = :commentRevisionHashId,
             author_signature = :authorSignature
       WHERE id = :revisionId
      |]

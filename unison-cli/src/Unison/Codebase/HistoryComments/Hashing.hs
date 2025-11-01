module Unison.Codebase.HistoryComments.Hashing
  ( hashComment,
    hashCommentRevision,
  )
where

commentHashingVersion :: Int32
commentHashingVersion = 1

-- Hash a base comment
instance ContentAddressable (HistoryComment UTCTime KeyThumbprint CausalHash any) where
  contentHash HistoryComment {createdAt, author, causal, authorThumbprint} =
    CH.hashUpdates
      CH.hashInit
      [ BL.toStrict . Builder.toLazyByteString $ Builder.int32BE commentHashingVersion,
        Hash.toByteString (into @Hash causal),
        Text.encodeUtf8 $ thumbprintToText authorThumbprint,
        Text.encodeUtf8 author,
        -- Encode UTCTime as a UTC 8601 seconds since epoch
        createdAt
          & Time.utcTimeToPOSIXSeconds
          & floor
          & Builder.int64BE
          & Builder.toLazyByteString
          & BL.toStrict
      ]
      & CH.hashFinalize @CH.SHA3_512
      & BA.convert
      & Hash.fromByteString

-- Hash a comment revision
instance ContentAddressable (HistoryCommentRevision any UTCTime CommentHash) where
  contentHash HistoryCommentRevision {subject, content, createdAt, comment = commentHash} =
    CH.hashUpdates
      CH.hashInit
      [ BL.toStrict . Builder.toLazyByteString $ Builder.int32BE commentHashingVersion,
        Hash.toByteString (into @Hash commentHash),
        Text.encodeUtf8 subject,
        Text.encodeUtf8 content,
        -- Encode UTCTime as a UTC 8601 seconds since epoch
        createdAt
          & Time.utcTimeToPOSIXSeconds
          & floor
          & Builder.int64BE
          & Builder.toLazyByteString
          & BL.toStrict
      ]
      & CH.hashFinalize @CH.SHA3_512
      & BA.convert
      & Hash.fromByteString

hashHistoryComment ::
  HistoryComment UTCTime KeyThumbprint CausalHash any ->
  HistoryComment UTCTime KeyThumbprint CausalHash HistoryCommentHash
hashHistoryComment historyComment =
  let commentHash = CommentHash $ contentHash historyComment
   in historyComment {commentId = commentHash}

hashHistoryCommentRevision ::
  HistoryCommentRevision any UTCTime CommentHash
  -> HistoryCommentRevision CommentRevisionHash UTCTime CommentHash
hashHistoryCommentRevision historyCommentRevision =
  let commentRevisionHash = CommentRevisionHash $ contentHash historyComment
    in historyCommentRevision {revisionId = commentRevisionHash}

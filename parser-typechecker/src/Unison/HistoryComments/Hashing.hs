{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.HistoryComments.Hashing
  ( hashHistoryComment,
    hashHistoryCommentRevision,
  )
where

import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as Time
import U.Codebase.HashTags (CausalHash, HistoryCommentHash (..), HistoryCommentRevisionHash (..))
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Hashing.V2 (ContentAddressable (..))
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.KeyThumbprint (KeyThumbprint (..))
import Unison.Prelude

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
instance ContentAddressable (HistoryCommentRevision any UTCTime HistoryCommentHash) where
  contentHash HistoryCommentRevision {subject, content, createdAt, comment = commentHash, isHidden} =
    CH.hashUpdates
      CH.hashInit
      [ BL.toStrict . Builder.toLazyByteString $ Builder.int32BE commentHashingVersion,
        Hash.toByteString (into @Hash commentHash),
        Text.encodeUtf8 subject,
        Text.encodeUtf8 content,
        if isHidden then "1" else "0",
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
  let commentHash = HistoryCommentHash $ contentHash historyComment
   in historyComment {commentId = commentHash}

hashHistoryCommentRevision ::
  HistoryCommentRevision any UTCTime HistoryCommentHash ->
  HistoryCommentRevision HistoryCommentRevisionHash UTCTime HistoryCommentHash
hashHistoryCommentRevision historyCommentRevision =
  let commentRevisionHash = HistoryCommentRevisionHash $ contentHash historyCommentRevision
   in historyCommentRevision {revisionId = commentRevisionHash}

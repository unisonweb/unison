{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Hashing.V2.HistoryComments
  ( hashHistoryComment,
    hashHistoryCommentRevision,
  )
where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR.Write
import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as Time
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashTags
import Unison.Hashing.ContentAddressable (ContentAddressable (..))
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.KeyThumbprint (KeyThumbprint (..))
import Unison.Prelude

commentHashingVersion :: Int32
commentHashingVersion = 1

revisionHashingVersion :: Int32
revisionHashingVersion = 1

-- Hash a base comment
instance ContentAddressable (HistoryComment UTCTime KeyThumbprint CausalHash any) where
  contentHash HistoryComment {createdAt, author, causal, authorThumbprint} =
    CH.hashUpdates
      CH.hashInit
      [commentBytes]
      & CH.hashFinalize @CH.SHA3_512
      & BA.convert
      & Hash.fromByteString
    where
      -- First encode as CBOR to normalize the representation,
      -- this ensures each value is unambiguously represented as a separate _field_, and that
      -- we don't get the same hash for something like (title: "ab", contents: "c") and (title: "a", contents "bc")
      -- when they get concatenated together for hashing.
      -- CBOR has a canonical encoding, so this is deterministic, even across different architectures.
      commentBytes :: ByteString
      commentBytes =
        CBOR.Write.toStrictByteString $
          CBOR.encodeInteger (fromIntegral commentHashingVersion)
            <> CBOR.encodeBytes (Hash.toByteString $ into @Hash causal)
            <> CBOR.encodeString author
            <> CBOR.encodeString (thumbprintToText authorThumbprint)
            <> CBOR.encodeInteger (floor $ Time.utcTimeToPOSIXSeconds createdAt)

-- Hash a comment revision
instance ContentAddressable (HistoryCommentRevision any UTCTime HistoryCommentHash) where
  contentHash HistoryCommentRevision {subject, content, createdAt, comment = commentHash, isHidden} =
    CH.hashUpdates
      CH.hashInit
      [ commentBytes
      ]
      & CH.hashFinalize @CH.SHA3_512
      & BA.convert
      & Hash.fromByteString
    where
      -- First encode as CBOR to normalize the representation,
      -- this ensures each value is unambiguously represented as a separate _field_, and that
      -- we don't get the same hash for something like (title: "ab", contents: "c") and (title: "a", contents "bc")
      -- when they get concatenated together for hashing.
      -- CBOR has a canonical encoding, so this is deterministic, even across different architectures.
      commentBytes :: ByteString
      commentBytes =
        CBOR.Write.toStrictByteString $
          CBOR.encodeInteger (fromIntegral revisionHashingVersion)
            <> CBOR.encodeBytes (Hash.toByteString $ into @Hash commentHash)
            <> CBOR.encodeString subject
            <> CBOR.encodeString content
            <> CBOR.encodeBool isHidden
            <> CBOR.encodeInteger (floor $ Time.utcTimeToPOSIXSeconds createdAt)

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

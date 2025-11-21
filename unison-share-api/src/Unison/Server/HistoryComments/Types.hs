module Unison.Server.HistoryComments.Types
  ( DownloadCommentsRequest (..),
    UploadCommentsResponse (..),
    HistoryCommentChunk (..),
    HistoryComment (..),
    HistoryCommentRevision (..),
  )
where

import Codec.CBOR.Decoding
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (Serialise (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word8)
import Unison.Hash32 (Hash32)
import Unison.Server.Orphans ()
import Unison.Server.Types
import Unison.Share.API.Hash (HashJWT)

data DownloadCommentsRequest = DownloadCommentsRequest
  { causalHash :: HashJWT,
    branchRef :: BranchRef,
    since :: UTCTime
  }

data UploadCommentsResponse = UploadCommentsResponse

data HistoryComment = HistoryComment
  { author :: Text,
    createdAt :: UTCTime,
    authorThumbprint :: ByteString,
    causalHash :: Hash32,
    commentHash :: Hash32
  }

instance Serialise HistoryComment where
  encode (HistoryComment {author, createdAt, authorThumbprint, causalHash, commentHash}) =
    encode author
      <> encode createdAt
      <> encode authorThumbprint
      <> encode causalHash
      <> encode commentHash
  decode = do
    author <- decode
    createdAt <- decode
    authorThumbprint <- decode
    causalHash <- decode
    commentHash <- decode
    pure HistoryComment {author, createdAt, authorThumbprint, causalHash, commentHash}

data HistoryCommentRevision = HistoryCommentRevision
  { subject :: Text,
    content :: Text,
    createdAt :: UTCTime,
    isHidden :: Bool,
    authorSignature :: ByteString,
    revisionHash :: Hash32
  }

instance Serialise HistoryCommentRevision where
  encode (HistoryCommentRevision {subject, content, createdAt, isHidden, authorSignature, revisionHash}) =
    encode subject
      <> encode content
      <> encode createdAt
      <> encode isHidden
      <> encode authorSignature
      <> encode revisionHash
  decode = do
    subject <- decode
    content <- decode
    createdAt <- decode
    isHidden <- decode
    authorSignature <- decode
    revisionHash <- decode
    pure HistoryCommentRevision {subject, content, createdAt, isHidden, authorSignature, revisionHash}

data HistoryCommentChunk
  = HistoryCommentChunk HistoryComment
  | HistoryCommentRevisionChunk HistoryCommentRevision
  | -- Generic error chunk
    HistoryCommentErrorChunk Text

instance Serialise HistoryCommentChunk where
  encode = \case
    HistoryCommentChunk comment ->
      encode HistoryCommentTag
        <> encode comment
    HistoryCommentRevisionChunk revision ->
      encode HistoryCommentRevisionTag
        <> encode revision
    HistoryCommentErrorChunk errMsg ->
      encode HistoryCommentErrorTag
        <> encode errMsg
  decode = do
    tag <- decode :: Decoder s HistoryCommentChunkTag
    case tag of
      HistoryCommentTag -> HistoryCommentChunk <$> decode
      HistoryCommentRevisionTag -> HistoryCommentRevisionChunk <$> decode
      HistoryCommentErrorTag -> HistoryCommentErrorChunk <$> decode

data HistoryCommentChunkTag
  = HistoryCommentTag
  | HistoryCommentRevisionTag
  | HistoryCommentErrorTag

instance Serialise HistoryCommentChunkTag where
  encode = \case
    HistoryCommentTag -> encode (0 :: Word8)
    HistoryCommentRevisionTag -> encode (1 :: Word8)
    HistoryCommentErrorTag -> encode (2 :: Word8)
  decode = do
    tag <- decode :: Decoder s Word8
    case tag of
      0 -> pure HistoryCommentTag
      1 -> pure HistoryCommentRevisionTag
      2 -> pure HistoryCommentErrorTag
      _ -> fail "Invalid HistoryCommentChunkTag"

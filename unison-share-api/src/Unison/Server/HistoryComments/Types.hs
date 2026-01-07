module Unison.Server.HistoryComments.Types
  ( DownloadCommentsRequest (..),
    UploadCommentsResponse (..),
    DownloadCommentsResponse (..),
    HistoryCommentUploaderChunk (..),
    HistoryCommentDownloaderChunk (..),
    HistoryComment (..),
    HistoryCommentRevision (..),
    HistoryCommentHash32 (..),
    HistoryCommentRevisionHash32 (..),
  )
where

import Codec.CBOR.Decoding
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (Serialise (..))
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
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
  deriving (Show, Eq)

data UploadCommentsResponse
  = UploadCommentsProjectBranchNotFound BranchRef
  | UploadCommentsNotAuthorized BranchRef
  | UploadCommentsGenericFailure Text
  deriving (Show, Eq)

instance Serialise UploadCommentsResponse where
  encode = \case
    UploadCommentsProjectBranchNotFound br ->
      encode (0 :: Word8) <> encode br
    UploadCommentsNotAuthorized br ->
      encode (1 :: Word8) <> encode br
    UploadCommentsGenericFailure errMsg ->
      encode (2 :: Word8) <> encode errMsg
  decode = do
    tag <- decode :: Decoder s Word8
    case tag of
      0 -> UploadCommentsProjectBranchNotFound <$> decode
      1 -> UploadCommentsNotAuthorized <$> decode
      2 -> UploadCommentsGenericFailure <$> decode
      _ -> fail "Invalid UploadCommentsResponse tag"

data DownloadCommentsResponse
  = DownloadCommentsProjectBranchNotFound BranchRef
  | DownloadCommentsNotAuthorized BranchRef
  | DownloadCommentsGenericFailure Text
  deriving (Show, Eq)

instance Serialise DownloadCommentsResponse where
  encode = \case
    DownloadCommentsProjectBranchNotFound br ->
      encode (0 :: Word8) <> encode br
    DownloadCommentsNotAuthorized br ->
      encode (1 :: Word8) <> encode br
    DownloadCommentsGenericFailure errMsg ->
      encode (2 :: Word8) <> encode errMsg
  decode = do
    tag <- decode :: Decoder s Word8
    case tag of
      0 -> DownloadCommentsProjectBranchNotFound <$> decode
      1 -> DownloadCommentsNotAuthorized <$> decode
      2 -> DownloadCommentsGenericFailure <$> decode
      _ -> fail "Invalid DownloadCommentsResponse tag"

data HistoryComment = HistoryComment
  { author :: Text,
    createdAt :: UTCTime,
    authorThumbprint :: Text,
    causalHash :: Hash32,
    commentHash :: Hash32
  }
  deriving (Show, Eq)

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
    revisionHash :: Hash32,
    commentHash :: Hash32
  }
  deriving (Show, Eq)

instance Serialise HistoryCommentRevision where
  encode (HistoryCommentRevision {subject, content, createdAt, isHidden, authorSignature, revisionHash, commentHash}) =
    encode subject
      <> encode content
      <> encode createdAt
      <> encode isHidden
      <> encode authorSignature
      <> encode revisionHash
      <> encode commentHash

  decode = do
    subject <- decode
    content <- decode
    createdAt <- decode
    isHidden <- decode
    authorSignature <- decode
    revisionHash <- decode
    commentHash <- decode
    pure HistoryCommentRevision {subject, content, createdAt, isHidden, authorSignature, revisionHash, commentHash}

data HistoryCommentDownloaderChunkTag
  = RequestCommentsTag
  | DoneCheckingHashesTag
  deriving (Show, Eq, Enum, Bounded)

instance Serialise HistoryCommentDownloaderChunkTag where
  encode = \case
    RequestCommentsTag -> encode (0 :: Word8)
    DoneCheckingHashesTag -> encode (1 :: Word8)

  decode = do
    tag <- decodeWord8
    case tag of
      0 -> pure RequestCommentsTag
      1 -> pure DoneCheckingHashesTag
      _ -> fail $ "Unknown HistoryCommentDownloaderChunkTag: " ++ show tag

newtype HistoryCommentHash32 = HistoryCommentHash32 {unHistoryCommentHash32 :: Hash32}
  deriving newtype (Show, Eq, Ord, Serialise)

newtype HistoryCommentRevisionHash32 = HistoryCommentRevisionHash32 {unHistoryCommentRevisionHash32 :: Hash32}
  deriving newtype (Show, Eq, Ord, Serialise)

data HistoryCommentDownloaderChunk
  = -- Request the comments we're missing.
    RequestCommentsChunk (NESet (Either HistoryCommentHash32 HistoryCommentRevisionHash32))
  | -- We've checked all provided hashes (and received DoneSendingHashesChunk from the uploader), and have issued all the Requests we need.
    DoneCheckingHashesChunk
  deriving (Show, Eq)

instance Serialise HistoryCommentDownloaderChunk where
  encode = \case
    RequestCommentsChunk hashSet ->
      encode RequestCommentsTag
        <> encode (NESet.toSet hashSet)
    DoneCheckingHashesChunk ->
      encode DoneCheckingHashesTag
  decode = do
    tag <- decode :: Decoder s HistoryCommentDownloaderChunkTag
    case tag of
      RequestCommentsTag -> do
        mayHashSet <- NESet.nonEmptySet <$> decode
        case mayHashSet of
          Just hashSet -> pure $ RequestCommentsChunk hashSet
          Nothing -> fail "HistoryCommentRequestComments: unexpected empty set"
      DoneCheckingHashesTag -> pure DoneCheckingHashesChunk

data HistoryCommentUploaderChunk
  = -- Tell the other side about some comment hashes that it may wish to request.
    PossiblyNewHashesChunk (NonEmpty (HistoryCommentHash32, [HistoryCommentRevisionHash32]))
  | DoneSendingHashesChunk
  | HistoryCommentChunk HistoryComment
  | HistoryCommentRevisionChunk HistoryCommentRevision
  deriving (Show, Eq)

instance Serialise HistoryCommentUploaderChunk where
  encode = \case
    PossiblyNewHashesChunk newHashesChunk ->
      encode PossiblyNewHashesTag
        <> encode newHashesChunk
    DoneSendingHashesChunk ->
      encode DoneSendingHashesTag
    HistoryCommentChunk comment ->
      encode HistoryCommentTag
        <> encode comment
    HistoryCommentRevisionChunk revision ->
      encode HistoryCommentRevisionTag
        <> encode revision
  decode = do
    tag <- decode :: Decoder s HistoryCommentChunkTag
    case tag of
      PossiblyNewHashesTag -> do
        mayHashList <- NEL.nonEmpty <$> decode
        case mayHashList of
          Just hashList -> pure $ PossiblyNewHashesChunk hashList
          Nothing -> fail "HistoryCommentPossiblyNewHashes: unexpected empty set"
      DoneSendingHashesTag -> pure DoneSendingHashesChunk
      HistoryCommentTag -> HistoryCommentChunk <$> decode
      HistoryCommentRevisionTag -> HistoryCommentRevisionChunk <$> decode

data HistoryCommentChunkTag
  = PossiblyNewHashesTag
  | DoneSendingHashesTag
  | HistoryCommentTag
  | HistoryCommentRevisionTag
  deriving (Show, Eq)

instance Serialise HistoryCommentChunkTag where
  encode = \case
    PossiblyNewHashesTag -> encode (0 :: Word8)
    DoneSendingHashesTag -> encode (1 :: Word8)
    HistoryCommentTag -> encode (2 :: Word8)
    HistoryCommentRevisionTag -> encode (3 :: Word8)
  decode = do
    tag <- decode :: Decoder s Word8
    case tag of
      0 -> pure PossiblyNewHashesTag
      1 -> pure DoneSendingHashesTag
      2 -> pure HistoryCommentTag
      3 -> pure HistoryCommentRevisionTag
      _ -> fail "Invalid HistoryCommentChunkTag"

module Unison.SyncV2.Types
  ( GetCausalHashRequest (..),
    GetCausalHashResponse (..),
    GetCausalHashError (..),
    DownloadEntitiesRequest (..),
    DownloadEntitiesChunk (..),
    SyncError (..),
    DownloadEntitiesError (..),
    CBORBytes (..),
    deserialiseOrFailCBORBytes,
    UploadEntitiesRequest (..),
    BranchRef (..),
    PullError (..),
  )
where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (Serialise (..))
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Data.Set (Set)
import Data.Text (Text)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.Server.Orphans ()
import Unison.Share.API.Hash (HashJWT)
import Unison.Sync.Types qualified as SyncV1

newtype BranchRef = BranchRef Text
  deriving (Serialise) via Text

data GetCausalHashRequest = GetCausalHashRequest
  { branchRef :: BranchRef
  }

instance Serialise GetCausalHashRequest where
  encode (GetCausalHashRequest {branchRef}) =
    encode branchRef
  decode = GetCausalHashRequest <$> decode

data GetCausalHashResponse
  = GetCausalHashSuccess HashJWT
  | GetCausalHashError GetCausalHashError
  deriving stock (Show, Eq, Ord)

instance Serialise GetCausalHashResponse where
  encode (GetCausalHashSuccess hash) =
    encode GetCausalHashSuccessTag <> encode hash
  encode (GetCausalHashError err) =
    encode GetCausalHashErrorTag <> encode err
  decode = do
    tag <- decode
    case tag of
      GetCausalHashSuccessTag -> GetCausalHashSuccess <$> decode
      GetCausalHashErrorTag -> GetCausalHashError <$> decode

data GetCausalHashResponseTag
  = GetCausalHashSuccessTag
  | GetCausalHashErrorTag
  deriving stock (Show, Eq, Ord)

instance Serialise GetCausalHashResponseTag where
  encode GetCausalHashSuccessTag = CBOR.encodeWord8 0
  encode GetCausalHashErrorTag = CBOR.encodeWord8 1
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure GetCausalHashSuccessTag
      1 -> pure GetCausalHashErrorTag
      _ -> fail "invalid tag"

data GetCausalHashError
  = GetCausalHashNoReadPermission SyncV1.RepoInfo
  | GetCausalHashUserNotFound
  | GetCausalHashInvalidRepoInfo Text SyncV1.RepoInfo
  deriving stock (Show, Eq, Ord)

instance Serialise GetCausalHashError where
  encode (GetCausalHashNoReadPermission repoInfo) =
    encode GetCausalHashNoReadPermissionTag <> encode repoInfo
  encode GetCausalHashUserNotFound =
    encode GetCausalHashUserNotFoundTag
  encode (GetCausalHashInvalidRepoInfo err repoInfo) =
    encode GetCausalHashInvalidRepoInfoTag <> encode err <> encode repoInfo
  decode = do
    tag <- decode
    case tag of
      GetCausalHashNoReadPermissionTag -> GetCausalHashNoReadPermission <$> decode
      GetCausalHashUserNotFoundTag -> pure GetCausalHashUserNotFound
      GetCausalHashInvalidRepoInfoTag -> GetCausalHashInvalidRepoInfo <$> decode <*> decode

data GetCausalHashErrorTag
  = GetCausalHashNoReadPermissionTag
  | GetCausalHashUserNotFoundTag
  | GetCausalHashInvalidRepoInfoTag
  deriving stock (Show, Eq, Ord)

instance Serialise GetCausalHashErrorTag where
  encode GetCausalHashNoReadPermissionTag = CBOR.encodeWord8 0
  encode GetCausalHashUserNotFoundTag = CBOR.encodeWord8 1
  encode GetCausalHashInvalidRepoInfoTag = CBOR.encodeWord8 2
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure GetCausalHashNoReadPermissionTag
      1 -> pure GetCausalHashUserNotFoundTag
      2 -> pure GetCausalHashInvalidRepoInfoTag
      _ -> fail "invalid tag"

data DownloadEntitiesRequest = DownloadEntitiesRequest
  { causalHash :: HashJWT,
    repoInfo :: SyncV1.RepoInfo,
    knownHashes :: Set Hash32
  }

instance Serialise DownloadEntitiesRequest where
  encode (DownloadEntitiesRequest {causalHash, repoInfo, knownHashes}) =
    encode causalHash <> encode repoInfo <> encode knownHashes
  decode = DownloadEntitiesRequest <$> decode <*> decode <*> decode

-- | Wrapper for CBOR data that has already been serialized.
-- In our case, we use this because we may load pre-serialized CBOR directly from the database,
-- but it's also useful in allowing us to more quickly seek through a CBOR stream, since we only need to decode the CBOR when/if we actually need to use it, and can skip past it using a byte offset otherwise.
--
-- The 't' phantom type is the type of the data encoded in the bytestring.
newtype CBORBytes t = CBORBytes BL.ByteString
  deriving (Serialise) via (BL.ByteString)

-- | Deserialize a 'CBORBytes' value into its tagged type, throwing an error if the deserialization fails.
deserialiseOrFailCBORBytes :: (Serialise t) => CBORBytes t -> Either CBOR.DeserialiseFailure t
deserialiseOrFailCBORBytes (CBORBytes bs) = CBOR.deserialiseOrFail bs

data DownloadEntitiesError
  = DownloadEntitiesNoReadPermission SyncV1.RepoInfo
  | -- | msg, repoInfo
    DownloadEntitiesInvalidRepoInfo Text SyncV1.RepoInfo
  | -- | userHandle
    DownloadEntitiesUserNotFound Text
  | -- | project shorthand
    DownloadEntitiesProjectNotFound Text
  | DownloadEntitiesEntityValidationFailure SyncV1.EntityValidationError
  deriving stock (Eq, Show)

data DownloadEntitiesErrorTag
  = NoReadPermissionTag
  | InvalidRepoInfoTag
  | UserNotFoundTag
  | ProjectNotFoundTag
  | EntityValidationFailureTag
  deriving stock (Eq, Show)

instance Serialise DownloadEntitiesErrorTag where
  encode = \case
    NoReadPermissionTag -> CBOR.encodeWord8 0
    InvalidRepoInfoTag -> CBOR.encodeWord8 1
    UserNotFoundTag -> CBOR.encodeWord8 2
    ProjectNotFoundTag -> CBOR.encodeWord8 3
    EntityValidationFailureTag -> CBOR.encodeWord8 4
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure NoReadPermissionTag
      1 -> pure InvalidRepoInfoTag
      2 -> pure UserNotFoundTag
      3 -> pure ProjectNotFoundTag
      4 -> pure EntityValidationFailureTag
      _ -> fail "invalid tag"

instance Serialise DownloadEntitiesError where
  encode = \case
    DownloadEntitiesNoReadPermission repoInfo -> CBOR.encode NoReadPermissionTag <> CBOR.encode repoInfo
    DownloadEntitiesInvalidRepoInfo msg repoInfo -> CBOR.encode InvalidRepoInfoTag <> CBOR.encode (msg, repoInfo)
    DownloadEntitiesUserNotFound userHandle -> CBOR.encode UserNotFoundTag <> CBOR.encode userHandle
    DownloadEntitiesProjectNotFound projectShorthand -> CBOR.encode ProjectNotFoundTag <> CBOR.encode projectShorthand
    DownloadEntitiesEntityValidationFailure err -> CBOR.encode EntityValidationFailureTag <> CBOR.encode err

  decode = do
    tag <- CBOR.decode
    case tag of
      NoReadPermissionTag -> DownloadEntitiesNoReadPermission <$> CBOR.decode
      InvalidRepoInfoTag -> uncurry DownloadEntitiesInvalidRepoInfo <$> CBOR.decode
      UserNotFoundTag -> DownloadEntitiesUserNotFound <$> CBOR.decode
      ProjectNotFoundTag -> DownloadEntitiesProjectNotFound <$> CBOR.decode
      EntityValidationFailureTag -> DownloadEntitiesEntityValidationFailure <$> CBOR.decode

-- | A chunk of the download entities response stream.
data DownloadEntitiesChunk
  = EntityChunk {hash :: Hash32, entityCBOR :: CBORBytes TempEntity}
  | ErrorChunk {err :: DownloadEntitiesError}

data DownloadEntitiesChunkTag = EntityChunkTag | ErrorChunkTag

instance Serialise DownloadEntitiesChunkTag where
  encode EntityChunkTag = CBOR.encodeWord8 0
  encode ErrorChunkTag = CBOR.encodeWord8 1
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure EntityChunkTag
      1 -> pure ErrorChunkTag
      _ -> fail "invalid tag"

instance Serialise DownloadEntitiesChunk where
  encode (EntityChunk {hash, entityCBOR}) =
    encode EntityChunkTag <> encode hash <> encode entityCBOR
  encode (ErrorChunk {err}) =
    encode ErrorChunkTag <> encode err
  decode = do
    tag <- decode
    case tag of
      EntityChunkTag -> EntityChunk <$> decode <*> decode
      ErrorChunkTag -> ErrorChunk <$> decode

-- TODO
data UploadEntitiesRequest = UploadEntitiesRequest

instance Serialise UploadEntitiesRequest where
  encode _ = mempty
  decode = pure UploadEntitiesRequest

-- | An error occurred while pulling code from Unison Share.
data PullError
  = PullError'DownloadEntities DownloadEntitiesError
  | PullError'GetCausalHash GetCausalHashError
  | PullError'Sync SyncError
  deriving stock (Show)

data SyncError
  = SyncErrorExpectedResultNotInMain CausalHash
  | SyncErrorDeserializationFailure CBOR.DeserialiseFailure
  deriving stock (Show)

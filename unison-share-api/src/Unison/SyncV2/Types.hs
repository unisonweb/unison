module Unison.SyncV2.Types
  ( DownloadEntitiesRequest (..),
    DownloadEntitiesChunk (..),
    SyncError (..),
    DownloadEntitiesError (..),
    CBORBytes (..),
    EntityKind (..),
    serialiseCBORBytes,
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
import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Set (Set)
import Data.Text (Text)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.Server.Orphans ()
import Unison.Share.API.Hash (HashJWT)
import Unison.Sync.Types qualified as SyncV1

newtype BranchRef = BranchRef {unBranchRef :: Text}
  deriving (Serialise, Eq, Show, Ord, ToJSON, FromJSON) via Text

data GetCausalHashErrorTag
  = GetCausalHashNoReadPermissionTag
  | GetCausalHashUserNotFoundTag
  | GetCausalHashInvalidBranchRefTag
  deriving stock (Show, Eq, Ord)

instance Serialise GetCausalHashErrorTag where
  encode GetCausalHashNoReadPermissionTag = CBOR.encodeWord8 0
  encode GetCausalHashUserNotFoundTag = CBOR.encodeWord8 1
  encode GetCausalHashInvalidBranchRefTag = CBOR.encodeWord8 2
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure GetCausalHashNoReadPermissionTag
      1 -> pure GetCausalHashUserNotFoundTag
      2 -> pure GetCausalHashInvalidBranchRefTag
      _ -> fail "invalid tag"

data DownloadEntitiesRequest = DownloadEntitiesRequest
  { causalHash :: HashJWT,
    branchRef :: BranchRef,
    knownHashes :: Set Hash32
  }

instance Serialise DownloadEntitiesRequest where
  encode (DownloadEntitiesRequest {causalHash, branchRef, knownHashes}) =
    encode causalHash <> encode branchRef <> encode knownHashes
  decode = DownloadEntitiesRequest <$> decode <*> decode <*> decode

instance FromJSON DownloadEntitiesRequest where
  parseJSON = withObject "DownloadEntitiesRequest" $ \o -> do
    causalHash <- o .: "causalHash"
    branchRef <- o .: "branchRef"
    knownHashes <- o .: "knownHashes"
    pure DownloadEntitiesRequest {causalHash, branchRef, knownHashes}

instance ToJSON DownloadEntitiesRequest where
  toJSON (DownloadEntitiesRequest {causalHash, branchRef, knownHashes}) =
    object
      [ "causalHash" .= causalHash,
        "branchRef" .= branchRef,
        "knownHashes" .= knownHashes
      ]

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

serialiseCBORBytes :: (Serialise t) => t -> CBORBytes t
serialiseCBORBytes = CBORBytes . CBOR.serialise

data DownloadEntitiesError
  = DownloadEntitiesNoReadPermission BranchRef
  | -- | msg, branchRef
    DownloadEntitiesInvalidBranchRef Text BranchRef
  | -- | userHandle
    DownloadEntitiesUserNotFound Text
  | -- | project shorthand
    DownloadEntitiesProjectNotFound Text
  | DownloadEntitiesEntityValidationFailure SyncV1.EntityValidationError
  deriving stock (Eq, Show)

data DownloadEntitiesErrorTag
  = NoReadPermissionTag
  | InvalidBranchRefTag
  | UserNotFoundTag
  | ProjectNotFoundTag
  | EntityValidationFailureTag
  deriving stock (Eq, Show)

instance Serialise DownloadEntitiesErrorTag where
  encode = \case
    NoReadPermissionTag -> CBOR.encodeWord8 0
    InvalidBranchRefTag -> CBOR.encodeWord8 1
    UserNotFoundTag -> CBOR.encodeWord8 2
    ProjectNotFoundTag -> CBOR.encodeWord8 3
    EntityValidationFailureTag -> CBOR.encodeWord8 4
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure NoReadPermissionTag
      1 -> pure InvalidBranchRefTag
      2 -> pure UserNotFoundTag
      3 -> pure ProjectNotFoundTag
      4 -> pure EntityValidationFailureTag
      _ -> fail "invalid tag"

instance Serialise DownloadEntitiesError where
  encode = \case
    DownloadEntitiesNoReadPermission branchRef -> CBOR.encode NoReadPermissionTag <> CBOR.encode branchRef
    DownloadEntitiesInvalidBranchRef msg branchRef -> CBOR.encode InvalidBranchRefTag <> CBOR.encode (msg, branchRef)
    DownloadEntitiesUserNotFound userHandle -> CBOR.encode UserNotFoundTag <> CBOR.encode userHandle
    DownloadEntitiesProjectNotFound projectShorthand -> CBOR.encode ProjectNotFoundTag <> CBOR.encode projectShorthand
    DownloadEntitiesEntityValidationFailure err -> CBOR.encode EntityValidationFailureTag <> CBOR.encode err

  decode = do
    tag <- CBOR.decode
    case tag of
      NoReadPermissionTag -> DownloadEntitiesNoReadPermission <$> CBOR.decode
      InvalidBranchRefTag -> uncurry DownloadEntitiesInvalidBranchRef <$> CBOR.decode
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
  | PullError'Sync SyncError
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncError
  = SyncErrorExpectedResultNotInMain CausalHash
  | SyncErrorDeserializationFailure CBOR.DeserialiseFailure
  deriving stock (Show)

data EntityKind
  = CausalEntity
  | NamespaceEntity
  | TermEntity
  | TypeEntity
  | PatchEntity
  deriving (Show, Eq, Ord)

instance Serialise EntityKind where
  encode = \case
    CausalEntity -> CBOR.encodeWord8 0
    NamespaceEntity -> CBOR.encodeWord8 1
    TermEntity -> CBOR.encodeWord8 2
    TypeEntity -> CBOR.encodeWord8 3
    PatchEntity -> CBOR.encodeWord8 4
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure CausalEntity
      1 -> pure NamespaceEntity
      2 -> pure TermEntity
      3 -> pure TypeEntity
      4 -> pure PatchEntity
      _ -> fail "invalid tag"

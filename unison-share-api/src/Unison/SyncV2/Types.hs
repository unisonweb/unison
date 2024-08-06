module Unison.SyncV2.Types
  ( GetCausalHashRequest (..),
    GetCausalHashResponse (..),
    DownloadEntitiesRequest (..),
    DownloadEntitiesChunk (..),
    CBORBytes (..),
    UploadEntitiesRequest (..),
    BranchRef (..),
  )
where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
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

data GetCausalHashResponse = GetCausalHashResponse
  { causalHash :: HashJWT
  }

instance Serialise GetCausalHashResponse where
  encode (GetCausalHashResponse {causalHash}) =
    encode causalHash
  decode = GetCausalHashResponse <$> decode

data DownloadEntitiesRequest = DownloadEntitiesRequest
  { causalHash :: HashJWT,
    knownHashes :: [Hash32]
  }

instance Serialise DownloadEntitiesRequest where
  encode (DownloadEntitiesRequest {causalHash, knownHashes}) =
    encode causalHash <> encode knownHashes
  decode = DownloadEntitiesRequest <$> decode <*> decode

-- | Wrapper for CBOR data that has already been serialized.
-- In our case, we use this because we may load pre-serialized CBOR directly from the database,
-- but it's also useful in allowing us to more quickly seek through a CBOR stream, since we only need to decode the CBOR when/if we actually need to use it, and can skip past it using a byte offset otherwise.
newtype CBORBytes = CBORBytes BL.ByteString
  deriving (Serialise) via (BL.ByteString)

-- | A chunk of the download entities response stream.
data DownloadEntitiesChunk
  = EntityChunk {hash :: Hash32, entityCBOR :: CBORBytes}
  | ErrorChunk {err :: SyncV1.DownloadEntitiesError}

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

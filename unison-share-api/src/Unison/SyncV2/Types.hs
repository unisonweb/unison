module Unison.SyncV2.Types
  ( GetCausalHashRequest (..),
    GetCausalHashResponse (..),
    DownloadEntitiesRequest (..),
    UploadEntitiesRequest (..),
    BranchRef (..),
  )
where

import Codec.Serialise (Serialise (..))
import Data.Text (Text)
import Unison.Hash32 (Hash32)
import Unison.Server.Orphans ()
import Unison.Share.API.Hash (HashJWT)

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

data UploadEntitiesRequest = UploadEntitiesRequest
  { branchRef :: BranchRef
  }

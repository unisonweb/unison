{-# LANGUAGE DuplicateRecordFields #-}

module Unison.Sync.Types where

import Unison.Hash
import Unison.Prelude

newtype HashJWT = HashJWT Text

data GetCausalHashForPathRequest = GetCausalHashForPathRequest
  { path :: Text
  }

data GetCausalHashForPathResponse
  = GetCausalHashForPathSuccess HashJWT
  | GetCausalHashForPathErr

data PushForceRequest = PushForceRequest
  { path :: Text,
    oldHash :: Hash,
    newHash :: Hash
  }

data PushForceResponse
  = PushForceSuccess
  | PushForceMissing (Set Hash)

data UploadToRepoRequest = UploadToRepoRequest
  { repo :: Text,
    entities :: [Entity]
  }

data UploadToRepoResponse
  = UploadToRepoNeedDeps (Set Hash)
  | UploadToRepoFailure
  | UploadToRepoSuccess

data DownloadFromRepoRequest = DownloadFromRepoRequest
  { repo :: Text,
    hashJWTs :: [HashJWT]
  }

data DownloadFromRepoResponse
  = NextRefs (Set HashJWT)
  | DownloadFromRepoFailure
  | Failure

data Entity = Entity

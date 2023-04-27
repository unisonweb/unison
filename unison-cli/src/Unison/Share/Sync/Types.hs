-- | Types used by the UCM client during sync.
module Unison.Share.Sync.Types
  ( CheckAndSetPushError (..),
    CodeserverTransportError (..),
    FastForwardPushError (..),
    GetCausalHashByPathError (..),
    PullError (..),
    SyncError (..),
  )
where

import qualified Servant.Client as Servant
import Unison.Prelude
import qualified Unison.Sync.Types as Share

-- | Error used by the client when pushing code to Unison Share.
data CheckAndSetPushError
  = CheckAndSetPushError'UpdatePath
      -- The repo we are pushing to. This is only necessary because an UpdatePathError does not have enough context to
      -- print the entire error message we want to print, but it really should, at which point maybe this can go away.
      Share.RepoInfo
      Share.UpdatePathError
  | CheckAndSetPushError'UploadEntities Share.UploadEntitiesError
  deriving stock (Show)

-- | An error occurred while fast-forward pushing code to Unison Share.
data FastForwardPushError
  = FastForwardPushError'FastForwardPath
      -- The path we are fast forwarding. This is only necessary because a FastForwardPathError does not have enough
      -- context to print the entire error message we want to print, but it really should, at which point maybe this can
      -- go away.
      Share.Path
      Share.FastForwardPathError
  | FastForwardPushError'GetCausalHash GetCausalHashByPathError
  | FastForwardPushError'NotFastForward Share.Path
  | FastForwardPushError'UploadEntities Share.UploadEntitiesError
  deriving stock (Show)

-- | An error occurred while pulling code from Unison Share.
data PullError
  = PullError'DownloadEntities Share.DownloadEntitiesError
  | PullError'GetCausalHash GetCausalHashByPathError
  | PullError'NoHistoryAtPath Share.Path
  deriving stock (Show)

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission Share.Path
  | -- | The repo info was invalid. (err, repoInfo)
    GetCausalHashByPathErrorInvalidRepoInfo Text Share.RepoInfo
  | -- | The user was not found.
    GetCausalHashByPathErrorUserNotFound Share.RepoInfo
  deriving (Show)

-- | Generic Codeserver transport errors
data CodeserverTransportError
  = DecodeFailure Text Servant.Response
  | -- We try to catch permission failures in the endpoint's response type, but if any slip
    -- through they'll be translated as a PermissionDenied.
    PermissionDenied Text
  | RateLimitExceeded
  | Timeout
  | Unauthenticated Servant.BaseUrl
  | UnexpectedResponse Servant.Response
  | UnreachableCodeserver Servant.BaseUrl
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncError e
  = TransportError CodeserverTransportError
  | SyncError e
  deriving stock (Functor, Show)

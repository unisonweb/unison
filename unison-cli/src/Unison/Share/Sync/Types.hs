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

import Data.Set.NonEmpty (NESet)
import qualified Servant.Client as Servant
import Unison.Hash32 (Hash32)
import Unison.Prelude
import qualified Unison.Sync.Types as Share

-- | Error used by the client when pushing code to Unison Share.
data CheckAndSetPushError
  = CheckAndSetPushErrorHashMismatch Share.HashMismatch
  | CheckAndSetPushErrorNoWritePermission Share.Path
  | CheckAndSetPushErrorServerMissingDependencies (NESet Hash32)
  | -- | The repo info was invalid. (err, repoInfo)
    CheckAndSetPushErrorInvalidRepoInfo Text Share.RepoInfo
  | -- | The user was not found.
    CheckAndSetPushErrorUserNotFound Share.Path
  | --  | (projectShortHand)
    CheckAndSetPushErrorProjectNotFound Text
  deriving (Show)

-- | An error occurred while fast-forward pushing code to Unison Share.
data FastForwardPushError
  = FastForwardPushErrorNoHistory Share.Path
  | FastForwardPushErrorNoReadPermission Share.Path
  | FastForwardPushErrorNotFastForward Share.Path
  | FastForwardPushErrorNoWritePermission Share.Path
  | FastForwardPushErrorServerMissingDependencies (NESet Hash32)
  | --                              Parent Child
    FastForwardPushInvalidParentage Hash32 Hash32
  | -- | The repo info was invalid. (err, repoInfo)
    FastForwardPushErrorInvalidRepoInfo Text Share.RepoInfo
  | FastForwardPushErrorUserNotFound Share.Path
  | --  | (projectShortHand)
    FastForwardPushErrorProjectNotFound Text
  deriving (Show)

-- | An error occurred while pulling code from Unison Share.
data PullError
  = PullErrorNoHistoryAtPath Share.Path
  | PullErrorNoReadPermission Share.Path
  | -- | The repo info was invalid. (err, repoInfo)
    PullErrorInvalidRepoInfo Text Share.RepoInfo
  | PullErrorUserNotFound Share.Path
  | -- | (projectShortHand)
    PullErrorProjectNotFound Text
  deriving (Show)

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission Share.Path
  | -- | The repo info was invalid. (err, repoInfo)
    GetCausalHashByPathErrorInvalidRepoInfo Text Share.RepoInfo
  | -- | The user was not found.
    GetCausalHashByPathErrorUserNotFound Share.Path
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

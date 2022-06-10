-- | Types used by the UCM client during sync.
module Unison.Share.Sync.Types where

import Data.Set.NonEmpty (NESet)
import U.Util.Hash32 (Hash32)
import qualified Unison.Sync.Types as Share

-- | Error used by the client when pushing code to Unison Share.
data CheckAndSetPushError
  = CheckAndSetPushErrorHashMismatch Share.HashMismatch
  | CheckAndSetPushErrorNoWritePermission Share.Path
  | CheckAndSetPushErrorServerMissingDependencies (NESet Hash32)

-- | An error occurred while fast-forward pushing code to Unison Share.
data FastForwardPushError
  = FastForwardPushErrorNoHistory Share.Path
  | FastForwardPushErrorNoReadPermission Share.Path
  | FastForwardPushErrorNotFastForward Share.Path
  | FastForwardPushErrorNoWritePermission Share.Path
  | FastForwardPushErrorServerMissingDependencies (NESet Hash32)
  | --                              Parent Child
    FastForwardPushInvalidParentage Hash32 Hash32

-- | An error occurred while pulling code from Unison Share.
data PullError
  = -- | An error occurred while resolving a repo+path to a causal hash.
    PullErrorGetCausalHashByPath GetCausalHashByPathError
  | PullErrorNoHistoryAtPath Share.Path

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission Share.Path

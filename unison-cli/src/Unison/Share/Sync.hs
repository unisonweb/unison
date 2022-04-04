module Unison.Share.Sync where

import U.Codebase.HashTags (CausalHash (unCausalHash))
import U.Codebase.Sqlite.Causal (DbCausal, GDbCausal (..))
import qualified U.Codebase.Sqlite.Causal as Sqlite.Causal (GDbCausal (..))
import U.Codebase.Sqlite.DbId (CausalHashId (..), HashId)
import qualified U.Util.Base32Hex as Base32Hex
import qualified U.Util.Hash as Hash
import Unison.Prelude
import qualified Unison.Sync.Types as Share

data UpdatePathResponse
  = UpdatePathSuccess
  | UpdatePathHashMismatch Share.HashMismatch
  | UpdatePathMissingDependencies (Share.NeedDependencies Share.Hash)

-- deriving stock (Show, Eq, Ord, Generic)

__api_updatePath :: Share.UpdatePathRequest -> IO UpdatePathResponse
__api_updatePath = undefined

-- Push
--
-- 1. Update path
-- 2. Possibly do some upload entities
--
-- I can communicate with my fingers
--

data PushError
  = PushErrorHashMismatch Share.HashMismatch

-- Option 1: have push be itself in the Transaction monad, use unsafePerformIdempotentIO
-- fuction to do the interleaved IO calls (http, etc)
--
--   push :: RepoPath -> ... -> Transaction (Either PushError ())
--   push = do
--     unsafePerformIdempotentIO (updatePath ...)
--
-- Option 2: have push "go around" the Transaction abstraction by beginning/commiting explicitly,
-- and immediately un-Transaction-newtyping the low-level calls like loadHashId
--
--   push :: Connection -> RepoPath -> ... -> IO (Either PushError ())
--   push conn = do
--     let foo transaction = unsafeUnTransaction transaction conn
--
--     ...
--     result <- foo (loadHashId hashId)
--     ...
--
-- newtype Transaction a = Transaction { unsafeUnTransaction :: Connection -> IO a }

type Connection = ()

type Transaction a = ()

expectHash :: HashId -> Transaction Hash.Hash
expectHash = undefined

push :: Connection -> Share.RepoPath -> Maybe Share.Hash -> CausalHash -> IO (Either PushError ())
push conn repoPath expectedHash causalHash = do
  -- 1. Attempt to update path.

  -- causalHash <-
  --   _ (unCausalHashId (Sqlite.Causal.selfHash causal))
  let request =
        Share.UpdatePathRequest
          { path = repoPath,
            expectedHash =
              expectedHash <&> \hash ->
                Share.TypedHash
                  { hash,
                    entityType = Share.CausalType
                  },
            newHash =
              Share.TypedHash
                { hash =
                    causalHash
                      & unCausalHash
                      & Hash.toBase32Hex
                      & Base32Hex.toText
                      & Share.Hash,
                  entityType = Share.CausalType
                }
          }
  __api_updatePath request >>= \case
    UpdatePathSuccess -> pure (Right ())
    UpdatePathHashMismatch mismatch -> pure (Left (PushErrorHashMismatch mismatch))
    UpdatePathMissingDependencies dependencies -> undefined
  undefined

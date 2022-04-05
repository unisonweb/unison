module Unison.Share.Sync
  ( push,
    PushError (..),
  )
where

import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import U.Codebase.HashTags (CausalHash (unCausalHash))
import U.Codebase.Sqlite.Causal (DbCausal, GDbCausal (..))
import qualified U.Codebase.Sqlite.Causal as Sqlite.Causal (GDbCausal (..))
import U.Codebase.Sqlite.DbId (CausalHashId (..), HashId)
import qualified U.Util.Base32Hex as Base32Hex
import qualified U.Util.Hash as Hash
import Unison.Prelude
import qualified Unison.Sync.Types as Share
import qualified Unison.Sync.Types as Share.RepoPath (RepoPath (..))

data UpdatePathResponse
  = UpdatePathSuccess
  | UpdatePathHashMismatch Share.HashMismatch
  | UpdatePathMissingDependencies (Share.NeedDependencies Share.Hash)

data UploadEntitiesResponse
  = UploadEntitiesSuccess
  | UploadEntitiesNeedDependencies (Share.NeedDependencies Share.Hash)

-- deriving stock (Show, Eq, Ord, Generic)

updatePath :: Share.UpdatePathRequest -> IO UpdatePathResponse
updatePath = undefined

-- Push
--
-- 1. Update path
-- 2. Possibly do some upload entities
--
-- I can communicate with my fingers
--

data PushError
  = PushErrorServerMissingDependencies (NESet Share.Hash)
  | PushErrorHashMismatch Share.HashMismatch

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
  updatePath request >>= \case
    UpdatePathSuccess -> pure (Right ())
    UpdatePathHashMismatch mismatch -> pure (Left (PushErrorHashMismatch mismatch))
    UpdatePathMissingDependencies (Share.NeedDependencies dependencies) -> do
      upload conn (Share.RepoPath.repoName repoPath) dependencies
      updatePath request <&> \case
        UpdatePathSuccess -> Right ()
        UpdatePathHashMismatch mismatch -> Left (PushErrorHashMismatch mismatch)
        UpdatePathMissingDependencies (Share.NeedDependencies dependencies) ->
          Left (PushErrorServerMissingDependencies dependencies)
  where
    request =
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

-- { repoName :: RepoName,
--   entities :: NEMap Hash (Entity Text Hash Hash)
-- }
upload :: Connection -> Share.RepoName -> NESet Share.Hash -> IO ()
upload conn repoName dependencies = do
  -- 1. Resolve each Hash to Entity
  request <- do
    entities <-
      NEMap.fromAscList <$> traverse (\dep -> (dep,) <$> resolveHashToEntity conn dep) (NESet.toAscList dependencies)
    pure Share.UploadEntitiesRequest {repoName, entities}

  -- 2. Perform upload HTTP call

  -- 3. If UploadEntitiesMissingDependencies, recur

  undefined

-- FIXME rename, etc
resolveHashToEntity :: Connection -> Share.Hash -> IO (Share.Entity Text Share.Hash Share.Hash)
resolveHashToEntity = undefined

-- let loop :: Set Share.Hash -> IO ()
--     loop dependencies0 =
--       case Set.minView dependencies0 of
--         Nothing -> pure ()
--         Just (dependency, dependencies) -> do
--           undefined
--  in loop (NESet.toSet dependencies1)

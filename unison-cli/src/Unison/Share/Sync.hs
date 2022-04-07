module Unison.Share.Sync
  ( -- * Get causal hash by path
    getCausalHashByPath,
    GetCausalHashByPathError (..),

    -- * Push
    push,
    PushError (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
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
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Set as Set

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission

-- | Get the causal hash of a path hosted on Unison Share.
getCausalHashByPath :: Share.RepoPath -> IO (Either GetCausalHashByPathError (Maybe Share.HashJWT))
getCausalHashByPath repoPath =
  _getCausalHashByPath (Share.GetCausalHashByPathRequest repoPath) <&> \case
    GetCausalHashByPathSuccess hashJwt -> Right (Just hashJwt)
    GetCausalHashByPathEmpty -> Right Nothing
    GetCausalHashByPathNoReadPermission -> Left GetCausalHashByPathErrorNoReadPermission

------------------------------------------------------------------------------------------------------------------------
-- Push

-- | An error occurred while pushing code to Unison Share.
data PushError
  = PushErrorServerMissingDependencies (NESet Share.Hash)
  | PushErrorHashMismatch Share.HashMismatch

-- | Push a causal to Unison Share.
push ::
  -- | SQLite connection, for reading entities to push.
  Connection ->
  -- | The repo+path to push to.
  Share.RepoPath ->
  -- | The hash that we expect this repo+path to be at on Unison Share. If not, we'll get back a hash mismatch error.
  -- This prevents accidentally pushing over data that we didn't know was there.
  Maybe Share.Hash ->
  -- | The hash of our local causal to push.
  CausalHash ->
  IO (Either PushError ())
push conn repoPath expectedHash causalHash = do
  let theUpdatePathRequest :: Share.UpdatePathRequest
      theUpdatePathRequest =
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

  -- Maybe the server already has this causal; try just setting its remote path. Commonly, it will respond that it needs
  -- this causal (UpdatePathMissingDependencies).
  _updatePath theUpdatePathRequest >>= \case
    UpdatePathSuccess -> pure (Right ())
    UpdatePathHashMismatch mismatch -> pure (Left (PushErrorHashMismatch mismatch))
    UpdatePathMissingDependencies (Share.NeedDependencies dependencies) -> do
      -- Upload the causal and all of its dependencies.
      upload conn (Share.RepoPath.repoName repoPath) dependencies

      -- After uploading the causal and all of its dependencies, try setting the remote path again.
      _updatePath theUpdatePathRequest <&> \case
        UpdatePathSuccess -> Right ()
        -- Between the initial updatePath attempt and this one, someone else managed to update the path. That's ok; we
        -- still managed to upload our causal, but the push has indeed failed overall.
        UpdatePathHashMismatch mismatch -> Left (PushErrorHashMismatch mismatch)
        -- Unexpected, but possible: we thought we uploaded all we needed to, yet the server still won't accept our
        -- causal. Bug in the client because we didn't upload enough? Bug in the server because we weren't told to
        -- upload some dependency? Who knows.
        UpdatePathMissingDependencies (Share.NeedDependencies dependencies) ->
          Left (PushErrorServerMissingDependencies dependencies)

upload :: Connection -> Share.RepoName -> NESet Share.Hash -> IO ()
upload conn repoName =
  loop
  where
    loop :: NESet Share.Hash -> IO ()
    loop (NESet.toAscList -> hashes) = do
      -- Get each entity that the server is missing out of the database.
      entities <- traverse (resolveHashToEntity conn) hashes

      let theUploadEntitiesRequest :: Share.UploadEntitiesRequest
          theUploadEntitiesRequest =
            Share.UploadEntitiesRequest
              { entities = NEMap.fromAscList (List.NonEmpty.zip hashes entities),
                repoName
              }

      -- Upload all of the entities we know the server needs, and if the server responds that it needs yet more, loop to
      -- upload those too.
      _uploadEntities theUploadEntitiesRequest >>= \case
        UploadEntitiesNeedDependencies (Share.NeedDependencies moreHashes) -> loop moreHashes
        UploadEntitiesSuccess -> pure ()

------------------------------------------------------------------------------------------------------------------------
-- Pull

pull :: Connection -> Share.RepoPath -> IO (Either PullError CausalHash)
pull _conn _repoPath = undefined

download :: Connection -> Share.RepoName -> NESet Share.HashJWT -> IO ()
download conn repoName =
  let loop :: NESet Share.HashJWT -> IO ()
      loop hashes0 = do
        let elaborateHashes :: Set Share.HashJWT -> Set Share.HashJWT -> IO (Maybe (NESet Share.HashJWT))
            elaborateHashes hashes outputs =
              case Set.minView hashes of
                Nothing -> pure (NESet.nonEmptySet outputs)
                Just (hash, hashes') ->
                  let inMainStorage = undefined
                      inTempStorage = undefined
                      directDepsOf = undefined
                   in inMainStorage hash >>= \case
                        False ->
                          inTempStorage hash >>= \case
                            False -> elaborateHashes hashes' (Set.insert hash outputs)
                            True -> elaborateHashes (Set.union (directDepsOf hash) hashes') outputs
                        True -> elaborateHashes hashes' outputs

        elaborateHashes (NESet.toSet hashes0) Set.empty >>= \case
          Nothing -> pure ()
          Just hashes1 -> do
            Share.DownloadEntitiesResponse entities <-
              _downloadEntities
                Share.DownloadEntitiesRequest
                  { repoName,
                    hashes = hashes1
                  }

            missingDependencies0 <-
              NEMap.toList entities & foldMapM \(hash, entity) -> do
                let inMainStorage = undefined
                let inTempStorage = undefined
                let putInMainStorage hash entity = undefined
                let putInTempStorage hash entity = undefined
                let insertMissingDependencies = undefined
                -- select dependency
                -- from temp_entity_missing_dependency
                -- where dependent = <this entity>
                let getTempEntityMissingDependencies = undefined
                let directDepsOf :: Share.Entity Text Share.Hash Share.HashJWT -> Set Share.HashJWT
                    directDepsOf = undefined

                inMainStorage hash >>= \case
                  True -> pure Set.empty
                  False ->
                    inTempStorage entity >>= \case
                      True -> getTempEntityMissingDependencies entity
                      False -> do
                        missingDependencies <- Set.filterM inMainStorage (directDepsOf entity)
                        if Set.null missingDependencies
                          then putInMainStorage hash entity
                          else do
                            putInTempStorage hash entity
                            insertMissingDependencies hash missingDependencies
                        pure missingDependencies

            case NESet.nonEmptySet missingDependencies0 of
              Nothing -> pure ()
              Just missingDependencies -> loop missingDependencies
   in loop

-- Do this at the top of the procedure.
--
-- deps0 = hashes we kinda-maybe think we should request
-- deps1 = hashes we will request
--
--   frobnicate : Set Hash -> Set Hash ->{IO} Set Hash
--   frobnicate deps0 deps1 =
--     case deps0 of
--       Nothing -> deps1
--       Just (dep0, deps0) ->
--         cases
--           inMainStorage dep0 -> frobnicate deps0 deps1
--           inTempStorage dep0 -> frobnicate (deps0 + directDepsOf dep0) deps1
--           otherwise          -> frobnicate deps0 (deps1 + {dep0})
--
-- If we just got #thing from the server,
--   If we already have the entity in the main database, we're done.
--     - This should't happen, why would the server have sent us this?
--
--   Otherwise, if we already have the entity in temp_entity,
--     1. Add to our work queue requesting all of its deps that we don't have in main storage
--
--   Otherwise (if we don't have it at all),
--     1. Deserialize blob and extract dependencies #dep1, #dep2, #dep3 from #thing blob.
--     2. If the {set of dependencies we don't have in the object/causal table} is empty, then store in object/causal.
--     3. Otherwise,
--         - Insert into temp_entity
--         - For each #dependency in the {set of dependencies we don't have in the object/causal table}
--             insert each (#thing, #dependency) into temp_entity_missing_dependency
--         - Add to our work queue requesting {set of dependencies we don't have in object/causal}
--
--  Note: beef up insert_entity procedure to flush temp_entity table
--    1. When inserting object #foo,
--        look up all dependents of #foo in
--        temp_entity_missing_dependency table (say #bar, #baz).
--    2. Delete (#bar, #foo) and (#baz, #foo) from temp_entity_missing_dependency.
--    3. Delete #foo from temp_entity (if it's there)
--    4. For each like #bar and #baz with no more rows in temp_entity_missing_dependency,
--        insert_entity them.

------------------------------------------------------------------------------------------------------------------------
--

data UploadEntitiesResponse
  = UploadEntitiesSuccess
  | UploadEntitiesNeedDependencies (Share.NeedDependencies Share.Hash)

data PullError

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

-- FIXME rename, etc
resolveHashToEntity :: Connection -> Share.Hash -> IO (Share.Entity Text Share.Hash Share.Hash)
resolveHashToEntity = undefined

------------------------------------------------------------------------------------------------------------------------
-- TODO these things come from servant-client / api types module(s)

data GetCausalHashByPathResponse
  = GetCausalHashByPathSuccess Share.HashJWT
  | GetCausalHashByPathEmpty
  | GetCausalHashByPathNoReadPermission

data UpdatePathResponse
  = UpdatePathSuccess
  | UpdatePathHashMismatch Share.HashMismatch
  | UpdatePathMissingDependencies (Share.NeedDependencies Share.Hash)

_getCausalHashByPath :: Share.GetCausalHashByPathRequest -> IO GetCausalHashByPathResponse
_getCausalHashByPath = undefined

_updatePath :: Share.UpdatePathRequest -> IO UpdatePathResponse
_updatePath = undefined

_downloadEntities :: Share.DownloadEntitiesRequest -> IO Share.DownloadEntitiesResponse
_downloadEntities = undefined

_uploadEntities :: Share.UploadEntitiesRequest -> IO UploadEntitiesResponse
_uploadEntities = undefined

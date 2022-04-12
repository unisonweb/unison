module Unison.Share.Sync
  ( -- * Get causal hash by path
    getCausalHashByPath,
    GetCausalHashByPathError (..),

    -- * Push
    push,
    PushError (..),
  )
where

import qualified Control.Lens as Lens
import Control.Monad.Extra ((||^))
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Bitraversable (bitraverse)
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector as Vector
import U.Codebase.HashTags (CausalHash (unCausalHash))
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Causal as Causal
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId, HashId, ObjectId)
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import U.Codebase.Sqlite.LocalIds (LocalIds' (..))
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Hash as Hash
import Unison.Prelude
import qualified Unison.Sync.Types as Share
import qualified Unison.Sync.Types as Share.LocalIds (LocalIds (..))
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

-- Upload a set of entities to Unison Share. If the server responds that it cannot yet store any hash(es) due to missing
-- dependencies, send those dependencies too, and on and on, until the server stops responding that it's missing
-- anything.
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

decodedHashJWTHash :: Share.DecodedHashJWT -> Share.Hash
decodedHashJWTHash = undefined

decodeHashJWT :: Share.HashJWT -> Share.DecodedHashJWT
decodeHashJWT = undefined

hashJWTHash :: Share.HashJWT -> Base32Hex
hashJWTHash =
  Share.toBase32Hex . decodedHashJWTHash . decodeHashJWT

-- Download a set of entities from Unison Share.
download :: Connection -> Share.RepoName -> NESet Share.HashJWT -> IO ()
download conn repoName = do
  let runDB :: ReaderT Connection IO a -> IO a
      runDB action = runReaderT action conn
  let directDepsOfEntity :: Share.Entity Text Share.Hash Share.HashJWT -> Set Share.DecodedHashJWT
      directDepsOfEntity =
        Set.map decodeHashJWT . \case
          Share.TC (Share.TermComponent terms) -> flip foldMap terms \(localIds, _term) ->
            Set.fromList (Share.LocalIds.hashes localIds)
          Share.DC (Share.DeclComponent terms) -> flip foldMap terms \(localIds, _term) ->
            Set.fromList (Share.LocalIds.hashes localIds)
          Share.P (Share.Patch {newHashLookup}) ->
            Set.fromList newHashLookup
          Share.N (Share.Namespace {defnLookup, patchLookup, childLookup}) ->
            Set.fromList defnLookup <> Set.fromList patchLookup <> Set.fromList childLookup
          Share.C (Share.Causal {parents}) -> parents
  {-
  let directDepsOfEntity2 :: Share.Entity Text Share.Hash Share.HashJWT -> Set Share.DecodedHashJWT
      directDepsOfEntity2 =
        Lens.setOf (typed @Share.HashJWT) -}

  let loop :: NESet Share.DecodedHashJWT -> IO ()
      loop hashes0 = do
        runDB (elaborateHashes (NESet.toSet hashes0) Set.empty) >>= \case
          Nothing -> pure ()
          Just hashes1 -> do
            Share.DownloadEntitiesResponse entities <-
              _downloadEntities
                Share.DownloadEntitiesRequest
                  { repoName,
                    hashes = hashes1
                  }

            missingDependencies0 <-
              runDB do
                NEMap.toList entities & foldMapM \(hash, entity) -> do
                  -- still trying to figure out missing dependencies of hash/entity.
                  entityExists hash >>= \case
                    True -> pure Set.empty
                    False ->
                      Q.getMissingDependencyJwtsForTempEntity (Share.toBase32Hex hash) >>= \case
                        Just missingDependencies ->
                          -- already in temp storage, due to missing dependencies
                          pure (Set.map (decodeHashJWT . Share.HashJWT) (NESet.toSet missingDependencies))
                        Nothing -> do
                          -- not in temp storage.
                          -- if it has missing dependencies, add it to temp storage;
                          -- otherwise add it to main storage.
                          missingDependencies0 <-
                            Set.filterM (entityExists . decodedHashJWTHash) (directDepsOfEntity entity)
                          case NESet.nonEmptySet missingDependencies0 of
                            Nothing -> insertEntity hash entity
                            Just missingDependencies -> insertTempEntity hash entity missingDependencies
                          pure missingDependencies0

            case NESet.nonEmptySet missingDependencies0 of
              Nothing -> pure ()
              Just missingDependencies -> loop missingDependencies
   in loop . NESet.map decodeHashJWT

---------

-- * we need hashjwts to make subsequent requests to the server

-- * when look up missing dependencies, it's because we anticipate making a subsequent request to the server for them,

--   so they should also be hashjwts

-- * before making a subsequent request to the server, we elaborate the request set,

--   which requires knowing hashjwts for the dependencies of the request set;
--   so we need some way of looking up missing dependency hashjwts from a hash or hashjwt

--    * one way of looking these up would be to include dependency hashjwts in the temp-entity-missing-dependency table
--        (dependent -> (dependency, dependencyjwt))

-- * we need `hash` to find entity in temp or main storage

-- * different entities may arrive with different variations on the same dependency jwts

-- * we need dependency hash (not only hashjwt) in temp-entity-missing-dependency so that we can also look up dependents

--   of a hash without knowing which hashjwt was stored for it

-- Mitchell is on team: add a column to temp-entity-missing-dependency that includes the jwt

{-
server sqlite db
  -> sqlite object bytes
  -> U.Codebase.Sqlite.decomposedComponent [(LocalIds' TextId ObjectId, ByteString)]
  -> Sync.Types.Entity.TermComponent
  -> cbor bytes
  -> network
  -> cbor bytes
  -> Sync.Types.Entity.TermComponent
      |-> temp_entity_missing_dependencies
      |
      |-> U.Codebase.Sqlite.decomposedComponent [(LocalIds' Text HashJWT, ByteString)]
                                                (not Unison.Sync.Types.LocallyIndexedComponent)
          -> serialize -> temp_entity (bytes)
          -> time to move to MAIN table!!!!
          -> deserialize -> U.Codebase.Sqlite.decomposedComponent [(LocalIds' Text HashJWT, ByteString)]
          -> traverse -> U.Codebase.Sqlite.decomposedComponent [(LocalIds' TextId ObjectId, ByteString)]
          -> serialize -> sqlite object bytes

-- if we just have a hash for the localids (as opposed to a TypedHash)

-}

---------
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

-- have to convert from Entity format to TempEntity format (`makeTempEntity` on 414)

-- also have to convert from TempEntity format to Sync format — this means exchanging Text for TextId and `Base32Hex`es for `HashId`s and/or `ObjectId`s
tempToSyncTermComponent :: Connection -> TempEntity.TempTermFormat -> IO TermFormat.SyncTermFormat
tempToSyncTermComponent conn =
  flip runReaderT conn . \case
    TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms) ->
      TermFormat.SyncTerm . TermFormat.SyncLocallyIndexedComponent
        <$> Lens.traverseOf (traverse . Lens._1) (bitraverse Q.saveText expectObjectIdForHashJWT) terms

expectObjectIdForHashJWT :: Q.DB m => TempEntity.HashJWT -> m ObjectId
expectObjectIdForHashJWT hashJwt = do
  hashId <- throwExceptT (Q.expectHashIdByHash (Hash.fromBase32Hex (hashJWTHash (Share.HashJWT hashJwt))))
  -- FIXME mitchell: should this be "for primary hash id" or "for any hash id"?
  throwExceptT (Q.expectObjectIdForAnyHashId hashId)

-- Serialization.recomposeComponent :: MonadPut m => [(LocalIds, BS.ByteString)] -> m ()
-- Serialization.recomposePatchFormat :: MonadPut m => PatchFormat.SyncPatchFormat -> m ()
-- Serialization.recomposeBranchFormat :: MonadPut m => BranchFormat.SyncBranchFormat -> m ()
-- Q.saveObject :: DB m => HashId -> ObjectType -> ByteString -> m ObjectId

tempToSyncDeclComponent :: TempEntity.TempDeclFormat -> IO DeclFormat.SyncDeclFormat
tempToSyncDeclComponent = do
  undefined

tempToSyncPatch :: TempEntity.TempPatchFormat -> IO PatchFormat.SyncPatchFormat
tempToSyncPatch = do
  undefined

tempToSyncNamespace :: TempEntity.TempNamespaceFormat -> IO NamespaceFormat.SyncBranchFormat
tempToSyncNamespace = do
  undefined

tempToSyncCausal :: TempEntity.TempCausalFormat -> IO (Causal.SyncCausalFormat' CausalHashId BranchHashId) -- could probably use a better type name here
tempToSyncCausal = do
  undefined

-- Q.saveCausalHash :: DB m => CausalHash -> m CausalHashId -- only affects `hash` table
-- Q.saveCausal :: DB m => CausalHashId -> BranchHashId -> m ()
-- Q.saveCausalParents :: DB m => CausalHashId -> [CausalHashId] -> m ()

------------------------------------------------------------------------------------------------------------------------
-- Database operations

-- | Where is an entity stored?
data EntityLocation
  = -- | `object` / `causal`
    EntityInMainStorage
  | -- | `temp_entity`
    EntityInTempStorage
  | -- | Nowhere
    EntityNotStored

-- | Does this entity already exist in the database, i.e. in the `object` or `causal` table?
entityExists :: Q.DB m => Share.Hash -> m Bool
entityExists (Share.Hash b32) = do
  -- first get hashId if exists
  Q.loadHashId b32 >>= \case
    Nothing -> pure False
    -- then check if is causal hash or if object exists for hash id
    Just hashId -> Q.isCausalHash hashId ||^ Q.isObjectHash hashId

-- | Does this entity already exist in the `temp_entity` table?
tempEntityExists :: Q.DB m => Share.Hash -> m Bool
tempEntityExists (Share.Hash b32) =
  Q.tempEntityExists b32

-- | Where is an entity stored?
entityLocation :: Q.DB m => Share.Hash -> m EntityLocation
entityLocation hash =
  entityExists hash >>= \case
    True -> pure EntityInMainStorage
    False ->
      tempEntityExists hash >>= \case
        True -> pure EntityInTempStorage
        False -> pure EntityNotStored

-- FIXME comment
elaborateHashes :: forall m. Q.DB m => Set Share.DecodedHashJWT -> Set Share.HashJWT -> m (Maybe (NESet Share.HashJWT))
elaborateHashes hashes outputs =
  case Set.minView hashes of
    Nothing -> pure (NESet.nonEmptySet outputs)
    Just (Share.DecodedHashJWT (Share.HashJWTClaims {hash}) jwt, hashes') ->
      entityLocation hash >>= \case
        EntityNotStored -> elaborateHashes hashes' (Set.insert jwt outputs)
        EntityInTempStorage -> do
          deps <- directDepsOfHash hash
          elaborateHashes (Set.union deps hashes') outputs
        EntityInMainStorage -> elaborateHashes hashes' outputs
  where
    directDepsOfHash :: Share.Hash -> m (Set Share.DecodedHashJWT)
    directDepsOfHash (Share.Hash b32) = do
      maybeJwts <- Q.getMissingDependencyJwtsForTempEntity b32
      let decode = decodeHashJWT . Share.HashJWT
      pure (maybe Set.empty (Set.map decode . NESet.toSet) maybeJwts)

insertEntity :: Q.DB m => Share.Hash -> Share.Entity Text Share.Hash Share.HashJWT -> m ()
insertEntity _hash = undefined

-- | Insert an entity and its missing dependencies.
insertTempEntity ::
  Q.DB m =>
  Share.Hash ->
  Share.Entity Text Share.Hash Share.HashJWT ->
  NESet Share.DecodedHashJWT ->
  m ()
insertTempEntity hash entity missingDependencies =
  Q.insertTempEntity
    (Share.toBase32Hex hash)
    (entityToTempEntity entity)
    ( NESet.map
        ( \Share.DecodedHashJWT {claims = Share.HashJWTClaims {hash}, hashJWT} ->
            (Share.toBase32Hex hash, Share.unHashJWT hashJWT)
        )
        missingDependencies
    )

entityToTempEntity :: Share.Entity Text Share.Hash Share.HashJWT -> TempEntity
entityToTempEntity = \case
  Share.TC (Share.TermComponent terms) ->
    terms
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & TermFormat.SyncLocallyIndexedComponent
      & TermFormat.SyncTerm
      & TempEntity.TC
  Share.DC _ -> undefined -- (TempEntity.DC _)
  Share.P _ -> undefined -- (TempEntity.P _)
  Share.N _ -> undefined -- (TempEntity.N _)
  Share.C _ -> undefined -- (TempEntity.C _)
  where
    mungeLocalIds :: Share.LocalIds Text Share.HashJWT -> LocalIds' Text TempEntity.HashJWT
    mungeLocalIds Share.LocalIds {texts, hashes} =
      LocalIds
        { textLookup = Vector.fromList texts,
          defnLookup = Vector.map Share.unHashJWT (Vector.fromList hashes)
        }

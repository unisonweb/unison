module Unison.Share.Sync
  ( -- * Get causal hash by path
    getCausalHashByPath,
    GetCausalHashByPathError (..),

    -- * Push
    push,
    PushError (..),

    -- * Pull
    pull,
    PullError (..),
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
import Servant.Client (BaseUrl)
import U.Codebase.HashTags (CausalHash (..))
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Causal as Causal
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId, ObjectId)
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import U.Codebase.Sqlite.LocalIds (LocalIds' (..))
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Util.Base32Hex (Base32Hex)
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import Unison.Auth.HTTPClient (AuthorizedHttpClient)
import Unison.Prelude
import qualified Unison.Sync.HTTP as Share (downloadEntitiesHandler, updatePathHandler, uploadEntitiesHandler)
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
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
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
push httpClient unisonShareUrl conn repoPath expectedHash causalHash = do
  -- Maybe the server already has this causal; try just setting its remote path. Commonly, it will respond that it needs
  -- this causal (UpdatePathMissingDependencies).
  updatePath >>= \case
    Share.UpdatePathSuccess -> pure (Right ())
    Share.UpdatePathHashMismatch mismatch -> pure (Left (PushErrorHashMismatch mismatch))
    Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) -> do
      -- Upload the causal and all of its dependencies.
      upload httpClient unisonShareUrl conn (Share.RepoPath.repoName repoPath) dependencies

      -- After uploading the causal and all of its dependencies, try setting the remote path again.
      updatePath <&> \case
        Share.UpdatePathSuccess -> Right ()
        -- Between the initial updatePath attempt and this one, someone else managed to update the path. That's ok; we
        -- still managed to upload our causal, but the push has indeed failed overall.
        Share.UpdatePathHashMismatch mismatch -> Left (PushErrorHashMismatch mismatch)
        -- Unexpected, but possible: we thought we uploaded all we needed to, yet the server still won't accept our
        -- causal. Bug in the client because we didn't upload enough? Bug in the server because we weren't told to
        -- upload some dependency? Who knows.
        Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) ->
          Left (PushErrorServerMissingDependencies dependencies)
  where
    updatePath :: IO Share.UpdatePathResponse
    updatePath =
      Share.updatePathHandler
        httpClient
        unisonShareUrl
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

-- Upload a set of entities to Unison Share. If the server responds that it cannot yet store any hash(es) due to missing
-- dependencies, send those dependencies too, and on and on, until the server stops responding that it's missing
-- anything.
upload ::
  AuthorizedHttpClient ->
  BaseUrl ->
  Connection ->
  Share.RepoName ->
  NESet Share.Hash ->
  IO ()
upload httpClient unisonShareUrl conn repoName =
  loop
  where
    loop :: NESet Share.Hash -> IO ()
    loop (NESet.toAscList -> hashes) = do
      -- Get each entity that the server is missing out of the database.
      entities <- traverse (resolveHashToEntity conn) hashes

      let uploadEntities :: IO Share.UploadEntitiesResponse
          uploadEntities =
            Share.uploadEntitiesHandler
              httpClient
              unisonShareUrl
              Share.UploadEntitiesRequest
                { entities = NEMap.fromAscList (List.NonEmpty.zip hashes entities),
                  repoName
                }

      -- Upload all of the entities we know the server needs, and if the server responds that it needs yet more, loop to
      -- upload those too.
      uploadEntities >>= \case
        Share.UploadEntitiesNeedDependencies (Share.NeedDependencies moreHashes) -> loop moreHashes
        Share.UploadEntitiesSuccess -> pure ()

------------------------------------------------------------------------------------------------------------------------
-- Pull

-- | An error occurred while pulling code from Unison Share.
data PullError
  = -- | An error occurred while resolving a repo+path to a causal hash.
    PullErrorGetCausalHashByPath GetCausalHashByPathError

pull ::
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite connection, for storing entities we pull.
  Connection ->
  -- | The repo+path to pull from.
  Share.RepoPath ->
  IO (Either PullError (Maybe CausalHash))
pull httpClient unisonShareUrl conn repoPath = do
  getCausalHashByPath repoPath >>= \case
    Left err -> pure (Left (PullErrorGetCausalHashByPath err))
    -- There's nothing at the remote path, so there's no causal to pull.
    Right Nothing -> pure (Right Nothing)
    Right (Just hashJwt) -> do
      let hash = Share.hashJWTHash hashJwt
      runDB (entityLocation hash) >>= \case
        EntityInMainStorage -> pure ()
        EntityInTempStorage missingDependencies -> doDownload missingDependencies
        EntityNotStored -> doDownload (NESet.singleton hashJwt)
      pure (Right (Just (CausalHash (Hash.fromBase32Hex (Share.toBase32Hex hash)))))
  where
    runDB :: ReaderT Connection IO a -> IO a
    runDB action = runReaderT action conn

    doDownload :: NESet Share.HashJWT -> IO ()
    doDownload =
      download httpClient unisonShareUrl conn (Share.RepoPath.repoName repoPath)

-- Download a set of entities from Unison Share.
download ::
  AuthorizedHttpClient ->
  BaseUrl ->
  Connection ->
  Share.RepoName ->
  -- FIXME mitchell: less decoding if this is a DecodedHashJWT
  NESet Share.HashJWT ->
  IO ()
download httpClient unisonShareUrl conn repoName = do
  let runDB :: ReaderT Connection IO a -> IO a
      runDB action = runReaderT action conn

  let loop :: NESet Share.DecodedHashJWT -> IO ()
      loop hashes0 = do
        runDB (elaborateHashes (NESet.toSet hashes0) Set.empty) >>= \case
          Nothing -> pure ()
          Just hashes1 -> do
            Share.DownloadEntitiesResponse entities <-
              Share.downloadEntitiesHandler
                httpClient
                unisonShareUrl
                Share.DownloadEntitiesRequest
                  { repoName,
                    hashes = hashes1
                  }

            missingDependencies0 <-
              runDB do
                NEMap.toList entities & foldMapM \(hash, entity) -> do
                  -- still trying to figure out missing dependencies of hash/entity.
                  entityLocation hash >>= \case
                    EntityInMainStorage -> pure Set.empty
                    EntityInTempStorage missingDependencies ->
                      pure (Set.map Share.decodeHashJWT (NESet.toSet missingDependencies))
                    EntityNotStored -> do
                      -- if it has missing dependencies, add it to temp storage;
                      -- otherwise add it to main storage.
                      missingDependencies0 <-
                        Set.filterM
                          (entityExists . Share.decodedHashJWTHash)
                          (Set.map Share.decodeHashJWT (Share.entityDependencies entity))
                      case NESet.nonEmptySet missingDependencies0 of
                        Nothing -> insertEntity hash entity
                        Just missingDependencies -> insertTempEntity hash entity missingDependencies
                      pure missingDependencies0

            case NESet.nonEmptySet missingDependencies0 of
              Nothing -> pure ()
              Just missingDependencies -> loop missingDependencies
   in loop . NESet.map Share.decodeHashJWT

---------

-- Some remaining work:
--
--   [ ] Beef up insert_entity to flush temp entities
--   [ ] Write resolveHashToEntity
--   [ ] Add "no read permission" to GetCausalHashByPathResponse in Share.Types
--   [ ] The tempToSync* stuff

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

-- FIXME rename, etc
resolveHashToEntity :: Connection -> Share.Hash -> IO (Share.Entity Text Share.Hash Share.Hash)
resolveHashToEntity = undefined

------------------------------------------------------------------------------------------------------------------------
-- TODO these things come from servant-client / api types module(s)

data GetCausalHashByPathResponse
  = GetCausalHashByPathSuccess Share.HashJWT
  | GetCausalHashByPathEmpty
  | GetCausalHashByPathNoReadPermission

_getCausalHashByPath :: Share.GetCausalHashByPathRequest -> IO GetCausalHashByPathResponse
_getCausalHashByPath = undefined

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
  hashId <- throwExceptT (Q.expectHashIdByHash (decode hashJwt))
  throwExceptT (Q.expectObjectIdForAnyHashId hashId)
  where
    decode :: TempEntity.HashJWT -> Hash
    decode =
      Hash.fromBase32Hex . Share.toBase32Hex . Share.hashJWTHash . Share.HashJWT

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
  | -- | `temp_entity`, evidenced by these missing dependencies.
    EntityInTempStorage (NESet Share.HashJWT)
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
      Q.getMissingDependencyJwtsForTempEntity (Share.toBase32Hex hash) <&> \case
        Nothing -> EntityNotStored
        Just missingDependencies -> EntityInTempStorage (NESet.map Share.HashJWT missingDependencies)

-- FIXME comment
elaborateHashes :: forall m. Q.DB m => Set Share.DecodedHashJWT -> Set Share.HashJWT -> m (Maybe (NESet Share.HashJWT))
elaborateHashes hashes outputs =
  case Set.minView hashes of
    Nothing -> pure (NESet.nonEmptySet outputs)
    Just (Share.DecodedHashJWT (Share.HashJWTClaims {hash}) jwt, hashes') ->
      entityLocation hash >>= \case
        EntityNotStored -> elaborateHashes hashes' (Set.insert jwt outputs)
        EntityInTempStorage missingDependencies ->
          elaborateHashes (Set.union (Set.map Share.decodeHashJWT (NESet.toSet missingDependencies)) hashes') outputs
        EntityInMainStorage -> elaborateHashes hashes' outputs

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

-- | Convert an entity that came over the wire from Unison Share into an equivalent type that we can store in the
-- `temp_entity` table.
entityToTempEntity :: Share.Entity Text Share.Hash Share.HashJWT -> TempEntity
entityToTempEntity = \case
  Share.TC (Share.TermComponent terms) ->
    terms
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & TermFormat.SyncLocallyIndexedComponent
      & TermFormat.SyncTerm
      & TempEntity.TC
  Share.DC (Share.DeclComponent decls) ->
    decls
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & DeclFormat.SyncLocallyIndexedComponent
      & DeclFormat.SyncDecl
      & TempEntity.DC
  Share.P Share.Patch {textLookup, oldHashLookup, newHashLookup, bytes} ->
    TempEntity.P
      ( PatchFormat.SyncFull
          PatchFormat.LocalIds
            { patchTextLookup = Vector.fromList textLookup,
              patchHashLookup = Vector.fromList (coerce @[Share.Hash] @[Base32Hex] oldHashLookup),
              patchDefnLookup = Vector.fromList (coerce @[Share.HashJWT] @[TempEntity.HashJWT] newHashLookup)
            }
          bytes
      )
  Share.N Share.Namespace {textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    TempEntity.N
      ( NamespaceFormat.SyncFull
          NamespaceFormat.LocalIds
            { branchTextLookup = Vector.fromList textLookup,
              branchDefnLookup = Vector.fromList (coerce @[Share.HashJWT] @[TempEntity.HashJWT] defnLookup),
              branchPatchLookup = Vector.fromList (coerce @[Share.HashJWT] @[TempEntity.HashJWT] patchLookup),
              branchChildLookup = Vector.fromList (coerce @[(Share.HashJWT, Share.HashJWT)] @[(TempEntity.HashJWT, TempEntity.HashJWT)] childLookup)
            }
          bytes
      )
  Share.C Share.Causal {namespaceHash, parents} ->
    TempEntity.C
      Causal.SyncCausalFormat
        { valueHash = coerce @Share.HashJWT @TempEntity.HashJWT namespaceHash,
          parents = Vector.fromList (coerce @[Share.HashJWT] @[TempEntity.HashJWT] (Set.toList parents))
        }
  where
    mungeLocalIds :: Share.LocalIds Text Share.HashJWT -> LocalIds' Text TempEntity.HashJWT
    mungeLocalIds Share.LocalIds {texts, hashes} =
      LocalIds
        { textLookup = Vector.fromList texts,
          defnLookup = Vector.map Share.unHashJWT (Vector.fromList hashes)
        }

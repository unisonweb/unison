module Unison.Share.Sync
  ( -- * High-level API

    -- ** Push
    checkAndSetPush,
    PushError (..),
    fastForwardPush,

    -- ** Pull
    pull,
    PullError (..),

    -- * Low-level API

    -- ** Get causal hash by path
    getCausalHashByPath,
    GetCausalHashByPathError (..),

    -- ** Upload entities
    uploadEntities,

    -- ** Download entities
    downloadEntities,
  )
where

import qualified Control.Lens as Lens
import Control.Monad.Extra ((||^))
import qualified Data.Foldable as Foldable (find)
import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Servant.Client (BaseUrl)
import U.Codebase.HashTags (CausalHash (..))
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Causal as Causal
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import qualified U.Codebase.Sqlite.Entity as Entity
import U.Codebase.Sqlite.LocalIds (LocalIds' (..))
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Hash as Hash
import Unison.Auth.HTTPClient (AuthorizedHttpClient)
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.HTTP as Share
  ( downloadEntitiesHandler,
    fastForwardPathHandler,
    getPathHandler,
    updatePathHandler,
    uploadEntitiesHandler,
  )
import qualified Unison.Sync.Types as Share
import qualified Unison.Sync.Types as Share.RepoPath (RepoPath (..))
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Set as Set

------------------------------------------------------------------------------------------------------------------------
-- Push

-- | An error occurred while pushing code to Unison Share.
-- FIXME rename CheckAndSetPushError
data PushError
  = PushErrorHashMismatch Share.HashMismatch
  | PushErrorNoWritePermission Share.RepoPath
  | PushErrorServerMissingDependencies (NESet Share.Hash)

-- | Push a causal to Unison Share.
-- FIXME reword this
checkAndSetPush ::
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite connection, for reading entities to push.
  Sqlite.Connection ->
  -- | The repo+path to push to.
  Share.RepoPath ->
  -- | The hash that we expect this repo+path to be at on Unison Share. If not, we'll get back a hash mismatch error.
  -- This prevents accidentally pushing over data that we didn't know was there.
  Maybe Share.Hash ->
  -- | The hash of our local causal to push.
  CausalHash ->
  IO (Either PushError ())
checkAndSetPush httpClient unisonShareUrl conn repoPath expectedHash causalHash = do
  -- Maybe the server already has this causal; try just setting its remote path. Commonly, it will respond that it needs
  -- this causal (UpdatePathMissingDependencies).
  updatePath >>= \case
    Share.UpdatePathSuccess -> pure (Right ())
    Share.UpdatePathHashMismatch mismatch -> pure (Left (PushErrorHashMismatch mismatch))
    Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) -> do
      -- Upload the causal and all of its dependencies.
      uploadEntities httpClient unisonShareUrl conn (Share.RepoPath.repoName repoPath) dependencies >>= \case
        False -> pure (Left (PushErrorNoWritePermission repoPath))
        True ->
          -- After uploading the causal and all of its dependencies, try setting the remote path again.
          updatePath <&> \case
            Share.UpdatePathSuccess -> Right ()
            -- Between the initial updatePath attempt and this one, someone else managed to update the path. That's ok;
            -- we still managed to upload our causal, but the push has indeed failed overall.
            Share.UpdatePathHashMismatch mismatch -> Left (PushErrorHashMismatch mismatch)
            -- Unexpected, but possible: we thought we uploaded all we needed to, yet the server still won't accept our
            -- causal. Bug in the client because we didn't upload enough? Bug in the server because we weren't told to
            -- upload some dependency? Who knows.
            Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) ->
              Left (PushErrorServerMissingDependencies dependencies)
            Share.UpdatePathNoWritePermission _ -> Left (PushErrorNoWritePermission repoPath)
    Share.UpdatePathNoWritePermission _ -> pure (Left (PushErrorNoWritePermission repoPath))
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
                { hash = causalHashToHash causalHash,
                  entityType = Share.CausalType
                }
          }

-- | An error occurred while fast-forward pushing code to Unison Share.
data FastForwardPushError
  = FastForwardPushErrorNoHistory Share.RepoPath
  | FastForwardPushErrorNoReadPermission Share.RepoPath
  | FastForwardPushErrorNotFastForward
  | FastForwardPushErrorNoWritePermission Share.RepoPath
  | FastForwardPushErrorServerMissingDependencies (NESet Share.Hash)

-- | Push a causal to Unison Share.
-- FIXME reword this
fastForwardPush ::
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite connection, for reading entities to push.
  Sqlite.Connection ->
  -- | The repo+path to push to.
  Share.RepoPath ->
  -- | The hash of our local causal to push.
  CausalHash ->
  IO (Either FastForwardPushError ())
fastForwardPush httpClient unisonShareUrl conn repoPath localHeadHash =
  getCausalHashByPath httpClient unisonShareUrl repoPath >>= \case
    Left (GetCausalHashByPathErrorNoReadPermission _) -> pure (Left (FastForwardPushErrorNoReadPermission repoPath))
    Right Nothing -> pure (Left (FastForwardPushErrorNoHistory repoPath))
    Right (Just (Share.hashJWTHash -> remoteHeadHash)) ->
      Sqlite.runTransaction conn (fancyBfs localHeadHash remoteHeadHash) >>= \case
        -- After getting the remote causal hash, we can tell from a local computation that this wouldn't be a
        -- fast-forward push, so we don't bother trying - just report the error now.
        Nothing -> pure (Left FastForwardPushErrorNotFastForward)
        Just localTailHashes ->
          doUpload (localHeadHash :| localTailHashes) >>= \case
            False -> pure (Left (FastForwardPushErrorNoWritePermission repoPath))
            True ->
              doFastForwardPath (localHeadHash : localTailHashes) <&> \case
                Share.FastForwardPathSuccess -> Right ()
                Share.FastForwardPathMissingDependencies (Share.NeedDependencies dependencies) ->
                  Left (FastForwardPushErrorServerMissingDependencies dependencies)
                -- Weird: someone must have force-pushed no history here, or something. We observed a history at this
                -- path but moments ago!
                Share.FastForwardPathNoHistory -> Left (FastForwardPushErrorNoHistory repoPath)
                Share.FastForwardPathNoWritePermission _ -> Left (FastForwardPushErrorNoWritePermission repoPath)
                Share.FastForwardPathNotFastForward _ -> Left FastForwardPushErrorNotFastForward
  where
    doUpload :: List.NonEmpty CausalHash -> IO Bool
    -- Maybe we could save round trips here by including the tail (or the head *and* the tail) as "extra hashes", but we
    -- don't have that API yet. So, we only upload the head causal entity (which we don't even know for sure the server
    -- doesn't have yet), and will (eventually) end up uploading the casuals in the tail that the server needs.
    doUpload (headHash :| _tailHashes) =
      uploadEntities
        httpClient
        unisonShareUrl
        conn
        (Share.RepoPath.repoName repoPath)
        (NESet.singleton (causalHashToHash headHash))

    doFastForwardPath :: [CausalHash] -> IO Share.FastForwardPathResponse
    doFastForwardPath causalSpine =
      Share.fastForwardPathHandler
        httpClient
        unisonShareUrl
        Share.FastForwardPathRequest
          { hashes = map causalHashToHash causalSpine,
            path = repoPath
          }

    fancyBfs :: CausalHash -> Share.Hash -> Sqlite.Transaction (Maybe [CausalHash])
    fancyBfs = undefined

dagbfs :: forall a m. Monad m => (a -> Bool) -> (a -> m [a]) -> a -> m (Maybe (List.NonEmpty a))
dagbfs goal children =
  let -- The loop state: all distinct paths from the root to the frontier, in reverse order, with the invariant that we
      -- haven't found a goal state yet. (Otherwise, we wouldn't still be in this loop, we'd return!).
      --
      -- For example, say we are exploring the tree
      --
      --                    1
      --                   / \
      --                  2   3
      --                 / \   \
      --                4   5   6
      --
      -- Graphically, the frontier here is the nodes 4, 5, and 3; we know that, because I haven't drawn any nodes below
      -- them. (This is a BFS algorithm that discovers children on-the-fly, so maybe node 5 (for example) has children,
      -- and maybe it doesn't).
      --
      -- The loop state, in this case, would be these three paths:
      --
      --   [ 4, 2, 1 ]
      --   [ 5, 2, 1 ]
      --   [ 6, 3, 1 ]
      go :: List.NonEmpty (List.NonEmpty a) -> m (Maybe (List.NonEmpty a))
      go (path :| paths) = do
        -- Get the children of the first path (in the above example, [ 4, 2, 1 ]).
        ys0 <- children (List.NonEmpty.head path)
        case List.NonEmpty.nonEmpty ys0 of
          -- If node 4 had no more children, we can toss that whole path: it didn't end in a goal. Now we either keep
          -- searching (as we would in the example, since we have two more paths to continue from), or we don't, because
          -- this was the only remaining path.
          Nothing ->
            case List.NonEmpty.nonEmpty paths of
              Nothing -> pure Nothing
              Just paths' -> go paths'
          -- If node 4 did have children, then maybe the search tree now looks like this.
          --
          --                1
          --               / \
          --              2   3
          --             / \   \
          --            4   5   6
          --           / \
          --          7   8
          --
          -- There are two cases to handle:
          --
          --   1. One of the children we just discovered (say 7) is a goal node. So we're done, and we'd return the path
          --
          --        [ 7, 4, 2, 1 ]
          --
          --   2. No child we just discovered (7 nor 8) were a goal node. So we loop, putting our new path(s) at the end
          --      of the list (so we search paths fairly). In this case, we'd re-enter the loop with the following four
          --      paths:
          --
          --        [ 5, 2, 1 ]      \ these two are are variable 'paths', the tail of the loop state.
          --        [ 6, 3, 1 ]      /
          --        [ 7, 4, 2, 1 ]   \ these two are new, just constructed by prepending each of [ 4, 2, 1 ]'s children
          --        [ 8, 4, 2, 1 ]   / to itself, making two new paths to search
          Just ys ->
            case Foldable.find goal ys of
              Nothing -> go (append paths ((\y -> cons y path) <$> ys))
              Just y -> pure (Just (cons y path))
   in \source -> go ((source :| []) :| [])
  where
    -- Cons an element onto the head of a non-empty list.
    cons :: x -> List.NonEmpty x -> List.NonEmpty x
    cons x (y :| ys) =
      x :| y : ys

    -- Concatenate a list and a non-empty list.
    append :: [x] -> List.NonEmpty x -> List.NonEmpty x
    append xs0 ys =
      case List.NonEmpty.nonEmpty xs0 of
        Nothing -> ys
        Just xs -> xs <> ys

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
  -- | SQLite connection, for writing entities we pull.
  Sqlite.Connection ->
  -- | The repo+path to pull from.
  Share.RepoPath ->
  IO (Either PullError (Maybe CausalHash))
pull httpClient unisonShareUrl conn repoPath = do
  getCausalHashByPath httpClient unisonShareUrl repoPath >>= \case
    Left err -> pure (Left (PullErrorGetCausalHashByPath err))
    -- There's nothing at the remote path, so there's no causal to pull.
    Right Nothing -> pure (Right Nothing)
    Right (Just hashJwt) -> do
      let hash = Share.hashJWTHash hashJwt
      Sqlite.runTransaction conn (entityLocation hash) >>= \case
        EntityInMainStorage -> pure ()
        EntityInTempStorage missingDependencies -> doDownload missingDependencies
        EntityNotStored -> doDownload (NESet.singleton hashJwt)
      pure (Right (Just (CausalHash (Hash.fromBase32Hex (Share.toBase32Hex hash)))))
  where
    doDownload :: NESet Share.HashJWT -> IO ()
    doDownload =
      downloadEntities httpClient unisonShareUrl conn (Share.RepoPath.repoName repoPath)

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission Share.RepoPath

-- | Get the causal hash of a path hosted on Unison Share.
getCausalHashByPath ::
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  Share.RepoPath ->
  IO (Either GetCausalHashByPathError (Maybe Share.HashJWT))
getCausalHashByPath httpClient unisonShareUrl repoPath =
  Share.getPathHandler httpClient unisonShareUrl (Share.GetCausalHashByPathRequest repoPath) <&> \case
    Share.GetCausalHashByPathSuccess maybeHashJwt -> Right maybeHashJwt
    Share.GetCausalHashByPathNoReadPermission _ -> Left (GetCausalHashByPathErrorNoReadPermission repoPath)

------------------------------------------------------------------------------------------------------------------------
-- Download entities

-- | Download a set of entities from Unison Share.
downloadEntities ::
  AuthorizedHttpClient ->
  BaseUrl ->
  Sqlite.Connection ->
  Share.RepoName ->
  NESet Share.HashJWT ->
  IO ()
downloadEntities httpClient unisonShareUrl conn repoName =
  loop . NESet.map Share.decodeHashJWT
  where
    loop :: NESet Share.DecodedHashJWT -> IO ()
    loop hashes0 =
      whenJustM (Sqlite.runTransaction conn (elaborateHashes hashes0)) \hashes1 -> do
        entities <- doDownload hashes1

        missingDependencies0 <-
          Sqlite.runTransaction conn do
            NEMap.toList entities & foldMapM \(hash, entity) ->
              upsertEntitySomewhere hash entity

        whenJust (NESet.nonEmptySet missingDependencies0) loop

    doDownload :: NESet Share.HashJWT -> IO (NEMap Share.Hash (Share.Entity Text Share.Hash Share.HashJWT))
    doDownload hashes = do
      Share.DownloadEntitiesResponse entities <-
        Share.downloadEntitiesHandler
          httpClient
          unisonShareUrl
          Share.DownloadEntitiesRequest {repoName, hashes}
      pure entities

------------------------------------------------------------------------------------------------------------------------
-- Upload entities

-- | Upload a set of entities to Unison Share. If the server responds that it cannot yet store any hash(es) due to
-- missing dependencies, send those dependencies too, and on and on, until the server stops responding that it's missing
-- anything.
--
-- Returns true on success, false on failure (because the user does not have write permission).
uploadEntities ::
  AuthorizedHttpClient ->
  BaseUrl ->
  Sqlite.Connection ->
  Share.RepoName ->
  NESet Share.Hash ->
  IO Bool
uploadEntities httpClient unisonShareUrl conn repoName =
  loop
  where
    loop :: NESet Share.Hash -> IO Bool
    loop (NESet.toAscList -> hashes) = do
      -- Get each entity that the server is missing out of the database.
      entities <- Sqlite.runTransaction conn (traverse expectEntity hashes)

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
        Share.UploadEntitiesNoWritePermission _ -> pure False
        Share.UploadEntitiesSuccess -> pure True

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
entityExists :: Share.Hash -> Sqlite.Transaction Bool
entityExists (Share.Hash b32) = do
  -- first get hashId if exists
  Q.loadHashId b32 >>= \case
    Nothing -> pure False
    -- then check if is causal hash or if object exists for hash id
    Just hashId -> Q.isCausalHash hashId ||^ Q.isObjectHash hashId

-- | Where is an entity stored?
entityLocation :: Share.Hash -> Sqlite.Transaction EntityLocation
entityLocation hash =
  entityExists hash >>= \case
    True -> pure EntityInMainStorage
    False ->
      Q.getMissingDependencyJwtsForTempEntity (Share.toBase32Hex hash) <&> \case
        Nothing -> EntityNotStored
        Just missingDependencies -> EntityInTempStorage (NESet.map Share.HashJWT missingDependencies)

-- | "Elaborate" a set of hashes that we are considering downloading from Unison Share.
--
-- For each hash, we determine whether we already have that entity in main storage, temp storage, or nowhere:
--
-- 1. If it's nowhere, we should indeed proceed to download this hash from Unison Share.
-- 2. If it's in temp storage, then we ought to instead download its missing dependencies (which themselves are
--    elaborated by this same procedure, in case we have any of *them* already in temp storage, too.
-- 3. If it's in main storage, we should ignore it.
--
-- In the end, we return a set of hashes that correspond to entities we actually need to download.
elaborateHashes :: NESet Share.DecodedHashJWT -> Sqlite.Transaction (Maybe (NESet Share.HashJWT))
elaborateHashes =
  let loop hashes outputs =
        case Set.minView hashes of
          Nothing -> pure (NESet.nonEmptySet outputs)
          Just (Share.DecodedHashJWT (Share.HashJWTClaims {hash}) jwt, hashes') ->
            entityLocation hash >>= \case
              EntityNotStored -> loop hashes' (Set.insert jwt outputs)
              EntityInTempStorage missingDependencies ->
                loop (Set.union (Set.map Share.decodeHashJWT (NESet.toSet missingDependencies)) hashes') outputs
              EntityInMainStorage -> loop hashes' outputs
   in \hashes -> loop (NESet.toSet hashes) Set.empty

-- | Read an entity out of the database that we know is in main storage.
expectEntity :: Share.Hash -> Sqlite.Transaction (Share.Entity Text Share.Hash Share.Hash)
expectEntity hash = do
  syncEntity <- Q.expectEntity (Share.toBase32Hex hash)
  tempEntity <- Q.syncToTempEntity syncEntity
  pure (tempEntityToEntity tempEntity)

-- | Upsert a downloaded entity "somewhere" -
--
--   1. Nowhere if we already had the entity (in main or temp storage).
--   2. In main storage if we already have all of its dependencies in main storage.
--   3. In temp storage otherwise.
--
-- Returns the set of dependencies we still need to store the entity in main storage (which will be empty if either it
-- was already in main storage, or we just put it in main storage).
upsertEntitySomewhere ::
  Share.Hash ->
  Share.Entity Text Share.Hash Share.HashJWT ->
  Sqlite.Transaction (Set Share.DecodedHashJWT)
upsertEntitySomewhere hash entity =
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

-- | Insert an entity that doesn't have any missing dependencies.
insertEntity :: Share.Hash -> Share.Entity Text Share.Hash Share.HashJWT -> Sqlite.Transaction ()
insertEntity hash entity = do
  syncEntity <- Q.tempToSyncEntity (entityToTempEntity entity)
  _id <- Q.saveSyncEntity (Share.toBase32Hex hash) syncEntity
  pure ()

-- | Insert an entity and its missing dependencies.
insertTempEntity ::
  Share.Hash ->
  Share.Entity Text Share.Hash Share.HashJWT ->
  NESet Share.DecodedHashJWT ->
  Sqlite.Transaction ()
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

------------------------------------------------------------------------------------------------------------------------
-- Conversions to/from Share API types

causalHashToHash :: CausalHash -> Share.Hash
causalHashToHash =
  Share.Hash . Hash.toBase32Hex . unCausalHash

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
      & Entity.TC
  Share.DC (Share.DeclComponent decls) ->
    decls
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & DeclFormat.SyncLocallyIndexedComponent
      & DeclFormat.SyncDecl
      & Entity.DC
  Share.P Share.Patch {textLookup, oldHashLookup, newHashLookup, bytes} ->
    Entity.P (PatchFormat.SyncFull (mungePatchLocalIds textLookup oldHashLookup newHashLookup) bytes)
  Share.PD Share.PatchDiff {parent, textLookup, oldHashLookup, newHashLookup, bytes} ->
    Entity.P (PatchFormat.SyncDiff (jwt32 parent) (mungePatchLocalIds textLookup oldHashLookup newHashLookup) bytes)
  Share.N Share.Namespace {textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    Entity.N (NamespaceFormat.SyncFull (mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup) bytes)
  Share.ND Share.NamespaceDiff {parent, textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    Entity.N
      ( NamespaceFormat.SyncDiff
          (jwt32 parent)
          (mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup)
          bytes
      )
  Share.C Share.Causal {namespaceHash, parents} ->
    Entity.C
      Causal.SyncCausalFormat
        { valueHash = jwt32 namespaceHash,
          parents = Vector.fromList (map jwt32 (Set.toList parents))
        }
  where
    mungeLocalIds :: Share.LocalIds Text Share.HashJWT -> TempEntity.TempLocalIds
    mungeLocalIds Share.LocalIds {texts, hashes} =
      LocalIds
        { textLookup = Vector.fromList texts,
          defnLookup = Vector.map jwt32 (Vector.fromList hashes)
        }

    mungeNamespaceLocalIds ::
      [Text] ->
      [Share.HashJWT] ->
      [Share.HashJWT] ->
      [(Share.HashJWT, Share.HashJWT)] ->
      TempEntity.TempNamespaceLocalIds
    mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup =
      NamespaceFormat.LocalIds
        { branchTextLookup = Vector.fromList textLookup,
          branchDefnLookup = Vector.fromList (map jwt32 defnLookup),
          branchPatchLookup = Vector.fromList (map jwt32 patchLookup),
          branchChildLookup = Vector.fromList (map (\(x, y) -> (jwt32 x, jwt32 y)) childLookup)
        }

    mungePatchLocalIds :: [Text] -> [Share.Hash] -> [Share.HashJWT] -> TempEntity.TempPatchLocalIds
    mungePatchLocalIds textLookup oldHashLookup newHashLookup =
      PatchFormat.LocalIds
        { patchTextLookup = Vector.fromList textLookup,
          patchHashLookup = Vector.fromList (coerce @[Share.Hash] @[Base32Hex] oldHashLookup),
          patchDefnLookup = Vector.fromList (map jwt32 newHashLookup)
        }

    jwt32 :: Share.HashJWT -> Base32Hex
    jwt32 =
      Share.toBase32Hex . Share.hashJWTHash

tempEntityToEntity :: TempEntity -> Share.Entity Text Share.Hash Share.Hash
tempEntityToEntity = \case
  Entity.TC (TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms)) ->
    terms
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & Vector.toList
      & Share.TermComponent
      & Share.TC
  Entity.DC (DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent decls)) ->
    decls
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & Vector.toList
      & Share.DeclComponent
      & Share.DC
  Entity.P format ->
    case format of
      PatchFormat.SyncFull PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} bytes ->
        Share.P
          Share.Patch
            { textLookup = Vector.toList patchTextLookup,
              oldHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchHashLookup),
              newHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchDefnLookup),
              bytes
            }
      PatchFormat.SyncDiff parent PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} bytes ->
        Share.PD
          Share.PatchDiff
            { parent = Share.Hash parent,
              textLookup = Vector.toList patchTextLookup,
              oldHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchHashLookup),
              newHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchDefnLookup),
              bytes
            }
  Entity.N format ->
    case format of
      NamespaceFormat.SyncFull
        NamespaceFormat.LocalIds
          { branchTextLookup,
            branchDefnLookup,
            branchPatchLookup,
            branchChildLookup
          }
        bytes ->
          Share.N
            Share.Namespace
              { textLookup = Vector.toList branchTextLookup,
                defnLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchDefnLookup),
                patchLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchPatchLookup),
                childLookup =
                  Vector.toList
                    (coerce @(Vector (Base32Hex, Base32Hex)) @(Vector (Share.Hash, Share.Hash)) branchChildLookup),
                bytes
              }
      NamespaceFormat.SyncDiff
        parent
        NamespaceFormat.LocalIds
          { branchTextLookup,
            branchDefnLookup,
            branchPatchLookup,
            branchChildLookup
          }
        bytes ->
          Share.ND
            Share.NamespaceDiff
              { parent = Share.Hash parent,
                textLookup = Vector.toList branchTextLookup,
                defnLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchDefnLookup),
                patchLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchPatchLookup),
                childLookup =
                  Vector.toList
                    (coerce @(Vector (Base32Hex, Base32Hex)) @(Vector (Share.Hash, Share.Hash)) branchChildLookup),
                bytes
              }
  Entity.C Causal.SyncCausalFormat {valueHash, parents} ->
    Share.C
      Share.Causal
        { namespaceHash = Share.Hash valueHash,
          parents = Set.fromList (coerce @[Base32Hex] @[Share.Hash] (Vector.toList parents))
        }
  where
    mungeLocalIds :: LocalIds' Text Base32Hex -> Share.LocalIds Text Share.Hash
    mungeLocalIds LocalIds {textLookup, defnLookup} =
      Share.LocalIds
        { texts = Vector.toList textLookup,
          hashes = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) defnLookup)
        }

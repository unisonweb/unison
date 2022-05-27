module Unison.Share.Sync
  ( -- * High-level API

    -- ** Push
    checkAndSetPush,
    CheckAndSetPushError (..),
    fastForwardPush,
    FastForwardPushError (..),

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
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable (find)
import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import qualified Data.Sequence.NonEmpty as NESeq (fromList, nonEmptySeq, (><|))
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector as Vector
import qualified Servant.API as Servant ((:<|>) (..))
import Servant.Client (BaseUrl)
import qualified Servant.Client as Servant (ClientEnv, ClientM, client, hoistClient, mkClientEnv, runClientM)
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
import qualified Unison.Auth.HTTPClient as Auth
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.API as Share (api)
import Unison.Sync.Common
import qualified Unison.Sync.Types as Share
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Set as Set

------------------------------------------------------------------------------------------------------------------------
-- Push

-- | An error occurred while pushing code to Unison Share.
data CheckAndSetPushError
  = CheckAndSetPushErrorHashMismatch Share.HashMismatch
  | CheckAndSetPushErrorNoWritePermission Share.Path
  | CheckAndSetPushErrorServerMissingDependencies (NESet Share.Hash)

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
  Share.Path ->
  -- | The hash that we expect this repo+path to be at on Unison Share. If not, we'll get back a hash mismatch error.
  -- This prevents accidentally pushing over data that we didn't know was there.
  Maybe Share.Hash ->
  -- | The hash of our local causal to push.
  CausalHash ->
  IO (Either CheckAndSetPushError ())
checkAndSetPush httpClient unisonShareUrl conn path expectedHash causalHash = do
  -- Maybe the server already has this causal; try just setting its remote path. Commonly, it will respond that it needs
  -- this causal (UpdatePathMissingDependencies).
  updatePath >>= \case
    Share.UpdatePathSuccess -> pure (Right ())
    Share.UpdatePathHashMismatch mismatch -> pure (Left (CheckAndSetPushErrorHashMismatch mismatch))
    Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) -> do
      -- Upload the causal and all of its dependencies.
      uploadEntities httpClient unisonShareUrl conn (Share.pathRepoName path) dependencies >>= \case
        False -> pure (Left (CheckAndSetPushErrorNoWritePermission path))
        True ->
          -- After uploading the causal and all of its dependencies, try setting the remote path again.
          updatePath <&> \case
            Share.UpdatePathSuccess -> Right ()
            -- Between the initial updatePath attempt and this one, someone else managed to update the path. That's ok;
            -- we still managed to upload our causal, but the push has indeed failed overall.
            Share.UpdatePathHashMismatch mismatch -> Left (CheckAndSetPushErrorHashMismatch mismatch)
            -- Unexpected, but possible: we thought we uploaded all we needed to, yet the server still won't accept our
            -- causal. Bug in the client because we didn't upload enough? Bug in the server because we weren't told to
            -- upload some dependency? Who knows.
            Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) ->
              Left (CheckAndSetPushErrorServerMissingDependencies dependencies)
            Share.UpdatePathNoWritePermission _ -> Left (CheckAndSetPushErrorNoWritePermission path)
    Share.UpdatePathNoWritePermission _ -> pure (Left (CheckAndSetPushErrorNoWritePermission path))
  where
    updatePath :: IO Share.UpdatePathResponse
    updatePath =
      httpUpdatePath
        httpClient
        unisonShareUrl
        Share.UpdatePathRequest
          { path,
            expectedHash,
            newHash = causalHashToHash causalHash
          }

-- | An error occurred while fast-forward pushing code to Unison Share.
data FastForwardPushError
  = FastForwardPushErrorNoHistory Share.Path
  | FastForwardPushErrorNoReadPermission Share.Path
  | FastForwardPushErrorNotFastForward Share.Path
  | FastForwardPushErrorNoWritePermission Share.Path
  | FastForwardPushErrorServerMissingDependencies (NESet Share.Hash)
  | --                              Parent     Child
    FastForwardPushInvalidParentage Share.Hash Share.Hash

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
  Share.Path ->
  -- | The hash of our local causal to push.
  CausalHash ->
  IO (Either FastForwardPushError ())
fastForwardPush httpClient unisonShareUrl conn path localHeadHash =
  getCausalHashByPath httpClient unisonShareUrl path >>= \case
    Left (GetCausalHashByPathErrorNoReadPermission _) -> pure (Left (FastForwardPushErrorNoReadPermission path))
    Right Nothing -> pure (Left (FastForwardPushErrorNoHistory path))
    Right (Just (Share.hashJWTHash -> remoteHeadHash)) ->
      Sqlite.runTransaction conn (fancyBfs localHeadHash remoteHeadHash) >>= \case
        -- After getting the remote causal hash, we can tell from a local computation that this wouldn't be a
        -- fast-forward push, so we don't bother trying - just report the error now.
        Nothing -> pure (Left (FastForwardPushErrorNotFastForward path))
        Just localInnerHashes -> do
          doUpload (localHeadHash :| localInnerHashes) >>= \case
            False -> pure (Left (FastForwardPushErrorNoWritePermission path))
            True -> do
              let doFastForwardPath =
                    httpFastForwardPath
                      httpClient
                      unisonShareUrl
                      Share.FastForwardPathRequest
                        { expectedHash = remoteHeadHash,
                          hashes = causalHashToHash <$> List.NonEmpty.fromList (localInnerHashes ++ [localHeadHash]),
                          path
                        }
              doFastForwardPath <&> \case
                Share.FastForwardPathSuccess -> Right ()
                Share.FastForwardPathMissingDependencies (Share.NeedDependencies dependencies) ->
                  Left (FastForwardPushErrorServerMissingDependencies dependencies)
                -- Weird: someone must have force-pushed no history here, or something. We observed a history at this
                -- path but moments ago!
                Share.FastForwardPathNoHistory -> Left (FastForwardPushErrorNoHistory path)
                Share.FastForwardPathNoWritePermission _ -> Left (FastForwardPushErrorNoWritePermission path)
                Share.FastForwardPathNotFastForward _ -> Left (FastForwardPushErrorNotFastForward path)
                Share.FastForwardPathInvalidParentage (Share.InvalidParentage parent child) -> Left (FastForwardPushInvalidParentage parent child)
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
        (Share.pathRepoName path)
        (NESet.singleton (causalHashToHash headHash))

    -- Return a list from oldest to newst of the ancestors between (excluding) the latest local and the current remote
    -- hash.
    -- note: seems like we /should/ cut this short, with another command to go longer? :grimace:
    fancyBfs :: CausalHash -> Share.Hash -> Sqlite.Transaction (Maybe [CausalHash])
    fancyBfs h0 h1 =
      tweak <$> dagbfs (== Share.toBase32Hex h1) Q.loadCausalParentsByHash (Hash.toBase32Hex (unCausalHash h0))
      where
        -- Drop 1 (under a Maybe, and twddling hash types):
        --
        --   tweak Nothing = Nothing
        --   tweak (Just []) = Just []
        --   tweak (Just [C,B,A]) = Just [B,A]
        --
        -- The drop 1 is because dagbfs returns the goal at the head of the returned list, but we know what the goal is
        -- already (the remote head hash).
        tweak :: Maybe [Base32Hex] -> Maybe [CausalHash]
        tweak =
          fmap (map (CausalHash . Hash.fromBase32Hex) . drop 1)

data Step a
  = DeadEnd
  | KeepSearching (List.NonEmpty a)
  | FoundGoal a

-- FIXME: document
dagbfs :: forall a m. Monad m => (a -> Bool) -> (a -> m [a]) -> a -> m (Maybe [a])
dagbfs goal children =
  let -- The loop state: all distinct paths from the root to the frontier (not including the root, because it's implied,
      -- as an input to this function), in reverse order, with the invariant that we haven't found a goal state yet.
      -- (Otherwise, we wouldn't still be in this loop, we'd return!).
      --
      -- For example, say we are exploring the tree
      --
      --                    1
      --                   / \
      --                  2   3
      --                 / \   \
      --                4   5   6
      --
      -- Graphically, the frontier here is the nodes 4, 5, and 6; we know that, because we haven't drawn any nodes below
      -- them. (This is a BFS algorithm that discovers children on-the-fly, so maybe node 5 (for example) has children,
      -- and maybe it doesn't).
      --
      -- The loop state, in this case, would be these three paths:
      --
      --   [ 4, 2 ]
      --   [ 5, 2 ]
      --   [ 6, 3 ]
      --
      -- (Note, again, that we do not include the root).
      go :: NESeq (List.NonEmpty a) -> m (Maybe (List.NonEmpty a))
      go (path :<|| paths) =
        -- Step forward from the first path in our loop state (in the example above, [4, 2]).
        step (List.NonEmpty.head path) >>= \case
          -- If node 4 had no more children, we can toss that whole path: it didn't end in a goal. Now we either keep
          -- searching (as we would in the example, since we have two more paths to continue from), or we don't, because
          -- this was the only remaining path.
          DeadEnd ->
            case NESeq.nonEmptySeq paths of
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
          --        [ 7, 4, 2 ]
          --
          --   2. No child we just discovered (7 nor 8) were a goal node. So we loop, putting our new path(s) at the end
          --      of the list (so we search paths fairly). In this case, we'd re-enter the loop with the following four
          --      paths:
          --
          --        [ 5, 2 ]      \ these two are are variable 'paths', the tail of the loop state.
          --        [ 6, 3 ]      /
          --        [ 7, 4, 2 ]   \ these two are new, just constructed by prepending each of [ 4, 2, 1 ]'s children
          --        [ 8, 4, 2 ]   / to itself, making two new paths to search
          KeepSearching ys -> go (append paths ((\y -> List.NonEmpty.cons y path) <$> NESeq.fromList ys))
          FoundGoal y -> pure (Just (List.NonEmpty.cons y path))

      -- Step forward from a single node. There are 3 possible outcomes:
      --
      --   1. We discover it has no children. (return DeadEnd)
      --   2. We discover is has children, none of which are a goal. (return KeepSearching)
      --   3. We discover it has children, (at least) one of which is a goal. (return FoundGoal)
      step :: a -> m (Step a)
      step x = do
        ys0 <- children x
        pure case List.NonEmpty.nonEmpty ys0 of
          Nothing -> DeadEnd
          Just ys ->
            case Foldable.find goal ys of
              Nothing -> KeepSearching ys
              Just y -> FoundGoal y
   in \root ->
        if goal root
          then pure (Just [])
          else
            step root >>= \case
              DeadEnd -> pure Nothing
              -- lts-18.28 doesn't have List.NonEmpty.singleton
              KeepSearching xs -> fmap List.NonEmpty.toList <$> go (NESeq.fromList ((:| []) <$> xs))
              FoundGoal x -> pure (Just [x])
  where
    -- Concatenate a seq and a non-empty seq.
    append :: Seq x -> NESeq x -> NESeq x
    append = (NESeq.><|)

------------------------------------------------------------------------------------------------------------------------
-- Pull

-- | An error occurred while pulling code from Unison Share.
data PullError
  = -- | An error occurred while resolving a repo+path to a causal hash.
    PullErrorGetCausalHashByPath GetCausalHashByPathError
  | PullErrorNoHistoryAtPath Share.Path

pull ::
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite connection, for writing entities we pull.
  Sqlite.Connection ->
  -- | The repo+path to pull from.
  Share.Path ->
  IO (Either PullError CausalHash)
pull httpClient unisonShareUrl conn repoPath = do
  getCausalHashByPath httpClient unisonShareUrl repoPath >>= \case
    Left err -> pure (Left (PullErrorGetCausalHashByPath err))
    -- There's nothing at the remote path, so there's no causal to pull.
    Right Nothing -> pure (Left (PullErrorNoHistoryAtPath repoPath))
    Right (Just hashJwt) -> do
      let hash = Share.hashJWTHash hashJwt
      Sqlite.runTransaction conn (entityLocation hash) >>= \case
        Just EntityInMainStorage -> pure ()
        Just (EntityInTempStorage missingDependencies) -> doDownload missingDependencies
        Nothing -> doDownload (NESet.singleton hashJwt)
      pure (Right (CausalHash (Hash.fromBase32Hex (Share.toBase32Hex hash))))
  where
    doDownload :: NESet Share.HashJWT -> IO ()
    doDownload =
      downloadEntities httpClient unisonShareUrl conn (Share.pathRepoName repoPath)

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission Share.Path

-- | Get the causal hash of a path hosted on Unison Share.
getCausalHashByPath ::
  -- | The HTTP client to use for Unison Share requests.
  AuthorizedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  Share.Path ->
  IO (Either GetCausalHashByPathError (Maybe Share.HashJWT))
getCausalHashByPath httpClient unisonShareUrl repoPath =
  httpGetCausalHashByPath httpClient unisonShareUrl (Share.GetCausalHashByPathRequest repoPath) <&> \case
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
              upsertEntitySomewhere hash entity <&> \case
                EntityInMainStorage -> Set.empty
                EntityInTempStorage missingDependencies -> Set.map Share.decodeHashJWT (NESet.toSet missingDependencies)

        whenJust (NESet.nonEmptySet missingDependencies0) loop

    doDownload :: NESet Share.HashJWT -> IO (NEMap Share.Hash (Share.Entity Text Share.Hash Share.HashJWT))
    doDownload hashes = do
      Share.DownloadEntitiesSuccess entities <-
        httpDownloadEntities
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
            httpUploadEntities
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
        Share.UploadEntitiesHashMismatchForEntity {} -> pure False
        Share.UploadEntitiesSuccess -> pure True

------------------------------------------------------------------------------------------------------------------------
-- Database operations

-- | Where is an entity stored?
data EntityLocation
  = -- | `object` / `causal`
    EntityInMainStorage
  | -- | `temp_entity`, evidenced by these missing dependencies.
    EntityInTempStorage (NESet Share.HashJWT)

-- | Where is an entity stored?
entityLocation :: Share.Hash -> Sqlite.Transaction (Maybe EntityLocation)
entityLocation (Share.Hash hash) =
  Q.entityExists hash >>= \case
    True -> pure (Just EntityInMainStorage)
    False ->
      Q.getMissingDependencyJwtsForTempEntity hash <&> \case
        Nothing -> Nothing
        Just missingDependencies -> Just (EntityInTempStorage (NESet.map Share.HashJWT missingDependencies))

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
              Nothing -> loop hashes' (Set.insert jwt outputs)
              Just (EntityInTempStorage missingDependencies) ->
                loop (Set.union (Set.map Share.decodeHashJWT (NESet.toSet missingDependencies)) hashes') outputs
              Just EntityInMainStorage -> loop hashes' outputs
   in \hashes -> loop (NESet.toSet hashes) Set.empty

-- | Upsert a downloaded entity "somewhere" -
--
--   1. Nowhere if we already had the entity (in main or temp storage).
--   2. In main storage if we already have all of its dependencies in main storage.
--   3. In temp storage otherwise.
upsertEntitySomewhere ::
  Share.Hash ->
  Share.Entity Text Share.Hash Share.HashJWT ->
  Sqlite.Transaction EntityLocation
upsertEntitySomewhere hash entity =
  entityLocation hash >>= \case
    Just location -> pure location
    Nothing -> do
      missingDependencies0 <-
        Set.filterM
          (fmap not . Q.entityExists . Share.toBase32Hex . Share.hashJWTHash)
          (Share.entityDependencies entity)
      case NESet.nonEmptySet missingDependencies0 of
        Nothing -> do
          insertEntity hash entity
          pure EntityInMainStorage
        Just missingDependencies -> do
          insertTempEntity hash entity missingDependencies
          pure (EntityInTempStorage missingDependencies)

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
  NESet Share.HashJWT ->
  Sqlite.Transaction ()
insertTempEntity hash entity missingDependencies =
  Q.insertTempEntity
    (Share.toBase32Hex hash)
    (entityToTempEntity entity)
    ( NESet.map
        ( \hashJwt ->
            let Share.DecodedHashJWT {claims = Share.HashJWTClaims {hash}} = Share.decodeHashJWT hashJwt
             in (Share.toBase32Hex hash, Share.unHashJWT hashJwt)
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

------------------------------------------------------------------------------------------------------------------------
-- HTTP calls

httpGetCausalHashByPath :: Auth.AuthorizedHttpClient -> BaseUrl -> Share.GetCausalHashByPathRequest -> IO Share.GetCausalHashByPathResponse
httpFastForwardPath :: Auth.AuthorizedHttpClient -> BaseUrl -> Share.FastForwardPathRequest -> IO Share.FastForwardPathResponse
httpUpdatePath :: Auth.AuthorizedHttpClient -> BaseUrl -> Share.UpdatePathRequest -> IO Share.UpdatePathResponse
httpDownloadEntities :: Auth.AuthorizedHttpClient -> BaseUrl -> Share.DownloadEntitiesRequest -> IO Share.DownloadEntitiesResponse
httpUploadEntities :: Auth.AuthorizedHttpClient -> BaseUrl -> Share.UploadEntitiesRequest -> IO Share.UploadEntitiesResponse
( httpGetCausalHashByPath,
  httpFastForwardPath,
  httpUpdatePath,
  httpDownloadEntities,
  httpUploadEntities
  ) =
    let ( httpGetCausalHashByPath
            Servant.:<|> httpFastForwardPath
            Servant.:<|> httpUpdatePath
            Servant.:<|> httpDownloadEntities
            Servant.:<|> httpUploadEntities
          ) = Servant.hoistClient Share.api hoist (Servant.client Share.api)
     in ( go httpGetCausalHashByPath,
          go httpFastForwardPath,
          go httpUpdatePath,
          go httpDownloadEntities,
          go httpUploadEntities
        )
    where
      hoist :: Servant.ClientM a -> ReaderT Servant.ClientEnv IO a
      hoist m = do
        clientEnv <- Reader.ask
        liftIO (throwEitherM (Servant.runClientM m clientEnv))

      go ::
        (req -> ReaderT Servant.ClientEnv IO resp) ->
        Auth.AuthorizedHttpClient ->
        BaseUrl ->
        req ->
        IO resp
      go f (Auth.AuthorizedHttpClient httpClient) unisonShareUrl req =
        runReaderT (f req) (Servant.mkClientEnv httpClient unisonShareUrl)

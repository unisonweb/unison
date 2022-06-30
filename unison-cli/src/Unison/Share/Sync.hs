{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Share.Sync
  ( -- * High-level API

    -- ** Get causal hash by path
    getCausalHashByPath,
    GetCausalHashByPathError (..),

    -- ** Push
    checkAndSetPush,
    CheckAndSetPushError (..),
    fastForwardPush,
    FastForwardPushError (..),

    -- ** Pull
    pull,
    DownloadProgressCallbacks (..),
    PullError (..),
  )
where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable (find)
import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Map as Map
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Proxy
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import qualified Data.Sequence.NonEmpty as NESeq (fromList, nonEmptySeq, (><|))
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import Data.Void (Void)
import qualified Ki
import qualified Network.HTTP.Client as Http.Client
import qualified Network.HTTP.Types as HTTP
import qualified Servant.API as Servant ((:<|>) (..), (:>))
import Servant.Client (BaseUrl)
import qualified Servant.Client as Servant
import U.Codebase.HashTags (CausalHash)
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Util.Hash32 (Hash32)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import qualified Unison.Auth.HTTPClient as Auth
import qualified Unison.Debug as Debug
import Unison.Prelude
import Unison.Share.Sync.Types
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.API as Share (API)
import Unison.Sync.Common (causalHashToHash32, entityToTempEntity, expectEntity, hash32ToCausalHash)
import qualified Unison.Sync.Types as Share
import Unison.Util.Monoid (foldMapM)
import qualified UnliftIO

------------------------------------------------------------------------------------------------------------------------
-- Push

-- | Perform a check-and-set push (initially of just a causal hash, but ultimately all of its dependencies that the
-- server is missing, too) to Unison Share.
--
-- This flavor of push takes the expected state of the server, and the desired state we want to set; if our expectation
-- is off, we won't proceed with the push.
checkAndSetPush ::
  -- | The HTTP client to use for Unison Share requests.
  AuthenticatedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite-connection-making function, for writing entities we pull.
  (forall a. (Sqlite.Connection -> IO a) -> IO a) ->
  -- | The repo+path to push to.
  Share.Path ->
  -- | The hash that we expect this repo+path to be at on Unison Share. If not, we'll get back a hash mismatch error.
  -- This prevents accidentally pushing over data that we didn't know was there.
  Maybe Hash32 ->
  -- | The hash of our local causal to push.
  CausalHash ->
  -- | Callback that is given the number of entities just uploaded, and the number of entities we just learned we need
  -- to upload as well.
  (Int -> Int -> IO ()) ->
  IO (Either (SyncError CheckAndSetPushError) ())
checkAndSetPush httpClient unisonShareUrl connect path expectedHash causalHash uploadProgressCallback = catchSyncErrors do
  -- Maybe the server already has this causal; try just setting its remote path. Commonly, it will respond that it needs
  -- this causal (UpdatePathMissingDependencies).
  updatePath >>= \case
    Share.UpdatePathSuccess -> pure (Right ())
    Share.UpdatePathHashMismatch mismatch -> pure (Left (CheckAndSetPushErrorHashMismatch mismatch))
    Share.UpdatePathMissingDependencies (Share.NeedDependencies dependencies) -> do
      -- Upload the causal and all of its dependencies.
      uploadEntities httpClient unisonShareUrl connect (Share.pathRepoName path) dependencies uploadProgressCallback >>= \case
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
            newHash = causalHashToHash32 causalHash
          }

-- | Perform a fast-forward push (initially of just a causal hash, but ultimately all of its dependencies that the
-- server is missing, too) to Unison Share.
--
-- This flavor of push provides the server with a chain of causal hashes leading from its current state to our desired
-- state.
fastForwardPush ::
  -- | The HTTP client to use for Unison Share requests.
  AuthenticatedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite-connection-making function, for writing entities we pull.
  (forall a. (Sqlite.Connection -> IO a) -> IO a) ->
  -- | The repo+path to push to.
  Share.Path ->
  -- | The hash of our local causal to push.
  CausalHash ->
  -- | Callback that is given the number of entities just uploaded, and the number of entities we just learned we need
  -- to upload as well.
  (Int -> Int -> IO ()) ->
  IO (Either (SyncError FastForwardPushError) ())
fastForwardPush httpClient unisonShareUrl connect path localHeadHash uploadProgressCallback = catchSyncErrors do
  getCausalHashByPath httpClient unisonShareUrl path >>= \case
    Left (GetCausalHashByPathErrorNoReadPermission _) -> pure (Left (FastForwardPushErrorNoReadPermission path))
    Right Nothing -> pure (Left (FastForwardPushErrorNoHistory path))
    Right (Just (Share.hashJWTHash -> remoteHeadHash)) -> do
      let doLoadCausalSpineBewteen = loadCausalSpineBetween remoteHeadHash (causalHashToHash32 localHeadHash)
      (connect \conn -> Sqlite.runTransaction conn doLoadCausalSpineBewteen) >>= \case
        -- After getting the remote causal hash, we can tell from a local computation that this wouldn't be a
        -- fast-forward push, so we don't bother trying - just report the error now.
        Nothing -> pure (Left (FastForwardPushErrorNotFastForward path))
        -- The path from remote-to-local, excluding local, was empty. So, remote == local; there's nothing to push.
        Just [] -> pure (Right ())
        Just (_ : localInnerHashes0) -> do
          -- drop remote hash
          let localInnerHashes = map hash32ToCausalHash localInnerHashes0
          doUpload (localHeadHash :| localInnerHashes) >>= \case
            False -> pure (Left (FastForwardPushErrorNoWritePermission path))
            True -> do
              let doFastForwardPath =
                    httpFastForwardPath
                      httpClient
                      unisonShareUrl
                      Share.FastForwardPathRequest
                        { expectedHash = remoteHeadHash,
                          hashes =
                            causalHashToHash32 <$> List.NonEmpty.fromList (localInnerHashes ++ [localHeadHash]),
                          path
                        }
              doFastForwardPath <&> \case
                Share.FastForwardPathSuccess -> Right ()
                Share.FastForwardPathMissingDependencies (Share.NeedDependencies dependencies) ->
                  Left (FastForwardPushErrorServerMissingDependencies dependencies)
                -- Weird: someone must have force-pushed no history here, or something. We observed a history at
                -- this path but moments ago!
                Share.FastForwardPathNoHistory -> Left (FastForwardPushErrorNoHistory path)
                Share.FastForwardPathNoWritePermission _ -> Left (FastForwardPushErrorNoWritePermission path)
                Share.FastForwardPathNotFastForward _ -> Left (FastForwardPushErrorNotFastForward path)
                Share.FastForwardPathInvalidParentage (Share.InvalidParentage parent child) ->
                  Left (FastForwardPushInvalidParentage parent child)
  where
    doUpload :: List.NonEmpty CausalHash -> IO Bool
    -- Maybe we could save round trips here by including the tail (or the head *and* the tail) as "extra hashes", but we
    -- don't have that API yet. So, we only upload the head causal entity (which we don't even know for sure the server
    -- doesn't have yet), and will (eventually) end up uploading the casuals in the tail that the server needs.
    doUpload (headHash :| _tailHashes) =
      uploadEntities
        httpClient
        unisonShareUrl
        connect
        (Share.pathRepoName path)
        (NESet.singleton (causalHashToHash32 headHash))
        uploadProgressCallback

-- Return a list (in oldest-to-newest order) of hashes along the causal spine that connects the given arguments,
-- excluding the newest hash (second argument).
loadCausalSpineBetween :: Hash32 -> Hash32 -> Sqlite.Transaction (Maybe [Hash32])
loadCausalSpineBetween earlierHash laterHash =
  dagbfs (== earlierHash) Q.loadCausalParentsByHash laterHash

data Step a
  = DeadEnd
  | KeepSearching (List.NonEmpty a)
  | FoundGoal a

-- | @dagbfs goal children root@ searches breadth-first through the monadic tree formed by applying @chilred@ to each
-- node (initially @root@), until it finds a goal node (i.e. when @goal@ returns True).
--
-- Returns the nodes along a path from root to goal in bottom-up or goal-to-root order, excluding the root node (because
-- it was provided as an input ;))
--
-- For example, when searching a tree that looks like
--
--                    1
--                   / \
--                  2   3
--                 / \   \
--                4  [5]  6
--
-- (where the goal is marked [5]), we'd return
--
--                Just [5,2]
--
-- And (as another example), if the root node is the goal,
--
--                   [1]
--                   / \
--                  2   3
--                 / \   \
--                4   5   6
--
-- we'd return
--
--                Just []
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

-- | Download progress callbacks.
data DownloadProgressCallbacks = DownloadProgressCallbacks
  { -- | Callback that's given a number of entities we just downloaded.
    downloaded :: Int -> IO (),
    -- | Callback that's given a number of entities we just realized we need to download later.
    toDownload :: Int -> IO ()
  }

pull ::
  -- | The HTTP client to use for Unison Share requests.
  AuthenticatedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  -- | SQLite-connection-making function, for writing entities we pull.
  (forall a. (Sqlite.Connection -> IO a) -> IO a) ->
  -- | The repo+path to pull from.
  Share.Path ->
  DownloadProgressCallbacks ->
  IO (Either (SyncError PullError) CausalHash)
pull httpClient unisonShareUrl connect repoPath callbacks = catchSyncErrors do
  getCausalHashByPath httpClient unisonShareUrl repoPath >>= \case
    Left err -> pure (Left (PullErrorGetCausalHashByPath err))
    -- There's nothing at the remote path, so there's no causal to pull.
    Right Nothing -> pure (Left (PullErrorNoHistoryAtPath repoPath))
    Right (Just hashJwt) -> do
      let hash = Share.hashJWTHash hashJwt
      maybeTempEntities <-
        connect \conn ->
          Sqlite.runTransaction conn (Q.entityLocation hash) >>= \case
            Just Q.EntityInMainStorage -> pure Nothing
            Just Q.EntityInTempStorage -> pure (Just (NESet.singleton hash))
            Nothing -> do
              toDownload callbacks 1
              Share.DownloadEntitiesSuccess entities <-
                httpDownloadEntities
                  httpClient
                  unisonShareUrl
                  Share.DownloadEntitiesRequest {repoName, hashes = NESet.singleton hashJwt}
              tempEntities <- insertEntities conn entities
              downloaded callbacks 1
              pure (NESet.nonEmptySet tempEntities)
      whenJust maybeTempEntities \tempEntities ->
        completeTempEntities
          httpClient
          unisonShareUrl
          connect
          repoName
          callbacks
          tempEntities
      pure (Right (hash32ToCausalHash hash))
  where
    repoName = Share.pathRepoName repoPath

type WorkerCount =
  TVar Int

newWorkerCount :: IO WorkerCount
newWorkerCount =
  newTVarIO 0

recordWorking :: WorkerCount -> STM ()
recordWorking sem =
  modifyTVar' sem (+ 1)

recordNotWorking :: WorkerCount -> STM ()
recordNotWorking sem =
  modifyTVar' sem \n -> n - 1

-- What the dispatcher is to do
data DispatcherJob
  = DispatcherForkWorker (NESet Share.HashJWT)
  | DispatcherDone

-- | Finish downloading entities from Unison Share. Returns the total number of entities downloaded.
--
-- Precondition: the entities were *already* downloaded at some point in the past, and are now sitting in the
-- `temp_entity` table, waiting for their dependencies to arrive so they can be flushed to main storage.
completeTempEntities ::
  AuthenticatedHttpClient ->
  BaseUrl ->
  (forall a. (Sqlite.Connection -> IO a) -> IO a) ->
  Share.RepoName ->
  DownloadProgressCallbacks ->
  NESet Hash32 ->
  IO ()
completeTempEntities httpClient unisonShareUrl connect repoName callbacks initialNewTempEntities = do
  -- The set of hashes we still need to download
  hashesVar <- newTVarIO Set.empty

  -- The set of hashes that we haven't inserted yet, but will soon, because we've committed to downloading them.
  uninsertedHashesVar <- newTVarIO Set.empty

  -- The entities payloads (along with the jwts that we used to download them) that we've downloaded
  entitiesQueue <- newTQueueIO

  -- The sets of new (at the time of inserting, anyway) temp entity rows, which we need to elaborate, then download.
  newTempEntitiesQueue <- newTQueueIO

  -- How many workers (downloader / inserter / elaborator) are currently doing stuff.
  workerCount <- newWorkerCount

  -- Kick off the cycle of inserter->elaborator->dispatcher->downloader by giving the elaborator something to do
  atomically (writeTQueue newTempEntitiesQueue (Set.empty, Just initialNewTempEntities))

  Ki.scoped \scope -> do
    Ki.fork_ scope (inserter entitiesQueue newTempEntitiesQueue workerCount)
    Ki.fork_ scope (elaborator hashesVar uninsertedHashesVar newTempEntitiesQueue workerCount)
    dispatcher hashesVar uninsertedHashesVar entitiesQueue newTempEntitiesQueue workerCount
  where
    -- Dispatcher thread: "dequeue" from `hashesVar`, fork one-shot downloaders.
    --
    -- We stop when all of the following are true:
    --
    --   - There are no outstanding workers (downloaders, inserter, elaboraror)
    --   - The inserter thread doesn't have any outstanding work enqueued (in `entitiesQueue`)
    --   - The elaborator thread doesn't have any outstanding work enqueued (in `newTempEntitiesQueue`)
    dispatcher ::
      TVar (Set Share.HashJWT) ->
      TVar (Set Share.HashJWT) ->
      TQueue (NESet Share.HashJWT, NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT)) ->
      TQueue (Set Share.HashJWT, Maybe (NESet Hash32)) ->
      WorkerCount ->
      IO ()
    dispatcher hashesVar uninsertedHashesVar entitiesQueue newTempEntitiesQueue workerCount =
      Ki.scoped \scope ->
        let loop :: IO ()
            loop =
              atomically (dispatchWorkMode <|> checkIfDoneMode) >>= \case
                DispatcherDone -> pure ()
                DispatcherForkWorker hashes -> do
                  atomically do
                    -- Only allow 5 concurrent downloaders (7 workers = inserter + elaborator + 5 downloaders)
                    workers <- readTVar workerCount
                    check (workers < 7)
                    -- we do need to record the downloader as working outside of the worker thread, not inside.
                    -- otherwise, we might erroneously fall through the the teardown logic below and conclude there's
                    -- nothing more for the dispatcher to do, when in fact a downloader thread just hasn't made it as
                    -- far as recording its own existence
                    recordWorking workerCount
                  _ <- Ki.fork @() scope (downloader entitiesQueue workerCount hashes)
                  loop
         in loop
      where
        dispatchWorkMode :: STM DispatcherJob
        dispatchWorkMode = do
          hashes <- readTVar hashesVar
          check (not (Set.null hashes))
          let (hashes1, hashes2) = Set.splitAt 50 hashes
          modifyTVar' uninsertedHashesVar (Set.union hashes1)
          writeTVar hashesVar hashes2
          pure (DispatcherForkWorker (NESet.unsafeFromSet hashes1))

        -- Check to see if there are no hashes left to download, no outstanding workers, and no work in either queue
        checkIfDoneMode :: STM DispatcherJob
        checkIfDoneMode = do
          workers <- readTVar workerCount
          check (workers == 0)
          isEmptyTQueue entitiesQueue >>= check
          isEmptyTQueue newTempEntitiesQueue >>= check
          pure DispatcherDone

    -- Downloader thread: download entities, enqueue to `entitiesQueue`
    downloader ::
      TQueue (NESet Share.HashJWT, NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT)) ->
      WorkerCount ->
      NESet Share.HashJWT ->
      IO ()
    downloader entitiesQueue workerCount hashes = do
      Share.DownloadEntitiesSuccess entities <-
        httpDownloadEntities
          httpClient
          unisonShareUrl
          Share.DownloadEntitiesRequest {repoName, hashes}
      downloaded callbacks (NESet.size hashes)
      atomically do
        writeTQueue entitiesQueue (hashes, entities)
        recordNotWorking workerCount

    -- Inserter thread: dequeue from `entitiesQueue`, insert entities, enqueue to `newTempEntitiesQueue`
    inserter ::
      TQueue (NESet Share.HashJWT, NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT)) ->
      TQueue (Set Share.HashJWT, Maybe (NESet Hash32)) ->
      WorkerCount ->
      IO Void
    inserter entitiesQueue newTempEntitiesQueue workerCount =
      connect \conn ->
        forever do
          (hashJwts, entities) <-
            atomically do
              entities <- readTQueue entitiesQueue
              recordWorking workerCount
              pure entities
          newTempEntities0 <-
            Sqlite.runTransaction conn do
              NEMap.toList entities & foldMapM \(hash, entity) ->
                upsertEntitySomewhere hash entity <&> \case
                  Q.EntityInMainStorage -> Set.empty
                  Q.EntityInTempStorage -> Set.singleton hash
          atomically do
            writeTQueue newTempEntitiesQueue (NESet.toSet hashJwts, NESet.nonEmptySet newTempEntities0)
            recordNotWorking workerCount

    -- Elaborator thread: dequeue from `newTempEntitiesQueue`, elaborate, "enqueue" to `hashesVar`
    elaborator ::
      TVar (Set Share.HashJWT) ->
      TVar (Set Share.HashJWT) ->
      TQueue (Set Share.HashJWT, Maybe (NESet Hash32)) ->
      WorkerCount ->
      IO Void
    elaborator hashesVar uninsertedHashesVar newTempEntitiesQueue workerCount =
      connect \conn ->
        forever do
          maybeNewTempEntities <-
            atomically do
              (hashJwts, mayNewTempEntities) <- readTQueue newTempEntitiesQueue
              -- Avoid unnecessary retaining of these hashes to keep memory usage more stable. This algorithm would
              -- still be correct if we never delete from `uninsertedHashes`.
              --
              -- We remove the inserted hashes from uninsertedHashesVar at this point rather than right after insertion
              -- in order to ensure that no running transaction of the elaborator is viewing a snapshot that precedes
              -- the snapshot that inserted those hashes.
              modifyTVar' uninsertedHashesVar \uninsertedHashes -> Set.difference uninsertedHashes hashJwts
              case mayNewTempEntities of
                Nothing -> pure Nothing
                Just newTempEntities -> do
                  recordWorking workerCount
                  pure (Just newTempEntities)
          whenJust maybeNewTempEntities \newTempEntities -> do
            newElaboratedHashes <- Sqlite.runTransaction conn (elaborateHashes newTempEntities)
            moreToDownload <-
              atomically do
                uninsertedHashes <- readTVar uninsertedHashesVar
                hashes0 <- readTVar hashesVar
                let !hashes1 = Set.union (Set.difference newElaboratedHashes uninsertedHashes) hashes0
                writeTVar hashesVar hashes1
                pure (Set.size hashes1 - Set.size hashes0)
            toDownload callbacks moreToDownload
            atomically (recordNotWorking workerCount)

-- | Insert entities into the database, and return the subset that went into temp storage (`temp_entitiy`) rather than
-- of main storage (`object` / `causal`) due to missing dependencies.
insertEntities :: Sqlite.Connection -> NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT) -> IO (Set Hash32)
insertEntities conn entities =
  Sqlite.runTransaction conn do
    NEMap.toList entities & foldMapM \(hash, entity) ->
      upsertEntitySomewhere hash entity <&> \case
        Q.EntityInMainStorage -> Set.empty
        Q.EntityInTempStorage -> Set.singleton hash

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

-- | Get the causal hash of a path hosted on Unison Share.
getCausalHashByPath ::
  -- | The HTTP client to use for Unison Share requests.
  AuthenticatedHttpClient ->
  -- | The Unison Share URL.
  BaseUrl ->
  Share.Path ->
  IO (Either GetCausalHashByPathError (Maybe Share.HashJWT))
getCausalHashByPath httpClient unisonShareUrl repoPath =
  httpGetCausalHashByPath httpClient unisonShareUrl (Share.GetCausalHashByPathRequest repoPath) <&> \case
    Share.GetCausalHashByPathSuccess maybeHashJwt -> Right maybeHashJwt
    Share.GetCausalHashByPathNoReadPermission _ -> Left (GetCausalHashByPathErrorNoReadPermission repoPath)

------------------------------------------------------------------------------------------------------------------------
-- Upload entities

data UploadDispatcherJob
  = UploadDispatcherReturnFailure
  | UploadDispatcherForkWorkerWhenAvailable (NESet Hash32)
  | UploadDispatcherForkWorker (NESet Hash32)
  | UploadDispatcherDone

-- | Upload a set of entities to Unison Share. If the server responds that it cannot yet store any hash(es) due to
-- missing dependencies, send those dependencies too, and on and on, until the server stops responding that it's missing
-- anything.
--
-- Returns true on success, false on failure (because the user does not have write permission).
uploadEntities ::
  AuthenticatedHttpClient ->
  BaseUrl ->
  (forall a. (Sqlite.Connection -> IO a) -> IO a) ->
  Share.RepoName ->
  NESet Hash32 ->
  (Int -> Int -> IO ()) ->
  IO Bool
uploadEntities httpClient unisonShareUrl connect repoName hashes0 uploadProgressCallback = do
  hashesVar <- newTVarIO (NESet.toSet hashes0)
  -- FIXME document this
  dedupeVar <- newTVarIO Set.empty
  nextWorkerIdVar <- newTVarIO 0
  workersVar <- newTVarIO Set.empty
  workerFailedVar <- newEmptyTMVarIO

  uploadProgressCallback 0 (NESet.size hashes0)

  Ki.scoped \scope ->
    dispatcher scope hashesVar dedupeVar nextWorkerIdVar workersVar workerFailedVar
  where
    dispatcher ::
      Ki.Scope ->
      TVar (Set Hash32) ->
      TVar (Set Hash32) ->
      TVar Int ->
      TVar (Set Int) ->
      TMVar () ->
      IO Bool
    dispatcher scope hashesVar dedupeVar nextWorkerIdVar workersVar workerFailedVar = do
      loop
      where
        loop :: IO Bool
        loop =
          doJob [checkForFailureMode, dispatchWorkMode, checkIfDoneMode]

        doJob :: [STM UploadDispatcherJob] -> IO Bool
        doJob jobs =
          atomically (asum jobs) >>= \case
            UploadDispatcherReturnFailure -> pure False
            UploadDispatcherForkWorkerWhenAvailable hashes -> doJob [forkWorkerMode hashes, checkForFailureMode]
            UploadDispatcherForkWorker hashes -> do
              workerId <-
                atomically do
                  workerId <- readTVar nextWorkerIdVar
                  writeTVar nextWorkerIdVar $! workerId + 1
                  modifyTVar' workersVar (Set.insert workerId)
                  pure workerId
              _ <-
                Ki.fork @() scope do
                  worker hashesVar dedupeVar workersVar workerFailedVar workerId hashes
              loop
            UploadDispatcherDone -> pure True

        checkForFailureMode :: STM UploadDispatcherJob
        checkForFailureMode = do
          () <- readTMVar workerFailedVar
          pure UploadDispatcherReturnFailure

        dispatchWorkMode :: STM UploadDispatcherJob
        dispatchWorkMode = do
          hashes <- readTVar hashesVar
          when (Set.null hashes) retry
          let (hashes1, hashes2) = Set.splitAt 50 hashes
          modifyTVar' dedupeVar (Set.union hashes1)
          writeTVar hashesVar hashes2
          pure (UploadDispatcherForkWorkerWhenAvailable (NESet.unsafeFromSet hashes1))

        forkWorkerMode :: NESet Hash32 -> STM UploadDispatcherJob
        forkWorkerMode hashes = do
          workers <- readTVar workersVar
          when (Set.size workers >= 10) retry
          pure (UploadDispatcherForkWorker hashes)

        checkIfDoneMode :: STM UploadDispatcherJob
        checkIfDoneMode = do
          workers <- readTVar workersVar
          when (not (Set.null workers)) retry
          pure UploadDispatcherDone

    worker :: TVar (Set Hash32) -> TVar (Set Hash32) -> TVar (Set Int) -> TMVar () -> Int -> NESet Hash32 -> IO ()
    worker hashesVar dedupeVar workersVar workerFailedVar workerId hashes = do
      entities <-
        fmap NEMap.fromAscList do
          connect \conn ->
            Sqlite.runTransaction conn do
              for (NESet.toAscList hashes) \hash -> do
                entity <- expectEntity hash
                pure (hash, entity)

      result <-
        httpUploadEntities httpClient unisonShareUrl Share.UploadEntitiesRequest {entities, repoName} <&> \case
          Share.UploadEntitiesNeedDependencies (Share.NeedDependencies moreHashes) -> Right (NESet.toSet moreHashes)
          Share.UploadEntitiesNoWritePermission _ -> Left ()
          Share.UploadEntitiesHashMismatchForEntity _ -> error "hash mismatch; fixme"
          Share.UploadEntitiesSuccess -> Right Set.empty

      case result of
        Left () -> void (atomically (tryPutTMVar workerFailedVar ()))
        Right moreHashes -> do
          uploadProgressCallback (NESet.size hashes) 0
          (maybeYoungestWorkerThatWasAlive, numNewHashes) <-
            atomically do
              -- Record ourselves as "dead". The only work we have left to do is remove the hashes we just uploaded from
              -- the `dedupe` set, but whether or not we are "alive" is relevant only to:
              --
              --   - The main dispatcher thread, which terminates when there are no more hashes to upload, and no alive
              --     workers. It is not important for us to delete from the `dedupe` set in this case.
              --
              --   - Other worker threads, each of which independently decides when it is safe to delete the set of
              --     hashes they just uploaded from the `dedupe` set (as we are doing now).
              !workers <- Set.delete workerId <$> readTVar workersVar
              writeTVar workersVar workers
              -- Add more work (i.e. hashes to upload) to the work queue (really a work set), per the response we just
              -- got from the server. Remember to only add hashes that aren't in the `dedupe` set (see the comment on
              -- the dedupe set above for more info).
              numNewHashes <-
                if not (Set.null moreHashes)
                  then do
                    dedupe <- readTVar dedupeVar
                    hashes0 <- readTVar hashesVar
                    let !hashes1 = Set.union (Set.difference moreHashes dedupe) hashes0
                    writeTVar hashesVar hashes1
                    pure (Set.size hashes1 - Set.size hashes0)
                  else pure 0
              pure (Set.lookupMax workers, numNewHashes)
          uploadProgressCallback 0 numNewHashes
          -- Block until we are sure that the server does not have any uncommitted transactions that see a version of
          -- the database that does not include the entities we just uploaded. After that point, it's fine to remove the
          -- hashes of the entities we just uploaded from the `dedupe` set, because they will never be relevant for any
          -- subsequent deduping operations. If we didn't delete from the `dedupe` set, this algorithm would still be
          -- correct, it would just use an unbounded amount of memory to remember all the hashes we've uploaded so far.
          whenJust maybeYoungestWorkerThatWasAlive \youngestWorkerThatWasAlive -> do
            atomically do
              workers <- readTVar workersVar
              whenJust (Set.lookupMin workers) \oldestWorkerAlive ->
                when (oldestWorkerAlive <= youngestWorkerThatWasAlive) retry
          atomically (modifyTVar' dedupeVar (`Set.difference` (NESet.toSet hashes)))

------------------------------------------------------------------------------------------------------------------------
-- Database operations

-- | "Elaborate" a set of `temp_entity` hashes.
--
-- For each hash, then we ought to instead download its missing dependencies (which themselves are
--    elaborated by this same procedure, in case we have any of *them* already in temp storage, too.
-- 3. If it's in main storage, we should ignore it.
--
-- In the end, we return a set of hashes that correspond to entities we actually need to download.
elaborateHashes :: NESet Hash32 -> Sqlite.Transaction (Set Share.HashJWT)
elaborateHashes hashes =
  Q.elaborateHashes (NESet.toList hashes) <&> Set.fromList . coerce @[Text] @[Share.HashJWT]

-- | Upsert a downloaded entity "somewhere" -
--
--   1. Nowhere if we already had the entity (in main or temp storage).
--   2. In main storage if we already have all of its dependencies in main storage.
--   3. In temp storage otherwise.
upsertEntitySomewhere ::
  Hash32 ->
  Share.Entity Text Hash32 Share.HashJWT ->
  Sqlite.Transaction Q.EntityLocation
upsertEntitySomewhere hash entity =
  Q.entityLocation hash >>= \case
    Just location -> pure location
    Nothing -> do
      missingDependencies1 :: Map Hash32 Share.HashJWT <-
        Share.entityDependencies entity
          & foldMapM
            ( \hashJwt -> do
                let hash = Share.hashJWTHash hashJwt
                Q.entityExists hash <&> \case
                  True -> Map.empty
                  False -> Map.singleton hash hashJwt
            )
      case NEMap.nonEmptyMap missingDependencies1 of
        Nothing -> do
          _id <- Q.saveTempEntityInMain v2HashHandle hash (entityToTempEntity Share.hashJWTHash entity)
          pure Q.EntityInMainStorage
        Just missingDependencies -> do
          Q.insertTempEntity
            hash
            (entityToTempEntity Share.hashJWTHash entity)
            ( coerce
                @(NEMap Hash32 Share.HashJWT)
                @(NEMap Hash32 Text)
                missingDependencies
            )
          pure Q.EntityInTempStorage

------------------------------------------------------------------------------------------------------------------------
-- HTTP calls

httpGetCausalHashByPath :: Auth.AuthenticatedHttpClient -> BaseUrl -> Share.GetCausalHashByPathRequest -> IO Share.GetCausalHashByPathResponse
httpFastForwardPath :: Auth.AuthenticatedHttpClient -> BaseUrl -> Share.FastForwardPathRequest -> IO Share.FastForwardPathResponse
httpUpdatePath :: Auth.AuthenticatedHttpClient -> BaseUrl -> Share.UpdatePathRequest -> IO Share.UpdatePathResponse
httpDownloadEntities :: Auth.AuthenticatedHttpClient -> BaseUrl -> Share.DownloadEntitiesRequest -> IO Share.DownloadEntitiesResponse
httpUploadEntities :: Auth.AuthenticatedHttpClient -> BaseUrl -> Share.UploadEntitiesRequest -> IO Share.UploadEntitiesResponse
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
          ) =
            -- FIXME remove this once the other thing lands
            let pp :: Proxy ("sync" Servant.:> Share.API)
                pp = Proxy
             in Servant.hoistClient pp hoist (Servant.client pp)
     in ( go httpGetCausalHashByPath,
          go httpFastForwardPath,
          go httpUpdatePath,
          go httpDownloadEntities,
          go httpUploadEntities
        )
    where
      hoist :: Servant.ClientM a -> ReaderT (BaseUrl, Servant.ClientEnv) IO a
      hoist m = do
        (shareURL, clientEnv) <- Reader.ask
        throwEitherM $
          liftIO (Servant.runClientM m clientEnv) >>= \case
            Right a -> pure $ Right a
            Left err -> do
              Debug.debugLogM Debug.Sync (show err)
              pure . Left $ case err of
                Servant.FailureResponse _req resp -> case HTTP.statusCode $ Servant.responseStatusCode resp of
                  401 -> Unauthenticated shareURL
                  -- The server should provide semantically relevant permission-denied messages
                  -- when possible, but this should catch any we miss.
                  403 -> PermissionDenied (Text.Lazy.toStrict . Text.Lazy.decodeUtf8 $ Servant.responseBody resp)
                  408 -> Timeout
                  429 -> RateLimitExceeded
                  500 -> InternalServerError
                  504 -> Timeout
                  code
                    | code >= 500 -> InternalServerError
                    | otherwise -> InvalidResponse resp
                Servant.DecodeFailure _msg resp -> InvalidResponse resp
                Servant.UnsupportedContentType _ct resp -> InvalidResponse resp
                Servant.InvalidContentTypeHeader resp -> InvalidResponse resp
                Servant.ConnectionError {} -> UnreachableCodeserver shareURL

      go ::
        (req -> ReaderT (BaseUrl, Servant.ClientEnv) IO resp) ->
        Auth.AuthenticatedHttpClient ->
        BaseUrl ->
        req ->
        IO resp
      go f (Auth.AuthenticatedHttpClient httpClient) unisonShareUrl req =
        runReaderT
          (f req)
          ( unisonShareUrl,
            (Servant.mkClientEnv httpClient unisonShareUrl)
              { Servant.makeClientRequest = \url request ->
                  -- Disable client-side timeouts
                  (Servant.defaultMakeClientRequest url request)
                    { Http.Client.responseTimeout = Http.Client.responseTimeoutNone
                    }
              }
          )

catchSyncErrors :: IO (Either e a) -> IO (Either (SyncError e) a)
catchSyncErrors action =
  UnliftIO.try @_ @CodeserverTransportError action >>= \case
    Left te -> pure (Left . TransportError $ te)
    Right (Left e) -> pure . Left . SyncError $ e
    Right (Right a) -> pure $ Right a

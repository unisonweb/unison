module Unison.Share.SyncV3
  ( syncFromCodeserver,
  )
where

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Data.Set qualified as Set
import Data.Set.Lens qualified as Lens
import GHC.Natural
import Ki qualified
import Network.WebSockets qualified as WS
import U.Codebase.HashTags
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Hash32 (Hash32)
import Unison.Prelude
import Unison.Server.Orphans ()
import Unison.Share.API.Hash qualified as Share
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Sync.Types qualified as Sync
import Unison.Sync.Common qualified as Sync
import Unison.SyncV3.Types
import Unison.SyncV3.Types as SyncV3
import Unison.Util.Servant.CBOR qualified as CBOR
import Unison.Util.Websockets (Queues (..), withQueues)
import UnliftIO.STM
import Wuss qualified

-- Websocket send/receive buffer sizes
inputBuffer :: Natural
inputBuffer = 1000

outputBuffer :: Natural
outputBuffer = 1000

transactionBatchSize :: Natural
transactionBatchSize = 1000

syncV3ClientVersion :: Int32
syncV3ClientVersion = 1

syncFromCodeserver ::
  Bool ->
  -- | The Unison Share URL.
  Codeserver.CodeserverURI ->
  -- | The branch to download from.
  BranchRef ->
  -- | The hash to download.
  Share.HashJWT ->
  Cli (Either (Sync.SyncError SyncV3.SyncError) (CausalHash, CausalHashId))
syncFromCodeserver _shouldValidate codeserver branchRef hashJwt = do
  Cli.Env {codebase} <- ask
  let host = Codeserver.codeserverRegName codeserver
  let tlsPort = 443
  let port = maybe tlsPort fromIntegral $ (Codeserver.codeserverPort) codeserver
  let syncV3Path = "/ucm/v3/sync"
  let rootCausalHash = Share.hashJWTHash hashJwt
  -- Enable compression
  let connectionOptions = WS.defaultConnectionOptions {WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate}
  -- TODO: Add authentication headers manually.
  let headers = []
  liftIO $ (Wuss.runSecureClientWith host port syncV3Path connectionOptions headers) \conn -> do
    withQueues inputBuffer outputBuffer conn $ \queues@Queues {send} -> do
      let initMsg =
            InitMsg
              { initMsgClientVersion = syncV3ClientVersion,
                initMsgBranchRef = branchRef,
                initMsgRootCausal = hashJwt,
                initMsgRequestedDepth = Nothing
              }
      atomically $ send $ Msg $ ReceiverInitStream initMsg
      pendingRequestsVar <- newTVarIO (Set.singleton (CausalEntity, rootCausalHash))
      yetToRequestVar <- newTVarIO Set.empty
      toIngestQueue <- newTBQueueIO transactionBatchSize
      let initState =
            SyncState
              { pendingRequestsVar,
                yetToRequestVar,
                toIngestQueue,
                rootCausalHash
              }

      liftIO (doSync codebase initState queues) >>= \case
        -- TODO: proper error handling
        Left err -> error $ show err
        Right () -> pure ()
      causalId <- liftIO $ flushTemp codebase (Share.hashJWTHash hashJwt)
      pure $ Right (Sync.hash32ToCausalHash rootCausalHash, causalId)

data SyncState = SyncState
  { pendingRequestsVar :: TVar (Set (EntityKind, Hash32)),
    yetToRequestVar :: TVar (Set (EntityKind, Hash32)),
    toIngestQueue :: TBQueue (Entity Hash32 Text),
    rootCausalHash :: Hash32
  }

-- | Given a stream that's already been initialized, receive entities and issue requests as needed.
doSync :: Codebase IO v a -> SyncState -> Queues (MsgOrError SyncError (FromReceiverMessage Share.HashJWT Hash32)) (MsgOrError SyncError (FromEmitterMessage Hash32 Text)) -> IO (Either SyncError ())
doSync codebase SyncState {pendingRequestsVar, yetToRequestVar, toIngestQueue, rootCausalHash} (Queues {send, receive, shutdown}) = Ki.scoped \scope -> do
  errorVar <- newEmptyTMVarIO
  let onErr err = do
        atomically $ putTMVar errorVar err
        shutdown
  _ <- Ki.fork scope (receiverWorker onErr)
  _ <- Ki.fork scope (requesterWorker onErr)
  _ <- Ki.fork scope (ingestionWorker onErr)
  atomically $ (Right <$> Ki.awaitAll scope) <|> (Left <$> readTMVar errorVar)
  where
    receiverWorker :: (SyncError -> IO ()) -> IO ()
    receiverWorker onErr = do
      atomically receive >>= \case
        Msg (EmitterErrorMsg err) -> onErr err
        Msg (EmitterEntityMsg entity) -> do
          atomically $ do
            writeTBQueue toIngestQueue entity
          receiverWorker onErr
        Msg EmitterDoneMsg -> return ()
        Err err -> onErr err
    requesterWorker :: (SyncError -> IO ()) -> IO ()
    requesterWorker _onErr = forever do
      atomically $ do
        requests <- readTVar yetToRequestVar
        writeTVar yetToRequestVar Set.empty
        modifyTVar' pendingRequestsVar (Set.union requests)
        send $ Msg $ ReceiverEntityRequest $ EntityRequestMsg (Set.toList requests)

    ingestionWorker :: (SyncError -> IO ()) -> IO ()
    ingestionWorker _onErr = forever do
      newEntities <- atomically $ do
        flushTBQueue toIngestQueue
      Codebase.runTransaction codebase $ do
        -- TODO: do hash validation based on shouldValidate
        for_ newEntities $ \(Entity {entityKind, entityHash, entityDepth, entityData = CBOR.CBORBytes entityBytes}) -> do
          Q.insertTempEntitySyncV3 rootCausalHash (tShow entityKind) entityHash (unEntityDepth entityDepth) entityBytes

      tempEntities <- case for newEntities (CBOR.deserialiseOrFailCBORBytes . entityData) of
        -- TODO: proper error handling
        Left err -> error $ show err
        Right tempEntities -> pure tempEntities
      let allDeps = foldMap tempEntityDependencies tempEntities
      -- TODO: double-check whether it's okay to have this as a separate atomic block.
      alreadyRequestedEntities <- atomically $ do
        pending <- readTVar pendingRequestsVar
        reqs <- readTVar yetToRequestVar
        pure $ Set.union pending reqs
      let unrequestedDeps = Set.difference allDeps alreadyRequestedEntities
      missingDeps <-
        (Set.toList unrequestedDeps) & filterA \(_depKind, depHash) -> do
          Codebase.runTransaction codebase (Q.entityLocationSyncV3 depHash) <&> \case
            Nothing -> True
            _ -> False
      let newlyInserted =
            newEntities
              <&> (entityKind &&& entityHash)
              & Set.fromList
      -- Request any deps we're missing which also haven't already been requested
      atomically $ do
        pending <- readTVar pendingRequestsVar
        let missingDepsSet = Set.fromList missingDeps
        let unRequestedDeps = Set.difference missingDepsSet pending
        modifyTVar' yetToRequestVar (Set.union unRequestedDeps)
        modifyTVar' pendingRequestsVar (\pending -> Set.difference pending newlyInserted)

flushTemp :: Codebase IO v a -> Hash32 -> IO CausalHashId
flushTemp codebase rootCausalHash = do
  Codebase.runTransaction codebase $ do
    Q.streamTempEntitiesSyncV3 rootCausalHash \next ->
      do
        let loop = do
              next >>= \case
                Nothing -> pure ()
                Just (hash, tempEntityBytes) ->
                  do
                    tempEntity <- case CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes tempEntityBytes) of
                      -- TODO: proper error handling
                      Left err -> error $ show err
                      Right tempEntity -> pure tempEntity
                    void $ Q.saveTempEntityInMain v2HashHandle hash tempEntity
                    loop
        loop
    Q.expectCausalHashIdByCausalHash (Sync.hash32ToCausalHash rootCausalHash)

tempEntityDependencies :: TempEntity -> Set (EntityKind, Hash32)
tempEntityDependencies entity = do
  let componentDeps = Lens.setOf Entity.defns_ entity
      patchDeps = Lens.setOf Entity.patches_ entity
      branchHashes = Lens.setOf Entity.branchHashes_ entity <> Lens.setOf Entity.branches_ entity
      causalHashes = Lens.setOf Entity.causalHashes_ entity
   in Set.unions
        [ Set.map (DefnComponentEntity,) componentDeps,
          Set.map (PatchEntity,) patchDeps,
          Set.map (NamespaceEntity,) branchHashes,
          Set.map (CausalEntity,) causalHashes
        ]

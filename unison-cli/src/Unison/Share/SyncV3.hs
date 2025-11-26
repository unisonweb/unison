module Unison.Share.SyncV3
  ( syncFromCodeserver,
  )
where

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Data.Set qualified as Set
import Data.Text.Encoding as Text
import GHC.Natural
import Ki qualified
import Network.Socket (withSocketsDo)
import Network.WebSockets qualified as WS
import U.Codebase.HashTags
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Cli.Monad
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.Prelude
import Unison.Server.Orphans ()
import Unison.Share.API.Hash qualified as Share
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Sync.Types qualified as Sync
import Unison.Share.Types
import Unison.Sync.Common qualified as Sync
import Unison.SyncV3.Types
import Unison.SyncV3.Types as SyncV3
import Unison.SyncV3.Utils (tempEntityDependencies)
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
  Cli.Env {codebase, tokenProvider} <- ask
  let host = Codeserver.codeserverRegName codeserver
  let syncV3Path = "/ucm/v3/sync/download"
  let rootCausalHash = Share.hashJWTHash hashJwt
  -- Enable compression
  let connectionOptions = WS.defaultConnectionOptions {WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate}
  headers <-
    (liftIO (tokenProvider (codeserverIdFromCodeserverURI codeserver))) <&> \case
      Left {} -> []
      Right token -> [("Authorization", "Bearer " <> Text.encodeUtf8 token)]
  let runner = case Codeserver.codeserverScheme codeserver of
        Codeserver.Https ->
          let tlsPort = 443
              port = maybe tlsPort fromIntegral $ (Codeserver.codeserverPort) codeserver
           in Wuss.runSecureClientWith host port
        Codeserver.Http ->
          let tlsPort = 443 :: Int
              port = maybe tlsPort id $ (Codeserver.codeserverPort) codeserver
           in WS.runClientWith host port
  Debug.debugLogM Debug.Temp "Obtaining Connection"
  liftIO $ withSocketsDo $ (runner syncV3Path connectionOptions headers) \conn -> do
    Debug.debugLogM Debug.Temp "Obtained Connection"
    withQueues inputBuffer outputBuffer conn $ \queues@Queues {send} -> do
      Debug.debugLogM Debug.Temp "Obtained Queues"
      let initMsg =
            InitMsg
              { initMsgClientVersion = syncV3ClientVersion,
                initMsgBranchRef = branchRef,
                initMsgRootCausal = hashJwt,
                initMsgRequestedDepth = Nothing
              }
      Debug.debugLogM Debug.Temp "Sending init message"
      atomically $ send $ Msg $ ReceiverInitStream initMsg
      Debug.debugLogM Debug.Temp "Init message sent"
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
      Debug.debugLogM Debug.Temp "!Done sync, flushing temp entities"
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
doSync codebase SyncState {pendingRequestsVar, yetToRequestVar, toIngestQueue, rootCausalHash} (Queues {send, receive, shutdown, connectionClosed}) = Ki.scoped \scope -> do
  errorVar <- newEmptyTMVarIO
  let onErr err = do
        atomically $ putTMVar errorVar err
        shutdown
  _ <- Ki.fork scope (receiverWorker onErr)
  _ <- Ki.fork scope (requesterWorker onErr)
  _ <- Ki.fork scope (ingestionWorker onErr)
  let finished = do
        pending <- readTVar pendingRequestsVar
        yetToReq <- readTVar yetToRequestVar
        guard $ Set.null pending && Set.null yetToReq

  Debug.debugLogM Debug.Temp "Awaiting completion"
  result <-
    atomically $
      (Right <$> finished)
        <|> (Right <$> Ki.awaitAll scope)
        <|> (Left . Left <$> readTMVar errorVar)
        <|> (Left . Right <$> connectionClosed)

  Debug.debugM Debug.Temp "End result" result
  case result of
    Left (Left syncErr) -> pure $ Left syncErr
    Left (Right mayConnErr) -> case mayConnErr of
      Nothing -> pure $ Right ()
      Just connErr -> pure $ Left $ ConnectionError (tShow connErr)
    Right () -> pure $ Right ()
  where
    receiverWorker :: (SyncError -> IO ()) -> IO ()
    receiverWorker onErr = do
      Debug.debugLogM Debug.Temp "Receiver waiting for message"
      atomically receive >>= \case
        Msg (EmitterEntityMsg entity) -> do
          atomically $ do
            writeTBQueue toIngestQueue entity
          receiverWorker onErr
        Err err -> onErr err
    requesterWorker :: (SyncError -> IO ()) -> IO ()
    requesterWorker _onErr = forever do
      Debug.debugLogM Debug.Temp "Requester waiting to send requests"
      atomically $ do
        requests <- readTVar yetToRequestVar
        guard $ not (Set.null requests)
        writeTVar yetToRequestVar Set.empty
        modifyTVar' pendingRequestsVar (Set.union requests)
        send $ Msg $ ReceiverEntityRequest $ EntityRequestMsg (Set.toList requests)

    ingestionWorker :: (SyncError -> IO ()) -> IO ()
    ingestionWorker _onErr = forever do
      Debug.debugLogM Debug.Temp "Ingestion waiting for entities"
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
                    Debug.debugLogM Debug.Temp $ "Flushing temp entity: " <> show hash
                    tempEntity <- case CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes tempEntityBytes) of
                      -- TODO: proper error handling
                      Left err -> error $ show err
                      Right tempEntity -> pure tempEntity
                    Debug.debugLogM Debug.Temp $ "Saving in main" <> show hash
                    void $ Q.saveTempEntityInMain v2HashHandle hash tempEntity
                    loop
        loop
    Debug.debugLogM Debug.Temp "Flushed temp entities, getting causal hash id"
    Q.expectCausalHashIdByCausalHash (Sync.hash32ToCausalHash rootCausalHash)

{-# LANGUAGE KindSignatures #-}

module Unison.Util.Websockets
  ( withQueues,
    Queues (..),
    withCodeserverWebsocket,
  )
where

import Control.Applicative
import Control.Concurrent.STM.TBMQueue
import Control.Lens (Profunctor (..))
import Control.Monad
import Data.Text.Encoding qualified as Text
import Ki.Unlifted qualified as Ki
import Network.Socket
import Network.WebSockets
import Network.WebSockets qualified as WS
import Unison.Prelude
import Unison.Share.Types
import UnliftIO
import Wuss qualified

-- | Allows interfacing with a websocket as a pair of bounded queues.
data Queues i o = Queues
  { -- Receive from the client. Returns Nothing if the connection is closed.
    receive :: STM (Maybe o),
    -- Send to the client. Returns False if the connection is closed.
    send :: i -> STM Bool
  }

instance Profunctor Queues where
  dimap f g (Queues {receive, send}) =
    Queues
      { receive = fmap g <$> receive,
        send = send . f
      }

withQueues :: forall i o m a. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Int -> Int -> Connection -> (Queues i o -> m a) -> m (Either ConnectionException a)
withQueues inputBuffer outputBuffer conn action = Ki.scoped $ \scope -> do
  receiveQ <- liftIO $ newTBMQueueIO inputBuffer
  sendQ <- liftIO $ newTBMQueueIO outputBuffer
  connectionClosedMVar <- liftIO $ newEmptyTMVarIO
  let receive = do readTBMQueue receiveQ
  let send msg = do
        writeTBMQueue sendQ msg
        isClosedTBMQueue sendQ

  let triggerClose :: forall n. (MonadIO n) => (Maybe ConnectionException) -> n ()
      triggerClose mayErr = do
        newlyClosed <- atomically $ do
          newlyClosed <- tryPutTMVar connectionClosedMVar mayErr
          when newlyClosed $ do
            -- Close the queues to signal to workers to stop.
            closeTBMQueue receiveQ
            closeTBMQueue sendQ
          pure newlyClosed

        when newlyClosed $ do
          -- If we closed due to a connection error, we don't need to send a close.
          -- If we're shutting down normally, we send a close message.
          case mayErr of
            Nothing -> liftIO $ sendClose conn ("Server is shutting down" :: Text)
            _ -> pure ()

  let queues = Queues {receive, send}
  _ <- Ki.fork scope $ recvWorker triggerClose receiveQ
  _ <- Ki.fork scope $ sendWorker triggerClose sendQ
  let waitConnectionError = atomically do
        mayErr <- readTMVar connectionClosedMVar
        case mayErr of
          Nothing -> empty
          Just err -> pure err
  result <- race waitConnectionError (action queues)
  -- Ensure the connection is closed when done.
  liftIO $ triggerClose Nothing
  pure result
  where
    recvWorker :: (Maybe ConnectionException -> m ()) -> TBMQueue o -> m ()
    recvWorker triggerClose q = UnliftIO.handle (handler triggerClose) $ do
      msg <- liftIO $ receiveData conn
      atomically $ writeTBMQueue q msg
      recvWorker triggerClose q

    handler :: (Maybe ConnectionException -> m ()) -> ConnectionException -> m ()
    handler triggerClose = \case
      CloseRequest {} -> do
        -- The client requested a close, we can just close normally.
        triggerClose Nothing
      -- Other cases are exceptional
      err -> triggerClose (Just err)

    sendWorker :: (Maybe ConnectionException -> m ()) -> TBMQueue i -> m ()
    sendWorker triggerClose q = UnliftIO.handle (handler triggerClose) $ do
      let flushQ = do
            xs <- many $ do
              readTBMQueue q >>= \case
                Nothing -> empty
                Just outMsg -> pure outMsg
            isClosedTBMQueue q >>= \case
              True -> pure (Left xs)
              False -> do
                pure (Right xs)
      outMsgs <- atomically $ flushQ
      case outMsgs of
        Left msgs ->
          liftIO $ sendBinaryDatas conn msgs
        Right msgs -> do
          liftIO $ sendBinaryDatas conn msgs
          sendWorker triggerClose q

-- | Connect a websocket to the codeserver at the given URI.
-- The action will be called with a 'Queues' to send and receive messages,
-- when the action completes, the websocket connection will be closed.
withCodeserverWebsocket :: (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Int -> CodeserverURI -> (CodeserverId -> IO (Either e Text)) -> String -> (Queues i o -> m r) -> m (Either ConnectionException r)
withCodeserverWebsocket msgBufferSize codeserver tokenProvider codeserverPath action = do
  let host = codeserverRegName codeserver
  let connectionOptions = WS.defaultConnectionOptions {WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate}
  headers <-
    (liftIO (tokenProvider (codeserverIdFromCodeserverURI codeserver))) <&> \case
      Left {} -> []
      Right token -> [("Authorization", "Bearer " <> Text.encodeUtf8 token)]
  let wsRunner = case codeserverScheme codeserver of
        Https ->
          let tlsPort = 443
              port = maybe tlsPort fromIntegral $ (codeserverPort) codeserver
           in Wuss.runSecureClientWith host port
        Http ->
          let tlsPort = 443 :: Int
              port = maybe tlsPort id $ (codeserverPort) codeserver
           in WS.runClientWith host port
  toIO <- askRunInIO
  liftIO $ withSocketsDo $ (wsRunner codeserverPath connectionOptions headers) \conn -> do
    withQueues msgBufferSize msgBufferSize conn $ \queues -> do
      toIO $ action queues

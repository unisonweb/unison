{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Unison.Util.Websockets
  ( withQueues,
    Queues (..),
    MsgOrError (..),
    withCodeserverWebsocket,
  )
where

import Codec.Serialise qualified as CBOR
import Control.Applicative
import Control.Concurrent.STM.TBMQueue
import Control.Lens (Profunctor (..))
import Control.Monad
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ki.Unlifted qualified as Ki
import Network.Socket
import Network.WebSockets
import Network.WebSockets qualified as WS
import Unison.Debug qualified as Debug
import Unison.Prelude
import Unison.Share.Types
import UnliftIO
import Wuss qualified

-- | Allows interfacing with a websocket as a pair of bounded queues.
data Queues i o = Queues
  { -- Receive from the other side. Returns Nothing if the connection is closed.
    receive :: STM (Maybe o),
    -- Send to the other side. Returns False if the connection is closed.
    send :: i -> STM Bool
  }

instance Profunctor Queues where
  dimap f g (Queues {receive, send}) =
    Queues
      { receive = fmap g <$> receive,
        send = send . f
      }

withQueues :: forall i o m a. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Int -> Int -> Connection -> (Queues i o -> m a) -> m (Either ConnectionException (a, [o {- Any leftover messages received from the other side after we've indicated we want to shut down. -}]))
withQueues inputBuffer outputBuffer conn action = Ki.scoped $ \scope -> do
  receiveQ <- liftIO $ newTBMQueueIO inputBuffer
  sendQ <- liftIO $ newTBMQueueIO outputBuffer
  connectionClosedMVar <- liftIO $ newEmptyTMVarIO
  let receive = do readTBMQueue receiveQ
  let send msg = do
        writeTBMQueue sendQ msg
        isClosedTBMQueue sendQ
  let queues = Queues {receive, send}

  _ <- Ki.fork scope $ recvWorker connectionClosedMVar receiveQ
  sendWorkerThread <- Ki.fork scope $ sendWorker sendQ
  let waitConnectionError = atomically do
        readTMVar connectionClosedMVar
  race waitConnectionError (action queues) >>= \case
    Left err -> do
      Debug.debugM Debug.Temp "Connection error occurred, shutting down websocket" (show err)
      -- An error occurred, return it.
      pure (Left err)
    Right result -> do
      -- The action completed, we need to close the connection gracefully
      -- and drain any remaining messages.
      atomically $ do
        -- Close the send queue, then wait for all messages to be sent before we close.
        closeTBMQueue sendQ
      atomically $ Ki.await sendWorkerThread
      -- Now we can close and drain any remaining messages.
      msgs <- selfClose receiveQ
      pure $ Right (result, msgs)
  where
    -- Shut down the connection gracefully, returning any remaining messages.
    selfClose :: (TBMQueue o) -> m [o]
    selfClose receiveQ = do
      -- We've requested to close the connection.
      Debug.debugLogM Debug.Temp "We've requested close, sending close message"
      liftIO $ sendClose conn ("Done" :: Text)
      let drainMessages :: m [o]
          drainMessages = do
            -- Read messages until the queue is closed, which indicates the other side has also closed their connection.
            atomically (readTBMQueue receiveQ) >>= \case
              Nothing -> pure []
              Just msg -> do
                rest <- drainMessages
                pure (msg : rest)
      drainMessages

    recvWorker :: (TMVar ConnectionException) -> TBMQueue o -> m ()
    recvWorker errMVar q = do
      UnliftIO.handle handler $ do
        msg <- liftIO $ receiveData conn
        Debug.debugM Debug.Temp "Received message from websocket" ()
        atomically $ writeTBMQueue q msg
      recvWorker errMVar q
      where
        handler :: ConnectionException -> m ()
        handler = \case
          CloseRequest {} -> do
            -- The other side requested a close, we close the recv channel to indicate
            -- we won't receive any more messages.
            Debug.debugM Debug.Temp "Other side requested close" ()
            atomically $ do
              closeTBMQueue q

          -- Other cases are exceptional, set the error var
          err -> do
            Debug.debugM Debug.Temp "ConnectionException in recvWorker" (show err)
            atomically $ do
              void $ tryPutTMVar errMVar err
            pure ()

    sendWorker :: TBMQueue i -> m ()
    sendWorker q = do
      let flushQ :: STM ([i], Bool)
          flushQ = do
            optional (readTBMQueue q) >>= \case
              -- No messages, but queue is still open
              Nothing -> empty
              -- Queue is closed
              Just Nothing -> pure ([], True)
              -- Got a message, keep flushing
              Just (Just outMsg) -> do
                first (outMsg :) <$> flushQ
      (outMsgs, isClosed) <- atomically $ flushQ
      liftIO $ sendBinaryDatas conn outMsgs
      when (not isClosed) $ sendWorker q

-- | Connect a websocket to the codeserver at the given URI.
-- The action will be called with a 'Queues' to send and receive messages,
-- when the action completes, the websocket connection will be closed.
withCodeserverWebsocket :: forall m i o r e. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Int -> CodeserverURI -> (CodeserverId -> IO (Either e Text)) -> String -> (Queues i o -> m r) -> m (Either ConnectionException (r, [o {- Any leftover messages received from the server after we've indicated we want to shut down. -}]))
withCodeserverWebsocket msgBufferSize codeserver tokenProvider codeserverPath action = do
  let host = codeserverRegName codeserver
  let connectionOptions = WS.defaultConnectionOptions {WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate}
  headers <-
    (liftIO (tokenProvider (codeserverIdFromCodeserverURI codeserver))) <&> \case
      Left {} -> []
      Right token -> [("Authorization", "Bearer " <> Text.encodeUtf8 token)]

  let wsRunner path opts headers action = case codeserverScheme codeserver of
        Https ->
          let tlsPort = 443
              port = maybe tlsPort fromIntegral $ (codeserverPort) codeserver
           in do
                print $ "Connecting to codeserver via WSS: " <> show (host, port, codeserverPath, headers)
                Wuss.runSecureClientWith host port path opts headers action
        Http ->
          let defaultPort = 80 :: Int
              port = maybe defaultPort id $ codeserverPort codeserver
              fixedHost = case host of
                -- The haskell ws client has issues with "localhost"
                "localhost" -> "127.0.0.1"
                _ -> host
           in do
                print $ "Connecting to codeserver via WS: " <> show (fixedHost, port, codeserverPath, headers)
                WS.runClientWith fixedHost port path opts headers action
  toIO <- askRunInIO
  Debug.debugM Debug.Temp "withCodeserverWebsocket:" (host, codeserverPath)
  liftIO $ withSocketsDo $ (wsRunner codeserverPath connectionOptions headers) \conn -> do
    Debug.debugM Debug.Temp "CONNECTED to websocket" (host, codeserverPath)
    withQueues msgBufferSize msgBufferSize conn $ \queues -> do
      toIO $ action queues

-- | Type used for websocket messages that can either be a message or an error.
data MsgOrError err a
  = Msg a
  | UserErr err
  | DeserialiseFailure Text
  deriving (Show, Eq, Ord)

-- | Roundtrip test:
-- >>> import qualified Codec.Serialise as CBOR
-- >>> CBOR.deserialise (CBOR.serialise (Msg "test" :: MsgOrError Text Text)) == Msg "test"
-- True
-- >>> CBOR.deserialise (CBOR.serialise (Err "error" :: MsgOrError Text Text)) == Err "error"
-- True
instance (CBOR.Serialise a, CBOR.Serialise err) => CBOR.Serialise (MsgOrError err a) where
  encode = \case
    Msg a -> CBOR.encode (0 :: Int) <> CBOR.encode a
    UserErr e -> CBOR.encode (1 :: Int) <> CBOR.encode e
    DeserialiseFailure msg -> CBOR.encode (2 :: Int) <> CBOR.encode msg

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> Msg <$> CBOR.decode
      1 -> UserErr <$> CBOR.decode
      2 -> DeserialiseFailure <$> CBOR.decode
      _ -> fail $ "Unknown MsgOrError tag: " <> show tag

-- | Roundtrip test:
-- >>> import qualified Network.WebSockets as WS
-- >>> let msgVal = Msg "test" :: MsgOrError Text Text
-- >>> WS.fromLazyByteString (WS.toLazyByteString msgVal) == msgVal
-- True
-- >>> let errVal = UserErr "whoops" :: MsgOrError Text Text
-- >>> WS.fromLazyByteString (WS.toLazyByteString errVal) == errVal
-- True
--
-- >>> let errVal = DeserialiseFailure "whoops" :: MsgOrError Text Text
-- >>> WS.fromLazyByteString (WS.toLazyByteString errVal) == errVal
-- True
-- >>> let dataMsg = WS.Binary (WS.toLazyByteString msgVal)
-- >>> WS.fromDataMessage dataMsg == msgVal
-- True
instance (CBOR.Serialise msg, CBOR.Serialise e) => WebSocketsData (MsgOrError e msg) where
  fromLazyByteString bytes =
    case CBOR.deserialiseOrFail bytes of
      Left err -> DeserialiseFailure (Text.pack (show err))
      Right msg -> msg

  toLazyByteString = CBOR.serialise

  fromDataMessage dm = do
    case dm of
      WS.Text bytes _ -> WS.fromLazyByteString bytes
      WS.Binary bytes -> WS.fromLazyByteString bytes

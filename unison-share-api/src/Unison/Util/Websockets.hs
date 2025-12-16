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
withCodeserverWebsocket :: forall m i o r e. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Int -> CodeserverURI -> (CodeserverId -> IO (Either e Text)) -> String -> (Queues i o -> m r) -> m (Either ConnectionException r)
withCodeserverWebsocket msgBufferSize codeserver tokenProvider codeserverPath action = do
  let host = codeserverRegName codeserver
  let connectionOptions = WS.defaultConnectionOptions -- {WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate}
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

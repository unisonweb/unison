{-# LANGUAGE KindSignatures #-}

module Unison.Util.Websockets
  ( withQueues,
    Queues (..),
  )
where

import Control.Applicative
import Control.Lens (Profunctor (..))
import Control.Monad
import Data.Text (Text)
import GHC.Natural
import Ki.Unlifted qualified as Ki
import Network.WebSockets
import UnliftIO

-- | Allows interfacing with a websocket as a pair of bounded queues.
data Queues i o = Queues
  { -- Receive from the client
    receive :: STM o,
    -- Send to the client
    send :: i -> STM ()
  }

instance Profunctor Queues where
  dimap f g (Queues {receive, send}) =
    Queues
      { receive = g <$> receive,
        send = send . f
      }

withQueues :: forall i o m a. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Natural -> Natural -> Connection -> (Queues i o -> m a) -> m (Either ConnectionException a)
withQueues inputBuffer outputBuffer conn action = Ki.scoped $ \scope -> do
  receiveQ <- liftIO $ newTBQueueIO inputBuffer
  sendQ <- liftIO $ newTBQueueIO outputBuffer
  connectionClosedMVar <- liftIO $ newEmptyTMVarIO
  let receive = do readTBQueue receiveQ
  let send msg = writeTBQueue sendQ msg

  let triggerClose :: forall n. (MonadIO n) => (Maybe ConnectionException) -> n ()
      triggerClose mayErr = do
        newlyClosed <- atomically $ do
          tryPutTMVar connectionClosedMVar mayErr
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
    recvWorker :: (Maybe ConnectionException -> m ()) -> TBQueue o -> m ()
    recvWorker triggerClose q = UnliftIO.handle (handler triggerClose) $ do
      msg <- liftIO $ receiveData conn
      atomically $ writeTBQueue q msg
      recvWorker triggerClose q

    handler :: (Maybe ConnectionException -> m ()) -> ConnectionException -> m ()
    handler triggerClose = \case
      CloseRequest {} -> do
        -- The client requested a close, we can just close normally.
        triggerClose Nothing
      -- Other cases are exceptional
      err -> triggerClose (Just err)

    sendWorker :: (Maybe ConnectionException -> m ()) -> TBQueue i -> m ()
    sendWorker triggerClose q = UnliftIO.handle (handler triggerClose) $ do
      outMsgs <- atomically $ some $ readTBQueue q
      liftIO $ sendBinaryDatas conn outMsgs
      sendWorker triggerClose q

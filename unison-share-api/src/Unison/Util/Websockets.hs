{-# LANGUAGE KindSignatures #-}

module Unison.Util.Websockets
  ( withQueues,
    Queues (..),
  )
where

import Control.Applicative
import Control.Concurrent.STM.TBMQueue
import Control.Lens (Profunctor (..))
import Control.Monad
import Data.Text (Text)
import Ki.Unlifted qualified as Ki
import Network.WebSockets
import UnliftIO

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

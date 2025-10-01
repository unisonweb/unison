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
    send :: i -> STM (),
    shutdown :: IO (),
    isConnectionClosed :: STM Bool
  }

instance Profunctor Queues where
  dimap f g (Queues {receive, send, shutdown, isConnectionClosed}) =
    Queues
      { receive = g <$> receive,
        send = send . f,
        shutdown,
        isConnectionClosed
      }

withQueues :: forall i o m a. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Natural -> Natural -> Connection -> (Queues i o -> m a) -> m a
withQueues inputBuffer outputBuffer conn action = Ki.scoped $ \scope -> do
  receiveQ <- liftIO $ newTBQueueIO inputBuffer
  sendQ <- liftIO $ newTBQueueIO outputBuffer
  isClosedVar <- liftIO $ newTVarIO False
  let receive = do readTBQueue receiveQ
  let send msg = writeTBQueue sendQ msg

  let triggerClose :: IO ()
      triggerClose = do
        alreadyClosed <- atomically $ do
          isClosed <- readTVar isClosedVar
          when (not isClosed) $ do
            writeTVar isClosedVar True
            pure ()
          pure isClosed
        when (not alreadyClosed) $ do
          sendClose conn ("Server is shutting down" :: Text)

  let queues = Queues {receive, send, shutdown = triggerClose, isConnectionClosed = readTVar isClosedVar}
  _ <- Ki.fork scope $ recvWorker receiveQ
  _ <- Ki.fork scope $ sendWorker sendQ
  r <- action queues
  liftIO $ triggerClose
  pure r
  where
    recvWorker :: TBQueue o -> m ()
    recvWorker q = UnliftIO.handle handler $ do
      msg <- liftIO $ receiveData conn
      atomically $ writeTBQueue q msg
      recvWorker q

    handler :: ConnectionException -> m ()
    handler = \case
      CloseRequest _ _ -> liftIO $ sendClose conn ("Client closed connection" :: Text)
      ConnectionClosed -> pure ()
      err -> throwIO err

    sendWorker :: TBQueue i -> m ()
    sendWorker q = UnliftIO.handle handler $ do
      outMsgs <- atomically $ some $ readTBQueue q
      liftIO $ sendBinaryDatas conn outMsgs
      sendWorker q

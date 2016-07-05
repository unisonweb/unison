{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric #-}

module Unison.Runtime.Multiplex where

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)
import Data.Bytes.Serial (Serial(serialize,deserialize))
import Data.IORef
import GHC.Generics
import qualified Control.Concurrent as C
import qualified Crypto.Random as Random
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified STMContainers.Map as M
-- import Control.Concurrent.STM

data Packet =
  Packet { destination :: B.ByteString, replyTo :: Maybe B.ByteString, content :: B.ByteString }
  deriving (Generic)
instance Serial Packet

newtype Callbacks = Callbacks (M.Map B.ByteString (B.ByteString -> IO ()))

newtype Multiplex a = Multiplex (ReaderT (Packet -> IO (),Callbacks,IO B.ByteString) IO a)
  deriving (Applicative, Functor, Monad, MonadIO)

run :: (Packet -> IO (), Callbacks, IO B.ByteString) -> Multiplex a -> IO a
run env (Multiplex go) = runReaderT go env

runLoop :: IO (Maybe Packet) -> (Packet -> IO (), Callbacks, IO B.ByteString) -> Multiplex () -> IO ()
runLoop recv env@(_, Callbacks cbs, _) a = do
  _ <- C.forkIO (run env a)
  repeatWhile $ do
    packet <- recv
    case packet of
      Nothing -> pure False
      Just (Packet destination _ content) -> do
        callback <- atomically $ M.lookup destination cbs
        case callback of
          Nothing -> pure True
          Just callback -> callback content >> pure True

repeatWhile :: Monad f => f Bool -> f ()
repeatWhile action = do
  ok <- action
  when ok (repeatWhile action)

-- todo: main loop, takes an `IO (Maybe Packet)`

env0 :: IO (Packet -> IO (), Callbacks, IO B.ByteString)
env0 = undefined

uniqueChannel :: IO (IO B.ByteString)
uniqueChannel = do
  nonce <- newIORef (0 :: Word)
  rng <- newIORef =<< Random.getSystemDRG
  pure $ do
    n <- atomicModifyIORef' nonce (\n -> (n+1,n))
    (bytes,rng') <- Random.randomBytesGenerate 12 <$> readIORef rng
    _ <- atomicModifyIORef' rng (\_ -> (rng',rng'))
    pure . Put.runPutS $ Put.putByteString (Put.runPutS $ serialize n) >> Put.putByteString bytes

callbacks0 :: STM Callbacks
callbacks0 = Callbacks <$> M.new

data Channel a b = Channel (Type a b) B.ByteString
data Type a b = Type

type Microseconds = Int

requestCancellable :: (Serial a, Serial b) => Channel a b -> a
                   -> Multiplex (Multiplex (Either String b), Multiplex ())
requestCancellable (Channel _ dest) a = do
  (send,Callbacks cbs,fresh) <- Multiplex ask
  replyTo <- liftIO fresh
  result <- liftIO newEmptyMVar
  liftIO . atomically $ M.insert (putMVar result) replyTo cbs
  liftIO $ send (Packet dest (Just replyTo) (Put.runPutS $ serialize a))
  cancel <- pure . liftIO . atomically . M.delete replyTo $ cbs
  force <- pure . liftIO $ do
    bytes <- takeMVar result
    pure $ Get.runGetS deserialize bytes
  pure (force, cancel)

request :: (Serial a, Serial b) => Channel a b -> a -> Multiplex (Either String b)
request chan a = do
  (request, _) <- requestCancellable chan a
  b <- fork request
  pure b

requestTimed :: (Serial a, Serial b) => Microseconds -> Channel a b -> a -> Multiplex (Either String b)
requestTimed micros chan a = do
  (request, cancel) <- requestCancellable chan a
  env <- Multiplex ask
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env cancel
  fork $ request <* liftIO (C.killThread watchdog)

requestTimed' :: (Serial a, Serial b) => Microseconds -> Channel a b -> a -> Multiplex b
requestTimed' micros chan a = do
  e <- requestTimed micros chan a
  either fail pure e

request' :: (Serial a, Serial b) => Channel a b -> a -> Multiplex b
request' chan a = do
  eb <- fork $ request chan a
  either fail pure eb

fork :: Multiplex a -> Multiplex a
fork m = do
  result <- liftIO newEmptyMVar
  env <- Multiplex ask
  _ <- liftIO . C.forkIO $ do
    a <- run env m
    putMVar result a
  liftIO (takeMVar result)

send :: Serial a => Channel a b -> a -> Multiplex ()
send (Channel _ key) a = do
  (send,_,_) <- Multiplex ask
  liftIO $ send (Packet key Nothing (Put.runPutS (serialize a)))

reply :: Serial b => Channel a b -> b -> Multiplex ()
reply (Channel _ key) b = send (Channel Type key) b

receiveCancellable :: Serial b => Channel a b -> Multiplex (Multiplex (Either String b), Multiplex ())
receiveCancellable (Channel _ key) = do
  (_,Callbacks cbs,_) <- Multiplex ask
  result <- liftIO newEmptyMVar
  liftIO . atomically $ M.insert (putMVar result) key cbs
  cancel <- pure . liftIO . atomically . M.delete key $ cbs
  force <- pure . liftIO $ do
    bytes <- takeMVar result
    pure $ Get.runGetS deserialize bytes
  pure (force, cancel)

receiveTimed :: Serial b => Microseconds -> Channel a b -> Multiplex (Either String b)
receiveTimed micros chan = do
  (force, cancel) <- receiveCancellable chan
  env <- Multiplex ask
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env cancel
  fork $ force <* liftIO (C.killThread watchdog)

subscribe :: Serial b => Channel a b -> Multiplex (Multiplex (Either String b), IO ())
subscribe (Channel _ key) = do
  (_, Callbacks cbs, _) <- Multiplex ask
  q <- liftIO . atomically $ newTQueue
  liftIO . atomically $ M.insert (atomically . writeTQueue q) key cbs
  unsubscribe <- pure . liftIO . atomically . M.delete key $ cbs
  force <- pure . liftIO $ do
    bytes <- atomically $ readTQueue q
    pure $ Get.runGetS deserialize bytes
  pure (force, unsubscribe)


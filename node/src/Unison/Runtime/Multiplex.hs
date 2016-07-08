{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TupleSections #-}

module Unison.Runtime.Multiplex where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT,runReaderT,MonadReader)
import Data.Bytes.Serial (Serial(serialize,deserialize))
import Data.Functor
import Data.IORef
import Data.Maybe
import GHC.Generics
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Reader as Reader
import qualified Crypto.Random as Random
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified ListT as ListT
import qualified STMContainers.Map as M
-- import Control.Concurrent.STM

data Packet = Packet { destination :: B.ByteString, content :: B.ByteString } deriving (Generic)
instance Serial Packet

type IsSubscription = Bool

newtype Callbacks = Callbacks (M.Map B.ByteString (IsSubscription, B.ByteString -> IO ()))

type Env = (STM Packet -> STM (), Callbacks, IO B.ByteString)

newtype Multiplex a = Multiplex (ReaderT Env IO a)
  deriving (Applicative, Alternative, Functor, Monad, MonadIO, MonadPlus, MonadReader Env)

run :: Env -> Multiplex a -> IO a
run env (Multiplex go) = runReaderT go env

ask :: Multiplex Env
ask = Multiplex Reader.ask

process :: IO (Maybe Packet) -> Multiplex ()
process recv = do
  (_, Callbacks cbs, _) <- ask
  liftIO . repeatWhile $ do
    packet <- recv
    case packet of
      Nothing -> pure False
      Just (Packet destination content) -> do
        callback <- atomically $ M.lookup destination cbs
        case callback of
          Nothing -> pure True
          Just (_, callback) -> callback content >> pure True

shutdown :: Multiplex ()
shutdown = do
  (_, Callbacks cbs, _) <- ask
  liftIO . atomically $ do
    -- todo: may want to do this with uncons, to avoid needing to snapshot entire map
    entries <- ListT.toList (M.stream cbs)
    forM_ entries $ \(_,(isSub,_)) ->
      if isSub then pure ()
      else STM.retry -- it's an active request, wait for it to complete

repeatWhile :: Monad f => f Bool -> f ()
repeatWhile action = do
  ok <- action
  when ok (repeatWhile action)

untilDefined :: Monad f => f (Maybe a) -> f a
untilDefined action = do
  ok <- action
  case ok of
    Nothing -> untilDefined action
    Just a -> pure a

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

data Channel a = Channel (Type a) B.ByteString deriving Generic

newtype EncryptedChannel u a = EncryptedChannel (Channel B.ByteString) deriving Generic
instance Serial (EncryptedChannel u a)

erase :: EncryptedChannel u a -> Channel B.ByteString
erase (EncryptedChannel chan) = chan

instance Serial (Channel a)

data Type a = Type deriving Generic
instance Serial (Type a)

type Request a b = Channel (a, Channel (Maybe b))

type Microseconds = Int

requestTimed' :: (Serial a, Serial b) => Microseconds -> Request a b -> STM a -> Multiplex b
requestTimed' micros req a = do
  replyTo <- channel
  env <- ask
  (receive, cancel) <- receiveCancellable replyTo
  send' req $ (,replyTo) <$> a
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env cancel
  fork $ receive <* liftIO (C.killThread watchdog)

requestTimed :: (Serial a, Serial b) => Microseconds -> Request a b -> a -> Multiplex b
requestTimed micros req a = do
  replyTo <- channel
  env <- ask
  (receive, cancel) <- receiveCancellable replyTo
  send req (a, replyTo)
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env cancel
  fork $ receive <* liftIO (C.killThread watchdog)

fork :: Multiplex a -> Multiplex a
fork m = do
  result <- liftIO newEmptyMVar
  env <- ask
  _ <- liftIO . C.forkIO $ do
    a <- run env m
    putMVar result a
  liftIO (takeMVar result)

nest :: Serial k => k -> Multiplex a -> Multiplex a
nest outer m = Reader.local tweak m where
  tweak (send,cbs,fresh) = (send' send,cbs,fresh)
  kbytes = Put.runPutS (serialize outer)
  send' send p = send $ (\p -> Packet kbytes (Put.runPutS (serialize p))) <$> p

channel :: Multiplex (Channel a)
channel = do
  (_,_,fresh) <- ask
  Channel Type <$> liftIO fresh

send :: Serial a => Channel a -> a -> Multiplex ()
send chan a = send' chan (pure a)

send' :: Serial a => Channel a -> STM a -> Multiplex ()
send' (Channel _ key) a = do
  (send,_,_) <- ask
  liftIO . atomically $ send (Packet key . Put.runPutS . serialize <$> a)

receiveCancellable :: Serial a => Channel (Maybe a) -> Multiplex (Multiplex a, Multiplex ())
receiveCancellable (Channel _ key) = do
  (_,Callbacks cbs,_) <- ask
  result <- liftIO newEmptyMVar
  liftIO . atomically $ M.insert (False, putMVar result) key cbs
  cancel <- pure $ do
    cb <- liftIO . atomically $ M.lookup key cbs <* M.delete key cbs
    case cb of
      Nothing -> pure ()
      Just (_,cb) -> liftIO $ cb (Put.runPutS (serialize (Nothing :: Maybe Int)))
  force <- pure . liftIO $ do
    bytes <- takeMVar result
    case Get.runGetS deserialize bytes of
      Left err -> fail err
      Right Nothing -> fail "cancelled"
      Right (Just a) -> pure a
  pure (force, cancel)

receiveTimed :: Serial a => Microseconds -> Channel (Maybe a) -> Multiplex a
receiveTimed micros chan = do
  (force, cancel) <- receiveCancellable chan
  env <- ask
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env cancel
  fork $ force <* liftIO (C.killThread watchdog)

timeout' :: Microseconds -> a -> Multiplex a -> Multiplex a
timeout' micros onTimeout m = fromMaybe onTimeout <$> timeout micros m

timeout :: Microseconds -> Multiplex a -> Multiplex (Maybe a)
timeout micros m = do
  env <- ask
  t1 <- liftIO $ Async.async (Just <$> run env m)
  t2 <- liftIO $ Async.async (C.threadDelay micros $> Nothing)
  liftIO $ snd <$> Async.waitAnyCancel [t1, t2]

subscribeTimed :: Serial a => Microseconds -> Channel a -> Multiplex (Multiplex (Maybe a), Multiplex ())
subscribeTimed micros chan = do
  (fetch, cancel) <- subscribe chan
  env <- ask
  activity <- liftIO . atomically . newTVar $ False
  alive <- liftIO . atomically . newTVar $ True
  fetch' <- pure $ do
    liftIO . atomically $ writeTVar activity True
    ok <- liftIO $ readTVarIO alive
    case ok of
      True -> Just <$> fetch
      False -> pure Nothing
  let cleanup = cancel >> (liftIO . atomically . writeTVar alive $ False)
  watchdog <- liftIO . C.forkIO . run env $ loop activity cleanup
  cancel' <- pure $ cleanup >> liftIO (C.killThread watchdog)
  pure (fetch', cancel')
  where
  loop activity cleanup = do
    liftIO . atomically $ writeTVar activity False
    liftIO $ C.threadDelay micros
    active <- liftIO . atomically $ readTVar activity
    case active of
      False -> cleanup -- no new fetches in last micros period
      True -> loop activity cleanup

subscribe :: Serial a => Channel a -> Multiplex (Multiplex a, Multiplex ())
subscribe (Channel _ key) = do
  (_, Callbacks cbs, _) <- ask
  q <- liftIO . atomically $ newTQueue
  liftIO . atomically $ M.insert (True, atomically . writeTQueue q) key cbs
  unsubscribe <- pure . liftIO . atomically . M.delete key $ cbs
  force <- pure . liftIO $ do
    bytes <- atomically $ readTQueue q
    either fail pure $ Get.runGetS deserialize bytes
  pure (force, unsubscribe)

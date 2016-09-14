{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TupleSections #-}

module Unison.Runtime.Multiplex where

import Control.Applicative
import Control.Concurrent.Async (Async)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Exception (catch,throwIO,SomeException,mask_)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT,runReaderT,MonadReader,local)
import Data.Bytes.Serial (Serial(serialize,deserialize))
import Data.Functor
import Data.IORef
import Data.Maybe
import Data.Word
import GHC.Generics
import qualified Data.ByteString.Base64.URL as Base64
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Reader as Reader
import qualified Crypto.Random as Random
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified STMContainers.Map as M
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Queue as Q
import qualified Unison.Util.Logger as L

data Packet = Packet { destination :: !B.ByteString, content :: !B.ByteString } deriving (Generic)
instance Serial Packet

instance Show Packet where
  show (Packet d c) =
    show $ (Base64.encode d, Base64.encode . BA.convert $ (Hash.hash c :: Hash.Digest Hash.SHA1))

type IsSubscription = Bool

data Callbacks =
  Callbacks (M.Map B.ByteString (B.ByteString -> IO ())) (TVar Word64)

type Env =
  ( STM Packet -> STM ()
  , Callbacks
  , IO B.ByteString
  , M.Map B.ByteString (Multiplex B.ByteString)
  , L.Logger)

newtype Multiplex a = Multiplex (ReaderT Env IO a)
  deriving (Applicative, Alternative, Functor, Monad, MonadIO, MonadPlus, MonadReader Env)

env0 :: L.Logger -> IO (Env, Maybe Packet -> IO (), IO (Maybe Packet), STM Bool)
env0 logger = do
  fresh <- uniqueChannel
  output <- atomically Q.empty :: IO (Q.Queue (Maybe Packet))
  input <- atomically newTQueue :: IO (TQueue (Maybe Packet))
  cb0@(Callbacks m _) <- Callbacks <$> atomically M.new <*> atomically (newTVar 0)
  recvs0 <- atomically M.new
  let env = (Q.enqueue output . (Just <$>), cb0, fresh, recvs0, logger)
      isActive = (||) <$> (not <$> M.null m) <*> (not <$> M.null recvs0)
  _ <- run env (fork $ process (atomically (readTQueue input)))
  pure ( env
       , atomically . writeTQueue input
       , atomically $ Q.dequeue output
       , isActive )

run :: Env -> Multiplex a -> IO a
run env (Multiplex go) = runReaderT go env

liftLogged :: String -> IO a -> Multiplex a
liftLogged msg action = ask >>= \env -> liftIO $ catch action (handle env) where
  handle :: Env -> SomeException -> IO a
  handle env ex = run env (warn $ msg ++ " " ++ show ex) >> throwIO ex

ask :: Multiplex Env
ask = Multiplex Reader.ask

logger :: Multiplex L.Logger
logger = do
  ~(_, _, _, _, logger) <- ask
  pure logger

scope :: String -> Multiplex a -> Multiplex a
scope msg = local tweak where
  tweak (a,b,c,d,logger) = (a,b,c,d,L.scope msg logger)

-- | Crash with a message. Include the current logging scope.
crash :: String -> Multiplex a
crash msg = scope msg $ do
  l <- logger
  fail (show $ L.getScope l)

info, warn, debug :: String -> Multiplex ()
info msg = logger >>= \logger -> liftIO $ L.info logger msg
warn msg = logger >>= \logger -> liftIO $ L.warn logger msg
debug msg = logger >>= \logger -> liftIO $ L.debug logger msg

process :: IO (Maybe Packet) -> Multiplex ()
process recv = scope "Mux.process" $ do
  (_, Callbacks cbs _, _, _, logger) <- ask
  liftIO . repeatWhile $ do
    packet <- recv
    case packet of
      Nothing -> L.info logger "EOF" $> False
      Just (Packet destination content) -> do
        callback <- atomically $ M.lookup destination cbs
        case callback of
          Nothing -> do
            L.warn logger $ "dropped packet @ " ++ show (Base64.encode destination)
            pure True
          Just callback -> do
            L.warn logger $ "packet delivered @ " ++ show (Base64.encode destination)
            callback content
            pure True

process1 :: Packet -> Multiplex ()
process1 pk = liftIO (newIORef (Just pk)) >>= \ref -> process $ do
  pk <- readIORef ref
  case pk of
    Just _ -> writeIORef ref Nothing $> pk
    Nothing -> pure Nothing

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
callbacks0 = Callbacks <$> M.new <*> newTVar 0

data Channel a = Channel (Type a) B.ByteString deriving Generic
instance Show (Channel a) where
  show = show . Base64.encode . channelId

newtype EncryptedChannel u o i = EncryptedChannel (Channel B.ByteString) deriving Generic
instance Serial (EncryptedChannel u o i)

erase :: EncryptedChannel u o i -> Channel B.ByteString
erase (EncryptedChannel chan) = chan

channelId :: Channel a -> B.ByteString
channelId (Channel _ id) = id

instance Serial (Channel a)

data Type a = Type deriving Generic
instance Serial (Type a)

type Request a b = Channel (a, Channel b)

type Microseconds = Int

requestTimedVia' :: (Serial a, Serial b)
                 => String
                 -> Microseconds
                 -> (STM (a, Channel b) -> Multiplex ())
                 -> Channel b
                 -> STM a
                 -> Multiplex (Multiplex b)
requestTimedVia' msg micros send replyTo a = do
  env <- ask
  (receive, cancel) <- receiveCancellable replyTo
  send $ (,replyTo) <$> a
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env (cancel $ "requestTimedVia timeout " ++ msg)
  pure $ receive <* liftIO (C.killThread watchdog)

requestTimedVia :: (Serial a, Serial b) => String -> Microseconds -> Request a b -> Channel b -> STM a
                -> Multiplex (Multiplex b)
requestTimedVia msg micros req replyTo a =
  requestTimedVia' msg micros (send' req) replyTo a

requestTimed' :: (Serial a, Serial b) => String -> Microseconds -> Request a b -> STM a -> Multiplex (Multiplex b)
requestTimed' msg micros req a = do
  replyTo <- channel
  requestTimedVia msg micros req replyTo a

requestTimed :: (Serial a, Serial b) => String -> Microseconds -> Request a b -> a -> Multiplex (Multiplex b)
requestTimed msg micros req a = do
  replyTo <- channel
  env <- ask
  (receive, cancel) <- receiveCancellable replyTo
  send req (a, replyTo)
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env (cancel $ "requestTimed timeout " ++ msg)
  pure $ receive <* liftIO (C.killThread watchdog) <* cancel ("requestTimed completed")

type Cleartext = B.ByteString
type Ciphertext = B.ByteString
type CipherState = (Cleartext -> STM Ciphertext, Ciphertext -> STM Cleartext)

encryptedRequestTimedVia
  :: (Serial a, Serial b)
  => String
  -> CipherState
  -> Microseconds
  -> ((a,Channel b) -> Multiplex ())
  -> Channel b
  -> a
  -> Multiplex b
encryptedRequestTimedVia msg (_,decrypt) micros send replyTo@(Channel _ bs) a = do
  responseCiphertext <- receiveTimed msg micros (Channel Type bs)
  send (a, replyTo)
  responseCiphertext <- responseCiphertext -- force the receive
  responseCleartext <- liftIO . atomically . decrypt $ responseCiphertext
  either crash pure $ Get.runGetS deserialize responseCleartext

encryptAndSendTo
  :: (Serial a, Serial node)
  => node -> Channel B.ByteString -> (Cleartext -> STM Ciphertext) -> a
  -> Multiplex ()
encryptAndSendTo recipient chan encrypt a = do
  let !bytes = Put.runPutS (serialize a)
  nest recipient (send' chan (encrypt bytes))

encryptAndSendTo'
  :: (Serial a, Serial node)
  => node -> Channel a -> (Cleartext -> STM Ciphertext) -> a
  -> Multiplex ()
encryptAndSendTo' recipient (Channel _ chan) encrypt a =
  encryptAndSendTo recipient (Channel Type chan) encrypt a

fork :: Multiplex a -> Multiplex (Async a)
fork m = do
  env <- ask
  liftIO . Async.async $ run env m

nest :: Serial k => k -> Multiplex a -> Multiplex a
nest outer m = Reader.local tweak m where
  tweak (send,cbs,fresh,recvs,log) = (send' send,cbs,fresh,recvs,log)
  kbytes = Put.runPutS (serialize outer)
  send' send p = send $ (\p -> Packet kbytes (Put.runPutS (serialize p))) <$> p

channel :: Multiplex (Channel a)
channel = do
  ~(_,_,fresh,_,_) <- ask
  Channel Type <$> liftIO fresh

send :: Serial a => Channel a -> a -> Multiplex ()
send chan a = send' chan (pure a)

send' :: Serial a => Channel a -> STM a -> Multiplex ()
send' (Channel _ key) a = do
  ~(send,_,_,_,_) <- ask
  liftIO . atomically $ send (Packet key . Put.runPutS . serialize <$> a)

receiveCancellable :: Serial a => Channel a -> Multiplex (Multiplex a, String -> Multiplex ())
receiveCancellable (Channel _ key) = do
  (_,Callbacks cbs _,_,_,_) <- ask
  result <- liftIO newEmptyMVar
  liftIO . atomically $ M.insert (putMVar result . Right) key cbs
  cancel <- pure $ \reason -> do
    liftIO . atomically $ M.delete key cbs
    liftIO $ putMVar result (Left $ "Mux.cancelled: " ++ reason)
  force <- pure . scope "receiveCancellable" $ do
    bytes <- liftIO $ takeMVar result
    bytes <- either crash pure bytes
    either crash pure $ Get.runGetS deserialize bytes
  pure (force, cancel)

receiveTimed :: Serial a => String -> Microseconds -> Channel a -> Multiplex (Multiplex a)
receiveTimed msg micros chan = do
  (force, cancel) <- receiveCancellable chan
  env <- ask
  watchdog <- liftIO . C.forkIO $ do
    liftIO $ C.threadDelay micros
    run env (cancel $ "receiveTimed timeout during " ++ msg)
  pure $ scope "receiveTimed" (force <* liftIO (C.killThread watchdog) <* cancel ("receiveTimed completed" ++ msg))

-- Save a receive future as part of
saveReceive :: Microseconds
            -> B.ByteString -> Multiplex B.ByteString -> Multiplex ()
saveReceive micros chan force = do
  (_,_,_,recvs,_) <- ask
  tid <- liftIO . C.forkIO $ do
    C.threadDelay micros
    atomically $ M.delete chan recvs
  let force' = do
        liftIO $ C.killThread tid
        liftIO $ atomically (M.delete chan recvs)
        force
  liftIO . atomically $ M.insert force' chan recvs

restoreReceive :: B.ByteString -> Multiplex B.ByteString
restoreReceive chan = do
  (_,_,_,recvs,_) <- ask
  o <- liftIO . atomically $ M.lookup chan recvs
  fromMaybe (crash $ "chan could not be restored: " ++ show (Base64.encode chan))
            o

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
  result <- liftIO . atomically $ newEmptyTMVar
  activity <- liftIO . atomically . newTVar $ False
  fetch' <- pure $ do
    void . fork $ do
      r <- fetch
      liftIO . atomically $ do
        putTMVar result (Just r)
        writeTVar activity True
    liftIO . atomically $ takeTMVar result
  watchdog <- do
    env <- ask
    l <- logger
    liftIO . C.forkIO $ loop l activity result (run env cancel)
  cancel' <- pure $ cancel >> liftIO (C.killThread watchdog)
  pure (fetch', cancel')
  where
  loop logger activity result cancel = do
    atomically $ writeTVar activity False
    C.threadDelay micros
    active <- atomically $ readTVar activity
    case active of
      False -> do
        L.debug logger $ "timed out on " ++ show chan
        void $ atomically (tryPutTMVar result Nothing) <* cancel
        L.debug logger $ "cancelled subscription to " ++ show chan
      True -> do
        L.trace logger $ "still activity on " ++ show chan
        loop logger activity result cancel

subscribe :: Serial a => Channel a -> Multiplex (Multiplex a, Multiplex ())
subscribe (Channel _ key) = scope "subscribe" $ do
  (_, Callbacks cbs _, _, _, _) <- ask
  q <- liftIO . atomically $ newTQueue
  liftIO . atomically $ M.insert (atomically . writeTQueue q) key cbs
  unsubscribe <- pure . liftIO . atomically . M.delete key $ cbs
  force <- pure $ do
    bytes <- liftIO . atomically $ readTQueue q
    either crash pure $ Get.runGetS deserialize bytes
  pure (force, unsubscribe)

seconds :: Microseconds -> Int
seconds micros = micros * 1000000

attemptMasked :: IO a -> IO (Either String a)
attemptMasked a =
  catch (Right <$> mask_ a) (\e -> pure (Left $ show (e :: SomeException)))

handshakeTimeout :: Microseconds
handshakeTimeout = seconds 5

connectionTimeout :: Microseconds
connectionTimeout = seconds 20

delayBeforeFailure :: Microseconds
delayBeforeFailure = seconds 2

pipeInitiate
  :: (Serial i, Serial o, Serial key, Serial u, Serial node)
  => C.Cryptography key t1 t2 t3 t4 t5 Cleartext
  -> EncryptedChannel u o i
  -> (node,key)
  -> u
  -> Multiplex (Maybe o -> Multiplex (), Multiplex (Maybe i), CipherState)
pipeInitiate crypto rootChan (recipient,recipientKey) u = scope "pipeInitiate" $ do
  info "starting"
  (doneHandshake, encrypt, decrypt) <- liftIO $ C.pipeInitiator crypto recipientKey
  handshakeChan <- channel
  connectedChan <- channel
  handshakeSub <- scope "handshakeSub" $ subscribeTimed handshakeTimeout handshakeChan
  connectedSub <- scope "connectedSub" $ subscribeTimed connectionTimeout connectedChan
  let chans = (handshakeChan,connectedChan)
  info $ "handshake channels " ++ show chans
  handshake doneHandshake encrypt decrypt chans handshakeSub connectedSub
  where
  handshake doneHandshake encrypt decrypt cs@(chanh,chanc) (fetchh,cancelh) (fetchc,cancelc) =
    encryptAndSendTo recipient (erase rootChan) encrypt (u,cs) >>
    fetchh >> -- sync packet, ignored, but lets us know recipient is listening
    go
    where
    recv = untilDefined $ do
      bytes <- fetchc
      debug $ "recv " ++ show (B.length <$> bytes)
      case bytes of
        Nothing -> pure (Just Nothing)
        Just bytes -> do
          decrypted <- liftIO . atomically $ decrypt bytes
          case Get.runGetS deserialize decrypted of
            Left err -> info err >> pure Nothing
            Right mi -> pure (Just mi)
    go = do
      ready <- liftIO $ atomically doneHandshake
      debug $ "ready: " ++ show ready
      case ready of
        True -> do
          info "handshake complete"
          -- encryptAndSendTo recipient chanh encrypt () -- todo: not sure this flush needed
          pure (encryptAndSendTo recipient chanc encrypt, recv, (encrypt,decrypt))
        False -> do
          debug "handshake round trip... "
          nest recipient $ send' chanh (encrypt B.empty)
          bytes <- fetchh
          debug "... handshake round trip completed"
          case bytes of
            Nothing -> cancelh >> cancelc >> crash "cancelled handshake"
            Just bytes -> liftIO (atomically $ decrypt bytes) >> go

-- todo: add access control here, better to bail ASAP (or after 1s delay
-- to discourage sniffing for nodes with access) rather than continuing with
-- handshake if we know we can't accept messages from that party
pipeRespond
  :: (Serial o, Serial i, Serial u, Serial node)
  => C.Cryptography key t1 t2 t3 t4 t5 Cleartext
  -> (key -> Multiplex Bool)
  -> EncryptedChannel u i o
  -> (u -> node)
  -> B.ByteString
  -> Multiplex (key, u, Maybe o -> Multiplex (), Multiplex (Maybe i), CipherState)
pipeRespond crypto allow _ extractSender payload = do
  (doneHandshake, senderKey, encrypt, decrypt) <- liftIO $ C.pipeResponder crypto
  debug $ "decrypting initial payload"
  bytes <- (liftLogged "[Mux.pipeRespond] decrypt" . atomically . decrypt) payload
  (u, chans@(handshakeChan,connectedChan)) <- either crash pure $ Get.runGetS deserialize bytes
  debug $ "handshake channels: " ++ show chans
  let sender = extractSender u
  handshakeSub <- subscribeTimed handshakeTimeout handshakeChan
  connectedSub <- subscribeTimed connectionTimeout connectedChan
  ok <- liftIO $ C.randomBytes crypto 8
  nest sender $ send (Channel Type $ channelId handshakeChan) ok
  handshake doneHandshake senderKey encrypt decrypt chans handshakeSub connectedSub sender u
  where
  handshake doneHandshake senderKey encrypt decrypt (chanh,chanc) (fetchh,cancelh) (fetchc,cancelc) sender u = go
    where
    recv = untilDefined $ do
      bytes <- fetchc
      case bytes of
        Nothing -> pure (Just Nothing)
        Just bytes -> do
          decrypted <- liftIO . atomically $ decrypt bytes
          case Get.runGetS deserialize decrypted of
            Left err -> info err >> pure Nothing
            Right mi -> pure (Just mi)
    checkSenderKey = do
      senderKey <- liftIO $ atomically senderKey
      case senderKey of
        Nothing -> pure ()
        Just senderKey -> allow senderKey >>= \ok ->
          if ok then pure ()
          else liftIO (C.threadDelay delayBeforeFailure) >> crash "disallowed key"
    go = do
      ready <- liftIO $ atomically doneHandshake
      checkSenderKey
      case ready of
        True -> do
          -- encryptAndSendTo sender chanh encrypt () -- todo: not sure this flush needed
          Just senderKey <- liftIO $ atomically senderKey
          info $ "completed and listening on " ++ show chanc
          pure (senderKey, u, encryptAndSendTo sender chanc encrypt, recv, (encrypt,decrypt))
        False -> do
          nest sender $ send' chanh (encrypt B.empty)
          bytes <- fetchh
          case bytes of
            Nothing -> cancelh >> cancelc >> crash "cancelled handshake"
            Just bytes -> liftIO (atomically $ decrypt bytes) >> go

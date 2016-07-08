{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Runtime.Remote where

-- import qualified Data.Set as Set
import Data.Functor
import Control.Monad
-- import Control.Applicative
import Control.Concurrent.STM (atomically)
import Control.Exception (catch,SomeException,mask_)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial,serialize,deserialize)
import Data.Set (Set)
import GHC.Generics
import Unison.Remote hiding (seconds)
import Unison.Remote.Extra ()
import Unison.Runtime.Multiplex (Multiplex)
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Set as Set
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Multiplex as Mux

data Language t h
  = Language
    { localDependencies :: t -> Set h
    , eval :: t -> IO t
    , apply :: t -> t -> t
    , node :: Node -> t
    , unit :: t
    , channel :: Channel -> t
    , local :: Local t -> t
    , unRemote :: t -> Maybe (Remote t)
    , remote :: Remote t -> t }

data Env t h
  = Env { saveHashes :: [(h,t)] -> IO ()
        , getHashes :: Set h -> IO [(h,t)]
        , missingHashes :: Set h -> IO (Set h)
        , universe :: Universe
        , currentNode :: Node }

instance Serial Universe

type Cleartext = ByteString

info :: String -> Multiplex ()
info msg = liftIO (putStrLn msg)

seconds :: Mux.Microseconds -> Int
seconds micros = micros * 1000000

attemptMasked :: IO a -> IO (Either String a)
attemptMasked a =
  catch (Right <$> mask_ a) (\e -> pure (Left $ show (e :: SomeException)))

handshakeInitiate
  :: (Serial a, Serial key, Serial u)
  => C.Cryptography key symmetricKey signKey signature hash Cleartext
  -> Mux.EncryptedChannel u a
  -> Node
  -> u
  -> Multiplex (Maybe a -> Multiplex ())
handshakeInitiate crypto rootChan recipient u = do
  recipientKey <- either fail pure $ Get.runGetS deserialize (publicKey recipient)
  (doneHandshake, encrypt, decrypt) <- liftIO $ C.pipeInitiator crypto recipientKey
  chan <- Mux.channel
  sub <- Mux.subscribeTimed (seconds 5) chan
  handshake doneHandshake encrypt decrypt chan recipient sub
  where
  handshake doneHandshake encrypt decrypt chan recipient (fetch,cancel) =
    encryptAndSendTo' rootChan u chan >> go
    where
    go = do
      ready <- liftIO $ atomically doneHandshake
      if ready then cancel $> encryptAndSendTo chan
      else do
        Mux.nest recipient $ Mux.send' chan (encrypt B.empty)
        bytes <- fetch
        case bytes of
          Nothing -> cancel $> (\_ -> fail "cancelled handshake")
          Just bytes -> liftIO (atomically $ decrypt bytes) >> go
    encryptAndSendTo :: Serial a => Mux.Channel B.ByteString -> a -> Multiplex ()
    encryptAndSendTo chan a = do
      let bytes = Put.runPutS (serialize a)
      bytes `seq` Mux.nest recipient (Mux.send' chan (encrypt bytes))
    encryptAndSendTo' :: Serial u
                      => Mux.EncryptedChannel u a -> u -> Mux.Channel B.ByteString -> Multiplex ()
    encryptAndSendTo' (Mux.EncryptedChannel chan) u reply = encryptAndSendTo chan (u,reply)

-- todo: add access control here, better to bail ASAP (or after 1s delay
-- to discourage sniffing for nodes with access) rather than continuing with
-- handshake if we know we can't accept messages from that party
handshakeRespond
  :: (Serial a, Serial u)
  => C.Cryptography key symmetricKey signKey signature hash Cleartext
  -> Mux.EncryptedChannel u a
  -> (u -> Node)
  -> B.ByteString
  -> Multiplex (key, u, Multiplex (Maybe a))
handshakeRespond crypto _ extractSender payload = do
  (doneHandshake, senderKey, encrypt, decrypt) <- liftIO $ C.pipeResponder crypto
  bytes <- (liftIO . atomically . decrypt) payload
  (u, chan) <- either fail pure $ Get.runGetS deserialize bytes
  let sender = extractSender u
  sub <- Mux.subscribeTimed (seconds 45) chan
  handshake sender u doneHandshake senderKey encrypt decrypt chan sub
  where
  handshake sender u doneHandshake senderKey encrypt decrypt chan (fetch,cancel) = go Nothing
    where
    go ciphertext = do
      ready <- (liftIO . atomically) doneHandshake
      cleartext <- case ciphertext of
        Nothing -> pure Nothing
        Just ciphertext -> liftIO . atomically . fmap Just . decrypt $ (ciphertext :: B.ByteString)
      case ready of
        -- Keep reading from channel, replying w/ `encrypt B.empty` if handshake incomplete
        False -> do
          Mux.nest (sender :: Node) $ Mux.send' chan (encrypt B.empty)
          go =<< (fetch :: Multiplex (Maybe B.ByteString))
        -- Handshake complete, current and future payloads decoded as `(Maybe (Remote t))`
        True -> do
          Just senderKey <- (liftIO . atomically) senderKey
          read <- pure $
            let
              decode cleartext = case Get.runGetS deserialize cleartext of
                Left err -> info err >> go
                Right a -> pure a
              go = case cleartext of
                Just cleartext -> decode cleartext
                Nothing -> do
                  ciphertext <- fetch
                  case ciphertext of
                    Nothing -> cancel $> Nothing
                    Just ciphertext -> do
                      e <- (liftIO . attemptMasked . atomically . decrypt) ciphertext
                      either (\err -> info err >> go) decode e
            in go
          pure (senderKey, u, read)

server :: Ord h
       => C.Cryptography key symmetricKey signKey signature hash Cleartext
       -> P.Protocol t signature h
       -> Language t h
       -> Env t h
       -> Multiplex ()
server crypto p lang env = do
  (accept, unsubscribe) <- Mux.subscribeTimed (seconds 60) (Mux.erase $ P._eval p)
  go accept unsubscribe
  where
  go accept unsubscribe = do
    payload <- accept
    case payload of
      Nothing -> unsubscribe
      Just payload -> do
        -- (key, senderUniverse, read) <- handshakeRespond crypto payload
        -- hash syncing should be encrypted as well, ideally using the same
        -- connection
        undefined

sync :: (Ord h, Serial key, Serial t, Serial h)
     => C.Cryptography key symmetricKey signKey signature hash Cleartext
     -> Language t h
     -> Env t h
     -> Mux.EncryptedChannel Node ([h], Mux.EncryptedChannel Node [(h,t)])
     -> Node
     -> [h]
     -> Multiplex ()
sync _ _ _ _ _ [] = pure ()
sync crypto lang env chan peer needs = do
  replyChan <- Mux.EncryptedChannel <$> Mux.channel
  request <- handshakeInitiate crypto chan peer (currentNode env)
  (peerKey,u,provide) <- handshakeRespond crypto replyChan id B.empty
  -- guard $ Remote.publicKey peer == Put.runPutS (serialize $ C.publicKey crypto)
  -- todo: verify peer key is what is expected
  id $
    let
      loop needs | Set.null needs = pure ()
      loop needs = do
        request (Just (Set.toList needs, replyChan))
        hashes <- provide
        case hashes of
          Nothing -> fail "expected hashes"
          Just hashes -> do
            liftIO $ saveHashes env hashes
            stillMissing <- traverse (\(_,t) -> liftIO $ missingHashes env (localDependencies lang t))
                                     hashes
            loop (Set.unions stillMissing)
    in loop (Set.fromList needs)

-- Remote term -> Multiplex ()
-- sync sender r
{-

newtype Err = Err String
type Callbacks t h = IORef (Map Channel (IO (), MVar (Result t h)))

callbacks0 :: IO (Callbacks t h)
callbacks0 = newIORef Map.empty

data Result t h = Error Err | Evaluated t | Syncing Channel Node [(h,t)]

data Packet t h
  = Eval Universe Node (Remote t)
  | Need Channel Node (Set h)
  | Provide Channel Node [(h,t)]
  deriving (Show,Generic)
instance (Serial t, Serial h, Ord h, Eq h) => Serial (Packet t h)

-- | Handle a packet. Does not return a meaningful response; it is expected that
-- processing of the packet will cause further progress of the computation either on
-- the current node or elsewhere (for instance, by causing packets to be sent to other nodes).
server :: Ord h => Language t h -> Env t h -> Multiplex ()
server lang env (Provide chan sender hashes) = do
  m <- readIORef (callbacks env)
  case Map.lookup chan m of
    Nothing -> pure ()
    Just (cancelGC, r) -> do
      cancelGC
      atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
      void (tryPutMVar r (Syncing chan sender hashes))
handle _ env (Need chan sender hashes) = do
  sources <- getHashes env hashes
  -- todo: separate error if missing requested hashes
  sendPacketTo env sender (Provide chan (currentNode env) sources)
handle lang env (Eval u sender r) = do
  missingDeps <-
    if (u == universe env) then pure Set.empty
    else do -- might need to fetch some dependencies
      let deps = localDependencies lang (remote lang r)
      needs <- missingHashes env deps
      pure needs
  sync0 missingDeps
  where
  sync0 missingDeps = newChannel >>= \chan -> sync chan missingDeps
  sync _ missingDeps | Set.null missingDeps = afterSync
  sync chan missingDeps = do
    resultMVar <- newEmptyMVar
    gcThread <- Concurrent.forkIO $ do
      Concurrent.threadDelay (floor $ 1000000 * seconds defaultSyncTimeout)
      putMVar resultMVar (Error (Err "Timeout during fetching of dependencies"))
      atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
    atomicModifyIORef' (callbacks env) (\m -> (Map.insert chan (Concurrent.killThread gcThread, resultMVar) m, ()))
    sendPacketTo env sender (Need chan (currentNode env) missingDeps)
    s <- takeMVar resultMVar
    case s of
      Error (Err err) -> fail err
      Evaluated _ -> fail "expected a `Syncing` message or an error, got an `Evaluated`"
      Syncing chan _ hashes -> do
        saveHashes env hashes
        stillMissing <- traverse (\(_,t) -> missingHashes env (localDependencies lang t)) hashes
        sync chan (Set.unions stillMissing)
  afterSync = case r of
    Step (Local l) -> void $ runLocal l
    Step (At n r) -> transfer n r Nothing
    Bind (At n r) k -> transfer n r (Just k)
    Bind (Local l) k -> do
      arg <- runLocal l
      r <- eval lang (apply lang k arg)
      case (unRemote lang r) of
        Just r -> handle lang env (Eval u sender r)
        Nothing -> fail "typechecker bug; function passed to Remote.bind did not return a Remote"
  newChannel :: IO Channel
  newChannel = Channel <$> randomBytes env 32
  transfer n t k =
    sendPacketTo env n (Eval (universe env) (currentNode env) r) where
      r = case k of
        Nothing -> Step (Local (Pure t))
        Just k -> Bind (Local (Pure t)) k
  runLocal (Fork r) = Concurrent.forkIO (handle lang env (Eval u sender r)) $> unit lang
  runLocal CreateChannel = channel lang <$> newChannel
  runLocal Here = pure $ node lang (currentNode env)
  runLocal (Pure t) = eval lang t
  runLocal (Send chan a) = do
    m <- readIORef (callbacks env)
    case Map.lookup chan m of
      Nothing -> pure (unit lang)
      Just (_, result) -> do
        -- NB: we do not cancel GC, need to block on the read side before timeout
        _ <- tryPutMVar result (Evaluated a)
        atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
        pure (unit lang)
  runLocal (ReceiveAsync chan (Seconds seconds)) = do
    -- todo: think about whether to allow multiple concurrent listeners on a channel
    resultMVar <- newEmptyMVar
    gcThread <- Concurrent.forkIO $ do
      Concurrent.threadDelay (floor $ 1000000 * seconds)
      atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
      putMVar resultMVar (Error (Err "Timeout on receiving from a channel"))
    atomicModifyIORef' (callbacks env) (\m -> (Map.insert chan (Concurrent.killThread gcThread, resultMVar) m, ()))
    pure (remote lang (Step (Local (Receive chan))))
  runLocal (Receive chan) = do
    m <- readIORef (callbacks env)
    case Map.lookup chan m of
      Nothing -> pure (unit lang)
      Just (cancelGC, result) -> do
        r <- takeMVar result
        cancelGC
        case r of
          Error (Err err) -> fail err
          Evaluated r -> pure r
          Syncing _ _ _ -> fail "expected an `Evaluated` message or an error, got a `Syncing`"
-}

{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.Runtime.Remote where

-- import qualified Data.Set as Set
import Control.Monad
import Control.Applicative
import Control.Exception (catch,SomeException,mask_)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial,deserialize)
import Data.Set (Set)
import GHC.Generics
import Unison.Remote
import Unison.Remote.Extra ()
import Unison.Runtime.Multiplex (Multiplex)
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
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

newtype Universe = Universe ByteString deriving (Show,Eq,Ord,Generic)
instance Serial Universe

type Cleartext = ByteString

info :: String -> Multiplex ()
info msg = liftIO (putStrLn msg)

{-
handshakeInitiate
  :: (Serial t, Serial key)
  => Env t h
  -> C.Cryptography key symmetricKey signKey signature hash Cleartext
  -> P.Protocol t signature hash
  -> Node
  -> Multiplex (Maybe (Remote t) -> Multiplex ())
handshakeInitiate env crypto p recipient = do
  recipientKey <- either fail pure $ Get.runGetS deserialize (publicKey recipient)
  (doneHandshake, encrypt, decrypt) <- liftIO $ C.pipeInitiator crypto recipientKey
  undefined
  -- it's the intiator who gets back a Handshake message?
  -- go doneHandshake encrypt decrypt

handshakeRespond
  :: Serial t
  => C.Cryptography key symmetricKey signKey signature hash Cleartext
  -> Node
  -> P.Handshake
  -> Multiplex (key, Multiplex (Maybe (Remote t)))
handshakeRespond crypto sender s = do
  (doneHandshake, senderKey, encrypt, decrypt) <- liftIO $ C.pipeResponder crypto
  go doneHandshake senderKey encrypt decrypt s
  where
  go doneHandshake senderKey encrypt decrypt s = case s of
    P.Done chan -> do
      guard =<< liftIO doneHandshake
      Just senderKey <- liftIO senderKey
      (encryptedPacket, unsubscribe) <- Mux.subscribe chan
      decryptMux <- pure . Mux.untilDefined $ do
        packet <- encryptedPacket
        bytes <- liftIO $ catch (Just <$> mask_ (decrypt packet))
                                (\e -> Nothing <$ putStrLn (show (e :: SomeException)))
        case bytes of
          Nothing -> pure Nothing
          Just bytes -> case Get.runGetS deserialize bytes of
            Left err -> pure Nothing <$ (info $ "Decoding error: " ++ err)
            Right a -> pure (Just a)
      pure (senderKey, decryptMux)
    P.More handshakeData req -> do
      _ <- liftIO $ decrypt handshakeData
      payload <- liftIO $ encrypt B.empty
      s <- Mux.nest sender $ Mux.requestTimed (2*1000*1000) req payload
      go doneHandshake senderKey encrypt decrypt s
-}
{-
server :: Ord h
       => C.Cryptography key symmetricKey signKey signature hash Cleartext
       -> P.Protocol t signature h
       -> Language t h
       -> Env t h
       -> Multiplex ()
server crypto p lang env = do
  (accept, unsubscribe) <- Mux.subscribe (P._handshake p)
  undefined

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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

-- big todos:
--   nodes need to track linked children, on destroy, destroy children
--   nodes need to track allocated persistent refs, on destroy can clear them
--   implement destroy
--   implement watchdog, idleness tracking
--   implement connect
--   for now, destroy can noop - just idle the node
--   so basically, it's just idleness tracking, nodes go to sleep
--   so just connect, idleness tracking
--   sandbox
module Unison.NodeProcess where

import Data.Functor
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically,STM)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.Map (Map)
import Data.Maybe
import Data.Serialize.Get (Get)
import Data.Word
import GHC.Generics
import System.IO (Handle, stdin, stdout, hSetBinaryMode)
import Unison.BlockStore (BlockStore(..), Series(..))
import qualified Unison.BlockStore as BlockStore
import Unison.Cryptography (Cryptography)
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Map as Map
import qualified Data.Serialize.Get as Get
import qualified Data.Set as Set
import qualified Unison.Runtime.Block as Block
import qualified Unison.Hash as Hash
import qualified Unison.Cryptography as C
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Remote as Remote
import qualified Unison.Runtime.Multiplex as Mux

instance Serial Series

data Handshake term
  = Done (Mux.Channel (Remote.Packet term Hash) ())
  | More (Mux.Channel B.ByteString (Handshake term)) deriving Generic

instance Serial term => Serial (Handshake term)

data Protocol term signature hash =
  Protocol
    -- | Shut down and destroy this node; requires proof of knowledge of private key
    { _destroyIn :: Mux.Channel signature ()
    -- | Destroy another node
    , _destroyOut :: Mux.Channel (Remote.Node, signature) ()
    -- | Initiate handshake with another node, eventually terminates in a channel that
    -- may be used to send Remote.Packet values to that node
    , _handshake :: Mux.Channel (Remote.Node, B.ByteString) (Handshake term)
    -- | Various `BlockStore` methods
    , _insert :: Mux.Channel B.ByteString hash
    , _lookup :: Mux.Channel hash B.ByteString
    , _declare :: Mux.Channel Series hash
    , _update :: Mux.Channel (Series,hash,B.ByteString) (Maybe hash)
    , _append :: Mux.Channel (Series,hash,B.ByteString) (Maybe hash)
    , _resolve :: Mux.Channel Series (Maybe hash)
    , _resolves :: Mux.Channel Series [hash] }

data MessageIn signature hash
  = DestroyIn signature
  -- An encrypted `Remote.Packet t Hash`
  | RemoteIn Word64 B.ByteString
  -- Handshaking message used to establish an encrypted pipe
  | HandshakeIn Word64 B.ByteString
  -- The result of issuing some `BlockStoreQuery` to the container.
  -- The `Word64` is the same `Word64`, used for correlation of request to response.
  | BlockStoreResult Word64 (Either String (BlockStoreResult hash))
  deriving Generic
instance (Serial signature, Serial hash) => Serial (MessageIn signature hash)

data MessageOut signature hash
  = DestroyOut Remote.Node signature
  | RemoteOut Word64 Remote.Node B.ByteString
  | HandshakeOut Remote.Node Word64 B.ByteString
  | BlockStoreQuery Word64 (BlockStoreQuery hash)
  deriving Generic
instance (Serial signature, Serial hash) => Serial (MessageOut signature hash)

data BlockStoreQuery hash
  = Insert B.ByteString
  | Lookup hash
  | DeclareSeries Series
  | Update Series hash B.ByteString
  | Append Series hash B.ByteString
  | Resolve Series
  | Resolves Series
  deriving Generic
instance Serial hash => Serial (BlockStoreQuery hash)

data BlockStoreResult hash
  = InsertResult hash
  | LookupResult hash (Maybe B.ByteString)
  | DeclareSeriesResult Series hash
  | UpdateResult (Maybe hash)
  | AppendResult (Maybe hash)
  | ResolveResult (Maybe hash)
  | ResolvesResult [hash]
  deriving Generic
instance Serial hash => Serial (BlockStoreResult hash)

deserializeHandle :: Serial a => Handle -> B.ByteString -> (a -> IO ()) -> IO ()
deserializeHandle h rem write = go (Get.runGetPartial deserialize rem) where
  go dec = do
    (a, rem) <- deserializeHandle1 h dec
    write a
    go (Get.runGetPartial deserialize rem)

deserializeHandle1' :: Serial a => Handle -> IO (a, B.ByteString)
deserializeHandle1' h = deserializeHandle1 h (Get.runGetPartial deserialize B.empty)

deserializeHandle1 :: Handle -> Get.Result a -> IO (a, B.ByteString)
deserializeHandle1 h dec = go dec where
  go result = case result of
    Get.Fail msg rem -> fail ("decoding failure " ++ msg ++ ", remainder: " ++ show rem)
    Get.Partial k -> B.hGetSome h 65536 >>= \bs -> go (k bs)
    Get.Done a rem -> pure (a, rem)

makeEnv :: (Serial term, Eq hash)
        => Remote.Universe
        -> Remote.Node
        -> Cryptography k1 k2 sk sig h ct
        -> BlockStore hash
        -> IO (Remote.Env term Hash)
makeEnv universe currentNode crypto bs = do
  callbacks <- Remote.callbacks0
  pure $ Remote.Env callbacks saveHashes getHashes missingHashes universe connect (C.randomBytes crypto) currentNode
  where
  -- connect :: Node -> IO (Packet t h -> IO (), IO ())
  -- todo: probably should do some caching here
  saveHashes hs =
    void $ Async.mapConcurrently saveHash hs
  saveHash (h,t) = do
    let b = Block.fromSeries (Series (Hash.toBytes h))
    let bytes = Put.runPutS (serialize t)
    h' <- Block.modify' bs b (maybe (Just bytes) Just)
    pure ()
  getHashes hs = do
    blocks <- Async.mapConcurrently getHash (Set.toList hs)
    blocks <- pure $ catMaybes blocks
    guard (length blocks == Set.size hs)
    let e = traverse (Get.runGetS deserialize) blocks
    case e of
      Left err -> fail err
      Right terms  -> pure $ Set.toList hs `zip` terms
  getHash h = do
    h <- BlockStore.resolve bs (Series (Hash.toBytes h))
    case h of
      Nothing -> pure Nothing
      Just h -> BlockStore.lookup bs h
  missingHashes hs0 = do
    let hs = Set.toList hs0
    hs' <- traverse (BlockStore.resolve bs . Series . Hash.toBytes) hs
    pure . Set.fromList $ [h | (h, Nothing) <- hs `zip` hs']
  connect = undefined

data Keypair = Keypair { public :: B.ByteString, private :: B.ByteString } deriving Generic
instance Serial Keypair

type BlockStoreCallbacks h = TVar (Word64, Map Word64 (MVar (Either String (BlockStoreResult h))))

blockStoreListener :: BlockStoreCallbacks h -> TQueue (Maybe (MessageIn signature h)) -> IO ()
blockStoreListener cbs q = void . forkIO . repeatWhile $ do
  msg <- atomically $ readTQueue q
  case msg of
    Just (BlockStoreResult id r) -> handleBlockStore cbs (id, r) $> True
    _ -> atomically (unGetTQueue q msg) $> False

handleBlockStore :: BlockStoreCallbacks hash -> (Word64, Either String (BlockStoreResult hash)) -> IO ()
handleBlockStore cbs (id, r) = do
  (_,m) <- readTVarIO cbs
  case Map.lookup id m of
    Nothing -> pure ()
    Just mvar -> putMVar mvar r

make :: forall term key symmetricKey signKey signature hash cleartext h
      . (BA.ByteArrayAccess key, Serial signature, Serial term, Serial hash, Serial h, Eq h)
     => (Keypair -> Keypair -> Cryptography key symmetricKey signKey signature hash cleartext)
     -> Get (BlockStore h -> IO (Remote.Language term Hash))
     -> IO ()
make mkCrypto makeSandbox = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  (nodeSeries, rem) <- deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  messagesOut <- atomically (newTQueue :: STM (TQueue (Maybe (MessageOut signature h))))
  messagesIn <- atomically (newTQueue :: STM (TQueue (Maybe (MessageIn signature h))))
  _ <- forkIO $ deserializeHandle stdin rem (atomically . writeTQueue messagesIn . Just)
  blockStoreCallbacks <- atomically (newTVar (0, Map.empty))
  blockStoreListener blockStoreCallbacks messagesIn
  let blockStore = blockStoreProxy blockStoreCallbacks (atomically . writeTQueue messagesOut . Just)
  Just (keypair, signKeypair, universe, node, sandbox) <- -- lifetime, budget, children
    Block.get blockStore . Block.serial Nothing . Block.fromSeries . Series $ nodeSeries
  makeSandbox <- either fail pure $ Get.runGetS makeSandbox sandbox
  sandbox <- makeSandbox blockStore
  let crypto = mkCrypto keypair signKeypair
  -- activityCounter <- newIORef (0 :: Word64)

  -- consumes messagesIn
  reader <- Async.async $ do
    env <- makeEnv universe node crypto blockStore
    repeatWhile $ do
      msgIn <- atomically (readTQueue messagesIn)
      -- modify the activity counter
      -- if messages processed sequentially can ensure none are in flight
      case msgIn of
        Nothing -> False <$ atomically (writeTQueue messagesOut Nothing)
        Just (DestroyIn sig) | C.verify crypto (C.publicSigningKey crypto) sig "destroy" -> error "todo"
        Just (BlockStoreResult id r) -> True <$ handleBlockStore blockStoreCallbacks (id,r)
        Just (RemoteIn _ r) -> (True <$) . forkIO $ do
          -- todo: decryption using the pipe session identifier
          packet <- either fail pure (Get.runGetS deserialize r)
          Remote.handle sandbox env packet
        _ -> pure True -- ignore everything else

  -- checks for activity and puts node to sleep if idle
  -- also checks for lease expiration
  watchdog <- Async.async $ error "todo"

  -- writes pending messages to standard out
  writer <- Async.async . repeatWhile $ do
    msgOut <- atomically (readTQueue messagesOut)
    case msgOut of
      Nothing -> pure False
      Just msg -> True <$ B.putStr (Put.runPutS (serialize msgOut))
  Async.waitCatch reader >> Async.waitCatch writer >> pure ()
  pure ()

repeatWhile :: Monad f => f Bool -> f ()
repeatWhile action = do
  ok <- action
  when ok (repeatWhile action)

blockStoreProxy :: TVar (Word64, Map Word64 (MVar (Either String (BlockStoreResult hash))))
                -> (MessageOut signature hash -> IO ())
                -> BlockStore hash
blockStoreProxy v send = BlockStore insert lookup declare update append resolve resolves where
  -- todo may want to add timeouts, errors
  init query = do
    result <- newEmptyMVar
    nonce <- atomically $ do
      modifyTVar v (\(nonce,m) -> (nonce+1, Map.insert nonce result m))
      fst <$> readTVar v
    send $ BlockStoreQuery nonce query
    e <- takeMVar result
    either fail pure e
  insert bytes = do
    InsertResult h <- init (Insert bytes)
    pure h
  lookup h = do
    LookupResult _ bytes <- init (Lookup h)
    -- could do some verification of bytes here
    pure bytes
  declare s0@(Series series) = do
    DeclareSeriesResult (Series s) h <- init (DeclareSeries s0)
    guard (series == s)
    pure h
  update series h bytes = do
    UpdateResult h <- init (Update series h bytes)
    pure h
  append series h bytes = do
    AppendResult h <- init (Append series h bytes)
    pure h
  resolve series = do
    ResolveResult h <- init (Resolve series)
    pure h
  resolves series = do
    ResolvesResult hs <- init (Resolves series)
    pure hs

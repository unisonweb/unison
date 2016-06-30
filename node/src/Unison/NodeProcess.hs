{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}

module Unison.NodeProcess where

import Control.Concurrent.Async (async, waitCatch)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically,STM)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.Map (Map)
import Data.Serialize.Get (Get)
import Data.Word
import GHC.Generics
import System.IO (Handle, stdin, stdout, hSetBinaryMode)
-- import Unison.BlockStore (BlockStore, Series)
import Unison.BlockStore as BlockStore
import Unison.Cryptography (Cryptography)
import Unison.Hash (Hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Put as Put
import qualified Data.Map as Map
import qualified Data.Serialize.Get as Get
import qualified Unison.Cryptography as C
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Remote as Remote

instance Serial Series

data MessageIn signature hash
  -- Shutdown this `Node`, requires proof of knowledge of private key
  = DestroyIn signature
  -- An encrypted `Remote.Packet t Hash`
  | RemoteIn Word64 B.ByteString
  -- Handshaking message used to establish an encrypted pipe
  | HandshakeIn Word64 B.ByteString
  -- The result of issuing some `BlockStoreQuery` to the container.
  -- The `Word64` is the same `Word64`, used for correlation of request to response.
  | BlockStoreResult Word64 (Either String (BlockStoreResult hash))
  -- The result of reading a block from the container `BlockStore`
  | Block hash B.ByteString
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

makeEnv :: Remote.Universe -> Remote.Node -> Cryptography k1 k2 sk sig h ct -> IO (Remote.Env term Hash)
makeEnv _ _ _ = pure (error "todo")

make :: forall term key symmetricKey signKey signature hash cleartext
      . (BA.ByteArrayAccess key, Serial signature, Serial term, Serial hash)
     => Get (Cryptography key symmetricKey signKey signature hash cleartext)
     -> Get (BlockStore hash -> IO (Remote.Language term Hash))
     -> IO ()
make crypto makeSandbox = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  (host, rem) <- deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  (universe, rem) <- deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  (crypto, rem) <- deserializeHandle1 stdin (Get.runGetPartial crypto rem)
  (makeSandbox, rem) <- deserializeHandle1 stdin (Get.runGetPartial makeSandbox rem)
  blockstoreCallbacks <- atomically (newTVar (0, Map.empty))
  messagesIn <- atomically (newTQueue :: STM (TQueue (MessageIn signature hash)))
  messagesOut <- atomically (newTQueue :: STM (TQueue (MessageOut signature hash)))
  let blockStore = blockStoreProxy blockstoreCallbacks (atomically . writeTQueue messagesOut)
      node = Remote.Node host (BA.convert (C.publicKey crypto))
  _ <- makeSandbox blockStore
  -- sandbox <- makeSandbox blockStore
  _ <- makeEnv universe node crypto
  -- env <- makeEnv universe node crypto
  reader <- async $ error "something that reads from the channel"
  writer <- async . forever $ do
    msgOut <- atomically (readTQueue messagesOut)
    B.putStr (Put.runPutS (serialize msgOut))
  deserializeHandle stdin rem (atomically . writeTQueue messagesIn)
  waitCatch reader >> waitCatch writer >> pure ()
  pure ()

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

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
--   to implement idleness tracking, can just walk through the callbacks map,
--   calling retry anytime we hit a non-subscription
--   not quite sufficient, since our Remote.Env might be waiting for a request
--   handshaking can return a non-idle subscription
--   rewrite Runtime.Remote to use Multiplex, then can accurately track
--   or can provide idleness tracking in Runtime.Remote
module Unison.NodeProcess where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically,STM)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans.Reader (ask)
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.Serialize.Get (Get)
import Data.Word
import GHC.Generics
import System.IO (Handle, stdin, stdout, hSetBinaryMode)
import Unison.BlockStore (BlockStore(..), Series(..))
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
import qualified Unison.BlockStore as BlockStore
import qualified Unison.Cryptography as C
import qualified Unison.Hash as Hash
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Block as Block
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote

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

data Keypair = Keypair { public :: B.ByteString, private :: B.ByteString } deriving Generic
instance Serial Keypair

{-
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
-}

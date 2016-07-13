{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}

module Unison.NodeProcess where

import qualified Unison.NodeProtocol as P
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically,STM)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
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
import Unison.Runtime.Multiplex (Multiplex)
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
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote

data Keypair = Keypair { public :: B.ByteString, private :: B.ByteString } deriving Generic
instance Serial Keypair

{-
make :: forall term key symmetricKey signKey signature hash thash cleartext h
      . (BA.ByteArrayAccess key, Serial signature, Serial term, Serial hash, Serial thash, Serial h, Eq h)
     => P.Protocol term signature hash thash
     -> (Keypair -> Keypair -> Cryptography key symmetricKey signKey signature hash cleartext)
     -> Get (BlockStore h -> IO (Remote.Language term thash))
     -> Multiplex ()
make protocol mkCrypto makeSandbox = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  (nodeSeries, rem) <- deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  messagesOut <- atomically (newTQueue :: STM (TQueue (Maybe Mux.Packet)))
  messagesIn <- atomically (newTQueue :: STM (TQueue (Maybe Mux.Packet)))
  _ <- Mux.fork $ do
    liftIO $ deserializeHandle stdin rem (atomically . writeTQueue messagesIn . Just)
    liftIO . atomically $ writeTQueue messagesIn Nothing
  blockStore <- P.blockStoreProxy protocol
  Just (keypair, signKeypair, universe, node, sandbox) <- -- lifetime, budget, children
    Block.get blockStore . Block.serial Nothing . Block.fromSeries . Series $ nodeSeries
  makeSandbox <- either fail pure $ Get.runGetS makeSandbox sandbox
  sandbox <- makeSandbox blockStore
  let crypto = mkCrypto keypair signKeypair
  pure ()
-}

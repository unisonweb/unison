{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.NodeProcess where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem
import Control.Monad.IO.Class
import Data.Bytes.Serial (Serial, deserialize)
import Data.Serialize.Get (Get)
import GHC.Generics
import System.IO (stdin, hSetBinaryMode)
import Unison.BlockStore (BlockStore(..), Series(..))
import Unison.Cryptography (Cryptography)
import Unison.Hash.Extra ()
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Serialize.Get as Get
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Block as Block
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote

data Keypair k = Keypair { public :: k, private :: B.ByteString } deriving Generic
instance Serial k => Serial (Keypair k)

make :: (BA.ByteArrayAccess key, Serial signature, Serial term, Serial hash, Serial thash, Serial h, Eq h, Serial key, Serial signKey, Ord thash)
     => P.Protocol term signature h thash
     -> (Keypair key -> Keypair signKey ->
           Cryptography key symmetricKey signKey signature hash Remote.Cleartext)
     -> Get (BlockStore h -> IO (Remote.Language term thash))
     -> IO ()
make protocol mkCrypto makeSandbox = do
  hSetBinaryMode stdin True
  (nodeSeries, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  interrupt <- atomically $ newTSem 0
  Mux.runStandardIO (Mux.seconds 5) rem (atomically $ waitTSem interrupt) $ do
    blockStore <- P.blockStoreProxy protocol
    Just (keypair, signKeypair, universe, node, sandbox) <- -- todo: lifetime, budget, children
      liftIO . Block.get blockStore . Block.serial Nothing . Block.fromSeries . Series $ nodeSeries
    makeSandbox <- either fail pure $ Get.runGetS makeSandbox sandbox
    sandbox <- liftIO $ makeSandbox blockStore
    let crypto = mkCrypto keypair signKeypair
    -- todo: load this from persistent store also
    connectionSandbox <- pure $ Remote.ConnectionSandbox (\_ -> pure True) (\_ -> pure True)
    env <- liftIO $ Remote.makeEnv universe node blockStore
    _ <- Mux.fork $ Remote.server crypto connectionSandbox env sandbox protocol
    _ <- Mux.fork $ do
      (destroy, cancel) <- Mux.subscribeTimed (Mux.seconds 60) (P._destroyIn protocol)
      Mux.repeatWhile $ do
        sig <- destroy
        case sig of
          Just sig | C.verify crypto (public signKeypair) sig "destroy" -> do
            cancel
            -- todo: actual cleanup, kill child nodes;
            -- possiby modify destroyed message to take the
            -- list of garbage nodes and/or references
            Mux.send (P._destroyed protocol) (node, sig)
            liftIO $ atomically (signalTSem interrupt)
            pure False
          _ -> pure True
    pure ()

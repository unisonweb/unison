{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.NodeWorker where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem
import Control.Monad.IO.Class
import Control.Exception (finally)
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.Serialize.Get (Get)
import GHC.Generics
import System.IO (stdin, stderr, hSetBinaryMode, hPutStrLn, Handle)
import Unison.BlockStore (BlockStore(..))
import Unison.Cryptography (Cryptography)
import Unison.Hash.Extra ()
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Serialize.Get as Get
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote
import qualified Unison.Remote as Remote

data Keypair k = Keypair { public :: k, private :: B.ByteString } deriving Generic
instance Serial k => Serial (Keypair k)

logger :: Handle -> IO (String -> IO ())
logger h = do
  sem <- atomically (newTSem 1)
  pure $ \s -> atomically (waitTSem sem) >> (hPutStrLn h s `finally` atomically (signalTSem sem))

make :: ( BA.ByteArrayAccess key
        , Serial signature
        , Serial term, Show term
        , Serial hash
        , Serial thash
        , Serial h
        , Eq h
        , Serial key
        , Ord thash)
     => P.Protocol term hash h thash
     -> (Keypair key -> Cryptography key symmetricKey signKey skp signature hash Remote.Cleartext)
     -> Get (Cryptography key symmetricKey signKey skp signature hash Remote.Cleartext
             -> BlockStore h
             -> IO (Remote.Language term thash, term -> IO (Either String ())))
     -> IO ()
make protocol mkCrypto makeSandbox = do
  log <- logger stderr
  let die msg = liftIO $ log msg >> error ""
  liftIO . log $ "[worker] initializing... "
  hSetBinaryMode stdin True
  (privateKey, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  liftIO . log $ "[worker] loaded keypair... "
  (node, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  liftIO . log $ "[worker] loaded node: " ++ show node
  (universe, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  liftIO . log $ "[worker] loaded universe: " ++ show universe
  (sandbox, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  liftIO . log $ "[worker] loaded sandbox... "
  publicKey <- either die pure $ Get.runGetS deserialize (Remote.publicKey node)
  let keypair = Keypair publicKey privateKey
  liftIO . log $ "[worker] remaining bytes: " ++ show (B.length rem)
  interrupt <- atomically $ newTSem 0
  Mux.runStandardIO log (Mux.seconds 5) rem (atomically $ waitTSem interrupt) $ do
    blockStore <- P.blockStoreProxy protocol
    makeSandbox <- either die pure $ Get.runGetS makeSandbox sandbox
    let crypto = mkCrypto keypair
    (sandbox, typecheck) <- liftIO $ makeSandbox crypto blockStore
    let skHash = Put.runPutS (serialize $ C.hash crypto [Put.runPutS (serialize $ private keypair)])
    -- todo: load this from persistent store also
    connectionSandbox <- pure $ Remote.ConnectionSandbox (\_ -> pure True) (\_ -> pure True)
    env <- liftIO $ Remote.makeEnv universe node blockStore
    liftIO . log $ "[worker] ... done initializing"
    _ <- Remote.server crypto connectionSandbox env sandbox protocol
    _ <- do
      (prog, cancel) <- Mux.subscribeTimed (Mux.seconds 60) (P._localEval protocol)
      Mux.fork . Mux.repeatWhile $ do
        e <- prog
        case e of
          Nothing -> False <$ cancel
          Just r -> do
            liftIO . log $ "[worker] _localEval got a term " ++ show r
            e <- liftIO . typecheck $ r
            case e of
              Left err -> do
                liftIO . log $ "[worker] _localEval typechecking failed:\n" ++ err
                pure True
              Right _ -> do
                liftIO . log $ "[worker] typechecked"
                r <- liftIO $ Remote.eval sandbox r
                liftIO . log $ "[worker] evaluated to " ++ show r
                case Remote.unRemote sandbox r of
                  Nothing -> liftIO (log "[worker] _localEval received a non-Remote") >> pure True
                  Just r -> True <$ Mux.fork (Remote.handle crypto connectionSandbox env sandbox protocol r)
    _ <- do
      (destroy, cancel) <- Mux.subscribeTimed (Mux.seconds 60) (P._destroyIn protocol)
      Mux.fork . Mux.repeatWhile $ do
        sig <- destroy
        case sig of
          Just sig | BA.constEq skHash (Put.runPutS (serialize sig)) -> do
            cancel
            Mux.send (Mux.Channel Mux.Type skHash) ()
            -- no other cleanup needed; container will reclaim resources and eventually
            -- kill off linked child nodes
            liftIO $ atomically (signalTSem interrupt)
            pure False
          _ -> pure True
    pure ()

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.NodeWorker where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem
import Control.Monad.IO.Class
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.Serialize.Get (Get)
import GHC.Generics
import System.IO (stdin, hSetBinaryMode)
import Unison.BlockStore (BlockStore(..))
import Unison.Cryptography (Cryptography)
import Unison.Hash.Extra ()
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Serialize.Get as Get
import qualified Data.ByteString.Base64.URL as Base64
import qualified Unison.Config as Config
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote
import qualified Unison.Util.Logger as L

data Keypair k = Keypair { public :: k, private :: B.ByteString } deriving Generic
instance Serial k => Serial (Keypair k)

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
  logger <- L.scope "worker" <$> Config.loggerStandardError
  let die msg = liftIO $ L.error logger msg >> error ""
  L.info logger $ "initializing... "
  hSetBinaryMode stdin True
  (privateKey, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  (node, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  logger <- pure $ L.scope (show . Base64.encode $ Remote.publicKey node) logger
  (universe, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  (sandbox, _, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize rem)
  publicKey <- either die pure $ Get.runGetS deserialize (Remote.publicKey node)
  let keypair = Keypair publicKey privateKey
  L.debug logger $ "remaining bytes: " ++ show (B.length rem)
  interrupt <- atomically $ newTSem 0
  Mux.runStandardIO logger (Mux.seconds 5) rem (atomically $ waitTSem interrupt) $ do
    blockStore <- P.blockStoreProxy protocol
    makeSandbox <- either die pure $ Get.runGetS makeSandbox sandbox
    let crypto = mkCrypto keypair
    (sandbox, typecheck) <- liftIO $ makeSandbox crypto blockStore
    let skHash = Put.runPutS (serialize $ C.hash crypto [Put.runPutS (serialize $ private keypair)])
    -- todo: load this from persistent store also
    connectionSandbox <- pure $ Remote.ConnectionSandbox (\_ -> pure True) (\_ -> pure True)
    env <- liftIO $ Remote.makeEnv universe node blockStore
    Mux.info $ "... done initializing"
    _ <- Remote.server crypto connectionSandbox env sandbox protocol
    _ <- do
      (prog, cancel) <- Mux.subscribeTimed (Mux.seconds 60) (P._localEval protocol)
      Mux.fork . Mux.scope "_localEval" . Mux.repeatWhile $ do
        e <- prog
        case e of
          Nothing -> False <$ cancel
          Just r -> do
            Mux.debug $ "got a term " ++ show r
            e <- liftIO . typecheck $ r
            case e of
              Left err -> do
                Mux.warn $ "typechecking failed on: " ++ show r
                Mux.warn $ "typechecking error:\n" ++ err
                pure True
              Right _ -> do
                Mux.debug "typechecked"
                r <- liftIO $ Remote.eval sandbox r
                Mux.debug $ "evaluated to " ++ show r
                case Remote.unRemote sandbox r of
                  Nothing -> True <$ (Mux.warn $ "received a non-Remote: " ++ show r)
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

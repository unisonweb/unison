{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.NodeWorker where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TSem
import Control.Exception.Base as Ex
import Control.Monad.IO.Class
import Data.Bytes.Serial (Serial, serialize)
import Unison.Cryptography (Cryptography)
import Unison.Hash.Extra ()
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteArray as BA
import qualified Data.Bytes.Put as Put
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote
import qualified Unison.Util.Logger as L

make :: ( BA.ByteArrayAccess key
        , Serial signature
        , Serial term, Show term
        , Serial hash
        , Serial thash
        , Serial h
        , Eq h
        , Serial key
        , Ord thash)
     => L.Logger
     -> P.Protocol term hash h thash
     -> Cryptography key symmetricKey signKey skp signature hash Remote.Cleartext
     -> Remote.Language term thash
     -> Remote.Node
     -> Remote.Universe
     -> (term -> IO (Either String term))
     -> IO (Maybe Mux.Packet -> IO (), IO (Maybe Mux.Packet), STM Bool)
make logger protocol crypto sandbox node universe typecheck = do
  logger <- pure $ L.scope "worker" logger
  (env, toNode, fromNode, isActive) <- Mux.env0 logger
  L.debug' logger $ do
    active <- atomically isActive
    pure $ "active0: " ++ show active
  -- used to make sure we are listening on all channels before returning,
  -- otherwise the caller could experience packet drops when sending
  ok <- atomically $ newTSem 0 -- incremented once initialization done
  L.debug logger "kicking off processor"
  node <- processor ok env
  _ <- Async.async $ supervise ok env node
  L.debug logger "about to wait on semaphore"
  atomically $ waitTSem ok
  L.debug logger "done waiting on semaphore"
  threadDelay (1000 * 500)
  L.debug' logger $ do
    active <- atomically isActive
    pure $ "active: " ++ show active
  let toNode' pk = check >> toNode pk
      fromNode' = check >> fromNode
      check = atomically isActive >>= \a ->
                if a then pure () else fail "inactive node"
  pure (toNode', fromNode', isActive)
  where
  supervise ok env node = Async.waitCatch node >>= \e -> case e of
    Left err | isCatchable err -> do
      L.warn logger $ "error during node processing, restarting "
      node <- processor ok env
      supervise ok env node
    Left err ->
      L.info logger $ "shutting down node due to uncatchable error: " ++ show err
    Right _ ->
      L.info logger "shutting down node due to graceful termination"
  processor ok env = Async.async . Mux.run env $ do
    blockStore <- P.blockStoreProxy protocol
    -- todo: load this from persistent store also
    connectionSandbox <- pure $ Remote.ConnectionSandbox (\_ -> pure True) (\_ -> pure True)
    env <- liftIO $ Remote.makeEnv universe node blockStore
    server <- Remote.server crypto connectionSandbox env sandbox protocol
    localEval <- do
      (prog, cancel) <- Mux.subscribeTimed (Mux.seconds 60) (P._localEval protocol)
      Mux.fork . Mux.scope "_localEval" . Mux.repeatWhile $ do
        e <- prog
        case e of
          Nothing -> False <$ (Mux.info "_localEval shutdown subscription" <* cancel)
          Just r -> do
            Mux.debug $ "got a term " ++ show r
            e <- liftIO . typecheck $ r
            case e of
              Left err -> do
                Mux.warn $ "typechecking failed on: " ++ show r
                Mux.warn $ "typechecking error:\n" ++ err
                pure True
              Right r -> do
                Mux.debug "typechecked"
                r <- liftIO $ Remote.eval sandbox r
                Mux.debug $ "evaluated to " ++ show r
                case Remote.unRemote sandbox r of
                  Nothing -> True <$ (Mux.warn $ "received a non-Remote: " ++ show r)
                  Just r -> True <$ Mux.fork (Remote.handle crypto connectionSandbox env sandbox protocol r)
    destroyIn <- do
      (destroy, _) <- Mux.subscribeTimed (Mux.seconds 60) (P._destroyIn protocol)
      Mux.fork . Mux.repeatWhile $ do
        sig <- destroy
        case sig of
          Just sig -> do
            -- cancel
            Mux.send (Mux.Channel Mux.Type (Put.runPutS (serialize sig))) ()
            -- no other cleanup needed; container will reclaim resources and eventually
            -- kill off linked child nodes
            pure False
          _ -> pure True
    Mux.info $ "... done initializing"
    liftIO . atomically $ signalTSem ok
    liftIO $ do Async.wait server; Async.wait localEval; Async.wait destroyIn

-- Don't catch asynchronous exceptions or deadlocks
isCatchable :: SomeException -> Bool
isCatchable e = not $
  (case Ex.fromException e of Just Ex.ThreadKilled -> True; _ -> False) ||
  (case Ex.fromException e of Just Ex.UserInterrupt -> True; _ -> False) ||
  (case Ex.fromException e of Just Ex.BlockedIndefinitelyOnSTM -> True; _ -> False) ||
  (case Ex.fromException e of Just Ex.BlockedIndefinitelyOnMVar -> True; _ -> False)


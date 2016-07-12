{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Runtime.Remote where

import Data.Functor
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial,serialize,deserialize)
import Data.Set (Set)
import Unison.Remote hiding (seconds)
import Unison.Remote.Extra ()
import Unison.Runtime.Multiplex (Multiplex)
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Set as Set
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.SharedResourceMap as RM
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
        , currentNode :: Node
        -- todo: cache of recent nodes to check for syncing hashes not found locally
        , connections :: RM.SharedResourceMap
                           Node
                           ( Maybe (Remote t, Mux.Channel P.Ack) -> Multiplex ()
                           , Multiplex (Maybe (Maybe ([h], Mux.Channel (Maybe [(h,t)]))))
                           , Mux.CipherState ) }

instance Serial Universe

type Cleartext = ByteString

info :: String -> Multiplex ()
info msg = liftIO (putStrLn msg)

server :: (Ord h, Serial key, Serial t, Serial h)
       => C.Cryptography key symmetricKey signKey signature hash Cleartext
       -> Env t h
       -> Language t h
       -> P.Protocol t signature h' h
       -> Multiplex ()
server crypto env lang p = do
  (accept,_) <- Mux.subscribeTimed (Mux.seconds 60) (Mux.erase (P._eval p))
  Mux.repeatWhile $ do
    initialPayload <- accept
    case initialPayload of
      Nothing -> pure False
      Just initialPayload -> (\mux -> True <$ Mux.fork mux) $ do -- fork off handling each connection
        (peerKey, (peer,peeru), send, recv, cipherstate@(encrypt,_)) <- Mux.pipeRespond crypto (P._eval p) fst initialPayload
        guard $ Put.runPutS (serialize peerKey) == publicKey peer
        Mux.repeatWhile $ do
          r <- recv
          case r of
            Nothing -> pure False
            Just (r, ackChan) -> do
              Mux.encryptAndSendTo' peer ackChan encrypt P.Ack
              let needs = localDependencies lang (remote lang r)
              when (universe env /= peeru) $ loop needs
              True <$ Mux.fork (handle crypto env lang p r) -- fork off evaluation of each request
              where
              fetch hs = do
                syncChan <- Mux.channel
                Mux.encryptedRequestTimedVia cipherstate (Mux.seconds 5) (send . Just . Just) syncChan (Set.toList hs)
              loop needs | Set.null needs = pure ()
              loop needs = fetch needs >>= \hashes -> case hashes of
                Nothing -> fail "expected hashes, got timeout"
                Just hashes -> do
                  liftIO $ saveHashes env hashes
                  stillMissing <- forM hashes $ \(_,t) ->
                    liftIO $ missingHashes env (localDependencies lang t)
                  loop (Set.unions stillMissing)

handle :: (Ord h, Serial key, Serial t, Serial h)
       => C.Cryptography key symmetricKey signKey signature hash Cleartext
       -> Env t h
       -> Language t h
       -> P.Protocol t signature h' h
       -> Remote t
       -> Multiplex ()
handle crypto env lang p r = case r of
  Step (Local l) -> void $ runLocal l
  Step (At n r) -> transfer n r Nothing
  Bind (At n r) k -> transfer n r (Just k)
  Bind (Local l) k -> do
    arg <- runLocal l
    r <- liftIO $ eval lang (apply lang k arg)
    case (unRemote lang r) of
      Just r -> handle crypto env lang p r
      Nothing -> fail "typechecker bug; function passed to Remote.bind did not return a Remote"
  where
  transfer n t k = client crypto env p n r where
    r = case k of
      Nothing -> Step (Local (Pure t))
      Just k -> Bind (Local (Pure t)) k
  runLocal (Fork r) = Mux.fork (handle crypto env lang p r) $> unit lang
  runLocal CreateChannel = channel lang . Channel . Mux.channelId <$> Mux.channel
  runLocal Here = pure $ node lang (currentNode env)
  runLocal (Pure t) = liftIO $ eval lang t
  runLocal (Send (Channel cid) a) = do
    Mux.process1 (Mux.Packet cid (Put.runPutS (serialize a)))
    pure (unit lang)
  runLocal (ReceiveAsync chan@(Channel cid) (Seconds seconds)) = do
    _ <- Mux.receiveTimed (floor $ seconds * 1000 * 1000) ((Mux.Channel Mux.Type cid) :: Mux.Channel (Maybe B.ByteString))
    pure (remote lang (Step (Local (Receive chan))))
  runLocal (Receive (Channel cid)) = do
    (recv,_) <- Mux.receiveCancellable (Mux.Channel Mux.Type cid)
    bytes <- recv
    case Get.runGetS deserialize bytes of
      Left err -> fail err
      Right r -> pure r

client :: (Ord h, Serial key, Serial t, Serial h)
       => C.Cryptography key symmetricKey signKey signature hash Cleartext
       -> Env t h
       -> P.Protocol t signature h' h
       -> Node
       -> Remote t
       -> Multiplex ()
client crypto env p recipient r = do
  recipientKey <- either fail pure $ Get.runGetS deserialize (publicKey recipient)
  menv <- Mux.ask
  connect <- pure . Mux.run menv $
    Mux.pipeInitiate crypto (P._eval p) (recipient, recipientKey) (currentNode env, universe env)
  (send,recv,cipherstate@(encrypt,_)) <- liftIO $ RM.lookupOrReplenish recipient connect (connections env)
  replyChan <- Mux.channel
  let send' (a,b) = send (Just (a,b))
  _ <- Mux.encryptedRequestTimedVia cipherstate (Mux.seconds 5) send' replyChan r
  -- todo - might want to retry if ack doesn't come back
  id $
    let
      go = do
        needs <- recv
        case needs of
          Nothing -> fail "timeout"
          Just Nothing -> pure () -- no other syncs requested, we're good
          Just (Just (hs, replyTo)) -> do
            hashes <- liftIO $ getHashes env (Set.fromList hs)
            Mux.encryptAndSendTo' recipient replyTo encrypt (Just hashes)
            go
    in go

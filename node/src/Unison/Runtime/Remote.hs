{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Runtime.Remote where

import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial,serialize,deserialize)
import Data.Set (Set)
import Unison.Remote hiding (seconds)
import Unison.Remote.Extra ()
import Unison.Runtime.Multiplex (Multiplex)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent as C
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Set as Set
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Block as Block
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.SharedResourceMap as RM

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

data Codestore t h
  = Codestore { saveHashes :: [(h,t)] -> IO ()
              , getHashes :: Set h -> IO [(h,t)]
              , missingHashes :: Set h -> IO (Set h) }

data Env t h
  = Env { codestore :: Codestore t h
        , universe :: Universe
        , currentNode :: Node
        -- todo: cache of recent nodes to check for syncing hashes not found locally
        , connections :: RM.SharedResourceMap
                           Node
                           ( Maybe (Remote t, Mux.Channel P.Ack) -> Multiplex ()
                           , Multiplex (Maybe (Maybe ([h], Mux.Channel (Maybe [(h,t)]))))
                           , Mux.CipherState ) }

instance Serial Universe

makeCodestore :: (Serial term, Eq hash, Serial termhash, Ord termhash)
  => BS.BlockStore hash
  -> Codestore term termhash
makeCodestore bs = Codestore saveHashes getHashes missingHashes where
  saveHashes hs =
    void $ Async.mapConcurrently saveHash hs
  saveHash (h,t) = do
    let b = Block.fromSeries (BS.Series (Put.runPutS (serialize h)))
    let bytes = Put.runPutS (serialize t)
    _ <- Block.modify' bs b (maybe (Just bytes) Just)
    pure ()
  -- todo: probably should do some caching/buffering/integrity checking here
  getHashes hs = do
    blocks <- Async.mapConcurrently getHash (Set.toList hs)
    blocks <- pure $ catMaybes blocks
    guard (length blocks == Set.size hs)
    let e = traverse (Get.runGetS deserialize) blocks
    case e of
      Left err -> fail err
      Right terms  -> pure $ Set.toList hs `zip` terms
  getHash h = do
    h <- BS.resolve bs (BS.Series (Put.runPutS (serialize h)))
    case h of
      Nothing -> pure Nothing
      Just h -> BS.lookup bs h
  missingHashes hs0 = do
    let hs = Set.toList hs0
    hs' <- traverse (BS.resolve bs . BS.Series . Put.runPutS . serialize) hs
    pure . Set.fromList $ [h | (h, Nothing) <- hs `zip` hs']

makeEnv :: (Serial term, Eq hash, Serial termhash, Ord termhash)
        => Universe
        -> Node
        -> BS.BlockStore hash
        -> IO (Env term termhash)
makeEnv universe currentNode bs = mk <$> RM.new 10 40 -- seconds
  where
  mk = Env (makeCodestore bs) universe currentNode
type Cleartext = ByteString

data ConnectionSandbox key =
  ConnectionSandbox { allowIn :: key -> Multiplex Bool
                    , allowOut :: key -> Multiplex Bool }

server :: (Ord h, Serial key, Serial t, Show t, Serial h)
       => C.Cryptography key t1 t2 t3 t4 hash Cleartext
       -> ConnectionSandbox key
       -> Env t h
       -> Language t h
       -> P.Protocol t hash h' h
       -> Multiplex ()
server crypto allow env lang p = do
  (accept,_) <- Mux.subscribeTimed (Mux.seconds 60) (Mux.erase (P._eval p))
  void . Mux.fork . Mux.repeatWhile $ do
    initialPayload <- accept
    case initialPayload of
      Nothing -> pure False
      Just initialPayload -> (True <$) . Mux.fork $ do -- fork off handling each connection
        (peerKey, (peer,peeru), send, recv, cipherstate@(encrypt,_)) <-
          Mux.pipeRespond crypto (allowIn allow) (P._eval p) fst initialPayload
        -- guard $ Put.runPutS (serialize peerKey) == publicKey peer
        Mux.scope "Remote.server" . Mux.repeatWhile $ do
          r <- recv
          Mux.debug $ "eval " ++ show r
          case r of
            Nothing -> pure False
            Just (r, ackChan) -> do
              Mux.encryptAndSendTo' peer ackChan encrypt (P.Ack (publicKey peer))
              let needs = localDependencies lang (remote lang r)
              when (universe env /= peeru) $ loop needs
              Mux.debug $ "forking off handler for " ++ show r
              True <$ Mux.fork (handle crypto allow env lang p r) -- fork off evaluation of each request
              where
              fetch hs = do
                syncChan <- Mux.channel
                Mux.encryptedRequestTimedVia "fetching hashes"
                  cipherstate (Mux.seconds 5) (send . Just . Just) syncChan (Set.toList hs)
              loop needs | Set.null needs = pure ()
              loop needs = fetch needs >>= \hashes -> case hashes of
                Nothing -> fail "expected hashes, got timeout"
                Just hashes -> do
                  liftIO $ saveHashes (codestore env) hashes
                  stillMissing <- forM hashes $ \(_,t) ->
                    liftIO $ missingHashes (codestore env) (localDependencies lang t)
                  loop (Set.unions stillMissing)

handle :: (Ord h, Serial key, Serial t, Serial h, Show t)
       => C.Cryptography key t1 t2 t3 t4 hash Cleartext
       -> ConnectionSandbox key
       -> Env t h
       -> Language t h
       -> P.Protocol t hash h' h
       -> Remote t
       -> Multiplex ()
handle crypto allow env lang p r = Mux.debug (show r) >> case r of
  Step (Local l) -> do
    r <- runLocal l
    Mux.info $ "computation completed with result: " ++ show r
  Step (At n r) -> transfer n r Nothing
  Bind (At n r) k -> transfer n r (Just k)
  Bind (Local l) k -> do
    arg <- runLocal l
    Mux.debug $ "left-hand side of bind completed: " ++ show arg
    r <- Mux.liftLogged "outer bind:" $ eval lang (apply lang k arg)
    Mux.debug $ "interpreted outer bind: " ++ show r
    case (unRemote lang r) of
      Just r -> handle crypto allow env lang p r
      Nothing -> fail "typechecker bug; function passed to Remote.bind did not return a Remote"
  where
  transfer n t k =
    let r = case k of
          Nothing -> Step (Local (Pure t))
          Just k -> Bind (Local (Pure t)) k
    in if n == currentNode env then handle crypto allow env lang p r
       else do
         Mux.debug $ "transferring to node: " ++ show n
         client crypto allow env p n r
         Mux.debug $ "transferred to node: " ++ show n
  runLocal (Fork r) = do
    Mux.debug $ "runLocal Fork"
    Mux.fork (handle crypto allow env lang p r) $> unit lang
  runLocal CreateChannel = do
    Mux.debug $ "runLocal CreateChannel"
    channel lang . Channel . Mux.channelId <$> Mux.channel
  runLocal Here = do
    Mux.debug $ "runLocal Here"
    pure $ node lang (currentNode env)
  runLocal Spawn = do
    Mux.debug $ "runLocal Spawn"
    n <- Mux.requestTimed "runLocal.spawn" (Mux.seconds 5) (P._spawn p) B.empty
    n <- n
    Mux.debug $ "runLocal Spawn completed: " ++ show n
    pure (node lang n)
  runLocal (Pure t) = do
    Mux.debug $ "runLocal Pure"
    liftIO $ eval lang t
  runLocal (Send (Channel cid) a) = do
    Mux.debug $ "runLocal Send " ++ show cid
    Mux.process1 (Mux.Packet cid (Put.runPutS (serialize a)))
    pure (unit lang)
  runLocal (ReceiveAsync chan@(Channel cid) (Seconds seconds)) = do
    Mux.debug $ "runLocal ReceiveAsync " ++ show (seconds, cid)
    _ <- Mux.receiveTimed ("receiveAsync on " ++ show chan)
      (floor $ seconds * 1000 * 1000) ((Mux.Channel Mux.Type cid) :: Mux.Channel (Maybe B.ByteString))
    pure (remote lang (Step (Local (Receive chan))))
  runLocal (Receive (Channel cid)) = do
    Mux.debug $ "runLocal Receive " ++ show cid
    (recv,_) <- Mux.receiveCancellable (Mux.Channel Mux.Type cid)
    bytes <- recv
    case Get.runGetS deserialize bytes of
      Left err -> fail err
      Right r -> pure r

client :: (Ord h, Serial key, Serial t, Serial h)
       => C.Cryptography key t1 t2 t3 t4 hash Cleartext
       -> ConnectionSandbox key
       -> Env t h
       -> P.Protocol t hash h' h
       -> Node
       -> Remote t
       -> Multiplex ()
client crypto allow env p recipient r = Mux.scope "Remote.client" $ do
  Mux.info $ "initiating connection to " ++ show recipient
  recipientKey <- either fail pure $ Get.runGetS deserialize (publicKey recipient)
  Mux.debug $ "parsed peer public key"
  ok <- allowOut allow recipientKey
  case ok of
    False -> liftIO $ C.threadDelay Mux.delayBeforeFailure >> fail "disallowed outgoing connection"
    True -> pure ()
  Mux.debug $ "allowing connection to proceed"
  menv <- Mux.ask
  connect <- pure . Mux.run menv $
    Mux.pipeInitiate crypto (P._eval p) (recipient, recipientKey) (currentNode env, universe env)
  (send,recv,cipherstate@(encrypt,_)) <-
    Mux.liftLogged "connecting" $ RM.lookupOrReplenish recipient connect (connections env)
  Mux.info $ "connected"
  replyChan <- Mux.channel
  let send' (a,b) = send (Just (a,b))
  _ <- Mux.encryptedRequestTimedVia "client ack" cipherstate (Mux.seconds 5) send' replyChan r
  Mux.debug $ "got ack on " ++ show replyChan
  -- todo - might want to retry if ack doesn't come back
  id $
    let
      go = do
        needs <- recv
        case needs of
          Nothing -> pure ()
          Just Nothing -> pure () -- no other syncs requested, we're good
          Just (Just (hs, replyTo)) -> do
            hashes <- liftIO $ getHashes (codestore env) (Set.fromList hs)
            Mux.encryptAndSendTo' recipient replyTo encrypt (Just hashes)
            go
    in go

{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.NodeContainer where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM (STM)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Data.IORef
import GHC.Generics
import Unison.Runtime.Remote ()
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Serial as S
import qualified Data.Trie as Trie
import qualified Unison.BlockStore as BS
import qualified Unison.Config as Config
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Lock as L
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Util.Logger as L

type Trie = Trie.Trie
type DeleteProof = ByteString

data Keypair k = Keypair { public :: k, private :: B.ByteString } deriving Generic
instance Serial k => Serial (Keypair k)

make :: (Ord h, S.Serial h, S.Serial hash)
     => BS.BlockStore h
     -> (Remote.Node -> IO L.Lock)
     -> P.Protocol term hash h thash
     -> (ByteString -> IO Remote.Node)
     -> (L.Logger -> Remote.Node -> IO (Maybe Mux.Packet -> IO (), IO (Maybe Mux.Packet), STM Bool, DeleteProof))
     -> IO (Mux.Packet -> IO ())
make bs nodeLock p genNode launchNode = do
  logger <- L.scope "container" <$> Config.loggerStandardOut
  -- packet queue, processed by main `go` loop below
  (packetWrite, packetRead) <- newChan :: IO (InChan Mux.Packet, OutChan Mux.Packet)
  -- routing trie for packets; initially empty
  routing <- newIORef (Trie.empty :: Trie (Mux.Packet -> IO ()))
  (writeChan packetWrite <$) . forkIO $
    let
      go = forever $ do
        packet <- readChan packetRead
        routes <- readIORef routing
        let nodeId = Mux.destination packet
        case Trie.lookup nodeId routes of
          -- route did not exist; either wake up a node, or drop the packet
          Nothing -> case Get.runGetS S.deserialize nodeId of
            Left err -> L.warn logger $ "unable to decode node destination: " ++ show err
            Right node -> do
              L.debug logger $ "routing packet to " ++ show node
              h <- BS.resolve bs (nodeSeries node)
              do
              --case h of
              --  Nothing -> (putStrLn $ "message send to nonexistent node: " ++ show node) >> go
              --  Just _ -> do
                  -- todo: check to see if node has been claimed by other container and if so, forward
                  lock <- nodeLock node
                  lease <- L.tryAcquire lock
                  case lease of
                    Nothing -> pure ()
                    Just lease -> do
                      L.info logger $ "waking up node " ++ show node
                      wakeup node packet `finally` L.release lease
          Just dest -> do
            L.debug logger "destination exists; routing"
            safely (dest packet)

      nodeSeries node = BS.Series $ "node-" `mappend` Remote.publicKey node

      wakeup node packet = do
        -- important: we return immediately to main loop after establishing buffer
        -- to hold packets sent to this node. Actual node thread launched asynchronously
        -- and will draw down this buffer
        (toNodeWrite, toNodeRead) <- newChan :: IO (InChan (Maybe Mux.Packet), OutChan (Maybe Mux.Packet))
        logger <- pure $ L.scope (show . Base64.encode . Remote.publicKey $ node) logger
        let send pk = case Get.runGetS S.deserialize (Mux.content pk) of
              Left err -> L.warn logger $ "packet decoding error: " ++ err
              Right pk -> writeChan toNodeWrite (Just pk)
            nodebytes = Put.runPutS $ S.serialize node
        atomicModifyIORef routing $ \t -> (Trie.insert nodebytes send t, ())
        send packet
        let removeRoute = atomicModifyIORef' routing $ \t -> (Trie.delete nodebytes t, ())

        -- spin up a new thread for the node
        void . forkIO . handle (\e -> L.warn logger (show (e :: SomeException)) >> removeRoute) $ do
          L.debug logger "waking.."
          (write, read, isActive, deleteProof) <- launchNode logger node
          L.debug logger "awakened"

          -- deregister the node when idle
          _ <- Async.async $ do
            STM.atomically $ do a <- isActive; when a STM.retry
            L.info logger "node idle, removing route"
            removeRoute

          -- thread for writing to the node, just processes the `toNodeRead` queue
          writer <- Async.async . forever $ do
            pk <- readChan toNodeRead
            L.debug logger $ "writing packet: " ++ show pk
            write pk

          -- establish routes for processing packets coming from the node
          routes <- id $
            let
              routes :: Trie.Trie (ByteString -> IO ())
              routes = Trie.fromList
                [ (Mux.channelId $ P._insert p, insert)
                , (Mux.channelId $ P._lookup p, lookup)
                , (Mux.channelId $ P._declare p, declare)
                , (Mux.channelId $ P._update p, update)
                , (Mux.channelId $ P._append p, append)
                , (Mux.channelId $ P._resolve p, resolve)
                , (Mux.channelId $ P._resolves p, resolves)
                , (Mux.channelId $ P._spawn p, spawn)
                , (deleteProof, delete) ]

              handleRequest :: (S.Serial a, S.Serial b) => (a -> IO b) -> ByteString -> IO ()
              handleRequest h bytes = safely $ do
                (a, replyTo) <- either fail pure (Get.runGetS S.deserialize bytes)
                L.debug logger $ "got request " ++ show (Base64.encode replyTo)
                b <- h a
                L.debug logger $ "got response " ++ show (Base64.encode replyTo)
                writeChan toNodeWrite . Just . Mux.Packet replyTo $ Put.runPutS (S.serialize b)
              insert = handleRequest (BS.insert bs)
              lookup = handleRequest (BS.lookup bs)
              declare = handleRequest (BS.declareSeries bs)
              update = handleRequest (\(series,hash,bytes) -> BS.update bs series hash bytes)
              append = handleRequest (\(series,hash,bytes) -> BS.append bs series hash bytes)
              resolve = handleRequest (BS.resolve bs)
              resolves = handleRequest (BS.resolves bs)
              spawn = handleRequest $ \nodeParams -> do
                -- todo: parse nodeParams enough to know whether to add self as parent
                node <- genNode nodeParams
                let series = nodeSeries node
                h0 <- BS.declareSeries bs series
                Just _ <- BS.update bs series h0 nodeParams
                pure node
              delete proof | BA.constEq proof deleteProof = pure ()
                           | otherwise  = do
                writeChan toNodeWrite Nothing
                BS.deleteSeries bs (BS.Series $ Remote.publicKey node)
                removeRoute
            in pure routes

          processor <- Async.async . Mux.repeatWhile $ do
            L.debug logger $ "processor about to read"
            nodePacket <- read
            case nodePacket of
              Nothing -> False <$ L.info logger "processor completed"
              Just packet -> True <$ do
                L.debug logger $ "got packet " ++ show packet ++ " from worker"
                case Trie.lookup (Mux.destination packet) routes of
                  Just handler -> do
                    L.debug logger $ "found handler for packet"
                    safely $ handler (Mux.content packet) -- handle directly
                  Nothing -> do
                    L.debug logger $ "no existing handler for packet; routing to main loop"
                    writeChan packetWrite packet -- forwarded to main loop

          _ <- forkIO $ do
            r <- Async.waitCatch processor
            L.debug logger $ "worker process terminated with: " ++ show r
            _ <- Async.waitCatch writer
            L.debug logger "worker writer thread terminated"

          pure ()

      safely :: IO () -> IO ()
      safely action = catch action (handle False) where
        handle :: Bool -> SomeException -> IO ()
        handle False ex = L.warn logger $ "Error: " ++ show ex
        handle _ _ = pure ()
    in go

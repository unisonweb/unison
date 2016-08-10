{-# Language OverloadedStrings #-}

module Unison.NodeContainer where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM.TSem
import Control.Concurrent.STM (atomically)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import System.IO (hClose, hFlush, hPutStrLn, stderr, Handle)
import Unison.Runtime.Remote ()
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Serial as S
import qualified Data.Trie as Trie
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified Unison.BlockStore as BS
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Lock as L
import qualified Unison.Runtime.Multiplex as Mux

type Trie = Trie.Trie
type DeleteProof = ByteString

logger :: Handle -> IO (String -> IO ())
logger h = do
  sem <- atomically (newTSem 1)
  pure $ \s -> atomically (waitTSem sem) >> (hPutStrLn h s `finally` atomically (signalTSem sem))

make :: (Ord h, S.Serial h, S.Serial hash)
     => BS.BlockStore h
     -> (Remote.Node -> IO L.Lock)
     -> P.Protocol term hash h thash
     -> (ByteString -> IO Remote.Node)
     -> (Remote.Node -> IO (Handle, Handle, Process.ProcessHandle, DeleteProof))
     -> IO (Mux.Packet -> IO ())
make bs nodeLock p genNode launchNodeCmd = do
  -- packet queue, processed by main `go` loop below
  (packetWrite, packetRead) <- newChan :: IO (InChan Mux.Packet, OutChan Mux.Packet)
  -- routing trie for packets; initially empty
  routing <- newIORef (Trie.empty :: Trie (ByteString -> IO ()))
  info' <- logger stderr
  let info msg = info' $ "[container] " ++ msg
  (writeChan packetWrite <$) . forkIO $
    let
      go = forever $ do
        packet <- readChan packetRead
        routes <- readIORef routing
        let nodeId = Mux.destination packet
        case Trie.lookup nodeId routes of
          -- route did not exist; either wake up a node, or drop the packet
          Nothing -> case Get.runGetS S.deserialize nodeId of
            Left err -> (info $ "unable to decode node destination: " ++ show err)
            Right node -> do
              info $ "routing packet to " ++ show node
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
                      info $ "waking up node " ++ show node
                      wakeup node [Mux.content packet] `finally` L.release lease
          Just dest -> do
            info "destination exists; routing"
            safely (dest (Mux.content packet))

      nodeSeries node = BS.Series $ "node-" `mappend` Remote.publicKey node

      wakeup node packets = do
        -- important: we return immediately to main loop after establishing buffer
        -- to hold packets sent to this node. Actual node process is launched asynchronously
        -- and will draw down this buffer
        (toNodeWrite, toNodeRead) <- newChan :: IO (InChan ByteString, OutChan ByteString)
        let send bytes = writeChan toNodeWrite bytes
        atomicModifyIORef routing $ \t -> (Trie.insert (Put.runPutS $ S.serialize node) send t, ())
        forM_ packets send
        let removeRoute = atomicModifyIORef' routing $ \t -> (Trie.delete (Remote.publicKey node) t, ())

        -- spin up a new process for the node, which we will communicate with over standard input/output
        void . forkIO . handle (\e -> putStrLn (show (e :: SomeException)) >> removeRoute) $ do
          (stdin, stdout, process, deleteProof) <- launchNodeCmd node
          -- read from the process as quickly as possible, buffering input in a queue
          (fromNodeWrite, fromNodeRead) <- newChan
            :: IO (InChan (Maybe Mux.Packet), OutChan (Maybe Mux.Packet))
          let write a _ = writeChan fromNodeWrite a
          reader <- Async.async $ Mux.deserializeHandle stdout B.empty write
          -- now that we have a handle to the process, we write to it from the `toNodeRead` queue
          writer <- Async.async . forever $ do
            (bytes, force) <- tryReadChan toNodeRead
            bytes <- tryRead bytes >>= \bytes -> case bytes of
              Nothing -> hFlush stdin >> force -- flush buffer whenever there's a pause
              Just bytes -> pure bytes -- we're saturating the channel, no need to flush manually
            let nodeBytes = Put.runPutS (S.serialize node)
            info $ "writing bytes " ++ show (B.length bytes)
            safely $
              B.hPut stdin bytes `onException`
              writeChan packetWrite (Mux.Packet nodeBytes bytes)

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
                b <- h a
                send $ Put.runPutS (S.serialize (Mux.Packet replyTo $ Put.runPutS (S.serialize b)))
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
              delete proof | proof /= deleteProof = pure ()
                           | otherwise  = do
                send (Put.runPutS $ S.serialize (Nothing :: Maybe Mux.Packet))
                BS.deleteSeries bs (BS.Series $ Remote.publicKey node)
                removeRoute
            in pure routes

          processor <- Async.async . forever $ do
            nodePacket <- readChan fromNodeRead
            case nodePacket of
              Nothing -> info "worker shut down"
              Just packet -> do
                info $ "got packet " ++ show packet ++ " from worker"
                case Trie.lookup (Mux.destination packet) routes of
                  Just handler -> do
                    info $ "found handler for packet"
                    safely $ handler (Mux.content packet) -- handle directly
                  Nothing -> do
                    info $ "no existing handler for packet; routing to main loop"
                    writeChan packetWrite packet -- forwarded to main loop

          _ <- forkIO $ do
            exitCode <- Process.waitForProcess process
            atomicModifyIORef' routing $ \t -> (Trie.delete (Remote.publicKey node) t, ())
            _ <- Async.waitCatch reader
            _ <- Async.waitCatch writer
            _ <- Async.waitCatch processor
            mapM_ (safely . hClose) [stdin, stdout]
            case exitCode of
              Exit.ExitSuccess -> pure ()
              Exit.ExitFailure n -> putStrLn $ "node process exited with: " ++ show n
          pure ()

      safely :: IO () -> IO ()
      safely action = catch action (handle False) where
        handle :: Bool -> SomeException -> IO ()
        handle False ex = putStrLn $ "Error: " ++ show ex
        handle _ _ = pure ()
    in go

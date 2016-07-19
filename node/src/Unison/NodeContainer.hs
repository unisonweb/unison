{-# Language OverloadedStrings #-}

module Unison.NodeContainer where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.STM (atomically)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import System.IO (hClose, hSetBinaryMode)
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Serial as S
import qualified Data.Trie as Trie
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Block as Block
import qualified Unison.Runtime.Journal as J
import qualified Unison.Runtime.JournaledTrie as JT
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote
import Data.Text (Text)

make :: (Ord h, S.Serial h, S.Serial key, S.Serial privateKey, S.Serial hash)
     => Text
     -> C.Cryptography key symmetricKey signKey signKeyPrivate signature hash Remote.Cleartext
     -> IO (key, privateKey)
     -> BS.BlockStore h
     -> P.Protocol term hash h thash
     -> String
     -> IO ()
make host crypto genKeypair bs p launchNodeCmd = do
  -- The set of all BlockStore.Series keys
  nodeSeries <- journaledTrie "node-series" :: IO (JT.JournaledTrie ())
  -- Node public keys mapped to hash where sandbox, budget, etc are stored
  knownNodes <- journaledTrie "known-nodes" :: IO (JT.JournaledTrie ByteString)
  -- Garbage BlockStore.Series keys, can be safely deleted
  garbage <- journaledTrie "garbage" :: IO (JT.JournaledTrie ())
  -- Maps node public key to parent public key
  nodeParents <- journaledTrie "node-parents" :: IO (JT.JournaledTrie ByteString)
  -- Maps node public key to serialized form of node private key
  nodeKeys <- journaledTrie "node-keys" :: IO (JT.JournaledTrie ByteString)
  -- packet queue, processed by main `go` loop below
  (packetWrite, packetRead) <- newChan :: IO (InChan Mux.Packet, OutChan Mux.Packet)
  -- routing trie for packets; initially empty
  routing <- newIORef Trie.empty
  id $
    let
      go = do
        packet <- readChan packetRead
        routes <- readIORef routing
        let nodeId = Mux.destination packet
        case Trie.lookup nodeId routes of
          -- route did not exist; either wake up a node, or drop the packet
          -- todo: more efficient J.get in terms of readTVarIO
          Nothing -> do
            alive <- isAlive nodeId
            case alive of
              False -> deleteNode nodeId >> putStrLn "packet sent to deleted destination"
              True -> case Get.runGetS S.deserialize nodeId of
                Left err -> (putStrLn $ "unable to decode node destination: " ++ show err) >> go
                Right node -> wakeup node [Mux.content packet]
          Just dest -> safely (dest (Mux.content packet)) >> go

      deleteNode nodeId = atomically $ do
        _ <- J.updateNowAsyncFlush (Just (JT.Delete nodeId)) knownNodes
        _ <- J.updateNowAsyncFlush (Just (JT.Delete nodeId)) nodeParents
        allocated <- J.get nodeSeries
        _ <- J.updateNowAsyncFlush (Just (JT.DeleteSubtree nodeId)) nodeSeries
        _ <- J.updateNowAsyncFlush (Just (JT.UnionL (Trie.submap nodeId allocated))) garbage
        pure ()

      cmd = (Process.shell launchNodeCmd) {
        Process.std_out = Process.CreatePipe,
        Process.std_in = Process.CreatePipe,
        Process.std_err = Process.CreatePipe }

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
          (Just stdin, Just stdout, Just stderr, process) <- Process.createProcess cmd
          hSetBinaryMode stdin True
          hSetBinaryMode stdout True
          -- read from the process as quickly as possible, buffering input in a queue
          (fromNodeWrite, fromNodeRead) <- newChan
            :: IO (InChan (Maybe Mux.Packet), OutChan (Maybe Mux.Packet))
          reader <- Async.async $ Mux.deserializeHandle stdout B.empty (writeChan fromNodeWrite)
          -- now that we have a handle to the process, we write to it from the `toNodeRead` queue
          writer <- Async.async . forever $ do
            bytes <- readChan toNodeRead
            let nodeBytes = Put.runPutS (S.serialize node)
            safely $
              B.hPut stdin bytes `onException`
              writeChan packetWrite (Mux.Packet nodeBytes bytes)

          -- tell the node its private key and hash where it's sandbox, etc, can be found
          Just private <- Trie.lookup (Remote.publicKey node) <$> atomically (J.get nodeKeys)
          Just hash <- Trie.lookup (Remote.publicKey node) <$> atomically (J.get knownNodes)

          writeChan toNodeWrite $ (Put.runPutS $ S.serialize private)
          writeChan toNodeWrite $ hash
          writeChan toNodeWrite $ (Put.runPutS $ S.serialize node)

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
                , (deleteProof, delete deleteProof) ]

              -- to delete a node from the container, send the container a hash of node private key
              deleteProof = Put.runPutS (S.serialize $ C.hash crypto [Put.runPutS (S.serialize private)])

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
              spawn = handleRequest $ \bytes -> do
                h <- BS.insert bs bytes
                -- todo: parse this enough to know whether to add self as parent
                (public,private) <- genKeypair
                let publicBytes = Put.runPutS (S.serialize public)
                let u = Just $ JT.Insert publicBytes (Put.runPutS $ S.serialize private)
                let u2 = Just $ JT.Insert publicBytes (Put.runPutS $ S.serialize h)
                let u3 = Just $ JT.Insert publicBytes (Remote.publicKey node)
                _ <- atomically (J.updateNowAsyncFlush u nodeKeys)
                _ <- atomically (J.updateNowAsyncFlush u2 knownNodes)
                _ <- atomically (J.updateNowAsyncFlush u3 nodeParents)
                pure $ Remote.Node host publicBytes
              delete proof _ | proof /= deleteProof = pure ()
                             | otherwise  = do
                send (Put.runPutS $ S.serialize (Nothing :: Maybe Mux.Packet))
                let nodePk = Remote.publicKey node
                let nodeBytes = Put.runPutS $ S.serialize node
                atomicModifyIORef' routing $ \t -> (Trie.delete nodeBytes t, ())
                _ <- atomically $
                  J.updateNowAsyncFlush (Just $ JT.Delete nodePk) knownNodes >>
                  J.updateNowAsyncFlush (Just $ JT.Delete nodePk) nodeKeys >>
                  J.updateNowAsyncFlush (Just $ JT.Delete nodePk) nodeParents
                pure ()
            in pure routes

          processor <- Async.async . forever $ do
            nodePacket <- readChan fromNodeRead
            case nodePacket of
              Nothing -> pure ()
              Just packet -> case Trie.lookup (Mux.destination packet) routes of
                Just handler -> safely $ handler (Mux.content packet) -- handle directly
                Nothing -> writeChan packetWrite packet -- forwarded to main loop

          _ <- forkIO $ do
            exitCode <- Process.waitForProcess process
            atomicModifyIORef' routing $ \t -> (Trie.delete (Remote.publicKey node) t, ())
            _ <- Async.waitCatch reader
            _ <- Async.waitCatch writer
            _ <- Async.waitCatch processor
            mapM_ hClose [stdin, stdout, stderr]
            case exitCode of
              Exit.ExitSuccess -> pure ()
              Exit.ExitFailure n -> putStrLn $ "node process exited with: " ++ show n
          pure ()

      isAlive nodeId = do
        knownNodes <- atomically $ J.get nodeSeries
        nodeParents <- atomically $ J.get nodeParents
        pure $ case Trie.member nodeId knownNodes of
          False -> False
          True -> case Trie.lookup nodeId nodeParents of
            Nothing -> True -- node is root, keep it alive
            Just parent | Trie.member parent knownNodes -> True -- node is non-root, parent alive
                        | otherwise -> False -- node is non-root, parent not alive

      safely :: IO () -> IO ()
      safely action = catch action (handle True) where
        handle :: Bool -> SomeException -> IO ()
        handle False ex = putStrLn $ "Error: " ++ show ex
        handle _ _ = pure ()
    in go
  where

  qname name = Put.runPutS $ S.serialize (C.publicKey crypto) >> Put.putByteString name
  journaledTrie :: S.Serial v => ByteString -> IO (JT.JournaledTrie v)
  journaledTrie name = atomically . J.checkpointEvery 10000 =<< JT.fromBlocks bs
    (Block.encrypted crypto $ Block.fromSeries (BS.Series (qname name)))
    (Block.encrypted crypto $ Block.fromSeries (BS.Series (qname name `mappend` "-updates")))

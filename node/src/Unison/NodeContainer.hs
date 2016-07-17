{-# Language OverloadedStrings #-}

module Unison.NodeContainer where

import Data.IORef
import Control.Exception
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Serial as S
import qualified Data.Map as Map
import qualified Data.Trie as Trie
import qualified System.Process as Process
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Block as Block
import qualified Unison.Runtime.Journal as J
import qualified Unison.Runtime.JournaledMap as JM
import qualified Unison.Runtime.JournaledTrie as JT
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote

make :: (Ord h, S.Serial key)
     => C.Cryptography key symmetricKey signKey signKeyPrivate signature hash Remote.Cleartext
     -> BS.BlockStore h
     -> P.Protocol term hash h thash
     -> String
     -> IO ()
make crypto bs p launchNodeCmd = do
  -- trie keyed by node public keys, maps nodes to their series
  -- root of the trie for a node key is the parameters for that node, it's key pair, etc
  knownNodes <- journaledTrie "known-nodes" :: IO (JT.JournaledTrie BS.Series)
  freeList <- journaledMap "free-list" :: IO (JM.JournaledMap Word64 BS.Series)
  -- trie keyed by node public keys, maps nodes to their parents' public key
  nodeParents <- journaledTrie "node-parents" :: IO (JT.JournaledTrie ByteString)
  packetQ <- atomically newTQueue :: IO (TQueue Mux.Packet)
  -- todo: nodes should be told their keypair over standard input on startup
  routing <- newIORef Trie.empty
  id $
    let
      -- just add BlockStore.deleteSeries, have BlockStore impl delete incrementally
      -- from this, using the free list
      -- basic idea for main loop
      --   packets are labeled with their destination
      --   if a destination does not exist in existing routes, check the knownNodes map
      --   if known, check parents map - if there is a parent, and the parent
      --   is not a known node, then the node is linked to its parent which is
      --   now destroyed; destroy that node
      --   if there is a parent, and the parent is a known node, spin up
      --   a process for that node and update routes accordingly
      --
      go = do
        packet <- atomically $ readTQueue packetQ
        routes <- readIORef routing
        let d = Mux.destination packet
        case Trie.lookup d routes of
          -- route did not exist; either wake up a node, or drop the packet
          -- todo: more efficient J.get in terms of readTVarIO
          Nothing -> do
            knownNodes <- atomically $ J.get knownNodes
            nodeParents <- atomically $ J.get nodeParents
            case Trie.member d knownNodes of
              False -> putStrLn "dropped packet sent to unknown destination"
              True ->
                if ok then undefined
                else undefined -- GC the node - add its entire subtree to the free list
                where
                ok = case Trie.lookup d nodeParents of
                  Nothing -> True
                  Just parent | Trie.member parent knownNodes -> True
                              | otherwise -> False
          Just dest -> safely (dest (Mux.content packet)) >> go
      routing0 :: Trie.Trie (ByteString -> IO ())
      routing0 = Trie.fromList
        [ (Mux.channelId $ P._spawn p, spawn)
        , (Mux.channelId $ P._insert p, insert)
        , (Mux.channelId $ P._lookup p, lookup)
        , (Mux.channelId $ P._declare p, declare)
        , (Mux.channelId $ P._update p, update)
        , (Mux.channelId $ P._append p, append)
        , (Mux.channelId $ P._resolve p, resolve)
        , (Mux.channelId $ P._resolves p, resolves) ]
      spawn bytes = undefined
      insert bytes = undefined
      lookup bytes = undefined
      declare bytes = undefined
      update bytes = undefined
      append bytes = undefined
      resolve bytes = undefined
      resolves bytes = undefined

      parse :: S.Serial a => ByteString -> IO a
      parse bytes = either fail pure $ Get.runGetS S.deserialize bytes
      safely :: IO () -> IO ()
      safely action = catch action handle
      handle :: SomeException -> IO ()
      handle ex = putStrLn $ "Error: " ++ show ex
    in writeIORef routing routing0 >> go
  where

  qname name = Put.runPutS $ S.serialize (C.publicKey crypto) >> Put.putByteString name
  journaledTrie :: S.Serial v => ByteString -> IO (JT.JournaledTrie v)
  journaledTrie name = atomically . J.checkpointEvery 10000 =<< JT.fromBlocks bs
    (Block.encrypted crypto $ Block.fromSeries (BS.Series (qname name)))
    (Block.encrypted crypto $ Block.fromSeries (BS.Series (qname name `mappend` "-updates")))
  journaledMap :: (S.Serial k, Ord k, S.Serial v) => ByteString -> IO (JM.JournaledMap k v)
  journaledMap name = atomically . J.checkpointEvery 1000 =<< JM.fromEncryptedSeries crypto bs
    (BS.Series $ qname name)
    (BS.Series $ qname (name `mappend` "-updates"))

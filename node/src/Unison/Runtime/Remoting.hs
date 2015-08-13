{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Runtime.Remoting where

import Control.Monad
import Control.Concurrent (MVar)
import Debug.Trace
import Data.Int (Int32, Int64)
import Data.ByteString (ByteString)
import Data.Serialize.Get (Get)
import Data.Word (Word64)
import qualified Data.ByteString as BS
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Serial (Serial)
import GHC.Generics
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.ByteString (readExactly)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString as ByteString
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Serial as Serial
import qualified Network.SockAddr as SockAddr -- wtf is with this name?
import qualified Network.Simple.TCP as TCP
import qualified System.IO.Streams as Streams

type CMap = CMap.Map

type Err = String
type Channel = Int
type Port = Int

unisonPort :: Port
unisonPort = 8097

type Host = String

data Reply = Reply Port Channel | DontReply deriving (Generic, Show)
instance Serial Reply

data Packet t =
   Evaluate Reply t
 | Result Channel (Either Err t)
 deriving (Generic, Show)

instance Serial t => Serial (Packet t)

class Evaluate t env | env -> t where
  evaluate :: env -> t -> IO (Either Err t)

serve :: (Show t, Serial t, Evaluate t env)
      => env
      -> Waiting t
      -> Port
      -> IO ()
serve env waiting port =
  TCP.serve (TCP.Host "127.0.0.1") (show port) go
  where
  go (clientSocket, remoteAddr) = do
    let remoteHost = SockAddr.showSockAddr remoteAddr
    putStrLn $ "client connected: " ++ remoteHost
    (i,o) <- Streams.socketToStreams clientSocket
    messageHandler waiting remoteHost env i o

-- Message Format:
--  * Number of bytes of the packet (32 bit int)
--  * a `Packet`, serialized via `Serial`

-- traceBS :: String -> ByteString -> ByteString
-- traceBS msg b | traceShow (BS.length b) False = undefined
-- traceBS msg b = b

putPacket :: forall m t. (Put.MonadPut m, Serial t) => Packet t -> m ()
putPacket p =
  Serial.serialize size *> Put.putByteString bs
  where bs = Put.runPutS (Serial.serialize p)
        size = fromIntegral (BS.length bs) :: Int32

-- reads an int first that says how long the payload is.
-- then reads the payload and deserializes it
deserializeLengthEncoded :: Serial a => String -> InputStream ByteString -> IO a
deserializeLengthEncoded msg i = do
  sizeBytes <- readExactly 4 i
  sizeInt32 <- tryDeserialize "Int32" (Serial.deserialize :: Get Int32) sizeBytes
  readExactly (fromIntegral sizeInt32) i >>= tryDeserialize msg Serial.deserialize

-- todo: write a proper version of readExactly that takes an `Word64` or an `Integral`

tryDeserialize :: String -> Get a -> ByteString -> IO a
tryDeserialize msg get bytes =
  case Get.runGetS get bytes of
    Left err  -> fail $ "expected " ++ msg ++ ", got " ++ err
    Right a -> pure a

type Waiting t = CMap Channel (MVar (Either Err t))

messageHandler :: (Show t, Serial t, Evaluate t env)
               => Waiting t
               -> Host
               -> env
               -> InputStream ByteString
               -> OutputStream ByteString -> IO ()
messageHandler waiting remoteHost env i _ = do
  packet <- deserializeLengthEncoded "packet" i
  void . Concurrent.forkIO $ act waiting remoteHost env packet

act :: (Show t, Serial t, Evaluate t env) => Waiting t -> Host -> env -> Packet t -> IO ()
act _ _ env (Evaluate DontReply t) = evaluate env t >> return ()
act _ remoteHost env (Evaluate (Reply port chan) t) = do
  e <- evaluate env t
  -- serialize and send back
  let bytes = Put.runPutS (putPacket (Result chan e))
  client remoteHost port $ Streams.write (Just bytes)
act waiting remoteHost _ (Result chan e) = do
  v <- CMap.lookup chan waiting
  maybe (return ()) (\mvar -> Concurrent.putMVar mvar e) v

-- punt on weak references for now
send :: Serial t => Waiting t -> Host -> Port -> Reply -> t -> IO (Either Err t)
send waiting remoteHost remotePort replyTo expr = do
  var <- Concurrent.newEmptyMVar
  let Reply _ chan = replyTo
  CMap.insert chan var waiting
  let bytes = Put.runPutS (putPacket (Evaluate replyTo expr))
  client remoteHost remotePort (Streams.write (Just bytes))
  Concurrent.takeMVar var

-- TCP client, just given an OutputStream
client :: Host -> Port -> (OutputStream ByteString -> IO ()) -> IO ()
client host port send =
  TCP.connect host (show port) go where
  go (socket, _) = do
    (_,o) <- Streams.socketToStreams socket
    send o

sendPacket :: Serial t => Host -> Port -> Packet t -> IO ()
sendPacket remoteHost port p =
  client remoteHost port $
  Streams.write (Just $ Put.runPutS (putPacket p))


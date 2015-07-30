 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FunctionalDependencies #-}

module Unison.Runtime.Remoting where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Serial (Serial)
import GHC.Generics
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.ByteString (readExactly)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString as ByteString
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
      -> Port
      -> IO ()
serve env port =
  TCP.serve (TCP.Host "127.0.0.1") (show port) go
  where
  go (clientSocket, remoteAddr) = do
    let remoteHost = SockAddr.showSockAddr remoteAddr
    putStrLn $ "client connected: " ++ remoteHost
    (i,o) <- Streams.socketToStreams clientSocket
    messageHandler remoteHost env i o

-- Message Format:
--  * Number of bytes of the packet (32 bit int)
--  * a `Packet`, serialized via `Serial`

putPacket :: (Put.MonadPut m, Serial t) => Packet t -> m ()
putPacket p = Put.putByteString $ Put.runPutS (Serial.serialize p)

-- reads an int first that says how long the payload is.
-- then reads the payload and deserializes it
deserializeLengthEncoded :: Serial a => String -> InputStream ByteString -> IO a
deserializeLengthEncoded msg i = deserialize' i 4 >>= deserialize' i where
  deserialize' :: Serial a => InputStream ByteString -> Int -> IO a
  deserialize' i n = readExactly n i >>= tryDeserialize msg

tryDeserialize :: Serial a => String -> ByteString -> IO a
tryDeserialize msg bytes =
  case runGetS Serial.deserialize bytes of
    Left err  -> fail $ "expected " ++ msg ++ ", got " ++ err
    Right a -> pure a

messageHandler :: (Show t, Serial t, Evaluate t env)
               => Host
               -> env
               -> InputStream ByteString
               -> OutputStream ByteString -> IO ()
messageHandler remoteHost env i _ = do
  packet <- deserializeLengthEncoded "packet" i
  void . Concurrent.forkIO $ act remoteHost env packet

type Waiting t = CMap Channel (Either Err t)

act :: (Show t, Serial t, Evaluate t env) => Host -> env -> Packet t -> IO ()
act _ env (Evaluate DontReply t) = evaluate env t >> return ()
act remoteHost env (Evaluate (Reply port chan) t) = do
  e <- evaluate env t
  -- serialize and send back
  let bytes = Put.runPutS (putPacket (Result chan e))
  client remoteHost port $ Streams.write (Just bytes)
act remoteHost _ (Result chan e) = do
  putStrLn $ "from: " ++ show remoteHost
  putStrLn $ "on channel: " ++ show chan
  putStrLn $ "got: " ++ show e
  -- todo, the real thing

-- TCP client, just given an OutputStream
client :: Host -> Int -> (OutputStream ByteString -> IO ()) -> IO ()
client host port send =
  TCP.connect host (show port) go where
  go (socket, _) = do
    (_,o) <- Streams.socketToStreams socket
    send o

data DummyEnv = DummyEnv
data Prog = Prog [String] deriving (Show, Generic)
instance Serial Prog

instance Evaluate Prog DummyEnv where
  evaluate _ (Prog s) = return . return . Prog $ [join s]

sendPacket :: Serial t => Host -> Port -> Packet t -> IO ()
sendPacket remoteHost port p =
  client remoteHost port $
  Streams.write (Just . ByteString.drop 4 $ Put.runPutS (putPacket p))

main :: IO ()
main = do
 port <- read <$> getLine
 serve DummyEnv port

sendTestStrings :: Port -> Channel -> Host -> Port -> [String] -> IO ()
sendTestStrings myPort replyChannel remoteHost remotePort strings =
  sendPacket remoteHost remotePort (Evaluate (Reply myPort replyChannel) strings)


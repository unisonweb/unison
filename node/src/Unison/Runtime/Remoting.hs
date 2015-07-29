 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FunctionalDependencies #-}

module Unison.Runtime.Remoting where

import Control.Monad
import Control.Exception.Base (finally)
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Serial (Serial)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics
import Network.Socket
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.ByteString (readExactly)
import System.IO.Streams.Network (socketToStreams)
import Unison.Node (Node)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString as ByteString
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Serial as Serial
import qualified Data.Text as Text
import qualified System.IO.Streams as Streams

type CMap = CMap.Map

type Err = String
type Channel = Int
type Port = Int

type Host = Text

data Message t =
   Evaluate t
 | Result (Either Err t)
 deriving (Generic, Show)
instance Serial t => Serial (Message t)

data Address = Address Host Port Channel deriving (Generic, Show)
instance Serial Address

class Evaluate t env | env -> t where
  evaluate :: env -> t -> IO (Either Err t)

serve :: (Show t, Serial t, Evaluate t env)
      => env
      -> Address
      -> IO ()
serve env localAddr@(Address _ port _) = withSocketsDo $ do
  addrinfos <- getAddrInfo
    	 	(Just (defaultHints {addrFlags = [AI_PASSIVE]}))
	  	Nothing (Just $ show port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)

  -- Start listening for connection requests.  Maximum queue size
  -- of 5 connection requests waiting to be accepted.
  listen sock 5

  -- Loop forever waiting for connections.  Ctrl-C to abort.
  procRequests sock

  where
  -- | Process incoming connection requests
  procRequests :: Socket -> IO ()
  procRequests mastersock = do
    (connsock, clientaddr) <- accept mastersock
    (i,o) <- Streams.socketToStreams connsock
    _ <- Concurrent.forkIO $ finally (messageHandler localAddr env i o) (close connsock)
    procRequests mastersock

---------------------
-- Message Format  --
---------------------
-- Number of bytes in sender address
---------------------
-- Receiver channel  ------
---------------------
-- Number of bytes in payload
---------------------
-- Payload ------
---------------------

----------
-- address size
-- hostname
-- port
-- channel  (this is the channel to reply to when evaluation completes)
-- channel  (the channel on the reciever that this message should go to)
-- payload size
-- payload: Evaluate (code)
----------

----------
-- address size -- these first 4 things don't matter yet
-- hostname
-- port
-- channel  (this is the channel to reply to when evaluation completes)
-- channel  (the channel on the reciever that this message should go to)
-- payload size
-- payload: Result (code)
----------

data Packet t = Packet Address Channel (Message t)

putPacket :: (Put.MonadPut m, Serial t) => Packet t -> m ()
putPacket (Packet addr chanTo m) = do
  Put.putByteString $ Put.runPutS (Serial.serialize addr)
  Serial.serialize chanTo
  Put.putByteString $ Put.runPutS (Serial.serialize m)

-- reads an int first that says how long the payload is.
-- then reads the payload and deserializes it
deserializeLengthEncoded :: Serial a => InputStream ByteString -> IO a
deserializeLengthEncoded i = deserialize' i 4 >>= deserialize' i where
  deserialize' :: Serial a => InputStream ByteString -> Int -> IO a
  deserialize' i n = readExactly n i >>= tryDeserialize "Int"

tryDeserialize :: Serial a => String -> ByteString -> IO a
tryDeserialize msg bytes =
  case runGetS Serial.deserialize bytes of
    Left err  -> fail $ "expected " ++ msg ++ ", got " ++ err
    Right a -> pure a

deserializeChannel :: InputStream ByteString -> IO Channel
deserializeChannel i = readExactly 4 i >>= tryDeserialize "Channel"

messageHandler :: (Show t, Serial t, Evaluate t env)
               => Address
               -> env
               -> InputStream ByteString
               -> OutputStream ByteString -> IO ()
messageHandler localAddr env i o = do
  addr <- deserializeLengthEncoded i -- reads the address size first, then the address
  chan <- deserializeChannel i
  msg  <- deserializeLengthEncoded i -- reads the message size first, then the message
  act localAddr env (Packet addr chan msg)

type Waiting t = CMap Channel (Either Err t)

act :: (Show t, Serial t, Evaluate t env) => Address -> env -> Packet t -> IO ()
act localAddr env (Packet sender@(Address host port chan) _ (Evaluate t)) = do
  -- evaluate
  e <- evaluate env t
  -- serialize and sent
  let bytes = Put.runPutS (putPacket (Packet localAddr chan (Result e)))
  client host port $ Streams.write (Just bytes)
act localAddr env (Packet remoteAddr chan (Result et)) = do
  putStrLn $ "from: " ++ show remoteAddr
  putStrLn $ "on channel: " ++ show chan
  putStrLn $ "got: " ++ show et
  -- todo, the real thing

-- tcp client via continuation
client :: Host -> Int -> (OutputStream ByteString -> IO ()) -> IO ()
client host port send = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just $ Text.unpack host) (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  flip finally (sClose sock) $ do
    (_,o) <- Streams.socketToStreams sock
    send o

data DummyEnv = DummyEnv
data Prog = Prog [String] deriving (Show, Generic)
instance Serial Prog

instance Evaluate Prog DummyEnv where
  evaluate _ (Prog s) = return . return . Prog $ [join s]

sendPacket :: Serial t => Address -> Packet t -> IO ()
sendPacket (Address host port chan) p =
  client host port $
  Streams.write (Just . ByteString.drop 4 $ Put.runPutS (putPacket p))

main :: IO ()
main = do
  putStrLn "Local host: "
  ip <- getLine
  let localAddress = Address (Text.pack ip) 8081 0
  _ <- Concurrent.forkIO $ serve DummyEnv localAddress
  return ()

sendTestStrings :: Host -> Port -> Channel -> Host -> Port -> [String] -> IO ()
sendTestStrings localHost localPort channel remoteHost remotePort strings = go where
  packet = Packet (Address localHost localPort channel) 0 (Evaluate strings)
  go = sendPacket (Address remoteHost remotePort 0) packet


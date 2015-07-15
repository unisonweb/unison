 {-# LANGUAGE DeriveGeneric #-}

module Unison.Runtime.Remoting where

import qualified Control.Concurrent as Concurrent
import Control.Exception.Base (finally)
import Data.Bytes.Serial (Serial)
import qualified Data.Bytes.Serial as Serial
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import qualified Data.ByteString as ByteString
import Data.Word (Word32)
import GHC.Generics
import Network.Socket
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.ByteString (readExactly)
import System.IO.Streams.Network (socketToStreams)
import Unison.Node (Node)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)

type Err = String
type Channel = Int
type Port = Int

data IP = IPv4 !Word32 | IPv6 !Word32 !Word32 !Word32 !Word32 deriving (Generic)
instance Serial IP

data Message =
   Evaluate Term 
 | Result (Either Err Term)
 deriving (Generic)
instance Serial Message

data Address = Address IP Port Channel deriving (Generic)
instance Serial Address


serve :: Node IO Reference Type Term
      -> String              -- ^ Port number or name; 514 is default
      -> IO ()
serve node port = withSocketsDo $ do
  addrinfos <- getAddrInfo 
    	 	(Just (defaultHints {addrFlags = [AI_PASSIVE]}))
	  	Nothing (Just port)
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
    _ <- Concurrent.forkIO $ finally (messageHandler node i o) (close connsock)
    procRequests mastersock

-- reads an int first that says how long the payload is.
-- then reads the payload and deserializes it
deserialize :: Serial a => InputStream ByteString -> IO a
deserialize i = deserialize' i 4 >>= deserialize' i where
  deserialize' :: Serial a => InputStream ByteString -> Int -> IO a
  deserialize' i n = readExactly n i >>= f where
    f bytes = case runGetS Serial.deserialize bytes of
      Left err  -> fail $ "expected to read an Int, got error: " ++ err
      Right len -> pure len

messageHandler :: Node IO Reference Type Term
               -> InputStream ByteString
               -> OutputStream ByteString -> IO ()
messageHandler node i o = do
  addr <- deserialize i -- reads the address size first, then the address
  msg  <- deserialize i -- reads the message size first, then the message
  act node addr msg

act :: Node IO Reference Type Term -> Address -> Message -> IO ()
act node addr (Evaluate t) = error "todo"
act node addr (Result e)   = error "todo"


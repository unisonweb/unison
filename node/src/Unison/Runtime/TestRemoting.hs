{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Unison.Runtime.TestRemoting where

import Control.Concurrent (MVar)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Serial (Serial)
import Data.Int (Int32, Int64)
import Data.Serialize.Get (Get)
import Data.Word (Word64)
import Debug.Trace
import GHC.Generics
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.ByteString (readExactly)
import Unison.Runtime.Remoting
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Map as CMap
import qualified Data.ByteString as BS
import qualified Data.ByteString as ByteString
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Serial as Serial
import qualified Network.Simple.TCP as TCP
import qualified Network.SockAddr as SockAddr -- wtf is with this name?
import qualified System.IO.Streams as Streams

data DummyEnv = DummyEnv
newtype Prog = Prog { progStrings :: [String] } deriving (Show, Generic)
instance Serial Prog

instance Evaluate Prog DummyEnv where
  evaluate _ (Prog s) = return . return . Prog $ [join s]

{-

Instructions to test, in one repl, do:

> import Unison.Runtime.TestRemoting
> server
Enter port number: 8081

In another repl, do:

> import Unison.Runtime.TestRemoting
> client 8081
Enter port number: 8082
client connected: 127.0.0.1
Right (Prog {progStrings = ["hello world!!"]})

-}

server :: IO ()
server = do
  waiting <- CMap.empty
  putStr "Enter port number: "
  port <- read <$> getLine
  serve DummyEnv waiting port

freshChannel :: IO Channel
freshChannel = pure 7 -- todo, generate a cryptographically secure fresh channel

client :: Port -> IO ()
client remotePort = do
  waiting <- CMap.empty
  putStr "Enter port number: "
  port <- read <$> getLine
  _ <- Concurrent.forkIO $ serve DummyEnv waiting port
  let prog = Prog ["hello", " world!!"]
  e <- sendTestStrings waiting port 42 "localhost" remotePort prog
  putStrLn $ show e

sendTestStrings :: Waiting Prog -> Port -> Channel -> Host -> Port -> Prog
                -> IO (Either Err Prog)
sendTestStrings waiting myPort replyChannel remoteHost remotePort p =
  send waiting remoteHost remotePort (Reply myPort replyChannel) p

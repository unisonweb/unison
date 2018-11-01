module Unison.Codebase.Runtime.JVM where

import           Control.Applicative
import           Control.Monad.State       (evalStateT)
import           Data.Bytes.Put            (runPutS)
import           Data.ByteString           (ByteString)
import           Network.Socket
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Streams
import qualified System.IO.Streams.Network as N
import qualified System.Process            as P
import           Unison.Codebase           (Codebase)
import           Unison.Codebase.Runtime   (Runtime (..))
import qualified Unison.Codecs             as Codecs
import           Unison.UnisonFile         (UnisonFile)
import           Unison.Var                (Var)

javaRuntime :: Var v => Int -> IO (Runtime v)
javaRuntime suggestedPort = do
  (listeningSocket, port) <- choosePortAndListen suggestedPort
  (killme, input, output) <- connectToRuntime listeningSocket port
  pure $ Runtime killme (feedme input output)
  where
    feedme :: Var v
           => InputStream ByteString -> OutputStream ByteString
           -> UnisonFile v a -> Codebase IO v b -> IO ()
    feedme _input output unisonFile _codebase = do
      -- todo: runtime should be able to request more terms/types/arities by hash
      let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile
      Streams.write (Just bs) output

    -- open a listening socket for the runtime to connect to
    choosePortAndListen :: Int -> IO (Socket, Int)
    choosePortAndListen suggestedPort = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      let bindLoop port =
            (port <$ bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY))
            <|> bindLoop (port + 1) -- try the next port if that fails
      chosenPort <- bindLoop suggestedPort
      listen sock 2
      pure (sock, chosenPort)

    -- start the runtime and wait for it to connect to us
    connectToRuntime ::
      Socket -> Int -> IO (IO (), InputStream ByteString, OutputStream ByteString)
    connectToRuntime listenSock port = do
      let cmd = "scala"
          args = ["-cp", "runtime-jvm/.bloop/main/scala-2.12/classes",
                  "org.unisonweb.BootstrapStream", show port]
      (_,_,_,ph) <- P.createProcess (P.proc cmd args) { P.cwd = Just "." }
      (socket, _address) <- accept listenSock -- accept a connection and handle it
      (input, output) <- N.socketToStreams socket
      pure (P.terminateProcess ph, input, output)

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.Runtime.JVM where

import           Control.Applicative
import           Control.Monad.IO.Class         ( MonadIO, liftIO )
import           Control.Monad.State            ( evalStateT )
import           Data.Bytes.Get                 ( getWord8
                                                , runGetS
                                                , MonadGet
                                                )
import           Data.Bytes.Put                 ( runPutS )
import           Data.ByteString                ( ByteString )
import           Network.Socket
import           System.IO.Streams              ( InputStream
                                                , OutputStream
                                                )
import           System.Directory               ( findExecutable)
import qualified System.IO.Streams             as Streams
import qualified System.IO.Streams.ByteString  as BSS
import qualified System.IO.Streams.Network     as N
import qualified System.Process                as P
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.Runtime        ( Runtime(..))
import           Unison.Codebase.CodeLookup     ( CodeLookup )
import qualified Unison.Codebase.Serialization.V0
                                               as Szn
import qualified Unison.Codecs                 as Codecs
import           Unison.Term                    ( AnnotatedTerm, Term )
import           Unison.Var                     ( Var )

javaRuntime :: (Var v, MonadIO m) => (forall g. MonadGet g => g v) -> Int -> m (Runtime v)
javaRuntime getv suggestedPort = do
  (listeningSocket, port) <- liftIO $ choosePortAndListen suggestedPort
  (killme, input, output) <- liftIO $ connectToRuntime listeningSocket port
  pure $ Runtime (liftIO killme) (feedme getv input output)
 where
    processWatches getv acc = do
      marker <- getWord8
      case marker of
        0 -> do
          label <- Szn.getText
          term  <- Szn.getTerm getv (pure ())
          processWatches getv $ (label, term) : acc
        1 -> do
          term <- Szn.getTerm getv (pure ())
          pure $ (reverse acc, term)
        x -> fail $ "Unexpected byte in JVM output: " ++ show x
    feedme
      :: forall v a m. (Var v, MonadIO m, Monoid a)
      => (forall g. MonadGet g => g v)
      -> InputStream ByteString
      -> OutputStream ByteString
      -> CodeLookup m v a
      -> AnnotatedTerm v a
      -> m (Term v)
    feedme getv input output codeLookup tm = do
      -- todo: runtime should be able to request more terms/types/arities by hash
      unisonFile <- Codebase.makeSelfContained codeLookup mempty tm
      liftIO $ do
        let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile tm
        Streams.write (Just bs) output

        bs <- BSS.readExactly 8 input
        case runGetS Szn.getInt bs of
          Left e -> fail e
          Right size -> do
            bs <- BSS.readExactly (fromIntegral size) input
            case runGetS (processWatches getv []) bs of
              Left e -> fail e
              Right x -> pure (snd x)

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
      exe <- findExecutable cmd
      case exe of
        Just _ -> do
          (_,_,_,ph) <- P.createProcess (P.proc cmd args) { P.cwd = Just "." }
          (socket, _address) <- accept listenSock -- accept a connection and handle it
          (input, output) <- N.socketToStreams socket
          pure (P.terminateProcess ph, input, output)
        Nothing -> fail $ cmd ++ " executable not found, Unison needs a matching scala version available on the path"

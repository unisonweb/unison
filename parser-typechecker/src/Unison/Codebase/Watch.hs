{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

module Unison.Codebase.Watch where

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forever)
import           System.FSNotify
import Network.Socket
import Control.Monad
import Control.Applicative
import qualified System.IO.Streams.Network as N
import           Data.Foldable      (toList)
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser      as Parser
import qualified Unison.Parsers     as Parsers
import           Unison.Util.AnnotatedText (renderTextUnstyled)
import           Unison.PrintError  (printNoteWithSourceAsAnsi, renderType')
import           Unison.Result      (Result (Result))
import           Unison.Util.Monoid
import qualified System.IO.Streams as Streams
import qualified System.Process as P
import Control.Exception (finally)

watchDirectory :: FilePath -> IO (IO FilePath)
watchDirectory d = do
  mvar <- newEmptyMVar
  let doIt fp = do
        _ <- tryTakeMVar mvar
        putMVar mvar fp
      handler e = case e of
                Added fp _ False -> doIt fp
                Modified fp _ False -> doIt fp
                _ -> pure ()
  _ <- forkIO $ withManager $ \mgr -> do
    _ <- watchDir mgr d (const True) handler
    forever $ threadDelay 1000000
  pure $ takeMVar mvar

watcher :: FilePath -> Int -> IO ()
watcher dir port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  let bindLoop port =
        bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
        <|> bindLoop (port + 1) -- try the next port if that fails
  bindLoop port
  listen sock 2
  serverLoop dir sock

serverLoop :: FilePath -> Socket -> IO ()
serverLoop dir sock = do
  let cmd = "scala"
      args = ["-cp", "runtime-jvm/main/target/scala-2.12/classes",
              "org.unisonweb.Bootstrap"] -- todo: update to point to correct class
  (_,_,_,ph) <- P.createProcess (P.proc cmd args) { P.cwd = Just "." }
  (socket, address) <- accept sock -- accept a connection and handle it
  void . forkIO $ do
    putStrLn $ "Accepted connection: " ++ show address
    (_input, output) <- N.socketToStreams socket
    d <- watchDirectory dir
    (`finally` P.terminateProcess ph) . forever $ do
      sourceFile <- d
      when (take 2 (reverse sourceFile) == "u.") $ do
        source <- Text.unpack <$> Data.Text.IO.readFile sourceFile
        (env0, unisonFile) <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
        let (Result notes' r) = FileParsers.serializeUnisonFile unisonFile
            showNote notes =
              intercalateMap "\n\n" (printNoteWithSourceAsAnsi env0 source) notes
        putStrLn . showNote . toList $ notes'
        case r of
          Nothing -> pure () -- just await next change
          Just (_unisonFile', typ, bs) -> do
            putStrLn . show . renderTextUnstyled $ "\129412 Your program has typechecked successfully as: " <> renderType' env0 typ
            Streams.write (Just bs) output
            -- todo: read from input to get the response and then show that
            -- for this we need a deserializer for Unison terms, mirroring what is in Unison.Codecs.hs
  serverLoop dir sock

main :: IO ()
main = do
  d <- watchDirectory "."
  forever $ do
    fp <- d
    putStrLn $ show fp

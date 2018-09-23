{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals
{-# LANGUAGE TypeApplications  #-}

module Unison.Codebase.Watch where

import System.Directory (canonicalizePath)
import qualified System.Console.ANSI as Console
import Data.IORef
import Data.Time.Clock (UTCTime, diffUTCTime)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Monad (forever)
import           System.FSNotify
import Network.Socket
import Control.Applicative
import qualified System.IO.Streams.Network as N
import           Data.Foldable      (toList)
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.Map as Map
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser      as Parser
import qualified Unison.Parsers     as Parsers
-- import           Unison.Util.AnnotatedText (renderTextUnstyled)
import           Unison.PrintError  (parseErrorToAnsiString, printNoteWithSourceAsAnsi) -- , renderType')
import           Unison.Result      (Result (Result))
import           Unison.Symbol      (Symbol)
import           Unison.Util.Monoid
import qualified System.IO.Streams as Streams
import qualified System.Process as P
import Control.Exception (finally)

watchDirectory' :: FilePath -> IO (IO (FilePath, UTCTime))
watchDirectory' d = do
  mvar <- newEmptyMVar
  let doIt fp t = do
        _ <- tryTakeMVar mvar
        putMVar mvar (fp, t)
      handler e = case e of
                Added fp t False -> doIt fp t
                Modified fp t False -> doIt fp t
                _ -> pure ()
  _ <- forkIO $ withManager $ \mgr -> do
    _ <- watchDir mgr d (const True) handler
    forever $ threadDelay 1000000
  pure $ takeMVar mvar

watchDirectory :: FilePath -> (FilePath -> Bool) -> IO (IO (FilePath, Text))
watchDirectory dir allow = do
  previousFiles <- newIORef Map.empty
  watcher <- watchDirectory' dir
  let await = do
        (file,t) <- watcher
        if allow file then do
          contents <- Data.Text.IO.readFile file
          prevs <- readIORef previousFiles
          case Map.lookup file prevs of
            -- if the file's content's haven't changed and less than a second has passed,
            -- wait for the next update
            Just (contents0, t0) | contents == contents0 && (t `diffUTCTime` t0) < 1 -> await
            _ -> (file,contents) <$ writeIORef previousFiles (Map.insert file (contents,t) prevs)
        else
          await
  pure await

watcher :: FilePath -> Int -> IO ()
watcher dir port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  let bindLoop port =
        (port <$ bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY))
        <|> bindLoop (port + 1) -- try the next port if that fails
  chosenPort <- bindLoop port
  listen sock 2
  serverLoop dir sock chosenPort

serverLoop :: FilePath -> Socket -> Int -> IO ()
serverLoop dir sock port = do
  Console.setTitle "Unison"
  Console.clearScreen
  Console.setCursorPosition 0 0
  let cmd = "scala"
      args = ["-cp", "runtime-jvm/main/target/scala-2.12/classes",
              "org.unisonweb.BootstrapStream", show port]
  (_,_,_,ph) <- P.createProcess (P.proc cmd args) { P.cwd = Just "." }
  (socket, _address) <- accept sock -- accept a connection and handle it
  cdir <- canonicalizePath dir
  putStrLn $ "🆗  I'm awaiting changes to *.u files in " ++ cdir
  -- putStrLn $ "   Note: I'm using the Unison runtime at " ++ show address
  (_input, output) <- N.socketToStreams socket
  d <- watchDirectory dir (".u" `isSuffixOf`)
  n <- newIORef (0 :: Int)
  (`finally` P.terminateProcess ph) . forever $ do
    (sourceFile, source0) <- d
    let source = Text.unpack source0
    Console.clearScreen
    Console.setCursorPosition 0 0
    marker <- do
      n0 <- readIORef n
      writeIORef n (n0 + 1)
      pure ["🌻🌸🌵🌺🌴" !! (n0 `mod` 5)]
    Console.setTitle "Unison"
    putStrLn ""
    putStrLn $ "I detected a change " ++ marker ++ "  of " ++ sourceFile ++ ", reloading... "
    parseResult <- Parsers.readAndParseFile @Symbol Parser.penv0 sourceFile
    case parseResult of
      Left parseError -> do
        Console.setTitle "Unison \128721"
        putStrLn $ parseErrorToAnsiString source parseError
      Right (env0, unisonFile) -> do
        let (Result notes' r) = FileParsers.serializeUnisonFile unisonFile
            showNote notes =
              intercalateMap "\n\n" (printNoteWithSourceAsAnsi env0 source) notes
        putStrLn . showNote . toList $ notes'
        case r of
          Nothing -> do
            Console.setTitle "Unison \128721"
            pure () -- just await next change
          Just (_unisonFile', _typ, bs) -> do
            Console.setTitle "Unison ✅"
            putStrLn "✅  Your program typechecks! Any watch expressions"
            putStrLn "   (lines starting with `>`) are shown below.\n"
            Streams.write (Just bs) output
            -- todo: read from input to get the response and then show that
            -- for this we need a deserializer for Unison terms, mirroring what is in Unison.Codecs.hs

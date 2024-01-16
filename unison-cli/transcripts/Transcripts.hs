{-# LANGUAGE OverloadedStrings #-}

{- This module kicks off the Transcript Tests.
   It doesn't do the transcript parsing itself.
-}
module Main (main) where

import Data.List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import EasyTest
import System.Directory
import System.Environment (getArgs)
import System.FilePath
  ( replaceExtension,
    splitFileName,
    takeExtensions,
    (</>),
  )
import System.IO.CodePage (withCP65001)
import System.IO.Silently (silence)
import Unison.Codebase.Init (withTemporaryUcmCodebase)
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.Codebase.TranscriptParser (TranscriptError (..), withTranscriptRunner)
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.Prelude
import UnliftIO.STM qualified as STM

data TestConfig = TestConfig
  { matchPrefix :: Maybe String
  }
  deriving (Show)

type TestBuilder = FilePath -> [String] -> String -> Test ()

testBuilder ::
  Bool -> ((FilePath, Text) -> IO ()) -> FilePath -> [String] -> String -> Test ()
testBuilder expectFailure recordFailure dir prelude transcript = scope transcript $ do
  outputs <- io . withTemporaryUcmCodebase SC.init Verbosity.Silent "transcript" SC.DoLock $ \(codebasePath, codebase) -> do
    withTranscriptRunner Verbosity.Silent "TODO: pass version here" Nothing \runTranscript -> do
      for files \filePath -> do
        transcriptSrc <- readUtf8 filePath
        out <- silence $ runTranscript filePath transcriptSrc (codebasePath, codebase)
        pure (filePath, out)
  for_ outputs \case
    (filePath, Left err) -> do
      let outputFile = outputFileForTranscript filePath
      case err of
        TranscriptParseError msg -> do
          when (not expectFailure) $ do
            let errMsg = "Error parsing " <> filePath <> ": " <> Text.unpack msg
            io $ recordFailure (filePath, Text.pack errMsg)
            crash errMsg
        TranscriptRunFailure errOutput -> do
          io $ writeUtf8 outputFile errOutput
          when (not expectFailure) $ do
            io $ Text.putStrLn errOutput
            io $ recordFailure (filePath, errOutput)
            crash $ "Failure in " <> filePath
    (filePath, Right out) -> do
      let outputFile = outputFileForTranscript filePath
      io $ writeUtf8 outputFile out
      when expectFailure $ do
        let errMsg = "Expected a failure, but transcript was successful."
        io $ recordFailure (filePath, Text.pack errMsg)
        crash errMsg
  ok
  where
    files = fmap (dir </>) (prelude ++ [transcript])

outputFileForTranscript :: FilePath -> FilePath
outputFileForTranscript filePath =
  replaceExtension filePath ".output.md"

buildTests :: TestConfig -> TestBuilder -> FilePath -> Test ()
buildTests config testBuilder dir = do
  io
    . putStrLn
    . unlines
    $ [ "",
        "Searching for transcripts to run in: " ++ dir
      ]
  files <- io $ listDirectory dir
  -- Any files that start with _ are treated as prelude
  let (prelude, transcripts) =
        files
          & sort
          & filter (\f -> takeExtensions f == ".md")
          & partition ((isPrefixOf "_") . snd . splitFileName)
          -- if there is a matchPrefix set, filter non-prelude files by that prefix - or return True
          & second (filter (\f -> maybe True (`isPrefixOf` f) (matchPrefix config)))

  case length transcripts of
    0 -> pure ()
    -- EasyTest exits early with "no test results recorded"
    -- if you don't give it any tests, this keeps it going
    -- till the end so we can search all transcripts for
    -- prefix matches.
    _ -> tests (testBuilder dir prelude <$> transcripts)

-- Transcripts that exit successfully get cleaned-up by the transcript parser.
-- Any remaining folders matching "transcript-.*" are output directories
-- of failed transcripts and should be moved under the "test-output" folder
cleanup :: Test ()
cleanup = do
  files' <- io $ listDirectory "."
  let dirs = filter ("transcript-" `isPrefixOf`) files'

  -- if any such codebases remain they are moved under test-output
  unless (null dirs) $ do
    io $ createDirectoryIfMissing True "test-output"
    io $ for_ dirs (\d -> renameDirectory d ("test-output" </> d))
    io
      . putStrLn
      . unlines
      $ [ "",
          "NOTE: All transcript codebases have been moved into",
          "the `test-output` directory. Feel free to delete it."
        ]

test :: TestConfig -> Test ()
test config = do
  -- We manually aggregate and display failures at the end to it much easier to see
  -- what went wrong in CI
  failuresVar <- io $ STM.newTVarIO []
  let recordFailure failure = STM.atomically $ STM.modifyTVar' failuresVar (failure :)
  buildTests config (testBuilder False recordFailure) $
    "unison-src" </> "transcripts"
  buildTests config (testBuilder False recordFailure) $
    "unison-src" </> "transcripts-using-base"
  buildTests config (testBuilder True recordFailure) $
    "unison-src" </> "transcripts" </> "errors"
  failures <- io $ STM.readTVarIO failuresVar
  -- Print all aggregated failures
  when (not $ null failures) . io $ Text.putStrLn $ "Failures:"
  for failures $ \(filepath, msg) -> io $ do
    Text.putStrLn $ Text.replicate 80 "="
    Text.putStrLn $ "🚨 " <> Text.pack filepath <> ": "
    Text.putStrLn msg
  cleanup

handleArgs :: [String] -> TestConfig
handleArgs args =
  let matchPrefix = case args of
        [prefix] -> Just prefix
        _ -> Nothing
   in TestConfig matchPrefix

main :: IO ()
main = withCP65001 do
  testConfig <- handleArgs <$> getArgs
  run (test testConfig)

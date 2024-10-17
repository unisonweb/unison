{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- This module kicks off the Transcript Tests.
   It doesn't do the transcript parsing itself.
-}
module Main (main) where

import Data.List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import EasyTest
import System.Directory
import System.Environment (getArgs, getExecutablePath)
import System.FilePath
  ( replaceExtension,
    splitFileName,
    takeDirectory,
    takeExtensions,
    (<.>),
    (</>),
  )
import System.IO.CodePage (withCP65001)
import System.IO.Silently (silence)
import Text.Megaparsec qualified as MP
import Unison.Codebase.Init (withTemporaryUcmCodebase)
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.Codebase.Transcript.Parser as Transcript
import Unison.Codebase.Transcript.Runner as Transcript
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.Prelude
import UnliftIO.STM qualified as STM

data TestConfig = TestConfig
  { matchPrefix :: Maybe String,
    runtimePath :: FilePath
  }
  deriving (Show)

type TestBuilder = FilePath -> FilePath -> [String] -> String -> Test ()

testBuilder ::
  Bool ->
  Bool ->
  ((FilePath, Text) -> IO ()) ->
  FilePath ->
  FilePath ->
  [String] ->
  String ->
  Test ()
testBuilder expectFailure replaceOriginal recordFailure runtimePath dir prelude transcript = scope transcript $ do
  outputs <- io . withTemporaryUcmCodebase SC.init Verbosity.Silent "transcript" SC.DoLock $ \(codebasePath, codebase) ->
    let isTest = True
     in Transcript.withRunner isTest Verbosity.Silent "TODO: pass version here" runtimePath \runTranscript ->
          for files \filePath -> do
            transcriptSrc <- readUtf8 filePath
            out <- silence $ runTranscript filePath transcriptSrc (codebasePath, codebase)
            pure (filePath, out)
  for_ outputs \case
    (filePath, Left err) -> do
      let outputFile = outputFileForTranscript filePath
      case err of
        Transcript.ParseError errors -> do
          let bundle = MP.errorBundlePretty errors
              errMsg = "Error parsing " <> filePath <> ": " <> bundle
          -- Drop the file name, to avoid POSIX/Windows conflicts
          io . writeUtf8 outputFile . Text.dropWhile (/= ':') $ Text.pack bundle
          when (not expectFailure) $ do
            io $ recordFailure (filePath, Text.pack errMsg)
            crash errMsg
        Transcript.RunFailure errOutput -> do
          let errText = Transcript.formatStanzas $ toList errOutput
          io $ writeUtf8 outputFile errText
          when (not expectFailure) $ do
            io $ Text.putStrLn errText
            io $ recordFailure (filePath, errText)
            crash $ "Failure in " <> filePath
    (filePath, Right out) -> do
      let outputFile = if replaceOriginal then filePath else outputFileForTranscript filePath
      io . writeUtf8 outputFile . Transcript.formatStanzas $ toList out
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
buildTests TestConfig {..} testBuilder dir = do
  io . putStrLn . unlines $
    [ "",
      "Searching for transcripts to run in: " ++ dir
    ]
  files <- io $ listDirectory dir
  -- Any files that start with _ are treated as prelude
  let (prelude, transcripts) =
        files
          & sort
          & filter (\f -> takeExtensions f == ".md")
          & partition (isPrefixOf "_" . snd . splitFileName)
          -- if there is a matchPrefix set, filter non-prelude files by that prefix - or return True
          & second (filter (\f -> maybe True (`isPrefixOf` f) matchPrefix))

  case length transcripts of
    0 -> pure ()
    -- EasyTest exits early with "no test results recorded"
    -- if you don't give it any tests, this keeps it going
    -- till the end so we can search all transcripts for
    -- prefix matches.
    _ -> tests (testBuilder runtimePath dir prelude <$> transcripts)

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
    io . putStrLn . unlines $
      [ "",
        "NOTE: All transcript codebases have been moved into",
        "the `test-output` directory. Feel free to delete it."
      ]

test :: TestConfig -> Test ()
test config = do
  -- We manually aggregate and display failures at the end to it much easier to see
  -- what went wrong in CI
  failuresVar <- io $ STM.newTVarIO []
  let recordFailure failure = STM.atomically $ STM.modifyTVar' failuresVar (failure :)
  buildTests config (testBuilder False False recordFailure) $ "unison-src" </> "transcripts"
  buildTests config (testBuilder False True recordFailure) $ "unison-src" </> "transcripts" </> "idempotent"
  buildTests config (testBuilder False False recordFailure) $ "unison-src" </> "transcripts-using-base"
  buildTests config (testBuilder True False recordFailure) $ "unison-src" </> "transcripts" </> "errors"
  failures <- io $ STM.readTVarIO failuresVar
  -- Print all aggregated failures
  when (not $ null failures) . io $ Text.putStrLn $ "Failures:"
  for failures $ \(filepath, msg) -> io $ do
    Text.putStrLn $ Text.replicate 80 "="
    Text.putStrLn $ "🚨 " <> Text.pack filepath <> ": "
    Text.putStrLn msg
  cleanup

handleArgs :: TestConfig -> [String] -> TestConfig
handleArgs acc ("--runtime-path" : p : rest) = handleArgs (acc {runtimePath = p}) rest
handleArgs acc [prefix] = acc {matchPrefix = Just prefix}
handleArgs acc _ = acc

defaultConfig :: IO TestConfig
defaultConfig = TestConfig Nothing <$> defaultRTP
  where
    defaultRTP = do
      ucm <- getExecutablePath
      pure (takeDirectory ucm </> "runtime" </> "unison-runtime" <.> exeExtension)

main :: IO ()
main = withCP65001 $ run . test =<< handleArgs <$> defaultConfig <*> getArgs

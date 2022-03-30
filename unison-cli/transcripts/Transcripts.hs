{-# LANGUAGE OverloadedStrings #-}

{- This module kicks off the Transcript Tests.
   It doesn't do the transcript parsing itself.
-}
module Main (main) where

import Data.Bifunctor (second)
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
import Unison.Codebase.Init (withTemporaryUcmCodebase)
import qualified Unison.Codebase.SqliteCodebase as SC
import Unison.Codebase.TranscriptParser (TranscriptError (..), withTranscriptRunner)
import Unison.Prelude

data TestConfig = TestConfig
  { matchPrefix :: Maybe String
  }
  deriving (Show)

type TestBuilder = FilePath -> [String] -> String -> Test ()

testBuilder ::
  Bool -> FilePath -> [String] -> String -> Test ()
testBuilder expectFailure dir prelude transcript = scope transcript $ do
  outputs <- io . withTemporaryUcmCodebase SC.init "transcript" $ \(codebasePath, codebase) -> do
    withTranscriptRunner "TODO: pass version here" Nothing $ \runTranscript -> do
      for files $ \filePath -> do
        transcriptSrc <- readUtf8 filePath
        out <- runTranscript filePath transcriptSrc (codebasePath, codebase)
        pure (filePath, out)
  for_ outputs $ \case
    (filePath, Left err) -> do
      let outputFile = outputFileForTranscript filePath
      case err of
        TranscriptParseError msg -> do
          when (not expectFailure) . crash $ "Error parsing " <> filePath <> ": " <> Text.unpack msg
        TranscriptRunFailure errOutput -> do
          io $ writeUtf8 outputFile errOutput
          io $ Text.putStrLn errOutput
          when (not expectFailure) . crash $ "Failure in " <> filePath
    (filePath, Right out) -> do
      let outputFile = outputFileForTranscript filePath
      io $ writeUtf8 outputFile out
      when expectFailure $ crash "Expected a failure, but transcript was successful."
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
  buildTests config (testBuilder False) $
    "unison-src" </> "transcripts"
  buildTests config (testBuilder False) $
    "unison-src" </> "transcripts-using-base"
  buildTests config (testBuilder True) $
    "unison-src" </> "transcripts" </> "errors"
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

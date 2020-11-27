{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Text
  ( pack,
    unpack,
  )
import EasyTest
import Shellmet (($|))
import System.Directory
import System.FilePath
  ( takeBaseName,
    takeExtensions,
    (</>),
  )
import System.Process (readProcessWithExitCode)
import Unison.Prelude

type TestBuilder = FilePath -> FilePath -> String -> Test ()

testBuilder :: FilePath -> FilePath -> String -> Test ()
testBuilder ucm dir transcript = scope transcript $ do
  io $ fromString ucm ["transcript", pack (dir </> transcript)]
  ok

testBuilderNewRuntime :: FilePath -> FilePath -> String -> Test ()
testBuilderNewRuntime ucm dir transcript = scope transcript $ do
  io $ fromString ucm ["--new-runtime", "transcript", pack (dir </> transcript)]
  ok

testBuilder' :: FilePath -> FilePath -> String -> Test ()
testBuilder' ucm dir transcript = scope transcript $ do
  let input = pack (dir </> transcript)
  let output = dir </> takeBaseName transcript <> ".output.md"
  io $ runAndCaptureError ucm ["transcript", input] output
  ok
  where
    -- Given a command and arguments, run it and capture the standard error to a file
    -- regardless of success or failure.
    runAndCaptureError :: FilePath -> [Text] -> FilePath -> IO ()
    runAndCaptureError cmd args outfile = do
      t <- readProcessWithExitCode cmd (map unpack args) ""
      let output = (\(_, _, stderr) -> stderr) t
      writeUtf8 outfile $ (pack . dropRunMessage) output

    -- Given the standard error, drops the part in the end that changes each run
    dropRunMessage :: String -> String
    dropRunMessage = unlines . reverse . drop 3 . reverse . lines

buildTests :: TestBuilder -> FilePath -> Test ()
buildTests testBuilder dir = do
  io
    . putStrLn
    . unlines
    $ [ "",
        "Searching for transcripts to run in: " ++ dir
      ]
  files <- io $ listDirectory dir
  let transcripts = sort . filter (\f -> takeExtensions f == ".md") $ files
  ucm <- io $ unpack <$> "stack" $| ["exec", "--", "which", "unison"] -- todo: what is it in windows?
  tests (testBuilder ucm dir <$> transcripts)

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

test :: Test ()
test = do
  buildTests testBuilder $ "unison-src" </> "transcripts"
  buildTests testBuilderNewRuntime $ "unison-src" </> "new-runtime-transcripts"
  buildTests testBuilder' $ "unison-src" </> "transcripts" </> "errors"
  cleanup

main :: IO ()
main = run test

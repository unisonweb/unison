{-# LANGUAGE OverloadedStrings #-}

{- This module kicks off the Transcript Tests.
   It doesn't do the transcript parsing itself.
-}
module Main (main) where

import Data.Bifunctor (second)
import Data.List
import Data.Text
  ( pack,
    unpack,
  )
import EasyTest
import Shellmet (($|))
import System.Directory
import System.Environment (getArgs)
import System.FilePath
  ( splitFileName,
    takeBaseName,
    takeExtensions,
    (</>),
  )
import System.Process (readProcessWithExitCode)
import Unison.Prelude

data TestConfig = TestConfig
  { matchPrefix :: Maybe String
  }
  deriving (Show)

type TestBuilder = FilePath -> FilePath -> [String] -> String -> Test ()

testBuilder ::
  FilePath -> FilePath -> [String] -> String -> Test ()
testBuilder ucm dir prelude transcript = scope transcript $ do
  io $ fromString ucm args
  ok
  where
    files = fmap (pack . (dir </>)) (prelude ++ [transcript])
    args = ["transcript"] ++ files

testBuilder' ::
  FilePath -> FilePath -> [String] -> String -> Test ()
testBuilder' ucm dir prelude transcript = scope transcript $ do
  let output = dir </> takeBaseName transcript <> ".output.md"
  io $ runAndCaptureError ucm args output
  ok
  where
    files = fmap (pack . (dir </>)) (prelude ++ [transcript])
    args = ["transcript"] ++ files
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

  ucm <- io $ unpack <$> "stack" $| ["exec", "--", "which", "unison"] -- todo: what is it in windows?
  case length transcripts of
    0 -> pure ()
    -- EasyTest exits early with "no test results recorded"
    -- if you don't give it any tests, this keeps it going
    -- till the end so we can search all transcripts for
    -- prefix matches.
    _ -> tests (testBuilder ucm dir prelude <$> transcripts)

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
  buildTests config testBuilder $
    "unison-src" </> "transcripts"
  buildTests config testBuilder $
    "unison-src" </> "transcripts-using-base"
  buildTests config testBuilder' $
    "unison-src" </> "transcripts" </> "errors"
  cleanup

handleArgs :: [String] -> TestConfig
handleArgs args =
  let matchPrefix = case args of
        [prefix] -> Just prefix
        _ -> Nothing
   in TestConfig matchPrefix

main :: IO ()
main = do
  testConfig <- handleArgs <$> getArgs
  run (test testConfig)

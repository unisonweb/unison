{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Unison.Prelude
import           EasyTest
import           Shellmet                       ( )
import           System.Directory
import           System.FilePath                ( (</>)
                                                , takeExtensions
                                                )
import           Data.Text                      ( pack )
import           Data.List

buildTest :: FilePath -> String -> Test ()
buildTest dir transcript = scope transcript $ do
  io $ "stack" ["exec", "unison", "--", "transcript", pack (dir </> transcript)]
  ok

test :: Test ()
test = do

  -- each transcript becomes a test case and all tests reduced into one
  let dir = "unison-src" </> "transcripts"
  files <- io $ listDirectory dir
  let transcripts = filter (\f -> takeExtensions f == ".md") files
  tests (buildTest dir <$> transcripts)

  -- the output of failed transcripts is preserved in the . dir
  files' <- io $ listDirectory "."
  let dirs = filter ("transcript-" `isPrefixOf`) files'

  -- if any such codebases remain they are moved under test-output
  unless (null dirs) $ do
    io $ createDirectoryIfMissing True "test-output"
    io $ for_ dirs (\d -> renameDirectory d ("test-output" </> d))
    io
      . putStrLn
      . unlines
      $ [ ""
        , "NOTE: All transcript codebases have been moved into"
        , "the `test-output` directory. Feel free to delete it."
        ]

main :: IO ()
main = run test

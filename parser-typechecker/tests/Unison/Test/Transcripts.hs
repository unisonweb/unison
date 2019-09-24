{-# LANGUAGE OverloadedStrings #-}

module Main where

import Unison.Prelude
import EasyTest
import Shellmet ()
import System.Directory
import System.FilePath ( (</>), takeExtensions )
import Data.Text (pack)
import Data.List

test :: Test ()
test = do
  let dir = "unison-src" </> "transcripts"
  files <- io $ listDirectory dir 
  let transcripts = filter (\f -> takeExtensions f == ".md") files 
      run t = scope t $ do  
        io $ "stack" ["exec", "unison", "--", "sandbox", pack (dir </> t)]
        ok
  tests (run <$> transcripts)
  -- Assuming everything passed, we now delete the transcript directories
  -- If the above fails, this won't be run, so you can inspect the codebase
  -- that resulted from any failures.
  files' <- io $ listDirectory "."
  let dirs = filter (\f -> "transcript-" `isPrefixOf` f) files' 
  io $ createDirectoryIfMissing True "test-output"
  io $ for_ dirs (\d -> renameDirectory d ("test-output" </> d))

main :: IO ()
main = run test

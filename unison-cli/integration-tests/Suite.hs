{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import Control.Monad
import EasyTest
import qualified IntegrationTests.ArgumentParsing as ArgumentParsing
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)

test :: Test ()
test =
  tests
    [ ArgumentParsing.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test

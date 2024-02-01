{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.ColorText qualified as ColorText
import Unison.Test.Range qualified as Range
import Unison.Test.Util.Pretty qualified as Pretty

test :: Test ()
test =
  tests
    [ Range.test,
      ColorText.test,
      Pretty.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test

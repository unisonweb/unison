module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.Sync.Roundtrip qualified as SyncRoundtrip
import Unison.Test.Server.Backend.DefinitionDiff qualified as DefinitionDiff

test :: Test ()
test =
  tests
    [ SyncRoundtrip.test
    , DefinitionDiff.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test
    _ -> error "expected no args, a prefix, or a seed and a prefix"

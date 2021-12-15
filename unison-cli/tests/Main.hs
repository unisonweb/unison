module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import qualified Unison.Test.ClearCache as ClearCache
import qualified Unison.Test.CommandLine as CommandLine
import qualified Unison.Test.GitSync as GitSync
import qualified Unison.Test.UriParser as UriParser
import qualified Unison.Test.VersionParser as VersionParser

test :: Test ()
test =
  tests
    [ ClearCache.test,
      CommandLine.test,
      GitSync.test,
      UriParser.test,
      VersionParser.test
    ]

main :: IO ()
main = do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test
    _ -> error "expected no args, a prefix, or a seed and a prefix"

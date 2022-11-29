module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import qualified Unison.Test.ClearCache as ClearCache
import qualified Unison.Test.Cli.Monad as Cli.Monad
import qualified Unison.Test.GitSync as GitSync
import qualified Unison.Test.LSP as LSP
import qualified Unison.Test.UriParser as UriParser
import qualified Unison.Test.VersionParser as VersionParser

test :: Test ()
test =
  tests
    [ ClearCache.test,
      Cli.Monad.test,
      GitSync.test,
      UriParser.test,
      VersionParser.test,
      LSP.test
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

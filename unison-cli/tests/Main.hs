module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.ClearCache qualified as ClearCache
import Unison.Test.Cli.Monad qualified as Cli.Monad
import Unison.Test.GitSync qualified as GitSync
import Unison.Test.LSP qualified as LSP
import Unison.Test.UriParser qualified as UriParser

test :: Test ()
test =
  tests
    [ LSP.test,
      ClearCache.test,
      Cli.Monad.test,
      GitSync.test,
      UriParser.test
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

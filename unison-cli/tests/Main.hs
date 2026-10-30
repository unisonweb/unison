module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.ClearCache qualified as ClearCache
import Unison.Test.Cli.Monad qualified as Cli.Monad
import Unison.Test.Cli.ScratchFile qualified as Cli.ScratchFile
import Unison.Test.LSP qualified as LSP
import Unison.Test.MCP qualified as MCP
import Unison.Test.MCP.Learn qualified as MCP.Learn
import Unison.Test.MCP.Runner qualified as MCP.Runner
import Unison.Test.TestProgress qualified as TestProgress
import Unison.Test.UriParser qualified as UriParser

test :: Test ()
test =
  tests
    [ LSP.test,
      ClearCache.test,
      Cli.Monad.test,
      Cli.ScratchFile.test,
      MCP.test,
      MCP.Learn.test,
      MCP.Runner.test,
      TestProgress.test,
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

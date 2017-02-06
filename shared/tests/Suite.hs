module Main where

import EasyTest
import System.IO
import qualified Unison.Test.Common as Common
import qualified Unison.Test.Doc as Doc
import qualified Unison.Test.Interpreter as Interpreter
import qualified Unison.Test.Typechecker.Components as Components

test :: Test ()
test = scope "unison-shared" $ do
  (codebase, resolveSymbol, allBindings, evaluate) <- io Common.codebase
  tests [ Doc.test
        , Interpreter.test codebase allBindings evaluate
        , Components.test codebase ]

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  run test

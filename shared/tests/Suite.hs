module Main where

import System.IO
import Test.Tasty
import qualified Unison.Test.Doc as Doc
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Term as Term
import qualified Unison.Test.TermParser as TermParser
import qualified Unison.Test.TypeParser as TypeParser
import qualified Unison.Test.Interpreter as Interpreter
import qualified Unison.Test.Typechecker.Components as Components

tests :: TestTree
tests = testGroup "unison" [Doc.tests, Typechecker.tests, Term.tests, TermParser.tests, TypeParser.tests, Interpreter.tests, Components.tests]

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  defaultMain tests

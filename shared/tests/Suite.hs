module Main where

import Test.Tasty
import qualified Unison.Test.Doc as Doc
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Term as Term
import qualified Unison.Test.TermParser as TermParser
import qualified Unison.Test.TypeParser as TypeParser
import qualified Unison.Test.Interpreter as Interpreter

tests :: TestTree
tests = testGroup "unison" [Doc.tests, Typechecker.tests, Term.tests, TermParser.tests, TypeParser.tests, Interpreter.tests]

main :: IO ()
main = defaultMain tests

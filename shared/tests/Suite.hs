module Main where

import Test.Tasty
import qualified Unison.Test.Doc as Doc
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Term as Term

tests :: TestTree
tests = testGroup "unison" [Doc.tests, Typechecker.tests, Term.tests]

main :: IO ()
main = defaultMain tests

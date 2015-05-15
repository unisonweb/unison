module Main where

import Test.Tasty
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Term as Term

tests :: TestTree
tests = testGroup "unison" [Typechecker.tests, Term.tests]

main = defaultMain tests

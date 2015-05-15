module Main where

import Test.Tasty
import qualified Unison.Test.Typechecker as Typechecker

tests :: TestTree
tests = testGroup "unison" [Typechecker.tests]

main = defaultMain tests

module Main where

import Test.Tasty
import qualified Unison.Test.ResourcePool as ResourcePool

tests :: TestTree
tests = testGroup "unison" [ResourcePool.tests]

main :: IO ()
main = defaultMain tests

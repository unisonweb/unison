module Main where

import Test.Tasty
import qualified Unison.Test.KeyValueStore as KVS
import qualified Unison.Test.ResourcePool as ResourcePool

tests :: IO TestTree
tests = do
  kvsTests <- KVS.ioTests
  pure $ testGroup "unison" [ResourcePool.tests, kvsTests]

main :: IO ()
main = tests >>= defaultMain

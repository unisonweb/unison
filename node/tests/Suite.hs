module Main where

import Test.Tasty
import qualified Unison.Test.KeyValueStore as KVS
import qualified Unison.Test.BlockStore.MemBlockStore as MBS
import qualified Unison.Test.BlockStore.FileBlockStore as FBS
import qualified Unison.Test.ResourcePool as ResourcePool
import qualified Unison.Test.SerializationAndHashing as SAH

tests :: IO (TestTree, IO ())
tests = do
  kvsTests <- KVS.ioTests
  mbsTests <- MBS.ioTests
  (fbsTests, cleanup) <- FBS.ioTests
  pure (testGroup "unison"
        [ResourcePool.tests, kvsTests, mbsTests, fbsTests, SAH.tests], cleanup)

main :: IO ()
main = tests >>= (\(tt, cleanup) -> defaultMain tt >> cleanup)

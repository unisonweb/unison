module Main where

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random
import Test.Tasty
import qualified Unison.Test.BlockStore.FileBlockStore as FBS
import qualified Unison.Test.BlockStore.MemBlockStore as MBS
import qualified Unison.Test.Journal as J
import qualified Unison.Test.KeyValueStore as KVS
import qualified Unison.Test.ResourcePool as ResourcePool
import qualified Unison.Test.SerializationAndHashing as SAH


tastyTests :: IO (TestTree, IO ())
tastyTests = do
  kvsTests <- KVS.ioTests
  mbsTests <- MBS.ioTests
  -- TODO fix FileBlockStore, and put tests back in rotation
  journalTests <- J.ioTests
  (fbsTests, cleanup) <- FBS.ioTests
  pure (testGroup "unison"
        [ResourcePool.tests, mbsTests, fbsTests, SAH.tests, journalTests], cleanup)

runTasty :: IO ()
runTasty = tastyTests >>= (\(tt, cleanup) -> defaultMain tt >> cleanup)

main = runTasty --runWithSeed 45 >> runTasty

runWithSeed :: Int -> IO ()
runWithSeed s = do
  let args = stdArgs { replay = Just (mkQCGen (s + 1), s * 2)}
  props <- MBS.justQuickcheck
  mapM_ quickCheck props


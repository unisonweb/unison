module Suite where

import Distribution.TestSuite
import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.Random
import System.Random
import qualified Unison.Test.KeyValueStore as KVS
import qualified Unison.Test.BlockStore.MemBlockStore as MBS
import qualified Unison.Test.BlockStore.FileBlockStore as FBS
import qualified Unison.Test.ResourcePool as ResourcePool
import qualified Unison.Test.SerializationAndHashing as SAH


tests :: IO [Test]
tests = return [ Test tasty, Test quickcheck ]
  where
    tasty = TestInstance
      { run = main >> return (Finished Pass)
      , name = "tasty"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right tasty
      }
    fails = TestInstance
      { run = return $ Finished $ Fail "Always fails!"
      , name = "fails"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right fails
      }
    quickcheck = TestInstance
      { run = runWithSeed 0 >> return (Finished Pass)
      , name = "quickcheck"
      , tags = []
      , options = []
      , setOption = \_ _ -> Right quickcheck
      }

tastyTests :: IO (TestTree, IO ())
tastyTests = do
  kvsTests <- KVS.ioTests
  mbsTests <- MBS.ioTests
  (fbsTests, cleanup) <- FBS.ioTests
  pure (Test.Tasty.testGroup "unison"
        [ResourcePool.tests, kvsTests, mbsTests, fbsTests, SAH.tests], cleanup)

main :: IO ()
main = tastyTests >>= (\(tt, cleanup) -> defaultMain tt >> cleanup)

runWithSeed :: Int -> IO ()
runWithSeed s = do
  let args = stdArgs { replay = Just (mkQCGen (s + 1), s * 2)}
  props <- MBS.justQuickcheck
  putStrLn "hey, what's up?"
  mapM_ verboseCheck props

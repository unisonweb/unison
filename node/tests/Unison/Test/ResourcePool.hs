module Unison.Test.ResourcePool where

import qualified Unison.Runtime.ResourcePool as RP
import Test.Tasty
import Test.Tasty.HUnit
import qualified System.IO.Strict as ST

type Resource = String
type Params = String

loadState :: FilePath -> IO String
loadState p = ST.readFile p

saveState :: String -> FilePath -> IO ()
saveState x p = do
  s <- loadState p
  length s `seq` writeFile p (s++x)

clearState :: FilePath -> IO ()
clearState p = writeFile p ""

fakeAcquire :: Params -> IO Resource
fakeAcquire p = do
  saveState (p++"r") "testacquires"
  >> return (p++"r")

cleanTestFiles =
  clearState "testreleases"
  >> clearState "testacquires"

fakeRelease :: Resource -> IO ()
fakeRelease r = do
  saveState r "testreleases"
  >> return ()

correctlyAcquiresTest :: Assertion
correctlyAcquiresTest = do
  x <- cleanTestFiles
  pool <- (RP.pool 3 fakeAcquire fakeRelease)
  (resource, _) <- RP.acquire pool "p1" 0
  (r2, _) <- RP.acquire pool "p1" 0
  didAcquire <- loadState "testacquires"
  didRelease <- loadState "testreleases"

  assertEqual "the correct resource is returned" "p1r" resource
    >> assertEqual "r is acquired twice" "p1rp1r" didAcquire
    >> assertEqual "didn't call release" "" didRelease

correctlyReleasesTest :: Assertion
correctlyReleasesTest = do
  x <- cleanTestFiles
  pool <- (RP.pool 3 fakeAcquire fakeRelease)
  (_, releaser) <- RP.acquire pool "p1" 0
  didRelease <- releaser >> loadState "testreleases"
  assertEqual "r was released after use" "p1r" didRelease

acquireShouldCacheConnectionTest :: Assertion
acquireShouldCacheConnectionTest = do
  x <- cleanTestFiles
  pool <- (RP.pool 3 fakeAcquire fakeRelease)
  (r, _) <- RP.acquire pool "p1" 100000 -- 100 seconds
  (r2, _) <- RP.acquire pool "p1" 100000 -- 100 seconds
  didAcquire <- loadState "testacquires"
  assertEqual "r was only acquired once" "p1r" didAcquire

tests :: TestTree
tests = testGroup "Doc"
  [
    testCase "AcquiresTest" $ correctlyAcquiresTest,
    testCase "ReleasesTest" $ correctlyReleasesTest,
    testCase "acquireShouldCacheConnectionTest" $  acquireShouldCacheConnectionTest
  ]

main = defaultMain tests

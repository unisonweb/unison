module Unison.Test.ResourcePool where

import qualified Unison.Runtime.ResourcePool as RP
import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent
import Data.Time (UTCTime,getCurrentTime, addUTCTime)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as M

type Resource = String
type Params = String
type TestState = MVar.MVar (M.Map String String)

lookupState :: M.Map String String -> String -> String
lookupState m k =
  case (M.lookup k m) of
    Just s -> s
    Nothing -> ""

loadState :: TestState -> String -> IO String
loadState var k = do
  m <- MVar.readMVar var
  return (lookupState m k)

saveState :: TestState -> String -> String -> IO ()
saveState var k newState = do
  m <- MVar.takeMVar var
  let s = lookupState m k
  MVar.putMVar var (M.insert k (s++newState) m)

fakeAcquire :: TestState -> Params -> IO Resource
fakeAcquire s p = do
  saveState s "testacquires" (p++"r")
  >> return (p++"r")

cleanTestFiles s =
  saveState s "testacquires" ""
  >> saveState s "testacquires" ""

fakeRelease :: TestState -> Resource -> IO ()
fakeRelease s r = do
  saveState s "testreleases" r

getPool = do
  state <- MVar.newMVar M.empty
  pool <- cleanTestFiles state
          >> (RP.pool 3 (fakeAcquire state) (fakeRelease state))
  return (pool, state)

correctlyAcquiresTest :: Assertion
correctlyAcquiresTest = do
  (pool, ts) <- getPool
  (resource, release1) <- RP.acquire pool "p1" 0
  didAcquire <- release1
                >> RP.acquire pool "p1" 0
                >> loadState ts "testacquires"
  didRelease <- loadState ts "testreleases"

  assertEqual "the correct resource is returned" "p1r" resource
    >> assertEqual "r is acquired twice" "p1rp1r" didAcquire
    >> assertEqual "called release once" "p1r" didRelease

correctlyReleasesTest :: Assertion
correctlyReleasesTest = do
  (pool, ts) <- getPool
  (_, releaser) <- RP.acquire pool "p1" 0
  didRelease <- releaser >> loadState ts "testreleases"
  assertEqual "r was released after use" "p1r" didRelease

acquireShouldCacheConnectionTest :: Assertion
acquireShouldCacheConnectionTest = do
  (pool, ts) <- getPool
  (r, releaser1) <- RP.acquire pool "p1" 100000 -- 100 seconds
  (r2, releaser2) <- releaser1 >> RP.acquire pool "p1" 100000 -- 100 seconds
  didAcquire <- releaser2 >> loadState ts "testacquires"
  didRelease <- loadState ts "testreleases"
  -- a 100 second wait would prevent immediate release both times
  assertEqual "r was only acquired once" "p1r" didAcquire
    >> assertEqual "didn't call release" "" didRelease

acquireCannotCacheTooManyConnections :: Assertion
acquireCannotCacheTooManyConnections = do
  (pool, ts) <- getPool
  (r1, releaser1) <- RP.acquire pool "p1" 100000 -- 100 seconds
  (r2, releaser2) <- RP.acquire pool "p2" 100000 -- 100 seconds
  (r3, releaser3) <- RP.acquire pool "p3" 100000 -- 100 seconds
  (r4, releaser4) <- RP.acquire pool "p4" 100000 -- 100 seconds
  releaser1 >> releaser2 >> releaser3 >> releaser4
  didRelease <- loadState ts "testreleases"
  assertEqual "only p4 got released" "p4r" didRelease

tenSecondsAgo :: UTCTime -> UTCTime
tenSecondsAgo now = addUTCTime (-10) now

inTenSeconds :: UTCTime -> UTCTime
inTenSeconds now = addUTCTime (10) now

aThreeFullCache now f1 f2 f3=
  M.fromList [("p1", ("p1", now, f1))
             , ("p2", ("p2", tenSecondsAgo now, f2))
             , ("p3", ("p3", inTenSeconds now, f3))
             ]

cleanCacheShouldReleaseFinalizer :: Assertion
cleanCacheShouldReleaseFinalizer = do
  (pool, ts) <- getPool
  now <- getCurrentTime
  cache <- MVar.newMVar (aThreeFullCache now
                         (fakeRelease ts "p1")
                         (fakeRelease ts "p2")
                         (fakeRelease ts "p3"))
  RP.cleanCache cache
  c <- MVar.takeMVar cache
  didRelease <- loadState ts "testreleases"

  assertEqual "p1 and p2 are cleaned from cache" ["p3"] (M.keys c)
    >> assertEqual "p1 and p2 are released" "p1p2" didRelease

getPoolWithGC = do
  state <- MVar.newMVar M.empty
  pool <- cleanTestFiles state
          >> (RP.poolWithGC 3 (fakeAcquire state) (fakeRelease state))
  return (pool, state)

threadGCsResourcesFromCacheTest :: Assertion
threadGCsResourcesFromCacheTest = do
  (pool, ts) <- getPoolWithGC
  (resource, release1) <- RP.acquire pool "p1" 4
  didRelease1 <- release1
                    >> threadDelay 1
                    >> loadState ts "testreleases"
  didRelease2 <- threadDelay 5 >> loadState ts "testreleases"
  assertEqual "didn't immediately release" "" didRelease1
    >> assertEqual "auto released after 3 seconds" "p1r" didRelease2

tests :: TestTree
tests = testGroup "Doc"
  [
    testCase "AcquiresTest" $ correctlyAcquiresTest
    , testCase "ReleasesTest" $ correctlyReleasesTest
    , testCase "acquireShouldCacheConnectionTest" $  acquireShouldCacheConnectionTest
    , testCase "threadGCsResourcesFromCacheTest " $  threadGCsResourcesFromCacheTest
    , testCase "cleanCacheShouldReleaseFinalizer" $  cleanCacheShouldReleaseFinalizer
    , testCase "acquireCannotCacheTooManyConnections" $  acquireCannotCacheTooManyConnections
   ]

main = defaultMain tests

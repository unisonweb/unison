module Unison.Test.ResourcePool where

import qualified Unison.Runtime.ResourcePool as RP
import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent
import Data.Time (UTCTime,getCurrentTime, addUTCTime)
import qualified Control.Concurrent as CC
import qualified Control.Monad.STM as STM
import qualified Data.IORef as IORef
import qualified Data.Hashable as H
import qualified Control.Concurrent.Map as M
import qualified Control.Concurrent.STM.TQueue as TQ
import Data.Maybe

type Resource = String
type Params = String
type TestState = M.Map String String

loadState :: TestState -> String -> IO String
loadState m k = do
  x <- M.lookup k m
  return $ case x of
              Just s -> s
              Nothing -> ""

saveState :: TestState -> String -> String -> IO ()
saveState m k newState = do
  s <- loadState m k
  M.insert k (s++newState) m

fakeAcquire :: TestState -> Params -> IO Resource
fakeAcquire s p = do
  saveState s "testacquires" (p++"r")
  >> return (p++"r")

fakeRelease :: TestState -> Resource -> IO ()
fakeRelease s r = do
  saveState s "testreleases" r

getPool = do
  state <- M.empty
  pool <- (RP.poolWithoutGC 3 (fakeAcquire state) (fakeRelease state))
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
  (r, releaser1) <- RP.acquire pool "p1" 1
  (r2, releaser2) <- releaser1 >> RP.acquire pool "p1" 1
  didAcquire <- releaser2 >> loadState ts "testacquires"
  didRelease <- loadState ts "testreleases"
  -- a 100 second wait would prevent immediate release both times
  assertEqual "r was only acquired once" "p1r" didAcquire
    >> assertEqual "didn't call release" "" didRelease

acquireCannotCacheTooManyConnections :: Assertion
acquireCannotCacheTooManyConnections = do
  (pool, ts) <- getPool
  (r1, releaser1) <- RP.acquire pool "p1" 1
  (r2, releaser2) <- RP.acquire pool "p2" 1
  (r3, releaser3) <- RP.acquire pool "p3" 1
  (r4, releaser4) <- RP.acquire pool "p4" 1
  didRelease <- releaser1 >> releaser2 >> releaser3 >> releaser4
                >> loadState ts "testreleases"
  assertEqual "only p4 got released" "p4r" didRelease

tenSecondsAgo :: UTCTime -> UTCTime
tenSecondsAgo now = addUTCTime (-10) now

twentySecondsAgo :: UTCTime -> UTCTime
twentySecondsAgo now = addUTCTime (-20) now

inTenSeconds :: UTCTime -> UTCTime
inTenSeconds now = addUTCTime (10) now

aThreeFullCache :: UTCTime -> CC.ThreadId -> IO () -> IO () -> IO () -> IO (RP.Cache String String)
aThreeFullCache now threadId f1 f2 f3 =
  M.fromList [(("p1",threadId), ("p1", now, f1))
             , (("p2",threadId), ("p2", tenSecondsAgo now, f2))
             , (("p3",threadId), ("p3", inTenSeconds now, f3))
             ]

-- The reaper queue should only get a notification
-- of the key that might need to be reaped, it then
-- checks to see if the expiry has passed
cleanPoolShouldReleaseFinalizer :: Assertion
cleanPoolShouldReleaseFinalizer = do
  ts <- M.empty
  now <- getCurrentTime
  threadId <- CC.myThreadId
  q <- STM.atomically TQ.newTQueue
  cache <- (aThreeFullCache now threadId
                         (fakeRelease ts "p1")
                         (fakeRelease ts "p2")
                         (fakeRelease ts "p3"))
  let k1 = ("p1", threadId)
  let k2 = ("p2", threadId)
  let k3 = ("p3", threadId)
  -- signal all three to reaper queue
  STM.atomically $ TQ.writeTQueue q k1
                     >> TQ.writeTQueue q k2
                     >> TQ.writeTQueue q k3
  RP.cleanPool cache q
  assertBool "p1 removed" <$> isNothing <$> M.lookup k1 cache
  assertBool "p2 removed" <$> isNothing <$> M.lookup k2 cache
  assertBool "p3 still cached" <$> isJust <$> M.lookup k3 cache
  didRelease <- loadState ts "testreleases"
  assertEqual "p1 and p2 are released" "p1p2" didRelease

getPoolWithGC = do
  state <- M.empty
  pool <- (RP.pool 3 (fakeAcquire state) (fakeRelease state))
  return (pool, state)

delaySeconds μs = threadDelay (1000000 * μs)

threadGCsResourcesFromPoolTest :: Assertion
threadGCsResourcesFromPoolTest = do
  (pool, ts) <- getPoolWithGC
  (_, release1) <- RP.acquire pool "p1" 2
  didRelease1a <- release1
                    >> delaySeconds 1
                    >> loadState ts "testreleases"
  didRelease1b <- delaySeconds 3 >> loadState ts "testreleases"
  (_, release2) <- RP.acquire pool "p2" 2
  didRelease2a <- release2
                    >> delaySeconds 1
                    >> loadState ts "testreleases"
  didRelease2b <- delaySeconds 3 >> loadState ts "testreleases"
  assertEqual "shouldn't immediately release p1" "" didRelease1a
    >> assertEqual "reaper released p1 after 3 seconds" "p1r" didRelease1b
    >> assertEqual "shouldn't immediately release p2" "p1r" didRelease2a
    >> assertEqual "reaper released p2 after 3 seconds" "p1rp2r" didRelease2b

-- T - Time, p - param, Q - reaper queue, E - Expiry Time, C - Cache
-- T1:  p1 acquired, released, Q(p1) on queue, C(p1,E16) cached
-- T2:  reaper awakens, Q(p1) peeked
--      cache shows C(p1,E16), read and re-enqueue Q(p1)
-- T3:  p2 acquired, released, Q(p2) on queue, C(p2,E5) cached
-- T5:  reaper awakens, Q(p2) peeked
--      cache shows C(p2,E5), read and release
--      Q(p1) peeked
--      cache shows C(p1,E16), read and re-enqueue Q(p1)
-- ...
-- T16: reaper awakens, Q(p1) peeked
--      cache shows C(p1, E16), read and release
threadGCsWillReenqueueFutureReleasesTest :: Assertion
threadGCsWillReenqueueFutureReleasesTest = do
  (pool, ts) <- getPoolWithGC
  (_, release1) <- RP.acquire pool "p1" 16
  didRelease1 <- release1
                    >> delaySeconds 3 -- allows the reaper to awake/sleep thrice
                    >> loadState ts "testreleases"
  (_, release2) <- RP.acquire pool "p2" 2
  didRelease2 <- release2
                    >> delaySeconds 4
                    >> loadState ts "testreleases"
  assertEqual "shouldn't immediately release p1" "" didRelease1
    >> assertEqual "released p2" "p2r" didRelease2

fakeGetThread :: CC.ThreadId -> IO CC.ThreadId
fakeGetThread t = return t

acquireIsThreadSpecificTest :: Assertion
acquireIsThreadSpecificTest = do
  ps  <- IORef.newIORef 0
  ts <- M.empty
  q <- STM.atomically TQ.newTQueue
  pool <- M.empty
  someOtherThread <- forkIO (putStrLn "")
  let aquire = RP._acquire (fakeAcquire ts) (fakeRelease ts) pool 3 ps q
  (r1, release1) <- aquire CC.myThreadId "p1" 10
  (r1b, release1b) <- release1 >> aquire (fakeGetThread someOtherThread) "p1" 10
  didAcquire <- release1b >> loadState ts "testacquires"

  poolSize <- IORef.readIORef ps

  assertEqual "got the correct r" "p1r" r1
    >> assertEqual "got the correct r" "p1r" r1b
    >> assertEqual "r is acquired twice" "p1rp1r" didAcquire
    >> assertEqual "two connections are in the map" 2 poolSize

acquireRemovesFromPoolTest :: Assertion
acquireRemovesFromPoolTest = do
  (pool, ts) <- getPool
  (r1, release1) <- RP.acquire pool "p1" 1
  (r2, _) <- release1 >> RP.acquire pool "p1" 1 -- acquire p1 a third time, without releasing r2
  (r3, _) <-  RP.acquire pool "p1" 1
  didAcquire <- loadState ts "testacquires"
  didRelease <- loadState ts "testreleases"

  assertEqual "the correct resource is returned" "p1r" r1
    >> assertEqual "the correct resource is returned" "p1r" r2
    >> assertEqual "the correct resource is returned" "p1r" r3
    >> assertEqual "r is acquired twice" "p1rp1r" didAcquire

tests :: TestTree
tests = testGroup "ResourcePool"
  [
    testCase "correctlyAcquiresTest" $ correctlyAcquiresTest
    , testCase "correctlyReleasesTest" $ correctlyReleasesTest
    , testCase "acquireShouldCacheConnectionTest" $  acquireShouldCacheConnectionTest
    , testCase "cleanPoolShouldReleaseFinalizer" $  cleanPoolShouldReleaseFinalizer
    , testCase "acquireCannotCacheTooManyConnections" $  acquireCannotCacheTooManyConnections
    , testCase "threadGCsResourcesFromPoolTest" $ threadGCsResourcesFromPoolTest
    , testCase "threadGCsWillReenqueueFutureReleasesTest" $ threadGCsWillReenqueueFutureReleasesTest
    , testCase "acquireIsThreadSpecificTest" $ acquireIsThreadSpecificTest
    , testCase "acquireRemovesFromPoolTest" $ acquireRemovesFromPoolTest
    ]

main = defaultMain tests

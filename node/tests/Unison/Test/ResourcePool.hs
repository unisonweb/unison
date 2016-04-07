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

getPool wait = do
  state <- M.empty
  pool <- (RP.poolWithoutGC wait 3 (fakeAcquire state) (fakeRelease state))
  return (pool, state)

correctlyAcquiresTest :: Assertion
correctlyAcquiresTest = do
  (pool, ts) <- getPool 0
  (resource, release1) <- RP.acquire pool "p1"
  didAcquire <- release1
                >> RP.acquire pool "p1"
                >> loadState ts "testacquires"
  didRelease <- loadState ts "testreleases"

  assertEqual "the correct resource is returned" "p1r" resource
    >> assertEqual "r is acquired twice" "p1rp1r" didAcquire
    >> assertEqual "called release once" "p1r" didRelease

correctlyReleasesTest :: Assertion
correctlyReleasesTest = do
  (pool, ts) <- getPool 0
  (_, releaser) <- RP.acquire pool "p1"
  didRelease <- releaser >> loadState ts "testreleases"
  assertEqual "r was released after use" "p1r" didRelease

acquireShouldCacheConnectionTest :: Assertion
acquireShouldCacheConnectionTest = do
  (pool, ts) <- getPool 1
  (r, releaser1) <- RP.acquire pool "p1"
  (r2, releaser2) <- releaser1 >> RP.acquire pool "p1"
  didAcquire <- releaser2 >> loadState ts "testacquires"
  didRelease <- loadState ts "testreleases"
  -- a 100 second wait would prevent immediate release both times
  assertEqual "r was only acquired once" "p1r" didAcquire
    >> assertEqual "didn't call release" "" didRelease

acquireCannotCacheTooManyConnections :: Assertion
acquireCannotCacheTooManyConnections = do
  (pool, ts) <- getPool 1
  (r1, releaser1) <- RP.acquire pool "p1"
  (r2, releaser2) <- RP.acquire pool "p2"
  (r3, releaser3) <- RP.acquire pool "p3"
  (r4, releaser4) <- RP.acquire pool "p4"
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

getPoolWithGC wait = do
  state <- M.empty
  pool <- (RP.pool wait 3 (fakeAcquire state) (fakeRelease state))
  return (pool, state)

delaySeconds μs = threadDelay (1000000 * μs)

threadGCsResourcesFromPoolTest :: Assertion
threadGCsResourcesFromPoolTest = do
  (pool, ts) <- getPoolWithGC 2
  (_, release1) <- RP.acquire pool "p1"
  didRelease1a <- release1
                    >> delaySeconds 1
                    >> loadState ts "testreleases"
  didRelease1b <- delaySeconds 3 >> loadState ts "testreleases"
  (_, release2) <- RP.acquire pool "p2"
  didRelease2a <- release2
                    >> delaySeconds 1
                    >> loadState ts "testreleases"
  didRelease2b <- delaySeconds 3 >> loadState ts "testreleases"
  assertEqual "shouldn't immediately release p1" "" didRelease1a
    >> assertEqual "reaper released p1 after 3 seconds" "p1r" didRelease1b
    >> assertEqual "shouldn't immediately release p2" "p1r" didRelease2a
    >> assertEqual "reaper released p2 after 3 seconds" "p1rp2r" didRelease2b

fakeGetThread :: CC.ThreadId -> IO CC.ThreadId
fakeGetThread t = return t

acquireIsThreadSpecificTest :: Assertion
acquireIsThreadSpecificTest = do
  ps  <- IORef.newIORef 0
  ts <- M.empty
  q <- STM.atomically TQ.newTQueue
  pool <- M.empty
  someOtherThread <- forkIO (putStrLn "")
  let aquire = RP._acquire (fakeAcquire ts) (fakeRelease ts) pool 10 3 ps q
  (r1, release1) <- aquire CC.myThreadId "p1"
  (r1b, release1b) <- release1 >> aquire (fakeGetThread someOtherThread) "p1"
  didAcquire <- release1b >> loadState ts "testacquires"

  poolSize <- IORef.readIORef ps

  assertEqual "got the correct r" "p1r" r1
    >> assertEqual "got the correct r" "p1r" r1b
    >> assertEqual "r is acquired twice" "p1rp1r" didAcquire
    >> assertEqual "two connections are in the map" 2 poolSize

acquireRemovesFromPoolTest :: Assertion
acquireRemovesFromPoolTest = do
  (pool, ts) <- getPool 1
  (r1, release1) <- RP.acquire pool "p1"
  (r2, _) <- release1 >> RP.acquire pool "p1" -- acquire p1 a third time, without releasing r2
  (r3, _) <-  RP.acquire pool "p1"
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
    , testCase "acquireCannotCacheTooManyConnections" $  acquireCannotCacheTooManyConnections
    , testCase "threadGCsResourcesFromPoolTest" $ threadGCsResourcesFromPoolTest
    , testCase "acquireIsThreadSpecificTest" $ acquireIsThreadSpecificTest
    , testCase "acquireRemovesFromPoolTest" $ acquireRemovesFromPoolTest
    ]

main = defaultMain tests

module Unison.Test.ResourcePool where

import qualified Unison.Runtime.ResourcePool as RP
import Test.Tasty
import Test.Tasty.HUnit

type Resource = String
type Params = String

fakeAcquire :: Params -> IO Resource
fakeAcquire p = return "r1"

fakeRelease :: Resource -> IO ()
fakeRelease r = return ()

correctlyReturnsTest :: Assertion
correctlyReturnsTest = do
  pool <- (RP.pool 3 fakeAcquire fakeRelease)
  (r, _) <- RP.acquire pool "p1"
  assertEqual "the correct resource is returned" "r1" r

tests :: TestTree
tests = testGroup "Doc"
  [
    testCase "Test" $ correctlyReturnsTest
  ]

main = defaultMain tests

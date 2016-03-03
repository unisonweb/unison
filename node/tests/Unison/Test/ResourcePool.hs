module Unison.Test.ResourcePool where

import qualified Unison.Runtime.ResourcePool as RP
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as Str

type Resource = String
type Params = String

loadState :: (Read a) => IO a
loadState = read <$> BS.unpack <$> Str.readFile "testreleases"

saveState :: (Show a) => a -> IO ()
saveState x = Str.writeFile "testreleases" (BS.pack . show $ x)

fakeAcquire :: Params -> IO Resource
fakeAcquire p = return "r1"

fakeRelease :: Resource -> IO ()
fakeRelease r = do
  saveState r
  return ()

correctlyAcquiresTest :: Assertion
correctlyAcquiresTest = do
  saveState ""
  pool <- (RP.pool 3 fakeAcquire fakeRelease)
  (resource, _) <- RP.acquire pool "p1"
  didRelease <- loadState
  assertEqual "the correct resource is returned" "r1" resource
    >> assertEqual "didn't call release" "" didRelease

correctlyReleasesTest :: Assertion
correctlyReleasesTest = do
  saveState ""
  pool <- (RP.pool 3 fakeAcquire fakeRelease)
  (_, releaser) <- RP.acquire pool "p1"
  didRelease <- releaser >> loadState
  assertEqual "r was released after use" "r1" didRelease

tests :: TestTree
tests = testGroup "Doc"
  [
    testCase "AcquiresTest" $ correctlyAcquiresTest,
    testCase "ReleasesTest" $ correctlyReleasesTest
  ]

main = defaultMain tests

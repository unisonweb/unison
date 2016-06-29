module Unison.Test.BlockStore.MemBlockStore where

import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Test.BlockStore
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified Unison.BlockStore.MemBlockStore as MBS

ioTests :: IO TestTree
ioTests = do
  gen <- getStdGen
  genVar <- IORef.newIORef gen
  let genHash = MBS.makeRandomHash genVar
  store <- MBS.make' genHash
  pure $ testGroup "MemBlockStore"
    [ testCase "roundTrip" (roundTrip store)
    , testCase "roundTripSeries" (roundTripSeries store)
    , testCase "appendAppendUpdate" (appendAppendUpdate store)
    , testCase "idempotentDeclare" (idempotentDeclare store)
    ]

{-# Language DeriveGeneric #-}
module Unison.Test.Journal where

import Control.Concurrent.STM (atomically)
import Control.Monad (when)
import Data.ByteString.Char8 (pack)
import Data.Bytes.Serial (Serial)
import GHC.Generics
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Runtime.Address
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.BlockStore as BS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J

makeRandomAddress :: RandomGen r => MVar.MVar r -> IO Address
makeRandomAddress genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  MVar.swapMVar genVar newGen
  pure hash

data SimpleUpdate = Inc deriving (Generic)
instance Serial SimpleUpdate

simpleUpdate :: SimpleUpdate -> Int -> Int
simpleUpdate Inc = (+1)

makeBlock :: Serial a => String -> a -> B.Block a
makeBlock name v = B.serial v . B.fromSeries . BS.Series $ pack name

readAfterUpdate :: BS.BlockStore Address -> Assertion
readAfterUpdate bs = do
  let values = makeBlock "v" 0
      updates = makeBlock "u " Nothing
  j <- J.fromBlocks bs simpleUpdate values updates
  J.update Inc j
  result <- atomically $ J.get j
  when (result /= 1) $ fail ("incorrect value after update, result " ++ show result)

ioTests :: IO TestTree
ioTests = do
  gen <- getStdGen
  genVar <- MVar.newMVar gen
  blockStore <- MBS.make' (makeRandomAddress genVar)
  let prereqs = (genVar, blockStore)
  pure $ testGroup "Journal"
    [ testCase "readAfterUpdate" (readAfterUpdate blockStore)
    ]

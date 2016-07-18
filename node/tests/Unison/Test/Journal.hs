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
import Unison.Hash (Hash)
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.BlockStore as BS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Hash as Hash
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  MVar.swapMVar genVar newGen
  pure hash

data SimpleUpdate = Inc | NoOp deriving (Generic)
instance Serial SimpleUpdate

simpleUpdate :: SimpleUpdate -> Int -> Int
simpleUpdate NoOp = id
simpleUpdate Inc = (+1)

makeBlock :: Serial a => String -> a -> B.Block a
makeBlock name v = B.serial v . B.fromSeries . BS.Series $ pack name

readAfterUpdate :: BS.BlockStore Hash -> Assertion
readAfterUpdate bs = do
  let values = makeBlock "v" 0
      updates = makeBlock "u " NoOp
  j <- J.fromBlocks bs NoOp simpleUpdate values updates
  J.update Inc j
  result <- atomically $ J.get j
  when (result /= 1) $ fail ("incorrect value after update, result " ++ show result)

ioTests :: IO TestTree
ioTests = do
  gen <- getStdGen
  genVar <- MVar.newMVar gen
  blockStore <- MBS.make' (makeRandomHash genVar)
  let prereqs = (genVar, blockStore)
  pure $ testGroup "Journal"
    [ testCase "readAfterUpdate" (readAfterUpdate blockStore)
    ]

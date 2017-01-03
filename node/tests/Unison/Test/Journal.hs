{-# Language DeriveGeneric #-}
module Unison.Test.Journal where

import Control.Concurrent.STM (atomically)
import Control.Monad (when)
import Data.ByteString.Char8 (pack)
import Data.Bytes.Serial (Serial)
import EasyTest
import GHC.Generics
import System.Random
import Unison.Runtime.Address
import Unison.Test.BlockStore (makeRandomAddress)
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.BlockStore as BS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J

data SimpleUpdate = Inc deriving (Generic)
instance Serial SimpleUpdate

simpleUpdate :: SimpleUpdate -> Int -> Int
simpleUpdate Inc = (+1)

makeBlock :: Serial a => String -> a -> B.Block a
makeBlock name v = B.serial v . B.fromSeries . BS.Series $ pack name

readAfterUpdate :: BS.BlockStore Address -> Test ()
readAfterUpdate bs = do
  let values = makeBlock "v" 0
      updates = makeBlock "u " Nothing
  j <- io $ J.fromBlocks bs simpleUpdate values updates
  io $ J.update Inc j
  result <- io . atomically $ J.get j
  expect (result == 1)

test :: Test ()
test = scope "Journal" $ do
  blockStore <- io $ MBS.make' makeRandomAddress makeAddress
  tests [ scope "readAfterUpdate" (readAfterUpdate blockStore) ]

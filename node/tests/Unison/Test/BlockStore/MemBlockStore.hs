module Unison.Test.BlockStore.MemBlockStore where

import System.Random
import EasyTest
import Unison.Runtime.Address
import Unison.Test.BlockStore as TBS
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified Unison.BlockStore.MemBlockStore as MBS

test :: Test ()
test = scope "MemBlockStore" $ do
  store <- io (MBS.make' makeRandomAddress makeAddress)
  TBS.test store

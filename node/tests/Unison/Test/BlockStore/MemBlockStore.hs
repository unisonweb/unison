module Unison.Test.BlockStore.MemBlockStore where

import System.Random
import Test.QuickCheck
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
  pure . testGroup "MemBlockStore" $ makeCases store

justQuickcheck :: IO [Property]
justQuickcheck = do
  gen <- getStdGen
  genVar <- IORef.newIORef gen
  let genHash = MBS.makeRandomHash genVar
  store <- MBS.make' genHash
  pure [prop_lastKeyIsValid store, prop_SomeoneHasAValidKey store]

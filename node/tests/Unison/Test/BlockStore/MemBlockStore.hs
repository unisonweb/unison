module Unison.Test.BlockStore.MemBlockStore where

import System.Random
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Runtime.Address
import Unison.Test.BlockStore
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified Unison.BlockStore.MemBlockStore as MBS

ioTests :: IO TestTree
ioTests = do
  store <- MBS.make' makeRandomAddress makeAddress
  pure . testGroup "MemBlockStore" $ makeExhaustiveCases store

justQuickcheck :: IO [Property]
justQuickcheck = do
  store <- MBS.make' makeRandomAddress makeAddress
  pure [ prop_lastKeyIsValid store
       , prop_SomeoneHasAValidKey store
       , prop_allSeriesHashesAreValid store]

module Unison.Test.BlockStore.FileBlockStore where

import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Test.BlockStore
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified System.Directory as Directory
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.BlockStore.MemBlockStore as MBS

ioTests :: IO (TestTree, IO ())
ioTests = do
  gen <- getStdGen
  genVar <- IORef.newIORef gen
  let genHash = MBS.makeRandomHash genVar
  tempDir <- Directory.makeAbsolute "temp"
  fileStore <- FBS.make' genHash tempDir
  pure ( testGroup "FileBlockStore" $ makeCases fileStore
       , Directory.removeDirectoryRecursive tempDir)

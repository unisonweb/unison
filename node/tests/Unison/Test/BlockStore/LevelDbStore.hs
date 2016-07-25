module Unison.Test.BlockStore.LevelDbStore where

import System.IO.Unsafe
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.BlockStore (BlockStore)
import Unison.Runtime.Address
import Unison.Test.BlockStore
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified System.Directory as Directory
import qualified Unison.BlockStore.LevelDbStore as LBS

data FileResource = FileResource
  { path :: FilePath
  , store :: BlockStore Address
  }

setup :: IO FileResource
setup = do
  tempDir <- Directory.makeAbsolute "tempLDB"
  fileStore <- LBS.make makeRandomAddress makeAddress tempDir
  pure $ FileResource tempDir fileStore

tests :: TestTree
tests = withResource setup (Directory.removeDirectoryRecursive . path)
  (testGroup "LevelDbBlockStore" . makeCases . store . unsafePerformIO)

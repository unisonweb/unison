module Unison.Test.BlockStore.FileBlockStore where

import System.IO.Unsafe
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.BlockStore (BlockStore)
import Unison.Hash (Hash)
import Unison.Test.BlockStore
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified System.Directory as Directory
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.BlockStore.MemBlockStore as MBS

data FileResource = FileResource
  { path :: FilePath
  , store :: BlockStore Hash
  }

setup :: IO FileResource
setup = do
  gen <- getStdGen
  genVar <- IORef.newIORef gen
  let genHash = MBS.makeRandomHash genVar
  tempDir <- Directory.makeAbsolute "temp"
  fileStore <- FBS.make' genHash tempDir
  pure $ FileResource tempDir fileStore

tests :: TestTree
tests = withResource setup (Directory.removeDirectoryRecursive . path)
  (testGroup "FileBlockStore" . makeCases . store . unsafePerformIO)

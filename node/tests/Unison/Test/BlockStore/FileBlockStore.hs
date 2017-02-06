module Unison.Test.BlockStore.FileBlockStore where

import System.IO.Unsafe
import System.Random
import EasyTest
import Unison.BlockStore (BlockStore)
import Unison.Runtime.Address
import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import qualified System.Directory as Directory
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Test.BlockStore as BST

data FileResource = FileResource
  { path :: FilePath
  , store :: BlockStore Address
  }

setup :: IO FileResource
setup = do
  tempDir <- Directory.makeAbsolute "tempFBS"
  fileStore <- FBS.make' BST.makeRandomAddress makeAddress tempDir
  pure $ FileResource tempDir fileStore

test :: Test ()
test = scope "FileBlockStore" $
  using setup
        (Directory.removeDirectoryRecursive . path)
        (BST.test . store)

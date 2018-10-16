module Unison.Codebase.CommandLine where

import Control.Monad (forever, void, when)
import Control.Monad.STM (atomically)
import Unison.Util.TQueue
import Control.Concurrent (forkIO)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import Data.Text (Text, unpack)
import System.FilePath (FilePath)
import Unison.Codebase.Name (Name)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Watch as Watch

data Event
  = In Char
  | UnisonFileChanged FilePath Text
  | UnisonBranchFileChanged FilePath

main :: FilePath -> Name -> Codebase m v a -> IO ()
main dir _currentBranch _codebase = do
  queue <- newTQueueIO
  branchFileChanges <- newTQueueIO

  void . forkIO . forever $ do
    c <- getChar
    atomically . writeTQueue queue $ In c

  void . forkIO $ do
    watcher <- Watch.watchDirectory dir (".u" `isSuffixOf`)
    forever $ do
      (filePath, text) <- watcher
      atomically.writeTQueue queue $ UnisonFileChanged filePath text

  void $ do
    void . forkIO $ do
    -- watch filesystem and add to branchFileChanges
      -- remember to ignore events about the branch we just wrote
      watcher <- Watch.watchDirectory' (Codebase.branchesPath dir)
      forever $ do
        (filePath,_) <- watcher
        when (".ubf" `isSuffixOf` filePath) $
          atomically.writeTQueue queue $ UnisonBranchFileChanged filePath

    void . forkIO $ do
      stuff <- Watch.collectUntilPause branchFileChanges 400000
      atomically $ traverse_ (writeTQueue queue . UnisonBranchFileChanged) stuff

  go queue "" where
    go queue currentLine = do
          event <- atomically $ readTQueue queue
          case event of
            In '\n' -> processLine currentLine
            In c -> go queue (currentLine ++ [c])
            UnisonFileChanged filePath text -> error $ filePath ++ unpack text
            UnisonBranchFileChanged filePath -> error $ filePath
    processLine line = case words line of
      "add" : args -> error $ show args
      ["branch"] -> error ""
      ["branch", name] -> error $ "branch " ++ name
      ["fork", newName] -> error $ "fork " ++ newName
      ["merge", from] -> error $ "merge " ++ from
      ["rename", from, to] -> error $ "rename " ++ from ++ " " ++ to
      _ -> error "help:"

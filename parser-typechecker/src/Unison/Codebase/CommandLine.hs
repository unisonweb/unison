module Unison.Codebase.CommandLine where

import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Concurrent (forkIO)
import Data.List (isSuffixOf)
import Data.Text (Text, unpack)
import System.FilePath (FilePath)
import Unison.Codebase.Name (Name)
import Unison.Codebase (Codebase)
import Unison.Codebase.Watch (watchDirectory)

data Event
  = In Char
  | UnisonFileChanged FilePath Text
  | UnisonBranchFileChanged FilePath

main :: FilePath -> Name -> Codebase m v a -> IO ()
main dir _currentBranch _codebase = do
  queue <- newTQueueIO

  _stdinThread <- forkIO . forever $ do
    c <- getChar
    atomically . writeTQueue queue $ In c

  _unisonFileThread <- forkIO $ do
    watcher <- watchDirectory dir (".u" `isSuffixOf`)
    forever $ do
      (filePath, text) <- watcher
      atomically.writeTQueue queue $ UnisonFileChanged filePath text

  _unisonBranchFileThread <- forkIO $ error "todo"

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

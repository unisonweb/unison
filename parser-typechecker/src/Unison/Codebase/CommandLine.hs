module Unison.Codebase.CommandLine where

import Control.Monad (forever, forM_, void, when)
import Control.Monad.STM (atomically)
import Unison.Util.TQueue
import Control.Concurrent (forkIO)
import Data.List (isSuffixOf, sort)
import Data.Text (Text, pack, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import Unison.Codebase.Name (Name)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase (Codebase)
import qualified Unison.Hash as Hash
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Watch as Watch

data Event
  = In Char
  | UnisonFileChanged FilePath Text
  | UnisonBranchFileChanged (Set FilePath)

main :: FilePath -> Name -> Codebase IO v a -> IO ()
main dir currentBranchName codebase = do
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
          atomically . writeTQueue branchFileChanges $ filePath

    void . forkIO $ do
      stuff <- Watch.collectUntilPause branchFileChanges 400000
      atomically . writeTQueue queue . UnisonBranchFileChanged $ Set.fromList stuff
  branch <- Codebase.getBranch codebase currentBranchName
  case branch of
    Nothing -> error "force user to pick or create a valid branch"
    Just b  -> go b currentBranchName queue True ""
  where
  go :: Branch -> Name -> TQueue Event -> Bool -> String -> IO ()
  go branch name queue showPrompt currentLine = do
    when showPrompt $ putStr $ unpack name ++ "> " ++ currentLine
    event <- atomically $ readTQueue queue
    case event of
      In '\n' -> processLine branch name queue currentLine
      In c -> go branch name queue False (currentLine ++ [c])
      UnisonFileChanged filePath text -> error "todo"
      UnisonBranchFileChanged filePaths -> do
      -- make sure we can assume that `branch` is already on disk
      let bFileName = Hash.base58s . Branch.toHash $ branch
          filePaths' = Set.filter (\s -> FilePath.takeBaseName s /= bFileName) filePaths
      if null filePaths' then pure ()
      else do
        putStr $ "I've detected external changes to the branch; reloading..."
        b' <- Codebase.getBranch codebase currentBranchName
        case b' of
          Just b' -> do
            putStrLn $ " done!"
            putStrLn $ "TODO: tell the user what changed as a result of the merge"
            go b' name queue True currentLine
          Nothing -> do
            putStrLn $ "\n...that didn't work.  I'm going to write out what I have in memory."
            -- note: this will be a combination of what's in memory plus
            -- whatever has appeared on disk since the time there was nothing
            -- on disk :|
            branch' <- Codebase.mergeBranch codebase name branch
            go branch' name queue True currentLine

  processLine :: Branch -> Name -> TQueue Event -> String -> IO ()
  processLine branch name queue line = case words line of
    "add" : args -> error $ show args
    ["branch"] -> do
      branches <- sort <$> Codebase.branches codebase
      forM_ branches $ \name' ->
        if name' == name then putStrLn $ " * " ++ unpack name
                         else putStrLn $ "   " ++ unpack name
      go branch name queue True ""
    ["branch", name'] -> do
      branch' <- Codebase.getBranch codebase $ pack name'
      case branch' of
        Nothing -> do
          putStrLn $ "I couldn't find a branch named " ++ name'
          go branch name queue True ""
        Just branch' -> go branch' (pack name') queue True ""
    ["fork", newName0] -> do
      let newName = pack newName0
      branchExists <- Codebase.branchExists codebase newName
      if branchExists then do
        putStrLn $ "Sorry, a branch by that name already exists."
        go branch name queue True ""
      else do
        branch' <- Codebase.mergeBranch codebase newName branch
        when (branch' /= branch) $
          putStrLn $ "Some extra stuff appeared right when you forked, "
                  ++ "and I went ahead and smashed it all together for you!"
        go branch' newName queue True ""

    ["merge", from] -> do
      branch' <- Codebase.getBranch codebase $ pack from
      case branch' of
        Nothing -> do
          putStrLn $ "Sorry, I can't find a branch by that name to merge from."
          go branch name queue True ""
        Just branch' -> do
          -- showBranchDiff $ diffBranch branch branch'
          branch'' <- Codebase.mergeBranch codebase name branch'
          putStrLn $ "Flawless victory!"
          go branch'' name queue True ""


    ["rename", from, to] -> error $ "todo" ++ "rename " ++ from ++ " " ++ to
    _ -> error $ "todo" ++ "help:"

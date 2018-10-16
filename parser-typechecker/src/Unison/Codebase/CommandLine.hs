{-# LANGUAGE LambdaCase #-}

module Unison.Codebase.CommandLine where

import Control.Monad (forever, forM_, void, when)
import Control.Monad.STM (STM, atomically)
import Unison.Util.TQueue (TQueue)
import qualified Unison.Util.TQueue as TQueue
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
  = UnisonFileChanged FilePath Text
  | UnisonBranchFileChanged (Set FilePath)

main :: FilePath -> Name -> Codebase IO v a -> IO ()
main dir currentBranchName codebase = do
  queue <- TQueue.newIO
  lineQueue <- TQueue.newIO
  branchFileChanges <- TQueue.newIO

  -- enqueue stdin into lineQueue
  void . forkIO . forever $ getChar >>= atomically . TQueue.enqueue lineQueue

  -- watch for .u file changes
  void . forkIO $ do
    watcher <- Watch.watchDirectory dir (".u" `isSuffixOf`)
    forever $ do
      (filePath, text) <- watcher
      atomically . TQueue.enqueue queue $ UnisonFileChanged filePath text

  -- watch for .ubf file changes
  void $ do
    -- add .ubf file changes to intermediate queue
    void . forkIO $ do
      watcher <- Watch.watchDirectory' (Codebase.branchesPath dir)
      forever $ do
        (filePath,_) <- watcher
        when (".ubf" `isSuffixOf` filePath) $
          atomically . TQueue.enqueue branchFileChanges $ filePath
    -- smooth out intermediate queue, onto regular queue
    void . forkIO $ do
      stuff <- Watch.collectUntilPause branchFileChanges 400000
      atomically . TQueue.enqueue queue . UnisonBranchFileChanged $ Set.fromList stuff

  -- load current branch from disk
  branch <- Codebase.getBranch codebase currentBranchName
  case branch of
    Nothing -> error "force user to pick or create a valid branch"
    Just b  -> go b currentBranchName queue lineQueue
  where
  go :: Branch -> Name -> TQueue Event -> TQueue Char -> IO ()
  go branch name queue lineQueue = do
    -- print prompt and whatever input was on it / at it
    incompleteLine <- atomically . peekIncompleteLine $ lineQueue
    putStr $ unpack name ++ "> " ++ incompleteLine

    -- wait for new lines from user or asynchronous events from filesystem
    TQueue.raceIO (TQueue.peek queue) (awaitCompleteLine lineQueue) >>= \case
      Right _ -> processLine branch name queue lineQueue
      Left _ -> atomically (TQueue.dequeue queue) >>= \case
        UnisonFileChanged _filePath _text -> error "todo"
        UnisonBranchFileChanged filePaths -> do
        -- make sure we can assume that `branch` is already on disk
          let bFileName = Hash.base58s . Branch.toHash $ branch
              filePaths' = Set.filter (\s -> FilePath.takeBaseName s /= bFileName) filePaths
          if null filePaths' then
            go branch name queue lineQueue
          else do
            putStr $ "I've detected external changes to the branch; reloading..."
            b' <- Codebase.getBranch codebase currentBranchName
            case b' of
              Just b' -> do
                putStrLn $ " done!"
                putStrLn $ "TODO: tell the user what changed as a result of the merge"
                go b' name queue lineQueue
              Nothing -> do
                putStrLn $ "\n...that didn't work.  I'm going to write out what I have in memory."
                -- note: this will be a combination of what's in memory plus
                -- whatever has appeared on disk since the time there was nothing
                -- on disk :|
                branch' <- Codebase.mergeBranch codebase name branch
                go branch' name queue lineQueue

  -- should never block
  peekIncompleteLine :: TQueue Char -> STM String
  peekIncompleteLine q = TQueue.tryPeekWhile (/= '\n') q

  -- block until a full line is available
  takeLine :: TQueue Char -> STM String
  takeLine q = do
    line <- TQueue.takeWhile (/= '\n') q
    ch <- TQueue.dequeue q
    if (ch /= '\n') then error "unpossibility in takeLine" else pure line

  -- blocks until a line ending in '\n' is available
  awaitCompleteLine :: TQueue Char -> STM ()
  awaitCompleteLine ch = void $ TQueue.peekWhile (/= '\n') ch

  processLine :: Branch -> Name -> TQueue Event -> TQueue Char -> IO ()
  processLine branch name queue lineQueue = do
    line <- atomically $ takeLine lineQueue
    case words line of
      "add" : args -> error $ show args
      ["branch"] -> do
        branches <- sort <$> Codebase.branches codebase
        forM_ branches $ \name' ->
          if name' == name then putStrLn $ " * " ++ unpack name
                           else putStrLn $ "   " ++ unpack name
        -- idea: could instead prompt user and read directly from lineQueue to handle
        go branch name queue lineQueue
      ["branch", name'] -> do
        branch' <- Codebase.getBranch codebase $ pack name'
        case branch' of
          Nothing -> do
            putStrLn $ "I couldn't find a branch named " ++ name'
            go branch name queue lineQueue
          Just branch' -> go branch' (pack name') queue lineQueue
      ["fork", newName0] -> do
        let newName = pack newName0
        branchExists <- Codebase.branchExists codebase newName
        if branchExists then do
          putStrLn $ "Sorry, a branch by that name already exists."
          go branch name queue lineQueue
        else do
          branch' <- Codebase.mergeBranch codebase newName branch
          when (branch' /= branch) $
            putStrLn $ "Some extra stuff appeared right when you forked, "
                    ++ "and I went ahead and smashed it all together for you!"
          go branch' newName queue lineQueue

      ["merge", from] -> do
        branch' <- Codebase.getBranch codebase $ pack from
        case branch' of
          Nothing -> do
            putStrLn $ "Sorry, I can't find a branch by that name to merge from."
            go branch name queue lineQueue
          Just branch' -> do
            -- showBranchDiff $ diffBranch branch branch'
            branch'' <- Codebase.mergeBranch codebase name branch'
            putStrLn $ "Flawless victory!"
            go branch'' name queue lineQueue

      -- rename a term in the current branch
      ["rename", _from, _to] -> error "todo"
      _ -> error $ "todo" ++ "help:"

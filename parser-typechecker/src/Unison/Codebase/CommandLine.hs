{-# LANGUAGE LambdaCase #-}

module Unison.Codebase.CommandLine where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (forM_, forever, void, when)
import           Control.Monad.STM      (STM, atomically)
import           Data.Foldable          (toList, traverse_)
import           Data.List              (isSuffixOf, find, sort)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text, pack, unpack)
import           Data.Strings           (strPadLeft)
import           System.FilePath        (FilePath)
import qualified System.FilePath        as FilePath
import           Unison.Codebase        (Codebase)
import qualified Unison.Codebase        as Codebase
import           Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import           Unison.Codebase.Name   (Name)
import qualified Unison.Codebase.Watch  as Watch
import qualified Unison.Hash            as Hash
import           Unison.Util.TQueue     (TQueue)
import qualified Unison.Util.TQueue     as TQueue
import qualified Text.Read as Read

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
                branch' <- mergeBranchAndShowDiff name branch
                go branch' name queue lineQueue

  mergeBranchAndShowDiff :: Name -> Branch -> IO Branch
  mergeBranchAndShowDiff name branch = do
    branch' <- Codebase.mergeBranch codebase name branch
    -- when (branch' /= branch) $
    --   putStrLn $ "Some extra stuff appeared right when you forked, "
    --           ++ "and I went ahead and smashed it all together for you!"
    pure branch'

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
          branch' <- mergeBranchAndShowDiff newName branch
          go branch' newName queue lineQueue

      ["merge", from] -> do
        branch' <- Codebase.getBranch codebase $ pack from
        case branch' of
          Nothing -> do
            putStrLn $ "Sorry, I can't find a branch by that name to merge from."
            go branch name queue lineQueue
          Just branch' -> do
            branch'' <- mergeBranchAndShowDiff name branch'
            putStrLn $ "Flawless victory!"
            go branch'' name queue lineQueue

      -- rename a term/type/... in the current branch
      ["rename", from, to] ->
        let terms = Branch.termsNamed (pack from) branch
            types = Branch.typesNamed (pack from) branch
            renameTerm branch = do
              let branch' = Branch.renameTerm (pack from) (pack to) branch
              mergeBranchAndShowDiff name branch'
            renameType branch = do
              let branch' = Branch.renameType (pack from) (pack to) branch
              mergeBranchAndShowDiff name branch'
            go' b = go b name queue lineQueue
        in case (toList terms, toList types) of
          ([], []) -> putStrLn "I couldn't find anything by that name."
          ([_term], []) -> renameTerm branch >>= go'
          ([], [_typ]) -> renameType branch >>= go'
          ([_term], [_typ]) -> do
            putStrLn "Do you want to rename the [term], [type], [both], or [neither]?"
            putStr ">> "
            (atomically . fmap words . takeLine) lineQueue >>= \case
              ["term"] -> renameTerm branch >>= go'
              ["type"] -> renameType branch >>= go'
              ["both"] -> renameTerm branch >>= renameType >>= go'
              _ -> go' branch
          (_terms, _types) -> do
            -- idea: print out _terms and _types, so user can view them
            putStrLn $ "There's more than one thing called " ++ from ++ "."
            putStrLn $ "Use `> <command to resolve conflicts> unname " ++ from ++ "` to resolve conflicts, then try again."
            go' branch
      _ -> error $ "todo" ++ "help:"

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

multipleChoice :: [(String, a)] -> TQueue Char -> IO [a]
multipleChoice as lineQueue = do
  let render ((s, _), index) = putStrLn $ strPadLeft ' ' 5 ("[" ++ show index ++ "] ") ++ s
  traverse_ render (as `zip` [(1::Int)..])
  putStrLn "Please enter your selection as a space separated list of numbers."
  putStr ">> "
  numbers <- (atomically . fmap words . takeLine) lineQueue
  case traverse Read.readMaybe numbers of
    Nothing ->
      putStrLn "Sorry, I couldn't understand at least one of those numbers."
      >> multipleChoice as lineQueue
    Just numbers -> case find (\i -> i < 1 || i > length as) numbers of
      Just i ->
        (putStrLn $ "You entered the number " ++ show i ++ " which wasn't one of the choices.")
          >> multipleChoice as lineQueue
      Nothing -> pure $ snd . (as !!) . (+ (-1)) <$> numbers

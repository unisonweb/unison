{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.CommandLine where

import           Control.Concurrent           (forkIO)
import           Control.Exception            (finally)
import           Control.Monad                (forM_, forever, void)
import           Control.Monad.STM            (STM, atomically)
import           Data.Foldable                (toList, traverse_)
import           Data.List                    (find, isPrefixOf, isSuffixOf,
                                               sort)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Strings                 (strPadLeft)
import           Data.Text                    (Text, pack, unpack)
import           Safe                         (atMay)
import qualified System.Console.ANSI          as Console
import           System.FilePath              (FilePath)
import qualified Text.Read                    as Read
import           Unison.Codebase              (Codebase)
import qualified Unison.Codebase              as Codebase
import           Unison.Codebase.Branch       (Branch)
import qualified Unison.Codebase.Branch       as Branch
import           Unison.Codebase.Name         (Name)
import           Unison.Codebase.Runtime      (Runtime)
import qualified Unison.Codebase.Runtime      as RT
import qualified Unison.Codebase.Watch        as Watch
import           Unison.FileParsers           (parseAndSynthesizeFile)
import           Unison.Parser                (PEnv)
import qualified Unison.Parser                as Parser
import           Unison.PrintError            (parseErrorToAnsiString,
                                               printNoteWithSourceAsAnsi)
import           Unison.Result                (Result (Result))
import qualified Unison.Result                as Result
import           Unison.Util.Monoid
import           Unison.Util.TQueue           (TQueue)
import qualified Unison.Util.TQueue           as TQueue
import           Unison.Var                   (Var)


data Event
  = UnisonFileChanged FilePath Text
  | UnisonBranchChanged (Set Name)

main :: forall v a. Var v => FilePath -> Name -> IO (Runtime v) -> Codebase IO v a -> IO ()
main dir currentBranchName startRuntime codebase = do
  queue <- TQueue.newIO
  lineQueue <- TQueue.newIO
  runtime <- startRuntime
  let takeActualLine = atomically (takeLine lineQueue)

  -- enqueue stdin into lineQueue
  void . forkIO . forever $ getChar >>= atomically . TQueue.enqueue lineQueue

  -- watch for .u file changes
  void . forkIO $ do
    watcher <- Watch.watchDirectory dir (".u" `isSuffixOf`)
    forever $ do
      (filePath, text) <- watcher
      atomically . TQueue.enqueue queue $ UnisonFileChanged filePath text

  -- watch for external branch changes
  (cancelExternalBranchUpdates, externalBranchUpdates) <- Codebase.branchUpdates codebase
  void . forkIO . forever $ do
      updatedBranches <- externalBranchUpdates
      atomically . TQueue.enqueue queue . UnisonBranchChanged $ updatedBranches

  -- load current branch from disk
  branch <- Codebase.getBranch codebase currentBranchName
  (`finally` (RT.terminate runtime *> cancelExternalBranchUpdates)) $ case branch of
    Nothing -> do
      selectBranch codebase currentBranchName takeActualLine >>= \case
        Just (name, branch) -> go0 branch name queue lineQueue runtime
        Nothing -> putStrLn "Exiting."
    Just b  -> go0 b currentBranchName queue lineQueue runtime
  where
  go0 branch branchName queue lineQueue runtime = go branch branchName
    where

    -- print prompt and whatever input was on it / at it
    printPrompt :: Name -> IO ()
    printPrompt branchName = do
      incompleteLine <- atomically . peekIncompleteLine $ lineQueue
      putStr $ unpack branchName ++ "> " ++ incompleteLine

    handleUnisonFile :: Runtime v -> Codebase IO v a -> PEnv v -> FilePath -> Text -> IO ()
    handleUnisonFile runtime codebase penv filePath src = do
      let Result notes r = parseAndSynthesizeFile penv filePath src
      case r of
        Nothing -> do -- parsing failed
          Console.setTitle "Unison \128721"
          forM_ notes $ \case
            Result.Parsing err ->
              putStrLn $ parseErrorToAnsiString (unpack src) err
            err ->
              error $"I was expecting a parsing error here but got:\n" ++ show err

        Just (errorEnv, r) -> case r of
          Nothing -> do -- typechecking failed
            Console.setTitle "Unison \128721"
            let showNote notes = intercalateMap
                  "\n\n" (printNoteWithSourceAsAnsi errorEnv (unpack src)) notes
            putStrLn . showNote . toList $ notes
          Just typecheckedUnisonFile -> do
            Console.setTitle "Unison ✅"
            putStrLn "✅  Typechecked! Any watch expressions (lines starting with `>`) are shown below.\n"
            -- todo: print out top-level bindings
            RT.evaluate runtime typecheckedUnisonFile codebase

    go :: Branch -> Name -> IO ()
    go branch name = do
      printPrompt name

      -- wait for new lines from user or asynchronous events from filesystem
      TQueue.raceIO (TQueue.peek queue) (awaitCompleteLine lineQueue) >>= \case
        Right _ -> processLine branch name
        Left _ -> atomically (TQueue.dequeue queue) >>= \case
          UnisonFileChanged filePath text ->
            handleUnisonFile runtime codebase Parser.penv0 filePath text -- todo: don't use penv0
          UnisonBranchChanged branches ->
            if Set.member name branches then do
              putStr $ "I've detected external changes to the branch; reloading..."
              b' <- Codebase.getBranch codebase name
              case b' of
                Just b' -> do
                  putStrLn $ " done!"
                  putStrLn $ "TODO: tell the user what changed as a result of the merge"
                  go b' name
                Nothing -> do
                  putStrLn $ "\n...that didn't work.  I'm going to save what I have in memory."
                  -- note: this will be a combination of what's in memory plus
                  -- whatever has appeared on disk since the time there was nothing
                  -- on disk :|
                  branch' <- mergeBranchAndShowDiff codebase name branch
                  go branch' name
            else go branch name

    -- newBranch :: Name -> IO Branch
    -- newBranch name = mergeBranchAndShowDiff newName mempty

    processLine :: Branch -> Name -> IO ()
    processLine branch name = do
      line <- atomically $ takeLine lineQueue
      case words line of
        "add" : args -> error $ show args

        ["branch"] -> do
          branches <- sort <$> Codebase.branches codebase
          forM_ branches $ \name' ->
            if name' == name then putStrLn $ " * " ++ unpack name'
                             else putStrLn $ "   " ++ unpack name'
          -- idea: could instead prompt user and read directly from lineQueue to handle
          go branch name

        ["branch", name'] -> do
          branch' <- Codebase.getBranch codebase $ pack name'
          case branch' of
            Nothing -> do
              putStrLn $ "I couldn't find a branch named \"" ++ name' ++ "\"."
              go branch name
            Just branch' -> go branch' (pack name')

        ["fork", newName0] -> do
          let newName = pack newName0
          branchExists <- Codebase.branchExists codebase newName
          if branchExists then do
            putStrLn $ "Sorry, a branch by that name already exists."
            go branch name
          else do
            branch' <- mergeBranchAndShowDiff codebase newName branch
            go branch' newName

        ["merge", from] -> do
          branch' <- Codebase.getBranch codebase $ pack from
          case branch' of
            Nothing -> do
              putStrLn $ "Sorry, I can't find a branch by that name to merge from."
              go branch name
            Just branch' -> do
              branch'' <- mergeBranchAndShowDiff codebase name branch'
              putStrLn $ "Flawless victory!"
              go branch'' name

        -- rename a term/type/... in the current branch
        ["rename", from, to] ->
          let terms = Branch.termsNamed (pack from) branch
              types = Branch.typesNamed (pack from) branch
              renameTerm branch = do
                let branch' = Branch.renameTerm (pack from) (pack to) branch
                mergeBranchAndShowDiff codebase name branch'
              renameType branch = do
                let branch' = Branch.renameType (pack from) (pack to) branch
                mergeBranchAndShowDiff codebase name branch'
              go' b = go b name
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

renderChoice :: Int -> Maybe String -> String -> Int -> String
renderChoice width deefault choice index =
  (if Just choice == deefault then "* " else "  ") ++
  (strPadLeft ' ' width $ show index) ++ ". " ++ choice

renderChoices :: forall a. Maybe String -> [((String, a), Int)] -> String
renderChoices deefault entries =
  intercalateMap "\n" go entries
  where go ((s, _a), index) = renderChoice pad deefault s index
        pad = ceiling $ logBase (10::Double)
                                (fromIntegral . snd . last $ entries)

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

-- let the user pick from a list of labeled `a`s
-- todo: rewrite this to let them toggle stuff
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

-- Merges `branch` into any the branch `name`, creating it if necessary.
mergeBranchAndShowDiff :: Monad m => Codebase m v a -> Name -> Branch -> m Branch
mergeBranchAndShowDiff codebase targetName sourceBranch = do
  branch' <- Codebase.mergeBranch codebase targetName sourceBranch
  -- when (branch' /= branch) $
  --   putStrLn $ "Some extra stuff appeared right when you forked, "
  --           ++ "and I went ahead and smashed it all together for you!"
  pure branch'

selectBranch :: Codebase IO v a -> Name -> IO String -> IO (Maybe (Name, Branch))
selectBranch codebase name takeLine = do
  -- todo: refactor to single-choice function
  branch <- Codebase.getBranch codebase name
  case branch of
    -- if branch named `name` exists, load it,
    Just branch -> pure . Just $ (name, branch)
    -- otherwise,
      -- list branches that do exist, plus option to create, plus option to cancel
    Nothing -> do
      putStrLn $
        "The branch " ++ show name ++ " doesn't exist. " ++
         "Do you want to create it, or pick a different one?"
      branches <- Codebase.branches codebase
      choice <- singleChoice (((unpack <$> branches) `zip` (Right <$> branches)) ++
                    [("create it", Left True)
                    ,("cancel", Left False)]) (Just $ "create it") takeLine
      case choice of
        Just (Left False) -> pure Nothing
        Just (Left True) -> do
          branch <- mergeBranchAndShowDiff codebase name mempty
          pure $ Just (name, branch)
        Just (Right name) -> selectBranch codebase name takeLine
        Nothing -> error "unpossible"

singleChoice :: [(String, a)] -> Maybe String -> IO String -> IO (Maybe a)
singleChoice entries deefault takeLine = do
  let restart = singleChoice entries deefault takeLine
      numberedEntries = entries `zip` [1..]
      resume = do
        putStr $ "Enter a number, or prefix, or a blank line " ++
                  if null deefault
                    then "to cancel: "
                    else "to accept the default (*): "
        input <- takeLine
        case words input of
          [] -> case deefault of
            Nothing -> pure Nothing
            Just deefault ->
              pure (snd <$> find (\(s,_) -> s == deefault) entries)
          input : _ -> case Read.readMaybe input of
            Nothing -> -- maybe it's a prefix
              case filter ((input `isPrefixOf`) . fst . fst) numberedEntries of
                [] ->
                  putStrLn "Sorry, I couldn't understand your selection."
                     *> restart
                [((_s, a), _i)] -> pure (Just a)
                matches -> do
                  putStrLn "I'm not sure which one of these you meant:"
                  putStrLn $ renderChoices deefault matches
                  resume
            Just i -> case atMay entries (i-1) of
              Just (_s, a) -> pure (Just a)
              Nothing ->
                putStrLn ("Please pick a number from 1 to "
                                    ++ show (length entries) ++ ".") *> restart
  putStrLn $ renderChoices deefault numberedEntries
  resume

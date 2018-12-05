{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Codebase.CommandLine2 where

import           Control.Arrow                  ( (&&&) )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.List                      ( isSuffixOf )
import           Data.Maybe                     ( listToMaybe, fromMaybe )
import qualified Data.Map                       as Map
import           Data.Map                       ( Map )
import qualified Data.Text                      as Text
import           Data.Text                      ( Text )
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( forever
                                                , void
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO, liftIO )
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Branch         ( Branch )
import           Unison.Codebase.Editor         ( Output(..)
                                                , BranchName
                                                , Event(..)
                                                , Input(..)
                                                )
import qualified Unison.Codebase.Editor        as Editor
import qualified Unison.Codebase.Editor.Actions
                                               as Actions
import           Unison.Codebase.Runtime
import qualified Unison.Codebase.Watch         as Watch
import           Unison.Parser                  ( Ann )
import qualified Unison.Util.Relation          as R
import           Unison.Util.TQueue             ( TQueue )
import qualified Unison.Util.TQueue            as Q
import           Unison.Var                     ( Var )
import qualified System.Console.Haskeline      as Line

notifyUser :: Var v => Output v -> IO ()
notifyUser o = case o of
  DisplayConflicts branch -> do
    let terms    = R.dom $ Branch.termNamespace branch
        patterns = R.dom $ Branch.patternNamespace branch
        types    = R.dom $ Branch.typeNamespace branch
    when (not $ null terms) $ do
      putStrLn "🙅 The following terms have conflicts: "
      traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) terms
    when (not $ null patterns) $ do
      putStrLn "🙅 The following patterns have conflicts: "
      traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) patterns
    when (not $ null types) $ do
      putStrLn "🙅 The following types have conflicts: "
      traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) types
    -- TODO: Present conflicting TermEdits and TypeEdits
    -- if we ever allow users to edit hashes directly.
  _ -> putStrLn $ show o

allow :: FilePath -> Bool
allow = (||) <$> (".u" `isSuffixOf`) <*> (".uu" `isSuffixOf`)

-- TODO: Return all of these thread IDs so we can throw async exceptions at
-- them when we need to quit.

watchFileSystem :: TQueue Event -> FilePath -> IO ()
watchFileSystem q dir = void . forkIO $ do
  watcher <- Watch.watchDirectory dir allow
  forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text

watchBranchUpdates :: TQueue Event -> Codebase IO v a -> IO ()
watchBranchUpdates q codebase = do
  (_cancelExternalBranchUpdates, externalBranchUpdates) <-
    Codebase.branchUpdates codebase
  void . forkIO . forever $ do
    updatedBranches <- externalBranchUpdates
    atomically . Q.enqueue q . UnisonBranchChanged $ updatedBranches

warnNote :: String -> String
warnNote s = "⚠️  " <> s

type IsOptional = Bool

data InputPattern = InputPattern
  { patternName :: String
  , args :: [(IsOptional, ArgumentType)]
  , help :: Text
  , parse :: [String] -> Either String Input
  }

data ArgumentType = ArgumentType
  { typeName :: String
  , suggestions :: forall m v a . Monad m
                => String
                -> Codebase m v a
                -> Branch
                -> m [Line.Completion]
  }

validInputs :: [InputPattern]
validInputs
  = [ InputPattern
      "add"
      []
      (  "`add` adds to the codebase all the definitions from "
      <> "the most recently typechecked file."
      )
      (\ws -> if not $ null ws
        then Left $ warnNote "`add` doesn't take any arguments."
        else pure AddI
      )
    , InputPattern
      "branch"
      [(True, branchArg)]
      (  "`branch` lists all branches in the codebase.\n"
      <> "`branch foo` switches to the branch named 'foo', "
      <> "creating it first if it doesn't exist."
      )
      (\case
        []  -> pure ListBranchesI
        [b] -> pure . SwitchBranchI $ Text.pack b
        _ ->
          Left
            .  warnNote
            $  "Use `branch` to list all branches "
            <> "or `branch foo` to switch to the branch 'foo'."
      )
    , InputPattern
      "fork"
      [(False, branchArg)]
      (  "`fork foo` creates the branch 'foo' "
      <> "as a fork of the current branch."
      )
      (\case
        [b] -> pure . ForkBranchI $ Text.pack b
        _   -> Left $ warnNote
          "Use `fork foo` to create the branch 'foo' from the current branch."
      )
    , InputPattern
      "merge"
      [(False, branchArg)]
      ("`merge foo` merges the branch 'foo' into the current branch.")
      (\case
        [b] -> pure . MergeBranchI $ Text.pack b
        _ ->
          Left
            .  warnNote
            $  "Use `merge foo` to merge the branch 'foo' "
            <> " into the current branch."
      )
    , quit "quit"
    , quit "exit"
    ]
 where
  branchArg = ArgumentType "branch" $ \q codebase _ -> do
    branches <- Codebase.branches codebase
    let bs = Text.unpack <$> branches
    pure $ autoComplete q bs
  quit s = InputPattern
    s
    []
    "Exits the Unison command line interface."
    (\case
      [] -> pure QuitI
      _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
    )

completion :: String -> Line.Completion
completion s = Line.Completion s s True

autoComplete :: String -> [String] -> [Line.Completion]
autoComplete q ss = completion <$> Codebase.sortedApproximateMatches q ss

parseInput :: Map String InputPattern -> [String] -> Either String Input
parseInput patterns ss = case ss of
  command : args -> case Map.lookup command patterns of
    Just pat -> parse pat args
    Nothing ->
      Left
        $  "I don't know how to "
        <> command
        <> ". Type `help` or `?` to get help."

queueInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> TQueue Input
  -> Codebase m v a
  -> Branch
  -> m ()
queueInput patterns q codebase branch = Line.runInputT settings $ do
  line <- Line.getInputLine "> "
  case line of
    Nothing -> liftIO . atomically $ Q.enqueue q QuitI
    Just l  -> case parseInput patterns $ words l of
      Left err ->
        liftIO (putStrLn $ warnNote err)
          *> queueInput patterns q codebase branch
      Right i -> liftIO . atomically $ Q.enqueue q i
 where
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    let ws = words $ reverse prev
    in  case ws of
          h : t -> fromMaybe (pure []) $ do
            p            <- Map.lookup h patterns
            (_, argType) <- listToMaybe $ drop (length t) (args p)
            pure $ suggestions argType word codebase branch
          _ -> pure []

main
  :: forall v
   . Var v
  => FilePath
  -> Branch
  -> BranchName
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranch currentBranchName _initialFile startRuntime codebase =
  do
    eventQueue <- Q.newIO
    lineQueue  <- Q.newIO
    runtime    <- startRuntime
    branchRef  <- newIORef (currentBranch, currentBranchName)
    watchFileSystem eventQueue dir
    watchBranchUpdates eventQueue codebase
    let patternMap = Map.fromList $ (patternName &&& id) <$> validInputs
        awaitInput = do
          (branch, _branchName) <- readIORef branchRef
          queueInput patternMap lineQueue codebase branch
          Q.raceIO (Q.peek eventQueue) (Q.peek lineQueue) >>= \case
            Right _ -> do
              line <- atomically $ Q.dequeue lineQueue
              case parseInput patternMap line of
                Left  msg -> putStrLn msg *> awaitInput
                Right i   -> pure (Right i)
            Left _ -> Left <$> atomically (Q.dequeue eventQueue)
    Editor.commandLine awaitInput
                       runtime
                       (curry $ writeIORef branchRef)
                       notifyUser
                       codebase
      $ Actions.startLoop currentBranch currentBranchName

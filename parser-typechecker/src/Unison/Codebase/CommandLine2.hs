{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Codebase.CommandLine2 where

import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.List                      ( isSuffixOf )
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

parseInput :: Maybe String -> Either String Input
parseInput Nothing = Right QuitI
parseInput (Just s) = case words s of
  [] -> Left ""
  "add" : tl -> case tl of
    [] -> pure AddI
    _ -> Left $ warnNote "`add` doesn't take any arguments."
  ["branch"] -> pure ListBranchesI
  ["branch", b] -> pure . SwitchBranchI $ Text.pack b
  "branch" : _ -> Left . warnNote $
    "Use `branch` to list all branches " <>
    "or `branch foo` to switch to the branch 'foo'."
  ["fork", b] -> pure . ForkBranchI $ Text.pack b
  "fork" : _ -> Left . warnNote $
    "Use `fork foo` to create the branch 'foo' from the current branch."
  ["merge", b] -> pure . MergeBranchI $ Text.pack b
  "merge" : _ -> Left . warnNote $
    "Use `merge foo` to merge the branch 'foo' into the current branch."
  ["quit"] -> pure QuitI
  _ -> undefined

-- inputParser :: Monad m => [InputPattern] -> Codebase m v a -> BranchName -> Maybe String -> Either String Input
-- inputParser patterns codebase branch input

warnNote :: String -> String
warnNote s = "⚠️  " <> s

data InputPattern = InputPattern
  { patternName :: String
  , args :: [ArgumentType]
  , help :: Text
  , parse :: [String] -> Input
  }

data ArgumentType = ArgumentType
  { typeName :: String
  , suggestions :: forall m v a . Monad m => Codebase m v a -> BranchName -> m [String]
  }

queueInput
  :: (MonadIO m, Line.MonadException m)
  => TQueue (Maybe String)
  -> Codebase m v a
  -> Branch
  -> m ()
queueInput q _codebase _branch = Line.runInputT settings $ do
  line <- Line.getInputLine "> "
  liftIO . atomically $ Q.enqueue q line
 where
  settings = Line.Settings (error "tab complete") (Just ".unisonHistory") True

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
    let awaitInput = do
          (branch, _branchName) <- readIORef branchRef
          queueInput lineQueue codebase branch
          Q.raceIO (Q.peek eventQueue) (Q.peek lineQueue) >>= \case
            Right _ -> do
              line <- atomically $ Q.dequeue lineQueue
              case parseInput line of
                Left  msg -> putStrLn msg *> awaitInput
                Right i   -> pure (Right i)
            Left _ -> Left <$> atomically (Q.dequeue eventQueue)
    Editor.commandLine awaitInput
                       runtime
                       (curry $ writeIORef branchRef)
                       notifyUser
                       codebase
      $ Actions.startLoop currentBranch currentBranchName

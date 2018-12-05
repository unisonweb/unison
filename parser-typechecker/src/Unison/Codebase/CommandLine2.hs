{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Codebase.CommandLine2 where

import           Control.Applicative
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( isSuffixOf )
import qualified Data.Text                      as Text
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.STM         ( STM
                                                , atomically
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                , when
                                                )
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Branch        as Branch
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

queueInput :: TQueue (Maybe Char) -> IO ()
queueInput q =
  void
    .   forkIO
    .   forever
    $   Just
    <$> getChar
    <|> pure Nothing
    >>= atomically
    .   Q.enqueue q

-- block until a full line is available
takeLine :: TQueue (Maybe Char) -> STM (Maybe String)
takeLine q = do
  line <- Q.takeWhile (\x -> x /= Just '\n' && x /= Nothing) q
  ch   <- Q.dequeue q
  if (ch /= Just '\n' && ch /= Nothing)
    then error "unpossibility in takeLine"
    else pure $ sequence line

-- blocks until a line ending in '\n' is available, or EOF
awaitCompleteLine :: TQueue (Maybe Char) -> STM ()
awaitCompleteLine ch =
  void $ Q.peekWhile (\x -> x /= Just '\n' && x /= Nothing) ch

takeLineIO :: TQueue (Maybe Char) -> IO (Maybe String)
takeLineIO = atomically . takeLine

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

warnNote :: String -> String
warnNote s = "⚠️  " <> s

main
  :: forall v
   . Var v
  => FilePath
  -> BranchName
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranchName _initialFile startRuntime codebase = do
  eventQueue <- Q.newIO
  lineQueue  <- Q.newIO
  _runtime   <- startRuntime
  queueInput lineQueue
  watchFileSystem eventQueue dir
  watchBranchUpdates eventQueue codebase
  let awaitInput =
        Q.raceIO (Q.peek eventQueue) (awaitCompleteLine lineQueue) >>= \case
          Right _ -> do
            line <- takeLineIO lineQueue
            case parseInput line of
              Left  msg -> putStrLn msg *> awaitInput
              Right i   -> pure (Right i)
          Left _ -> Left <$> atomically (Q.dequeue eventQueue)
  Editor.commandLine awaitInput notifyUser codebase
    $ Actions.startLoop currentBranchName

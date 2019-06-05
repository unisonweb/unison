{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns #-}


module Unison.CommandLine.Main2 where

import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.IORef
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Prelude hiding (readFile, writeFile)
import Safe
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor (BranchName, Input (..))
import Unison.Codebase.Runtime (Runtime)
import Unison.Codebase2 (Codebase)
import Unison.CommandLine
import Unison.CommandLine.InputPattern (ArgumentType (suggestions), InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns2 (validInputs)
import Unison.CommandLine.OutputMessages (notifyUser)
import Unison.Parser (Ann)
import Unison.Var (Var)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Line
import qualified Unison.Codebase.Editor as E
import qualified Unison.Codebase.Editor.Actions as Actions
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Codebase2 as Codebase
import qualified Unison.CommandLine.InputPattern2 as IP
import qualified Unison.Util.Free as Free
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as Q

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch
  -> BranchName
  -> [String]
  -> m Input
getUserInput patterns codebase branch branchName numberedArgs =
  Line.runInputT settings $ do
    line <- Line.getInputLine $
      P.toANSI 80 (P.green (P.text branchName <> fromString prompt))
    case line of
      Nothing -> pure QuitI
      Just l -> case parseInput patterns . fmap expandNumber . words $ l of
        Left msg -> lift $ do
          liftIO $ putPrettyLn msg
          getUserInput patterns codebase branch branchName numberedArgs
        Right i -> pure i
 where
  expandNumber s = case readMay s of
    Just i -> case atMay numberedArgs (i - 1) of
      Just s -> s
      Nothing -> show i
    Nothing -> s
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    -- User hasn't finished a command name, complete from command names
    if null prev
      then pure $ fuzzyComplete word (Map.keys patterns)
    -- User has finished a command name; use completions for that command
      else case words $ reverse prev of
        h : t -> fromMaybe (pure []) $ do
          p       <- Map.lookup h patterns
          argType <- IP.argType p (length t)
          pure $ suggestions argType word codebase branch
        _ -> pure []

main
  :: forall v
   . Var v
  => FilePath
  -> BranchName
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranchName _initialFile startRuntime codebase =
  undefined
  --do
  --currentBranch <- Codebase.getBranch codebase currentBranchName
  --eventQueue    <- Q.newIO
  --currentBranch <- case currentBranch of
  --  Nothing ->
  --    Codebase.syncBranch codebase
  --                        currentBranchName
  --                        E.builtinBranch
  --      <* (  putStrLn
  --         $  "☝️  I found no branch named '"
  --         <> Text.unpack currentBranchName
  --         <> "' so I've created it for you."
  --         )
  --  Just b -> pure b
  --do
  --  runtime                  <- startRuntime
  --  branchRef                <- newIORef (currentBranch, currentBranchName)
  --  numberedArgsRef          <- newIORef []
  --  cancelFileSystemWatch    <- watchFileSystem eventQueue dir
  --  cancelWatchBranchUpdates <- watchBranchUpdates (readIORef branchRef)
  --                                                 eventQueue
  --                                                 codebase
  --  let patternMap =
  --        Map.fromList
  --          $   validInputs
  --          >>= (\p -> [(patternName p, p)] ++ ((, p) <$> aliases p))
  --      getInput = do
  --        (branch, branchName) <- readIORef branchRef
  --        numberedArgs <- readIORef numberedArgsRef
  --        getUserInput patternMap codebase branch branchName numberedArgs
  --  let
  --    awaitInput = do
  --      -- Race the user input and file watch.
  --      Async.race (atomically $ Q.peek eventQueue) getInput >>= \case
  --        Left _ -> Left <$> atomically (Q.dequeue eventQueue)
  --        x      -> pure x
  --    cleanup = do
  --      Runtime.terminate runtime
  --      cancelFileSystemWatch
  --      cancelWatchBranchUpdates
  --    loop :: Actions.LoopState v -> IO ()
  --    loop state = do
  --      writeIORef
  --        branchRef
  --        (Actions._currentBranch state, Actions._currentBranchName state)
  --      let free
  --            :: Free.Free
  --                 (E.Command (Either E.Event Input) v)
  --                 (Maybe (), Actions.LoopState v)
  --          free = runStateT (runMaybeT Actions.loop) state
  --      (o, state') <- E.commandLine awaitInput
  --                                   runtime
  --                                   (notifyUser dir)
  --                                   codebase
  --                                   free
  --      case o of
  --        Nothing -> pure ()
  --        Just () -> do
  --          writeIORef numberedArgsRef (Actions._numberedArgs state')
  --          loop state'
  --  (`finally` cleanup)
  --    $ loop (Actions.loopState0 currentBranch currentBranchName)

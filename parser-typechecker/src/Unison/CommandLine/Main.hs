{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Unison.CommandLine.Main where

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
import System.FilePath.Posix ((</>))
import Safe
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.Input (Input (..))
import qualified Unison.Codebase.Editor.HandleInput as HandleInput
import qualified Unison.Codebase.Editor.HandleCommand as HandleCommand
import Unison.Codebase.Runtime (Runtime)
import Unison.Codebase (Codebase)
import Unison.CommandLine
import Unison.PrettyTerminal
import Unison.CommandLine.InputPattern (ArgumentType (suggestions), InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyUser, shortenDirectory)
import Unison.Parser (Ann)
import Unison.Var (Var)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified System.Console.Haskeline as Line
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Codebase as Codebase
import qualified Unison.CommandLine.InputPattern as IP
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as Q

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch m
  -> Path.Absolute
  -> [String]
  -> m Input
getUserInput patterns codebase branch currentPath numberedArgs =
  Line.runInputT settings $ do
    line <- Line.getInputLine $
      P.toANSI 80 ((P.green . P.shown) currentPath <> fromString prompt)
    case line of
      Nothing -> pure QuitI
      Just l -> case parseInput patterns . fmap expandNumber . words $ l of
        Left msg -> lift $ do
          liftIO $ putPrettyLn msg
          getUserInput patterns codebase branch currentPath numberedArgs
        Right i -> pure i
 where
  expandNumber s = case readMay s of
    Just i -> fromMaybe (show i) . atMay numberedArgs $ i - 1
    Nothing -> s
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    -- User hasn't finished a command name, complete from command names
    if null prev
      then pure . exactComplete word $ Map.keys patterns
    -- User has finished a command name; use completions for that command
      else case words $ reverse prev of
        h : t -> fromMaybe (pure []) $ do
          p       <- Map.lookup h patterns
          argType <- IP.argType p (length t)
          pure $ suggestions argType word codebase branch currentPath
        _ -> pure []

welcomeMessage :: FilePath -> P.Pretty P.ColorText
welcomeMessage dir =
  P.red " _____"
    <> P.hiYellow "     _             "
    <> P.newline
    <> P.red "|  |  |"
    <> P.hiRed "___"
    <> P.hiYellow "|_|"
    <> P.hiGreen "___ "
    <> P.cyan "___ "
    <> P.purple "___ "
    <> P.newline
    <> P.red "|  |  |   "
    <> P.hiYellow "| |"
    <> P.hiGreen "_ -"
    <> P.cyan "| . |"
    <> P.purple "   |"
    <> P.newline
    <> P.red "|_____|"
    <> P.hiRed "_|_"
    <> P.hiYellow "|_|"
    <> P.hiGreen "___"
    <> P.cyan "|___|"
    <> P.purple "_|_|"
    <> P.newline
    <> P.newline
    <> P.linesSpaced
         [ P.wrap "Welcome to Unison!"
         , P.wrap
           (  "I'm currently watching for changes to .u files under "
           <> (P.group . P.blue $ fromString dir)
           )
         , P.wrap ("Type " <> P.hiBlue "help" <> " to get help. 😎")
         ]

main
  :: forall v
  . Var v
  => FilePath
  -> Path.Absolute
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir initialPath _initialFile startRuntime codebase = do
  dir' <- shortenDirectory dir
  putPrettyLn $ welcomeMessage dir'
  root <- Codebase.getRootBranch codebase
  eventQueue <- Q.newIO
  do
    runtime                  <- startRuntime
    -- we watch for root branch tip changes, but want to ignore ones we expect.
    rootRef                  <- newIORef root
    pathRef                  <- newIORef initialPath
    numberedArgsRef          <- newIORef []
    (config, cancelConfig)   <- watchConfig $ dir </> ".unisonConfig"
    cancelFileSystemWatch    <- watchFileSystem eventQueue dir
    cancelWatchBranchUpdates <- watchBranchUpdates (Branch.headHash <$>
                                                      readIORef rootRef)
                                                   eventQueue
                                                   codebase
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> (patternName p, p) : ((, p) <$> aliases p))
        getInput = do
          root <- readIORef rootRef
          path <- readIORef pathRef
          numberedArgs <- readIORef numberedArgsRef
          getUserInput patternMap codebase root path numberedArgs
    let
      awaitInput =
        -- Race the user input and file watch.
        Async.race (atomically $ Q.peek eventQueue) getInput >>= \case
          Left _ -> Left <$> atomically (Q.dequeue eventQueue)
          x      -> pure x
      cleanup = do
        Runtime.terminate runtime
        cancelConfig
        cancelFileSystemWatch
        cancelWatchBranchUpdates
      loop state = do
        writeIORef pathRef (HandleInput._currentPath state)
        let free = runStateT (runMaybeT HandleInput.loop) state
        (o, state') <- HandleCommand.commandLine config awaitInput
                                     (writeIORef rootRef)
                                     runtime
                                     (notifyUser dir)
                                     codebase
                                     free
        case o of
          Nothing -> pure ()
          Just () -> do
            writeIORef numberedArgsRef (HandleInput._numberedArgs state')
            loop state'
    (`finally` cleanup)
      $ loop (HandleInput.loopState0 root initialPath)

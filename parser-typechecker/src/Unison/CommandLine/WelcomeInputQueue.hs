{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.CommandLine.WelcomeInputQueue where

import Unison.Prelude

import Control.Concurrent.STM (atomically)
-- import Control.Exception (finally, catch, AsyncException(UserInterrupt), asyncExceptionFromException)
-- import Control.Monad.State (runStateT)
-- import Data.Configurator.Types (Config)
import Data.IORef
import Prelude hiding (readFile, writeFile)
-- import System.IO.Error (isDoesNotExistError)
import Unison.Codebase.Branch (Branch)
-- import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.Input (Input (..), Event)
-- import qualified Unison.Server.CodebaseServer as Server
-- import qualified Unison.Codebase.Editor.HandleInput as HandleInput
-- import qualified Unison.Codebase.Editor.HandleCommand as HandleCommand
-- import Unison.Codebase.Editor.Command (LoadSourceResult(..))
import Unison.Codebase (Codebase)
import Unison.CommandLine
import Unison.PrettyTerminal
import Unison.CommandLine.InputPattern (ArgumentType (suggestions), InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
-- import Unison.CommandLine.OutputMessages (notifyUser, notifyNumbered)
import Unison.Parser.Ann (Ann)
import Unison.Symbol (Symbol)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified System.Console.Haskeline as Line
import qualified Unison.Codebase.Path as Path
import qualified Unison.CommandLine.InputPattern as IP
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as Q
import Text.Regex.TDFA

-- Expand a numeric argument like `1` or a range like `3-9`
expandNumber :: [String] -> String -> [String]
expandNumber numberedArgs s =
  maybe [s]
        (map (\i -> fromMaybe (show i) . atMay numberedArgs $ i - 1))
        expandedNumber
 where
  rangeRegex = "([0-9]+)-([0-9]+)" :: String
  (junk,_,moreJunk, ns) =
    s =~ rangeRegex :: (String, String, String, [String])
  expandedNumber =
    case readMay s of
      Just i -> Just [i]
      Nothing ->
        -- check for a range
        case (junk, moreJunk, ns) of
          ("", "", [from, to]) ->
            (\x y -> [x..y]) <$> readMay from <*> readMay to
          _ -> Nothing

getInput :: IORef (Branch IO) -> IORef Path.Absolute -> IORef [String] -> Codebase IO v a -> IO Input 
getInput rootRef pathRef numberedArgsRef codebase = do
  root <- readIORef rootRef
  path <- readIORef pathRef
  numberedArgs <- readIORef numberedArgsRef
  getUserInput patternMap codebase root path numberedArgs
  where patternMap = Map.fromList $ validInputs >>= (\p -> (patternName p, p) : ((, p) <$> aliases p))

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch m
  -> Path.Absolute
  -> [String]
  -> m Input
getUserInput patterns codebase branch currentPath numberedArgs = Line.runInputT
  settings
  go
 where
  go = do
    line <- Line.getInputLine
      $ P.toANSI 80 ((P.green . P.shown) currentPath <> fromString prompt)
    case line of
      Nothing -> pure QuitI
      Just l  -> case words l of
        [] -> go
        ws ->
          case parseInput patterns . (>>= expandNumber numberedArgs) $ ws of
            Left msg -> do
              liftIO $ putPrettyLn msg
              go
            Right i -> pure i
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


awaitInput :: 
  [Either Event Input] 
  -> Q.TQueue Event
  -> IORef Bool
  -> IORef (Branch IO)
  -> IORef Path.Absolute 
  -> IORef [String]
  -> Codebase IO Symbol Ann
  -> IO (Either Event Input)
awaitInput initialInputs eventQueue pageOutput rootRef pathRef numberedArgsRef codebase = do -- await input ends up encompassing initial inputs (for welcome) and the user inputs 
  -- use up buffered input before consulting external events
  initialInputsRef         <- newIORef initialInputs
  i <- readIORef initialInputsRef -- Here was where we used to do the reading for base commands 
  (case i of
    h:t -> writeIORef initialInputsRef t >> pure h -- Here was where we used to write the IO of commands to the event queue. Will need to mimic in an new function
    [] -> -- this means the initial inputs are done 
      -- Race the user input and file watch.
      Async.race (atomically $ Q.peek eventQueue) (getInput rootRef pathRef numberedArgsRef codebase) >>= \case
        Left _ -> do
          let e = Left <$> atomically (Q.dequeue eventQueue)
          writeIORef pageOutput False
          e
        x      -> do
          writeIORef pageOutput True
          pure x) --`catch` interruptHandler 
    --interuptHandler (asyncExceptionFromException -> Just UserInterrupt) = awaitInput initialInputs eventQueue pageOutput rootRef pathRef numberedArgsRef codebase
    --interruptHandler e = error (show e)

